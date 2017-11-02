open System
open System.Text.RegularExpressions

open System.Threading.Tasks
open System.IO

open SlackConnector
open SlackConnector.Models
open SlackConnector.EventHandlers

let filepath = 
    Path.Combine(__SOURCE_DIRECTORY__, "text.txt")

let splitOnSpace text = Regex.Split(text, @"\s+")

let getWordPairs pairSize filePath =
    File.ReadAllText filePath
    |> splitOnSpace
    |> Seq.windowed pairSize

let joinWords words = String.concat " " words

let bisectWords (arr:_[]) =
    let len = Array.length arr
    let preds = arr |> Seq.take (len - 1)
    (preds |> joinWords, arr.[len - 1])
    

let updateMap (map:Map<_,_>) key value =
    if map.ContainsKey key then
        let existingValue = map.[key]
        let map = map |> Map.remove key
        map |> Map.add key (value :: existingValue)
    else
        map.Add(key, [value])

let mapBuilder map words = bisectWords words ||> updateMap map

let buildMap = Seq.fold mapBuilder Map.empty

let map = buildMap <| getWordPairs 3 filepath

let isStartSentence = Regex(@"^[A-Z]").IsMatch 

let startWords, otherWords = map |> Map.partition (fun k _ -> isStartSentence k)

let startWordArray = map |> Seq.map (fun kvp -> kvp.Key) |> Array.ofSeq

let rng = Random()

let getRandomItem seq =
    let randIndex = rng.Next (Seq.length seq)
    seq |> Seq.nth randIndex

let (|IsEndOfSentence|) = Regex(@"(?<!Mr(s)?)[\.\?\!]$").IsMatch

let combineWords prev next =
    [prev; next]
    |> List.filter (fun s -> not (String.IsNullOrWhiteSpace s))
    |> joinWords

let rec markovChain state acc =
    let nextChoice = map.[state] |> getRandomItem
    match nextChoice with
    | IsEndOfSentence true ->
        nextChoice :: acc
    | IsEndOfSentence false ->
        let currWords = state |> splitOnSpace |> Seq.skip 1 |> joinWords
        markovChain (combineWords currWords nextChoice) (nextChoice :: acc)
        
let getMarkovSentence startWord =
    markovChain startWord [startWord]
    |> List.rev
    |> joinWords

let receiveMessageFrom (connection: ISlackConnection) =
    fun (msg: SlackMessage) ->
        if msg.MentionsBot then
            let text = getMarkovSentence (getRandomItem startWords).Key
            connection.Say(BotMessage(ChatHub=msg.ChatHub,Text=text))
        else
            new Task(fun () -> ())
            
let initBot (botAccessToken: string) =
    let rec initBot' () =
        try
            let connection = SlackConnector().Connect(botAccessToken).Result
            connection.add_OnMessageReceived (MessageReceivedEventHandler(receiveMessageFrom(connection)))
            connection.add_OnDisconnect (DisconnectEventHandler initBot')
            ()
        with
        | e ->
            printfn "%s" e.InnerException.Message
            initBot'()

    initBot'()

let token = Environment.GetEnvironmentVariable("TOKEN")

[<EntryPoint>]
let main argv =
    initBot token
    Console.ReadLine() |> ignore
    0
// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Network.Network
open Network.GeneticTrainer

type side = Left | Right
    with override this.ToString() =
          match this with
          | Left -> "{Left}"
          | Right -> "{Right}"
let maxValueInOutput (one::two::[]) = if (one > two) then Left else Right

let generateRandomNumberPairs (count : int) =
    let maxValueInPair (one, two) = if (one > two) then Left else Right
    let rnd = System.Random()
    let generateRandomList(rnd: System.Random) = Array.init count (fun _ -> rnd.NextDouble()*0.0000001)
    let pairs = Array.zip (generateRandomList(rnd)) (generateRandomList(rnd))
    Array.map (fun pair -> (maxValueInPair(pair)), pair) pairs

let personalTrainer inputs (network: Network) =
    let difference one two = if (one > two) then one - two else two - one 
    let result = Array.fold (fun acc (biggest, (left, right)) -> if ((maxValueInOutput (Array.toList (network.feedForward(List.toArray [left;right])))) = biggest) then (acc + 1.0 - difference left right) else acc) 0.0 inputs
    result

[<EntryPoint>]
let main argv = 
    let newNet = Network.Network.Network(List.toArray [2])
    let inputs = (generateRandomNumberPairs 2500)
    let test = (generateRandomNumberPairs 100000)
    let result = trainGenerations 4 newNet 100 inputs 1.0 (personalTrainer)
    let sum = Array.fold (fun acc (biggest, (left, right)) -> if ((maxValueInOutput (Array.toList (result.feedForward([|left;right|])))) = biggest) then acc + 1 else acc) 0 test
    printfn "%A" sum
    printfn "%A" (result.feedForward([|0.0; 1.0|]))
    printfn "%A" (result.feedForward([|1.0; 0.0|]))
    0

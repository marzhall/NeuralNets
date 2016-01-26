// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Network.Network
open System.Linq

type side = Left | Right
    with override this.ToString() =
          match this with
          | Left -> "{Left}"
          | Right -> "{Right}"
let maxValueInOutput (one::two::[]) = if (one > two) then Left else Right

let generateRandomNumberPairs (count : int) =
    let maxValueInPair (one, two) = if (one > two) then Left else Right
    let rnd = System.Random()
    let generateRandomList(rnd: System.Random) = List.init count (fun _ -> rnd.NextDouble())
    let pairs = List.zip (generateRandomList(rnd)) (generateRandomList(rnd))
    List.map (fun pair -> (maxValueInPair(pair)), pair) pairs

let rec trainGenerations (numGenerations:int) (seedNetwork : Network)  (generationSize:int) (testInputs: (side*(float*float)) list) (learningRate:double) trainingFunction =
    let testGeneration =
        let generation = List.init generationSize (fun _ -> seedNetwork.mutatedClone(learningRate))
        let best = List.maxBy (trainingFunction testInputs) (seedNetwork::generation)
        best
    match numGenerations with
        | x  when x <= 0 -> testGeneration
        | x              -> testGeneration |> (fun x -> trainGenerations (numGenerations - 1) x generationSize testInputs (learningRate*(0.86)) trainingFunction)
        
let personalTrainer inputs (network: Network) =
    let difference one two = if (one > two) then one - two else two - one 
    let result = List.fold (fun acc (biggest, (left, right)) -> if ((maxValueInOutput (network.feedForward([left;right]))) = biggest) then (acc + 1.0 + difference left right) else acc) 0.0 inputs
    result
            
[<EntryPoint>]
let main argv =
    let newNet = Network.Network.Network([2])
    let inputs = (generateRandomNumberPairs 1000)
    let test = (generateRandomNumberPairs 1000000)
    let result = trainGenerations 30 newNet 100 inputs 1.0 (personalTrainer)
    let sum = List.fold (fun acc (biggest, (left, right)) -> if ((maxValueInOutput (result.feedForward([left;right]))) = biggest) then acc + 1 else acc) 0 test
    printfn "%A" sum
    printfn "%A" (result.feedForward([0.0; 1.0]))
    printfn "%A" (result.feedForward([1.0; 0.0]))
    0 // return an integer exit code

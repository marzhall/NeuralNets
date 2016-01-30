namespace Network

open Network.Network

module GeneticTrainer =
    let rec trainGenerations (numGenerations:int) (seedNetwork:Network)  (generationSize:int) (testInputs: 'a []) (learningRate:double) (trainingFunction : (('a []) -> Network.Network.Network -> 'b)) =
        let testGeneration =
            let generation = Array.Parallel.init generationSize (fun _ -> seedNetwork.mutatedClone(learningRate))
            let best = Array.maxBy (trainingFunction testInputs) (Array.append (generation) [|seedNetwork|])
            printfn "%A generations to go." numGenerations
            best
        match numGenerations with
            | x  when x <= 0 -> testGeneration
            | x              -> testGeneration |> (fun x -> trainGenerations (numGenerations - 1) x generationSize testInputs learningRate trainingFunction)
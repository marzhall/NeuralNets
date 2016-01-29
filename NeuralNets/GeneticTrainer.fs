namespace Network

open Network.Network

module GeneticTrainer =
    let rec trainGenerations (numGenerations:int) (seedNetwork:Network)  (generationSize:int) (testInputs: 'a list) (learningRate:double) (trainingFunction : (('a list) -> Network.Network.Network -> 'b)) =
        let testGeneration =
            let generation = List.init generationSize (fun _ -> seedNetwork.mutatedClone(learningRate))
            let best = List.maxBy (trainingFunction testInputs) (seedNetwork::generation)
            best
        match numGenerations with
            | x  when x <= 0 -> testGeneration
            | x              -> testGeneration |> (fun x -> trainGenerations (numGenerations - 1) x generationSize testInputs learningRate trainingFunction)
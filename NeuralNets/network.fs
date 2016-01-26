namespace Network

open NeuralNet

module Network =
    type Network(layersSizes:int list) =
        let rnd = new System.Random()
        member val layers = List.map (fun (x:int) -> Array.toList [| for i in 1 .. x -> new NeuralNet.Node.Node(rnd)|]) layersSizes with get, set
        member this.feedForward(inputs:float list) =
            List.fold (fun acc layer -> (List.map (fun (x:NeuralNet.Node.Node) -> x.calcCharge(acc)) layer)) inputs this.layers
        member this.clone() = this.MemberwiseClone() :?> Network
        member this.mutatedClone(mutationRate) =
            let mutatedLayers = List.map(fun layer -> List.map (fun (node:NeuralNet.Node.Node) -> node.mutate(mutationRate)) layer) this.layers
            let mutatedclone = this.clone()
            mutatedclone.layers <- mutatedLayers
            mutatedclone
        with override this.ToString() = 
               (sprintf "%A" this.layers)
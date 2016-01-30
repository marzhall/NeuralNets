namespace Network

open NeuralNet

module Network =
    type Network(layersSizes:int []) =
        let rnd = new System.Random()
        member val layers = Array.map (fun (x:int) -> [| for i in 1 .. x -> new NeuralNet.Node.Node(rnd)|]) layersSizes with get, set
        member this.feedForward(inputs:float []) =
            Array.fold (fun acc layer -> (Array.map (fun (x:NeuralNet.Node.Node) -> x.calcCharge(acc)) layer)) inputs this.layers
        member this.clone() = this.MemberwiseClone() :?> Network
        member this.mutatedClone(mutationRate) =
            let mutatedLayers = Array.map(fun layer -> Array.map (fun (node:NeuralNet.Node.Node) -> node.mutate(mutationRate)) layer) this.layers
            let mutatedclone = this.clone()
            mutatedclone.layers <- mutatedLayers
            mutatedclone
        with override this.ToString() = 
               (sprintf "%A" this.layers)
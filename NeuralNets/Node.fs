namespace NeuralNet

module Node =

    type Node(rnd:System.Random) =
        let getRandomFloats count = 
            List.init count (fun _ -> rnd.NextDouble())
        let choice func1 func2 = if (rnd.Next() % 2 = 0) then func1 else func2
        let randomNegative = (*) (choice 1.0 (-1.0))
        member val weights = [] with get, set
        member val bias = 0.0 with get, set
        member this.clone() = this.MemberwiseClone() :?> Node
        member this.mutate(learningRate : double) =
            let result = this.clone()
            let mutatedBias = this.bias + randomNegative(choice (0.0) (rnd.NextDouble()*learningRate))
            let mutatedWeights = List.map (fun x -> x + (randomNegative (rnd.NextDouble()*learningRate))) this.weights
            result.bias <- mutatedBias
            result.weights <- mutatedWeights
            result
        member this.calcCharge(layer:float list) =
            this.weights <- (if (this.weights = []) then getRandomFloats (List.length(layer)) else this.weights)
            let sigmoid (x:float) = 1.0 / (1.0 + System.Math.Exp(x))
            let result = this.bias + List.sumBy(fun (x:float, y:float) -> x*y) (List.zip layer this.weights) in
            sigmoid(result)
        with override this.ToString() = 
               (sprintf "{Node bias: %A, nodes: %A}" this.bias this.weights)
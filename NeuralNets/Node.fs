namespace NeuralNet

module Node =

    type Node(rnd:System.Random) =
        let getRandomFloats count = 
            Array.init count (fun _ -> lock rnd (fun () -> 0.5))
        let choice func1 func2 = lock rnd (fun () -> if (rnd.Next() % 2 = 0) then func1 else func2)
        let randomNegative = (*) (choice 1.0 (-1.0))
        member val weights = [||] with get, set
        member val bias = 0.0 with get, set
        member this.clone() = this.MemberwiseClone() :?> Node
        member this.mutate(learningRate : double) =
            let result = this.clone()
            let mutatedBias = this.bias + lock rnd (fun () -> randomNegative(rnd.NextDouble()*learningRate))
            let mutatedWeights = Array.map (fun x -> x + (randomNegative (lock rnd (fun () -> rnd.NextDouble()*learningRate)))) this.weights
            result.bias <- mutatedBias
            result.weights <- mutatedWeights
            result
        member this.calcCharge(layer:float []) =
            this.weights <- (if (this.weights = List.toArray []) then getRandomFloats (Array.length(layer)) else this.weights)
            let exponented x = System.Math.Exp(x)
            let sigmoid (x:float) = 1.0 / (1.0 + (exponented (-1.0 * x)))
            let result = this.bias + Array.sumBy(fun (x:float, y:float) -> x*y) (Array.zip layer this.weights) in
            sigmoid(result)
        with override this.ToString() = 
               (sprintf "{Node bias: %A, nodes: %A}" this.bias this.weights)
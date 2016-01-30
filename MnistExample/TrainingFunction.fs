namespace mnist
open Network.Network
open mnist.pictureReader

module trainingFunction = 
    let trainingFunction (inputs:((Label * float []) [])) (network:Network.Network.Network) =
        let getLabelFromNeuronArray neurons = match (Array.tryFindIndex(fun x -> x = Array.max(neurons)) neurons) with
                                                | None -> Mismatch
                                                | Some(guy) -> match guy with
                                                               | 0 -> One
                                                               | 1 -> Two
                                                               | 2 -> Three
                                                               | 3 -> Four
                                                               | 4 -> Five
                                                               | 5 -> Six
                                                               | 6 -> Seven
                                                               | 7 -> Eight
                                                               | 8 -> Nine
                                                               | 9 -> Ten
                                                               | _ -> Mismatch

        let getResult (label, image) = if (getLabelFromNeuronArray (network.feedForward(image))) = label then 1 else 0
        let results = Array.Parallel.map getResult inputs
        let numCorrect = results |> Array.reduce (+)
        printfn "%A" numCorrect
        numCorrect
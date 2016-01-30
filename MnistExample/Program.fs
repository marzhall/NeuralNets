// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open mnist.pictureReader
open Network.Network
open Network.GeneticTrainer
open mnist


[<EntryPoint>]
let main argv = 
    let labels = mnistLabelReader @"C:\Users\marshall\Downloads\mnist\train-labels.idx1-ubyte"
    let images = mnistImageReader @"C:\Users\marshall\Downloads\mnist\train-images.idx3-ubyte"
    let labelsAndImages = Array.zip labels images
    let pixelsPerImage = 784
    let numberOfLabels = 9
    let mnister = new Network([|10;numberOfLabels|])
    trainingFunction.trainingFunction labelsAndImages mnister |> ignore
    let result = trainGenerations 200 mnister 5 (labelsAndImages) 1.0 (trainingFunction.trainingFunction)

    printfn "%A" (trainingFunction.trainingFunction labelsAndImages result)
    0 // return an integer exit code
     
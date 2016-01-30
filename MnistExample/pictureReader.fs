namespace mnist
open System
open System.IO
open System.Net

module pictureReader =

    type Label = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Mismatch
    let translateByteToLabel byteToTranslate = match byteToTranslate with
                                                | 0uy -> One
                                                | 1uy -> Two
                                                | 2uy -> Three
                                                | 3uy -> Four
                                                | 4uy -> Five
                                                | 5uy -> Six
                                                | 6uy -> Seven
                                                | 7uy -> Eight
                                                | 8uy -> Nine
                                                | 9uy -> Ten
                                                | _ -> Mismatch
    

    let make4ByteArrayFromByte value = List.toArray [value; Byte(); Byte(); Byte()]
    let byteArrayToFloatArray (byteArray : Byte []) = Array.Parallel.map (fun x -> (float (BitConverter.ToInt32((make4ByteArrayFromByte x), 0)))/255.0) byteArray
    
    let mnistLabelReader filename =
        use reader = new BinaryReader(File.Open(filename, FileMode.Open, FileAccess.Read, FileShare.None))
        let magicNumber = IPAddress.NetworkToHostOrder(reader.ReadInt32())
        magicNumber |> ignore
        let numberOfItems = IPAddress.NetworkToHostOrder(reader.ReadInt32())
        let items = [| for i in 1 .. (numberOfItems) -> i |] |> Array.map (fun x -> translateByteToLabel (reader.ReadByte()))
        items

    let mnistImageReader filename =
        use reader = new BinaryReader(File.Open(filename, FileMode.Open, FileAccess.Read, FileShare.Read))
        let magicNumber = IPAddress.NetworkToHostOrder(reader.ReadInt32())
        magicNumber |> ignore
        let numberOfImages = IPAddress.NetworkToHostOrder(reader.ReadInt32())
        let numberOfRows = IPAddress.NetworkToHostOrder(reader.ReadInt32())
        let numberOfColumns = IPAddress.NetworkToHostOrder(reader.ReadInt32())
        let image = [| for i in 1 .. (numberOfColumns*numberOfRows) -> i |] |> Array.map (fun x -> reader.ReadByte())
        let images = [| for i in 1 .. (numberOfImages) -> byteArrayToFloatArray image|]
        images
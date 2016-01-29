namespace mnist
open System
open System.IO
open System.Net

module pictureReader =

    let mnistLabelReader filename =
        use reader = new BinaryReader(File.Open(filename, FileMode.Open, FileAccess.Read, FileShare.Read))
        let magicNumber = IPAddress.NetworkToHostOrder(reader.ReadInt32())
        magicNumber |> ignore
        let numberOfItems = IPAddress.NetworkToHostOrder(reader.ReadInt32())
        let items = [| for i in 1 .. (numberOfItems) -> i |] |> Array.map (fun x -> reader.ReadByte())
        items

    let mnistImageReader filename =
        use reader = new BinaryReader(File.Open(filename, FileMode.Open, FileAccess.Read, FileShare.Read))
        let magicNumber = IPAddress.NetworkToHostOrder(reader.ReadInt32())
        magicNumber |> ignore
        let numberOfImages = IPAddress.NetworkToHostOrder(reader.ReadInt32())
        let numberOfRows = IPAddress.NetworkToHostOrder(reader.ReadInt32())
        let numberOfColumns = IPAddress.NetworkToHostOrder(reader.ReadInt32())
        let image = Array.toList [| for i in 1 .. (numberOfColumns*numberOfRows) -> i |] |> List.map (fun x -> reader.ReadByte())
        let images = Array.toSeq [| for i in 1 .. (numberOfImages) -> image|]
        images
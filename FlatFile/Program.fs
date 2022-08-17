open System
open FlatFile
    

let mbc = 
    Schema.Fixed "," [
        FixedWidth.Int      (name = "Id",   width = 10, padding = Left '0')
        FixedWidth.String   (name = "Tag",  width = 5)
        FixedWidth.Int      (name = "Age",  width = 3,  padding = Left '0')
        FixedWidth.DateTime (name = "Dob",  width = 10, format = "yyyy-MM-dd")
    ]

type WriteModel =
    { Id: int; Tag: Guid; Age: int; Dob: DateTime }

let data = 
    [0 .. 10 .. 1000]
    |> List.map (fun i ->
        { Id = i; Tag = Guid.NewGuid(); Age = i; Dob = (DateTime(1989, 01, 12)) })

let res = Writer.write mbc data

printfn "%s" res
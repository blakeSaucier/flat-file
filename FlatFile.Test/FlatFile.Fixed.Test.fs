module FlatFile.Fixed.Test

open System
open Xunit
open FlatFile
open Expecto

type Person = { Id: int; Name: string; Dob: DateTime }

module Writer =
  [<Fact>]
  let ``should compare two schemas`` () =
    let schema = Schema.Fixed "|" [
      FixedWidth.Int("Id", 10)
      FixedWidth.String("Name", 20)
      FixedWidth.DateTime("Dob", 20, "yyyy-MM-dd")
    ]
    
    let data = [
      { Id = 2121; Name = "Blake Saucier"; Dob = DateTime(1980, 07, 24) }
    ]
    
    let expected =
      "      2121|       Blake Saucier|          1980-07-24"
      
    let res = Writer.write schema data
    Expect.equal res expected "should write fixed width"
    
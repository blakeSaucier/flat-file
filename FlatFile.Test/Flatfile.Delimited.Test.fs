module FlatFile.Delimited.Test

open System
open Xunit
open FlatFile
open FlatFile.Writer

type Person = { Id: int; Name: string; Dob: DateTime }

module Writer =
    [<Fact>]
    let ``should write a csv file`` () =
        let data = [
            { Id = 2121; Name = "Blake Saucier"; Dob = DateTime(1980, 07, 24) }
        ]
        let schema = Schema.Csv [
            Delimited.Int "Id"
            Delimited.String "Name"
            Delimited.DateTime ("Dob", "yyyy-MM-dd")
        ]
        
        let expected = "2121,Blake Saucier,1980-07-24"
        let res = write schema data
        Assert.Equal(expected = expected, actual = res)
    
module Reader =
    [<Fact>]
    let ``should read a csv file`` () =
        let data = "2121,Blake Saucier,1980-07-24"

        let expected = [
            { Id = 2121
              Name = "Blake Saucier"
              Dob = DateTime(1980, 07, 24) }
        ]
        
        let schema = Schema.Csv [
            Delimited.Int "Id"
            Delimited.String "Name"
            Delimited.DateTime ("Dob", "yyyy-MM-dd")
        ]
        
        let res = Reader.read<Person> schema data
        Assert.Equal(actual = res.Head, expected = expected.Head)

module FlatFile

open System
open Microsoft.FSharp.Core

type Separator = string
type ColumnName = string
type DateFormat = string
type Truncate = Left | Right

type Pad = 
    | Left of char
    | Right of char

type FixedSize =
    { Name: ColumnName
      Padding: Pad Option
      Truncation: Truncate
      Width: int }

type IColumn =
    abstract member Name : string

type FixedColumn  =
    | FixedStringColumn of FixedSize
    | FixedIntColumn of FixedSize
    | FixedDateColumn of FixedSize * DateFormat
    member this.FixedSize = 
        match this with 
        | FixedDateColumn (a, _) -> a 
        | FixedIntColumn s -> s
        | FixedStringColumn s -> s
    interface IColumn with
        member this.Name =
            match this with
            | FixedStringColumn def -> def.Name
            | FixedIntColumn def -> def.Name
            | FixedDateColumn (def, _) -> def.Name

type DelimitedColumn =
    | DelimitedStringColumn of ColumnName
    | DelimitedIntColumn of ColumnName
    | DelimitedDateColumn of ColumnName * DateFormat
    interface IColumn with
        member this.Name =
            match this with
            | DelimitedStringColumn c -> c
            | DelimitedIntColumn c -> c
            | DelimitedDateColumn (c, _) -> c

type ISchema =
    abstract member Separator: string

type DelimitedSchema =
    { Separator: Separator 
      Columns: DelimitedColumn list }

type FixedSchema =
    { Separator: Separator option
      Columns: FixedColumn list }

type Schema =
    | DelimitedSchema of DelimitedSchema
    | FixedSchema of FixedSchema
    member this.Separator =
        match this with
        | DelimitedSchema s -> s.Separator
        | FixedSchema fixedSchema -> fixedSchema.Separator |> Option.defaultValue ""

    /// Schema for Comma Separated Values 
    static member Csv columns =
        { DelimitedSchema.Separator = ","
          Columns = columns } |> DelimitedSchema

    /// Define a Fixed Width schema
    static member Fixed separator columns =
        { FixedSchema.Separator = Some separator
          Columns = columns } |> FixedSchema

type FixedWidth =
    /// Fixed with column for integers. 
    /// Defaults to blank character padding and Truncates from the left
    static member Int(name: string, width: int, ?padding: Pad, ?truncation: Truncate) =
        FixedIntColumn 
            { Name = name
              Padding = padding
              Truncation = truncation |> Option.defaultValue Truncate.Right
              Width = width }

    static member String(name: string, width: int, ?padding, ?truncation) =
        FixedStringColumn
            { Name = name
              Padding = padding
              Truncation = truncation |> Option.defaultValue Truncate.Right
              Width = width }
    
    static member DateTime(name: string, width, format: string, ?padding, ?truncation) =
        FixedDateColumn
            ({ Name = name
               Width = width
               Truncation = truncation |> Option.defaultValue Truncate.Right
               Padding = padding }, format)

type Delimited =
    static member Int = DelimitedIntColumn
    static member String = DelimitedStringColumn
    static member DateTime = DelimitedDateColumn


module Writer =
    open FSharp.Reflection
    open System.Reflection

    let findProperty (column: IColumn) (properties: Map<string, PropertyInfo>) =
        properties
        |> Map.tryFind column.Name
        |> function
            | None -> failwith $"Cannot find data source for column: {column.Name}. "
            | Some p -> p

    let trimOrPadColumn (column: FixedSize) (s:string) =
        match s.Length with
        | len when len > column.Width ->
            match column.Truncation with
            | Truncate.Right -> s.Substring(0, column.Width)
            | Truncate.Left -> s.Substring(len - column.Width, column.Width)
        | len when len < column.Width ->
            match column.Padding with
            | None -> s.PadLeft(column.Width, ' ') // Default to pad left with spaces
            | Some (Pad.Left ch) -> s.PadLeft(column.Width, ch)
            | Some (Pad.Right ch) -> s.PadRight(column.Width, ch)  
        | _ -> s

    let safeToString  (toStr: obj -> string) o =
        o |> function | null -> "" | _ -> toStr o

    let writeFixedColumn (column: FixedColumn) (data: 'a) (prop: PropertyInfo) =
        match column with
        | FixedIntColumn def -> 
            FSharpValue.GetRecordField(data, prop) 
            |> safeToString (fun o -> o.ToString())
        | FixedStringColumn def ->
            FSharpValue.GetRecordField(data, prop)
            |> safeToString (fun o -> o.ToString())
        | FixedDateColumn (def, formatting) ->
            FSharpValue.GetRecordField(data, prop)
            |> safeToString (fun o -> (o :?> DateTime).ToString(formatting))
        |> trimOrPadColumn column.FixedSize

    let writeFixedWidthRow (schema: FixedSchema) (propsDict: Map<string, Reflection.PropertyInfo>) (data : 'a) =
        let separator = schema.Separator |> Option.defaultValue ""
        schema.Columns
        |> List.map (fun c ->
            let p = findProperty c propsDict
            writeFixedColumn c data p)
        |> String.concat separator

    let writeDelimitedColumn (column: DelimitedColumn) (data: 'a) (prop: PropertyInfo) =
        match column with
        | DelimitedStringColumn name ->
            FSharpValue.GetRecordField(data, prop)
            |> safeToString (fun o -> o.ToString())
        | DelimitedIntColumn name ->
            FSharpValue.GetRecordField(data, prop)
            |> safeToString (fun o -> o.ToString())
        | DelimitedDateColumn (name, f) ->
            FSharpValue.GetRecordField(data, prop)
            |> safeToString (fun o -> (o :?> DateTime).ToString(f))

    let writeDelimitedRow (schema: DelimitedSchema) (props: Map<string, Reflection.PropertyInfo>) (data: 'a) =
        schema.Columns
        |> List.map (fun c ->
            let p = findProperty c props
            writeDelimitedColumn c data p)
        |> String.concat schema.Separator

    let write (schema: Schema) (data: 'a seq)  =
        let t = typeof<'a>
        let propsDict =
            t.GetProperties()
            |> Array.map (fun p -> p.Name, p) |> Map
        
        match schema with
        | FixedSchema fixedSchema ->
            data
            |> Seq.map (writeFixedWidthRow fixedSchema propsDict)
            |> String.concat Environment.NewLine
        | DelimitedSchema delimited ->
            data 
            |> Seq.map (writeDelimitedRow delimited propsDict)
            |> String.concat Environment.NewLine
            
    let writeFile (schema: Schema) (fileName: string) (data: 'a seq) =
        let res = write schema data
        ()
        
            
module Reader =
    open System
    open FSharp.Reflection
    open System.Reflection
    
    let readDelimitedLine<'a> (delimitedSchema: DelimitedSchema) (line: string) =
        let props = typeof<'a>
        let rawValues = line.Split(delimitedSchema.Separator) |> Array.toList
        let columns = delimitedSchema.Columns
        
        let parsed =
            (rawValues, columns)
            ||> List.map2 (fun raw column ->
                match column with
                | DelimitedIntColumn s -> Int32.Parse(raw) |> box
                | DelimitedStringColumn s ->
                    raw |> function | null -> "" | _ -> raw |> box
                | DelimitedDateColumn (s, s1) -> DateTime.Parse(raw) |> box)
            |> List.toArray
        
        FSharpValue.MakeRecord(recordType = props, values = parsed) :?> 'a
    
    let read<'a>(schema: Schema) (csv: string) =
        
        let lines = csv.Split(Environment.NewLine)
                    |> Array.filter (fun line -> String.IsNullOrWhiteSpace(line) |> not)
                    |> Array.toList
        match schema with
        | DelimitedSchema delimited ->
            lines
            |> List.map (readDelimitedLine delimited)
        | FixedSchema fixedSchema ->
            let res : 'a list =[]
            res
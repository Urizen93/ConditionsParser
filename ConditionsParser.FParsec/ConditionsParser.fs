namespace ConditionsParser.FParsec

open FParsec
open FParsec.Pipes

type Operator =
    | Equals
    | Greater
    | GreaterOrEqual
    | Less
    | LessOrEqual
    | In
    | NotIn
    | Like
    | NotLike
    
type LogicalOperator = And | Or

type Condition = {
    Column : string
    Operator : Operator
    Value : string // Probably should be of type like Value = Literal | Geometry
}

type Expression = {
    Head : Condition
    Tail : (LogicalOperator * Condition) list
}

module ConditionsParser =
    let allowedOperator = %[
        %'=' >>% Equals
        %">=" >>% GreaterOrEqual
        %'>' >>% Greater
        %"<=" >>% LessOrEqual
        %'<' >>% Less
        %ci "IN" >>% In
        %ci "NOT IN" >>% NotIn
        %ci "LIKE" >>% Like
        %ci "NOT LIKE" >>% NotLike
    ]
    let logicalOperator = %[
        %ci " AND " >>% And
        %ci " OR " >>% Or
    ]
    let identifier = many1Chars2 letter (letter <|> digit)
    let propertyAccessor =
        %% +."->" -- +.identifier
        -|> fun arrow property -> arrow + property
    let columnName =
        identifier
        .>>. (attempt (propertyAccessor |>> Some) <|>% None)
        |>> fun (column, maybePropertyAccessor) -> match maybePropertyAccessor with
                                                   | Some propertyAccessor -> column + propertyAccessor
                                                   | None -> column
    
    let endOfValue = logicalOperator |>> ignore <|> eof
    let value = many1CharsTill anyChar <| lookAhead endOfValue
    
    // Here is an example of more specific value parsing;
    // I don't know what exactly should be allowed as I'm far from understanding all the cases
    // let stringLiteral = %% ''' -- +.(manyCharsTill anyChar %''') -%> auto
    // let array = %% '(' -- +.(manyCharsTill anyChar %')') -|> sprintf "({%s})"
    // let manyCommaSeparated element = %% +.(qty[0..] / ',' * element) -%> auto
    // let intBetweenSpaces = %% spaces -- +.p<int32> -- spaces -%> auto
    //
    // let commaSeparated (writer : TextWriter) args =
    //     writer.Write (Seq.map (sprintf "%d") args |> String.concat ", ")
    //     writer
    //     
    // let polygon = %% %ci "POLYGON(" -- spaces -- +. (manyCommaSeparated intBetweenSpaces) -- %')'
    //               -|> sprintf "POLYGON(%a)" commaSeparated
    // let value1 =
    //     pfloat |>> sprintf "%f" <|> stringLiteral <|> array <|> polygon
    
    let condition =
        %% spaces -- +.columnName
        -- spaces -- +.allowedOperator
        -- spaces -- +.value
        -|> fun columnName operator value -> {
            Column = columnName
            Operator = operator
            Value = value.Trim()
        }
        
    let conditionsParser =
        %% +.condition -- +.(many (logicalOperator .>>. condition))
        -|> fun head tail -> { Head = head; Tail = tail }
    
    let parseConditions input =
        match runParserOnString conditionsParser () "" input with
        | Success (result, _, _) -> Choice1Of2 result
        | Failure (error, _, _) -> Choice2Of2 error
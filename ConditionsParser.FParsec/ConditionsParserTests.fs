module Tests
    open ConditionsParser.FParsec.ConditionsParser
    open Xunit
    open Xunit.Abstractions
    
    type ``parseConditions tests`` (output : ITestOutputHelper) =
        
        [<Fact>]
        member _.``Should parse a complex expression`` () =
            let input = " ID <= 5.0 AND    Name > 'Taras' OR Location->Geometry=   POLYGON(1, 12)   "
            match parseConditions input with
            | Choice1Of2 result -> output.WriteLine $"%A{result}"
            | Choice2Of2 error -> Assert.Fail(error)
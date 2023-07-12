using Xunit;
using Xunit.Abstractions;
using static ConditionsParser.LanguageExtParsec.ConditionsParser;
using static LanguageExt.Prelude;

namespace ConditionsParser.LanguageExtParsec;

public class ExpressionParserTests
{
    private readonly ITestOutputHelper _output;

    public ExpressionParserTests(ITestOutputHelper output) => _output = output;

    [Fact]
    public void ShouldParseComplexExpression()
    {
        // LanguageExt port doesn't supply many1Until function
        // So it's non trivial to make it not consume empty values
        //   POLYGON(1, 12)   
        const string input = " ID <= 5.0 AND    Name > 'Taras' OR Location->Geometry=";
        ParseExpression(input)
            .Map(toString)
            .Match(_output.WriteLine, Assert.Fail);
    }
}
using LanguageExt;
using LanguageExt.ClassInstances;
using static LanguageExt.Prelude;
using static LanguageExt.Parsec.Prim;
using static LanguageExt.Parsec.Char;

namespace ConditionsParser.LanguageExtParsec;

public enum Operator
{
    Equals,
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
    In,
    NotIn,
    Like,
    NotLike
}

public enum LogicalOperator
{
    And,
    Or
}

public sealed record Condition(string Column, Operator Operator, string Value);

public sealed record Expression(Condition Head, Seq<(LogicalOperator, Condition)> Tail);

public static class ConditionsParser
{
    static ConditionsParser()
    {
        var operatorParser = choice(
            ch('=').Map(_ => Operator.Equals),
            attempt(str(">=")).Map(_ => Operator.GreaterOrEqual),
            ch('>').Map(_ => Operator.Greater),
            attempt(str("<=")).Map(_ => Operator.LessOrEqual),
            ch('<').Map(_ => Operator.Less),
            attempt(str<EqCharOrdinalIgnoreCase>("IN")).Map(_ => Operator.In),
            attempt(str<EqCharOrdinalIgnoreCase>("NOT IN")).Map(_ => Operator.NotIn),
            attempt(str<EqCharOrdinalIgnoreCase>("LIKE")).Map(_ => Operator.Like),
            attempt(str<EqCharOrdinalIgnoreCase>("NOT LIKE")).Map(_ => Operator.NotLike)
        );

        var logicalOperatorParser = choice(
            attempt(str<EqCharOrdinalIgnoreCase>(" AND ")).Map(_ => LogicalOperator.And),
            attempt(str<EqCharOrdinalIgnoreCase>(" OR ")).Map(_ => LogicalOperator.Or)
        );

        var identifierParser =
            from firstChar in letter
            from theRest in asString(many1(either(letter, digit)))
            select firstChar + theRest;
        
        var propertyAccessorParser =
            from arrow in str("->")
            from property in identifierParser
            select arrow + property;

        var columnNameParser =
            from column in identifierParser
            from propertyAccessor in optionOrElse("", propertyAccessorParser)
            select column + propertyAccessor;

        var endOfValueParser = either(logicalOperatorParser.Map(ignore), eof);
        var valueParser = asString(manyUntil(anyChar, lookAhead(endOfValueParser)));

        var conditionParser =
            from _ in spaces
            from columnName in columnNameParser
            from __ in spaces
            from @operator in operatorParser
            from ___ in spaces
            from value in valueParser
            select new Condition(columnName, @operator, value.Trim());

        var tailParser =
            from logicalOperator in logicalOperatorParser
            from condition in conditionParser
            select (logicalOperator, condition);

        var expressionParser =
            from head in conditionParser
            from tail in many(tailParser)
            select new Expression(head, tail);

        ParseExpression = input =>
        {
            var result = parse(expressionParser, input);
            return result.IsFaulted
                ? Left(result.Reply.Error.ToString()).Bind<Expression>()
                : Right(result.Reply.Result);
        };
    }
    
    public static Func<string, Either<string, Expression>> ParseExpression { get; }
}
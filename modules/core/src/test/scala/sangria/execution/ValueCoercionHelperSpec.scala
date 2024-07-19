package sangria.execution

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import sangria.ast
import sangria.marshalling.FromInput
import sangria.parser.QueryParser
import sangria.schema._
import sangria.validation.{NotExactlyOneOfField, Violation}

import scala.util.{Failure, Success, Try}

class ValueCoercionHelperSpec extends AnyWordSpec with Matchers {
  "ValueCoercionHelper" should {
    "converts according to input coercion rules" in {
      check(opt(BooleanType), "true", Right(Some(Some(true))))
      check(opt(BooleanType), "false", Right(Some(Some(false))))
      check(opt(IntType), "123", Right(Some(Some(123))))
      check(opt(FloatType), "123", Right(Some(Some(123))))
      check(opt(FloatType), "123.456", Right(Some(Some(123.456))))
      check(opt(StringType), "\"abc123\"", Right(Some(Some("abc123"))))
      check(opt(IDType), "123456", Right(Some(Some("123456"))))
      check(opt(IDType), "\"123456\"", Right(Some(Some("123456"))))
    }

    "does not convert when input coercion rules reject a value" in {
      check(opt(BooleanType), "123", Right(None))
      check(opt(IntType), "123.456", Right(None))
      check(opt(IntType), "true", Right(None))
      check(opt(IntType), "\"123\"", Right(None))
      check(opt(FloatType), "\"123\"", Right(None))
      check(opt(StringType), "123", Right(None))
      check(opt(StringType), "true", Right(None))
      check(opt(IDType), "123.456", Right(None))
    }

    val testEnum = EnumType(
      "TestColor",
      values = List(
        EnumValue("RED", value = 1),
        EnumValue("GREEN", value = 2),
        EnumValue("BLUE", value = 3)))

    "converts enum values according to input coercion rules" in {
      check(opt(testEnum), "RED", Right(Some(Some(1))))
      check(opt(testEnum), "BLUE", Right(Some(Some(3))))
      check(opt(testEnum), "null", Right(Some(None)))
      check(opt(testEnum), "3", Right(None))
      check(opt(testEnum), "\"BLUE\"", Right(None))
    }

    // Boolean!
    val nonNullBool = BooleanType
    // [Boolean]
    val listOfBool = OptionInputType(ListInputType(OptionInputType(BooleanType)))
    // [Boolean!]
    val listOfNonNullBool = OptionInputType(ListInputType(nonNullBool))
    // [Boolean]!
    val nonNullListOfBool = ListInputType(OptionInputType(BooleanType))
    // [Boolean!]!
    val nonNullListOfNonNullBool = ListInputType(nonNullBool)

    "coerces to null unless non-null" in {
      check(opt(BooleanType), "null", Right(Some(None)))
      check(nonNullBool, "null", Right(None))
    }

    "coerces lists of values" in {
      check(opt(listOfBool), "true", Right(Some(Some(List(Some(true))))))
      check(opt(listOfBool), "123", Right(None))
      check(opt(listOfBool), "null", Right(Some(None)))
      check(opt(listOfBool), "[true, false]", Right(Some(Some(List(Some(true), Some(false))))))
      check(opt(listOfBool), "[true, 123]", Right(None))
      check(opt(listOfBool), "[true, null]", Right(Some(Some(List(Some(true), None)))))
      check(opt(listOfBool), "{ true: true }", Right(None))
    }

    "coerces non-null lists of values" in {
      check(nonNullListOfBool, "true", Right(Some(List(Some(true)))))
      check(nonNullListOfBool, "123", Right(None))
      check(nonNullListOfBool, "null", Right(None))
      check(nonNullListOfBool, "[true, false]", Right(Some(List(Some(true), Some(false)))))
      check(nonNullListOfBool, "[true, 123]", Right(None))
      check(nonNullListOfBool, "[true, null]", Right(Some(List(Some(true), None))))
    }

    "coerces lists of non-null values" in {
      check(listOfNonNullBool, "true", Right(Some(Some(List(true)))))
      check(listOfNonNullBool, "123", Right(None))
      check(listOfNonNullBool, "null", Right(Some(None)))
      check(listOfNonNullBool, "[true, false]", Right(Some(Some(List(true, false)))))
      check(listOfNonNullBool, "[true, 123]", Right(None))
      check(listOfNonNullBool, "[true, null]", Right(None))
    }

    "coerces non-null lists of non-null values" in {
      check(nonNullListOfNonNullBool, "true", Right(Some(List(true))))
      check(nonNullListOfNonNullBool, "123", Right(None))
      check(nonNullListOfNonNullBool, "null", Right(None))
      check(nonNullListOfNonNullBool, "[true, false]", Right(Some(List(true, false))))
      check(nonNullListOfNonNullBool, "[true, 123]", Right(None))
      check(nonNullListOfNonNullBool, "[true, null]", Right(None))
    }

    val testInputObj = InputObjectType(
      "TestInput",
      fields = List(
        InputField("int", opt(IntType), 42),
        InputField("bool", opt(BooleanType)),
        InputField("requiredBool", BooleanType)))

    val testOneOfInputObj = InputObjectType(
      "TestOneOfInput",
      fields = List(
        InputField("foo", opt(IntType)),
        InputField("bar", opt(BooleanType))
      )
    ).withDirective(ast.Directive(OneOfDirective.name))

    "coerces input objects according to input coercion rules" in {
      check(opt(testInputObj), "null", Right(Some(None)))
      check(opt(testInputObj), "123", Right(None))
      check(opt(testInputObj), "[]", Right(None))
      check(
        opt(testInputObj),
        "{ int: 123, requiredBool: false }",
        Right(Some(Some(Map("int" -> Some(123), "requiredBool" -> false)))))
      check(
        opt(testInputObj),
        "{ bool: true, requiredBool: false }",
        Right(Some(Some(Map("int" -> Some(42), "bool" -> Some(true), "requiredBool" -> false)))))
      check(opt(testInputObj), "{ int: true, requiredBool: true }", Right(None))
      check(opt(testInputObj), "{ requiredBool: null }", Right(None))
      check(opt(testInputObj), "{ bool: true }", Right(None))
    }

    "coerces oneOf input objects with exactly one field" in {
      check(
        opt(testOneOfInputObj),
        "null",
        Right(Some(None)),
        ignoreErrors = false
      )
      check(
        opt(testOneOfInputObj),
        "{ foo: 123 }",
        Right(Some(Some(Map("foo" -> Some(123))))),
        ignoreErrors = false
      )
      check(
        opt(testOneOfInputObj),
        "{ foo: 123, bar: null }",
        Right(Some(Some(Map("foo" -> Some(123), "bar" -> None)))),
        ignoreErrors = false
      )
      check(
        opt(testOneOfInputObj),
        "{ bar: true }",
        Right(Some(Some(Map("bar" -> Some(true))))),
        ignoreErrors = false)
      check(
        opt(testOneOfInputObj),
        "$input",
        Right(Some(Some(Map("foo" -> Some(123))))),
        "$input: TestOneOfInput" -> """{"input": {"foo": 123}}""",
        false
      )

      check(
        opt(testOneOfInputObj),
        "$input",
        Right(Some(Some(Map("foo" -> Some(123), "bar" -> None)))),
        "$input: TestOneOfInput" -> """{"input": {"foo": 123, "bar": null}}""",
        false
      )
      check(
        opt(testOneOfInputObj),
        "{ foo: $foo, bar: $bar}",
        Right(Some(Some(Map("foo" -> None, "bar" -> Some(true))))),
        "$foo: Int, $bar: Boolean" -> """{"foo": null, "bar": true}""",
        false
      )
    }

    "error if oneOf input objects have not exactly one non-null field" in {
      check(
        opt(testOneOfInputObj),
        "{ foo: 123, bar: true }",
        Left(NotExactlyOneOfField("TestOneOfInput", None, List(ast.AstLocation("", 0, 1, 1)))),
        ignoreErrors = false
      )

      check(
        opt(testOneOfInputObj),
        "{}",
        Left(NotExactlyOneOfField("TestOneOfInput", None, List(ast.AstLocation("", 0, 1, 1)))),
        ignoreErrors = false
      )

      check(
        opt(testOneOfInputObj),
        "{ foo: $foo, bar: false }",
        Left(NotExactlyOneOfField("TestOneOfInput", None, List(ast.AstLocation("", 0, 1, 1)))),
        "$foo: Int" -> """{"foo": 123}""",
        false
      )

      check(
        opt(testOneOfInputObj),
        "$input",
        Left(NotExactlyOneOfField("TestOneOfInput", None, List(ast.AstLocation("", 0, 1, 1)))),
        "$input: TestOneOfInput" -> """{"input": {"foo": 123, "bar": true}}""",
        false
      )

      check(
        opt(testOneOfInputObj),
        "{foo: $foo}",
        Left(NotExactlyOneOfField("TestOneOfInput", None, List(ast.AstLocation("", 0, 1, 1)))),
        "$foo: Int" -> """{"foo": null}""",
        false
      )
    }

    "accepts variable values assuming already coerced" in {
      check(opt(BooleanType), "$var", Right(None))
      check(
        opt(BooleanType),
        "$var",
        Right(Some(Some(true))),
        "$var: Boolean" -> """{"var": true}""")
      check(opt(BooleanType), "$var", Right(Some(None)), "$var: Boolean" -> """{"var": null}""")
    }

    "asserts variables are provided as items in lists" in {
      check(listOfBool, "[ $foo ]", Right(Some(Some(List(None)))))
      check(listOfNonNullBool, "[ $foo ]", Right(None))
      check(
        listOfNonNullBool,
        "[ $foo ]",
        Right(Some(Some(List(true)))),
        "$foo: Boolean!" -> """{"foo": true}""")
      check(
        listOfNonNullBool,
        "$foo",
        Right(Some(Some(List(true)))),
        "$foo: [Boolean!]" -> """{"foo": true}""")
      check(
        listOfNonNullBool,
        "$foo",
        Right(Some(Some(List(true)))),
        "$foo: [Boolean!]" -> """{"foo": [true]}""")
    }

    "omits input object fields for unprovided variables" in {
      check(
        opt(testInputObj),
        "{ int: $foo, bool: $foo, requiredBool: true }",
        Right(Some(Some(Map("int" -> Some(42), "requiredBool" -> true)))))

      check(
        opt(testInputObj),
        "{ int: $foo, bool: $foo, requiredBool: true }",
        Right(Some(Some(Map("int" -> None, "bool" -> None, "requiredBool" -> true)))),
        "$foo: Boolean" -> """{"foo": null}"""
      )

      check(opt(testInputObj), "{ requiredBool: $foo }", Right(None))

      check(
        opt(testInputObj),
        "{ bool: $foo, requiredBool: $foo }",
        Right(Some(Some(Map("int" -> Some(42), "bool" -> Some(true), "requiredBool" -> true)))),
        "$foo: Boolean" -> """{"foo": true}"""
      )

      check(
        opt(testInputObj),
        "$foo",
        Right(Some(Some(Map("int" -> Some(42), "requiredBool" -> true)))),
        "$foo: TestInput" -> """{"foo": {"requiredBool": true}}""")

      check(
        opt(testInputObj),
        "$foo",
        Right(Some(Some(Map("int" -> Some(42), "bool" -> None, "requiredBool" -> true)))),
        "$foo: TestInput" -> """{"foo": {"bool": null, "requiredBool": true}}"""
      )
    }
  }

  private[this] def coerceInputValue[T](
      tpe: InputType[T],
      value: String,
      vars: (String, String),
      ignoreErrors: Boolean
  )(implicit fromInput: FromInput[T]): Try[Any] = {
    val testSchema = Schema.buildFromAst(
      QueryParser
        .parse(s"""
      input TestInput {
        int: Int = 42
        bool: Boolean
        requiredBool: Boolean!
      }

      input TestOneOfInput @oneOf {
        foo: Int
        bar: Boolean
      }

      type Query {
        foo: String
      }
    """).get)

    import spray.json._
    import sangria.marshalling.sprayJson._

    val valueCollector = new ValueCollector(
      testSchema,
      (if (vars._2.nonEmpty) vars._2 else "{}").parseJson,
      None,
      None,
      (),
      ExceptionHandler.empty,
      None,
      ignoreErrors)
    val variables = valueCollector
      .getVariableValues(
        QueryParser
          .parse(s"query Foo${if (vars._1.nonEmpty) "(" + vars._1 + ")" else ""} {foo}")
          .get
          .operations(Some("Foo"))
          .variables,
        None)
      .get

    val parsed = QueryParser.parseInputWithVariables(value).get
    val args = valueCollector
      .getArgumentValues(
        None,
        Argument("a", tpe) :: Nil,
        Vector(ast.Argument("a", parsed)),
        variables)

    args.map(_.raw.get("a"))
  }

  private[this] def check[T](
      tpe: InputType[T],
      value: String,
      result: Either[Violation, Any],
      vars: (String, String) = "" -> "",
      ignoreErrors: Boolean = true
  )(implicit fromInput: FromInput[T]) = {
    val coerced = coerceInputValue(tpe, value, vars, ignoreErrors)
    (result, coerced) match {
      case (Right(result), Success(coerced)) => coerced should be(result)
      case (Left(expected), Failure(AttributeCoercionError(violations, _))) =>
        violations.headOption should be(Some(expected))
      case _ => coerced should be(result) // will fail
    }
  }

  private[this] def opt[T](tpe: InputType[T]): InputType[Option[T]] = OptionInputType(tpe)
}

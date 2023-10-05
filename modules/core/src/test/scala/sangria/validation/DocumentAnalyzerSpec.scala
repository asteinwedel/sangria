package sangria.validation

import sangria.schema._
import sangria.macros._
import sangria.util.StringMatchers
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.renderer.QueryRenderer

import sangria.util.tag.@@ // Scala 3 issue workaround
import sangria.marshalling.FromInput.CoercedScalaResult

class DocumentAnalyzerSpec extends AnyWordSpec with Matchers with StringMatchers {
  val NumberType = EnumType(
    "Number",
    values = List(
      EnumValue("ONE", value = 1),
      EnumValue("TWO", value = 2, deprecationReason = Some("Some enum reason."))))

  // TODO: directive and input object field arg deprecation
  val QueryType = ObjectType(
    "Query",
    fields[Unit, Unit](
      Field(
        "normalField",
        OptionType(NumberType),
        arguments = Argument[Option[Int @@ CoercedScalaResult]](
          "enumArg",
          OptionInputType(NumberType)) :: Nil,
        resolve = ctx => ctx.argOpt[Int]("enumArg")
      ),
      Field(
        "deprecatedField",
        OptionType(StringType),
        deprecationReason = Some("Some field reason."),
        resolve = _ => "foo"),
      Field(
        "fieldWithDeprecatedArg",
        OptionType(StringType),
        resolve = ctx => ctx.argOpt[String]("deprecatedArg"),
        arguments = Argument(
          "deprecatedArg",
          OptionInputType(StringType)
        ).withDeprecationReason("Some arg reason.") :: Nil
      ),
      Field(
        "fieldWithInputObjectFieldDeprecated",
        OptionType(StringType),
        resolve = _ => "foo",
        arguments = Argument(
          "input",
          InputObjectType(
            "FooInput",
            "",
            fieldsFn = () =>
              InputField("deprecatedField", StringType)
                .withDeprecationReason("Some input field reason.") :: Nil
            // TODO: directive with deprecated here
          )
        ) :: Nil
      )
    )
  )

  val schema = Schema(QueryType)

  "DocumentAnalyzer" should {
    "report empty set for no deprecated usages" in {
      schema.analyzer(gql"""{ normalField(enumArg: ONE) }""").deprecatedUsages should have size 0
    }

    "report usage of deprecated fields" in {
      schema
        .analyzer(gql"""{ normalField, deprecatedField }""")
        .deprecatedUsages
        .map(_.errorMessage) should
        contain("The field 'Query.deprecatedField' is deprecated. Some field reason.")
    }

    "report usage of deprecated enums" in {
      schema
        .analyzer(gql"""{ normalField(enumArg: TWO) }""")
        .deprecatedUsages
        .map(_.errorMessage) should
        contain("The enum value 'Number.TWO' is deprecated. Some enum reason.")
    }

    "report usage of deprecated field args" in {
      schema
        .analyzer(gql"""{ fieldWithDeprecatedArg(deprecatedArg: "foo") }""")
        .deprecatedUsages
        .map(_.errorMessage) should
        contain(
          "The argument 'deprecatedArg' on 'Query.fieldWithDeprecatedArg' is deprecated. Some arg reason.")
    }

    "report usage of deprecated input object field args" in {
      schema
        .analyzer(
          gql"""{ fieldWithInputObjectFieldDeprecated(input: { deprecatedField: "foo" }) }""")
        .deprecatedUsages
        .map(_.errorMessage) should
        contain(
          "The input field 'FooInput.deprecatedField' is deprecated. Some input field reason.")
    }

    "report usage of deprecated enums in variables" in {
      schema
        .analyzer(gql"""query Foo($$x: Number = TWO) { normalField }""")
        .deprecatedUsages
        .map(_.errorMessage) should
        contain("The enum value 'Number.TWO' is deprecated. Some enum reason.")
    }

    "report introspectionUsage" in {
      val query =
        gql"""
          {
            __type(name: "Query") {
              name
            }

            __schema {
              queryType {
                name
              }
            }

            normalField

            __typeName

          }
        """

      schema.analyzer(query).introspectionUsages.map(_.errorMessage) should
        contain("Introspection field '__Schema.queryType' is used.")
          .and(contain("Introspection field 'Query.__type' is used."))
          .and(contain("Introspection field 'Query.__schema' is used."))
          .and(contain("Introspection field '__Type.name' is used."))
    }

    "separates one AST into multiple, maintaining document order" in {
      val query =
        gql"""
          {
            ...Y
            ...X
          }

          query One {
            foo
            bar
            ...A
            ...X
          }

          fragment A on T {
            field
            ...B
          }

          fragment X on T {
            fieldX
          }

          query Two {
            ...A
            ...Y
            baz
          }

          fragment Y on T {
            fieldY
          }

          fragment B on T {
            something
          }
        """

      val operations = DocumentAnalyzer(query).separateOperations
      operations.keySet should be(Set(None, Some("One"), Some("Two")))

      QueryRenderer.renderPretty(operations(None)) should equal("""{
          |  ...Y
          |  ...X
          |}
          |
          |fragment X on T {
          |  fieldX
          |}
          |
          |fragment Y on T {
          |  fieldY
          |}""".stripMargin)(after.being(strippedOfCarriageReturns))

      QueryRenderer.renderPretty(operations(Some("One"))) should equal("""query One {
          |  foo
          |  bar
          |  ...A
          |  ...X
          |}
          |
          |fragment A on T {
          |  field
          |  ...B
          |}
          |
          |fragment X on T {
          |  fieldX
          |}
          |
          |fragment B on T {
          |  something
          |}""".stripMargin)(after.being(strippedOfCarriageReturns))

      QueryRenderer.renderPretty(operations(Some("Two"))) should equal("""fragment A on T {
          |  field
          |  ...B
          |}
          |
          |query Two {
          |  ...A
          |  ...Y
          |  baz
          |}
          |
          |fragment Y on T {
          |  fieldY
          |}
          |
          |fragment B on T {
          |  something
          |}""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "ASt separation survives circular dependencies" in {
      val query =
        gql"""
          query One {
            ...A
          }

          fragment A on T {
            ...B
          }

          fragment B on T {
            ...A
          }

          query Two {
            ...B
          }
        """

      val operations = DocumentAnalyzer(query).separateOperations
      operations.keySet should be(Set(Some("One"), Some("Two")))

      QueryRenderer.renderPretty(operations(Some("One"))) should equal("""query One {
          |  ...A
          |}
          |
          |fragment A on T {
          |  ...B
          |}
          |
          |fragment B on T {
          |  ...A
          |}""".stripMargin)(after.being(strippedOfCarriageReturns))

      QueryRenderer.renderPretty(operations(Some("Two"))) should equal("""fragment A on T {
          |  ...B
          |}
          |
          |fragment B on T {
          |  ...A
          |}
          |
          |query Two {
          |  ...B
          |}""".stripMargin)(after.being(strippedOfCarriageReturns))
    }
  }
}

package sangria.validation.rules

import sangria.ast.AstLocation

import scala.collection.mutable.{Map => MutableMap}

import sangria.ast
import sangria.schema
import sangria.ast.AstVisitorCommand
import sangria.validation._

/** For oneOf input objects, exactly one field is non-null
  *
  * A GraphQL operation is only valid if all the variables it defines are of input types (scalar,
  * enum, or input object).
  */

class ExactlyOneOfFieldGiven extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    override val onEnter: ValidationVisit = { case ast.ObjectValue(fields, _, pos) =>
      ctx.typeInfo.inputType match {
        case Some(inputType) =>
          inputType.namedInputType match {
            case schema.InputObjectType(name, _, _, directives, _) if directives.exists { d =>
                  d.name == schema.OneOfDirective.name
                } =>
              val nonNullFields = fields.filter { field =>
                field.value match {
                  case ast.NullValue(_, _) => false
                  case _ => true
                }
              }

              nonNullFields.size match {
                case 1 => AstVisitorCommand.RightContinue
                case _ =>
                  Left(Vector(MoreThanOneOfField(name, ctx.sourceMapper, pos.toList)))
              }

            case _ => AstVisitorCommand.RightContinue
          }
        case None => AstVisitorCommand.RightContinue
      }
    }
  }
}

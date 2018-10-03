package org.jetbrains.plugins.scala.lang
package parser
package parsing
package builder

import com.intellij.lang.PsiBuilder
import com.intellij.lang.impl.PsiBuilderAdapter
import com.intellij.openapi.project.DumbService
import org.jetbrains.plugins.scala.lang.lexer.{ScalaTokenTypes => Types}
import org.jetbrains.plugins.scala.lang.parser.util.ParserUtils._
import org.jetbrains.plugins.scala.lang.psi.stubs.elements.ScStubElementType
import org.jetbrains.plugins.scala.project.Version
import org.jetbrains.plugins.scala.util.ScalaUtil.getScalaVersion

import scala.collection.mutable
import scala.meta.intellij.IdeaUtil.inModuleWithParadisePlugin

/**
  * @author Alexander Podkhalyuzin
  */
class ScalaPsiBuilderImpl(builder: PsiBuilder)
  extends PsiBuilderAdapter(builder) with ScalaPsiBuilder {

  private final val newlinesEnabled = new mutable.Stack[Boolean]

  private lazy val scalaVersion = getPsiFile(this)
    .flatMap(getScalaVersion)
    .map(Version(_))

  private lazy val hasMeta = !ScStubElementType.isStubBuilding &&
    !DumbService.isDumb(getProject) &&
    getPsiFile(this).exists(inModuleWithParadisePlugin)

  override def newlineBeforeCurrentToken: Boolean = newLinesBeforeCurrent > 0

  override def twoNewlinesBeforeCurrentToken: Boolean = newLinesBeforeCurrent > 1

  override final def disableNewlines(): Unit = {
    newlinesEnabled.push(false)
  }

  override final def enableNewlines(): Unit = {
    newlinesEnabled.push(true)
  }

  override final def restoreNewlinesState(): Unit = {
    assert(newlinesEnabled.nonEmpty)
    newlinesEnabled.pop()
  }

  override final def isTrailingCommasEnabled: Boolean = !scalaVersion.exists(_ < Version("2.12.2"))

  override final def isIdBindingEnabled: Boolean = scalaVersion.exists(_ >= Version("2.12"))

  override final def isMetaEnabled: Boolean = hasMeta

  protected final def isNewlinesEnabled: Boolean = newlinesEnabled.isEmpty || newlinesEnabled.top

  /**
    * @return 0 if new line is disabled here, or there is no \n chars between tokens
    *         1 if there is no blank lines between tokens
    *         2 otherwise
    */
  private def newLinesBeforeCurrent: Int =
    if (!isNewlinesEnabled && !eof && elementCanStartStatement) countNewLinesBeforeCurrentTokenRaw(this)
    else 0

  private def elementCanStartStatement: Boolean = getTokenType match {
    case Types.kCATCH |
         Types.kELSE |
         Types.kEXTENDS |
         Types.kFINALLY |
         Types.kMATCH |
         Types.kWITH |
         Types.kYIELD |
         Types.tCOMMA |
         Types.tDOT |
         Types.tSEMICOLON |
         Types.tCOLON |
         Types.tASSIGN |
         Types.tFUNTYPE |
         Types.tCHOOSE |
         Types.tUPPER_BOUND |
         Types.tLOWER_BOUND |
         Types.tVIEW |
         Types.tINNER_CLASS |
         Types.tLSQBRACKET |
         Types.tRSQBRACKET |
         Types.tRPARENTHESIS |
         Types.tRBRACE => false
    case Types.kCASE =>
      val marker = mark
      advanceLexer()

      val result = getTokenType match {
        case Types.kOBJECT |
             Types.kCLASS => true
        case _ => false
      }

      marker.rollbackTo()
      result
    case _ => true
  }
}
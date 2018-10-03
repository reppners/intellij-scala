package org.jetbrains.plugins.scala.lang
package parser
package parsing
package builder

import com.intellij.lang.PsiBuilder
import com.intellij.lang.impl.PsiBuilderAdapter
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.project.DumbService
import com.intellij.psi.impl.source.resolve.FileContextUtil
import com.intellij.testFramework.LightVirtualFileBase
import org.jetbrains.plugins.scala.lang.lexer.{ScalaTokenTypes => Types}
import org.jetbrains.plugins.scala.lang.parser.util.ParserUtils._
import org.jetbrains.plugins.scala.lang.psi.stubs.elements.ScStubElementType
import org.jetbrains.plugins.scala.project.Version
import org.jetbrains.plugins.scala.settings.ScalaProjectSettings
import org.jetbrains.plugins.scala.util.ScalaUtil

import scala.collection.mutable
import scala.meta.intellij.IdeaUtil

/**
  * @author Alexander Podkhalyuzin
  */
class ScalaPsiBuilderImpl(delegate: PsiBuilder)
  extends PsiBuilderAdapter(delegate) with ScalaPsiBuilder {

  private val newlinesEnabled = new mutable.Stack[Boolean]

  private lazy val scalaVersion = findPsiFile
    .flatMap(ScalaUtil.getScalaVersion)
    .map(Version(_))

  final lazy val isMetaEnabled: Boolean =
    !ScStubElementType.isStubBuilding &&
      !DumbService.isDumb(getProject) &&
      findPsiFile.exists(IdeaUtil.inModuleWithParadisePlugin)

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

  override final def isTrailingCommasEnabled: Boolean = {
    import ScalaProjectSettings.TrailingCommasMode._
    ScalaProjectSettings.getInstance(getProject).getTrailingCommasMode match {
      case Enabled => true
      case Auto => isTestFile || scalaVersion.forall(_ >= Version("2.12.2"))
      case Disabled => false
    }
  }

  override final def isIdBindingEnabled: Boolean =
    isTestFile || scalaVersion.exists(_ >= Version("2.12"))

  protected final def isNewlinesEnabled: Boolean = newlinesEnabled.isEmpty || newlinesEnabled.top

  /**
    * @return 0 if new line is disabled here, or there are no new lines between tokens
    *         1 if there are no new lines between tokens
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

  private def isTestFile =
    ApplicationManager.getApplication.isUnitTestMode &&
      findPsiFile.exists { file =>
        file.getVirtualFile.isInstanceOf[LightVirtualFileBase]
      }

  private def findPsiFile = Option {
    myDelegate.getUserData(FileContextUtil.CONTAINING_FILE_KEY)
  }
}
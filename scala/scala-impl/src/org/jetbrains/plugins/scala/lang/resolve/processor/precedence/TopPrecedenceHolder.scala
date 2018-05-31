package org.jetbrains.plugins.scala.lang
package resolve
package processor
package precedence

import scala.collection.mutable

trait TopPrecedenceHolder[Repr] {

  /**
    * Returns highest precedence of all resolve results.
    * 1 - import a._
    * 2 - import a.x
    * 3 - definition or declaration
    */
  def apply(result: ScalaResolveResult): Int

  def update(result: ScalaResolveResult, i: Int): Unit

  def filterNot(left: ScalaResolveResult,
                right: ScalaResolveResult)
               (precedence: ScalaResolveResult => Int): Boolean =
    precedence(left) < apply(right)

  def toRepresentation(result: ScalaResolveResult): Repr
}

abstract class TopPrecedenceHolderImpl[Repr] extends TopPrecedenceHolder[Repr] {

  private val precedences = new mutable.HashMap[Repr, Int]()
    .withDefaultValue(0)

  override def apply(result: ScalaResolveResult): Int =
    precedences(toRepresentation(result))

  override def update(result: ScalaResolveResult, i: Int): Unit = {
    precedences(toRepresentation(result)) = i
  }

  override def filterNot(left: ScalaResolveResult,
                         right: ScalaResolveResult)
                        (precedence: ScalaResolveResult => Int): Boolean =
    toRepresentation(left) == toRepresentation(right) &&
      super.filterNot(left, right)(precedence)
}

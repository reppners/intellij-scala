package org.jetbrains.plugins.scala.testingSupport.scalatest.scala2_11.scalatest2_1_7

import org.jetbrains.plugins.scala.SlowTests
import org.jetbrains.plugins.scala.testingSupport.scalatest.fileStructureView._
import org.junit.experimental.categories.Category

/**
  * @author Roman.Shein
  * @since 20.04.2015.
  */
@Category(Array(classOf[SlowTests]))
class Scalatest2_11_2_1_7_StructureViewTest extends Scalatest2_11_2_1_7_Base with FeatureSpecFileStructureViewTest
with FlatSpecFileStructureViewTest with FreeSpecFileStructureViewTest with FunSuiteFileStructureViewTest
with PropSpecFileStructureViewTest with WordSpecFileStructureViewTest with FunSpecFileStructureViewTest

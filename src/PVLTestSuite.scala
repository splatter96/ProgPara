import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

import AlgebStruct._

class ExampleSuite extends AssertionsForJUnit {

  @Before def initialize() {
    println("Starting Unittests")
  }
  @Test def verifynnf() {
    val f1 = Not(And(Atom(true), Atom(false)))
    assert(f1.nnf() == Or(Not(Atom(true)), Not(Atom(false))))
  }
}

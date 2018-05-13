import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

import AlgebStruct._

class ExampleSuite extends AssertionsForJUnit {

  /*
  @Before def initialize() {
    println("Starting Unittests")
  }*/
  
  @Test def verifynnf() {
    val f1 = Not(And(Atom(true), Atom(false)))
    assert(f1.nnf() == Or(Not(Atom(true)), Not(Atom(false))))
    
    val f2 = Not(Or(Atom(true), Atom(false)))
    assert(f2.nnf() == And(Not(Atom(true)), Not(Atom(false))))
    
    val f3 = Or(Not(And(Atom(true), Atom(false))), Atom(false))
    assert(f3.nnf() == Or(Or(Not(Atom(true)), Not(Atom(false))), Atom(false)))
    
    val f4 = Or(Not(And(Atom(true), Atom(false))), Not(And(Atom(true), Atom(false))))
    assert(f4.nnf() == Or(Or(Not(Atom(true)), Not(Atom(false))), Or(Not(Atom(true)), Not(Atom(false)))))
  }
  
  @Test def verifyimplfree(){
    val p = true
    val q = false
    val r = true
    val s = false
    
    val f1 = Imp(Atom(p), Atom(q))
    assert(f1.implFree() == Or(Not(Atom(p)), Atom(q)))
    
    val f2 = And(Or(Atom(p), Atom(q)), Imp(Atom(r), Atom(s)))
    assert(f2.implFree() == And(Or(Atom(p), Atom(q)), Or(Not(Atom(r)), Atom(s))))
    
    val f3 = And(Atom(p), Not(Imp(Atom(q), Atom(r))))
    assert(f3.implFree() == And(Atom(p), Not(Or(Not(Atom(q)), Atom(r)))))
  }
  
  @Test def verifysimplify(){
    val f1 = Not(Not(Atom(true)))
    assert(f1.simplify() == Atom(true))
    
    val f2 = Not(And(Atom(true), Atom(true)))
    assert(f2.simplify() == Not(Atom(true)))
    
    val p = true
    val q = false
    val f31 = And(Not(Atom(p)), Not(Atom(q)))
    val f32 = And(Not(Atom(p)), Not(Atom(q)))
    assert(And(f31, f32).simplify() == f31)
    assert(Or(f31, f32).simplify() == f31)
    
    val f41 = And(Atom(true), Atom(false))
    assert(f41.simplify() == Atom(false))
    val f42 = And(Atom(false), Atom(true))
    assert(f42.simplify() == Atom(false))
    
    val f51 = Or(Atom(true), Atom(true))
    assert(f51.simplify() == Atom(true))
    val f52 = Or(Atom(true), Atom(false))
    assert(f52.simplify() == Atom(true))
  }
  
  @Test def verifyCnf(){
    val a = true;
    val b = false;
    val c = true;
    val d = false;
    val e = true;
    
    val fsimp = Or(Or(Atom(a),Atom(b)), Atom(c))
    assert(cnf(fsimp) == fsimp)
    
    val fsimp2 = Or(Or(Atom(d), Atom(e)), Or(Atom(a), Atom(b)))
    assert(cnf(fsimp2) == fsimp2)
    
    val fsimp4 = Or(Or(Atom(a),Atom(b)), And(Atom(c), Atom(d)))
    assert(cnf(fsimp4) == And(Or(Atom(c), Or(Atom(a), Atom(b))), Or(Atom(d), Or(Atom(a), Atom(b)))))
    
    val fcnf = Or(Or(Atom(a), Atom(b)), And(Atom(c), Or(Atom(d), Atom(e))))
    assert(cnf(fcnf) == And(Or(Atom(c), Or(Atom(a), Atom(b))), Or(Or(Atom(d), Atom(e)), Or(Atom(a), Atom(b)))))
  }
}

/*
 * Class for all homework in the PVL for Parallele Programmierung
 */

object AlgebStruct {
  abstract sealed class Formula{
    def toString(): String
    def implFree(): Formula //Replace all implications equivalently
    def nnf(): Formula //Move all negations directly in front of atoms
    def simplify(): Formula //basic simplification
  }
  
  //Wrapper around Boolean variables to guarantee recursive definition
  case class Atom(atom: Boolean) extends Formula{
   override def toString(): String = {
      atom.toString();
    }
   def implFree(): Formula = {
     this
   }
   def nnf(): Formula = {
     this
   }
   def simplify(): Formula = {
     this
   }
  }
  
  case class And(l: Formula, r: Formula) extends Formula{
    override def toString(): String = {
      l.toString() + " and " + r.toString();
    }
    def implFree(): Formula = {
      And(l.implFree(), r.implFree())
    }
    def nnf(): Formula = {
       And(l.nnf(), r.nnf())
     }
    def simplify(): Formula = {
      (l, r) match {
        case (Atom(true), _) =>  r.simplify() //T ^ x = x
        case (_, Atom(true)) => l.simplify()  //x ^ T = x
        case (Atom(false), _) => Atom(false)  //F ^ x = F
        case (_, Atom(false)) => Atom(false)  //x ^ F = F
        case (lv, rv) if (lv == rv) => lv.simplify() //x ^ x = x
        case (_,_) => And(l.simplify(), r.simplify())
      }
    }
  }
  
  case class Or(l: Formula, r: Formula) extends Formula{
     override def toString(): String = {
      l.toString() + " or " + r.toString();
     }
     def implFree(): Formula = {
       Or(l.implFree(), r.implFree())
     }
     def nnf(): Formula = {
       Or(l.nnf(), r.nnf())
     }
     def simplify(): Formula = {
      (l, r) match {
        case (Atom(true), _) =>  Atom(true) //x v T = T
        case (_, Atom(true)) => Atom(true)  //T v x = T
        case (Atom(false), _) => r.simplify() //F v x = x
        case (_, Atom(false)) => l.simplify() //x v F = x
        case (lv, rv) if lv==rv => r.simplify() //x v x = x
        case (_,_) => Or(l.simplify(), r.simplify())
      }
    }
  }
  
  case class Imp(l: Formula, r: Formula) extends Formula{
     override def toString(): String = {
      l.toString() + " implies " + r.toString();
    }
     def implFree(): Formula = {
       Or(Not(l.implFree()), r.implFree())
     }
     def nnf(): Formula = {
       Imp(l.nnf(), r.nnf())
     }
     def simplify(): Formula = {
       Imp(l.simplify(), r.simplify())
     }
  }
  
  case class Not(r: Formula) extends Formula{
     override def toString(): String = {
      "not (" + r.toString() + ")";
     }
     def implFree(): Formula = {
       Not(r.implFree())
     }
     def nnf(): Formula = {
       r match {
         case Atom(o) => Not(Atom(o))
         case And(al,ar) => Or(Not(al), Not(ar)).nnf()
         case Or(ol, or) => And(Not(ol), Not(or)).nnf()
         case Imp(il, ir) => And(il, Not(ir)).nnf()
         case Not(no) => no.nnf() //remove double negation
       }
     }
     def simplify(): Formula = {
       r match {
         case Not(v) => v.simplify() //!!x = x
         case _ => Not(r.simplify())
       }
     }
  }
  
  def main(args: Array[String]){
    val p = true
    val q = false
    val r = true
    val s = false
    
    val f = And(Or(Atom(p), Atom(q)), Imp(Atom(r), Atom(s)))
    val f2 = Not(And(Not(Or(Atom(p), Atom(r))), Atom(q)))
    
    println(f)
    println(f.implFree())
    println(f.nnf())
    
    println("f2")
    println(f2)
    println(f2.nnf())
    
    val a = And(Not(Atom(p)), Not(Atom(q)))
    val b = And(Not(Atom(p)), Not(Atom(q)))
    
    println(a==b)
    
    val f3 = Or(a, b)
    println(f3)
    println(f3.simplify())
    
  }
}
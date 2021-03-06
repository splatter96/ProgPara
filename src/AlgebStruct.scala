/*
 * Class for all homework in the PVL for the lecture Parallele Programmierung
 */

object AlgebStruct {
  abstract sealed class Formula{
    def toString(): String
    def implFree(): Formula //replace all implications equivalently
    def nnf(): Formula //move all negations directly in front of atoms
    def simplify(): Formula //basic simplifications
    def contains(f: Formula): Boolean //used for solving cnf formulas
  }
  
  //wrapper around boolean variables to guarantee recursive definition
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
   def contains(f: Formula): Boolean = {
     this == f
   }
  }
  
  case class And(l: Formula, r: Formula) extends Formula{
    override def toString(): String = {
      "(" + l.toString() + " and " + r.toString() + ")";
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
    def contains(f: Formula): Boolean = {
     this == f || l.contains(f) || r.contains(f)
    }
  }
  
  case class Or(l: Formula, r: Formula) extends Formula{
     override def toString(): String = {
      "(" + l.toString() + " or " + r.toString() + ")";
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
     def contains(f: Formula): Boolean = {
       this == f || l.contains(f) || r.contains(f)
    }
  }
  
  case class Imp(l: Formula, r: Formula) extends Formula{
     override def toString(): String = {
      "(" + l.toString() + " implies " + r.toString() + ")";
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
     def contains(f: Formula): Boolean = {
       this == f || l.contains(f) || r.contains(f)
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
         case Imp(il, ir) => And(il, Not(ir)).nnf() //via a -> b == !a v b 
         case Not(no) => no.nnf() //remove double negation
       }
     }
     def simplify(): Formula = {
       r match {
         case Not(v) => v.simplify() //!!x = x
         case _ => Not(r.simplify())
       }
     }
     def contains(f: Formula): Boolean = {
       this == f || r.contains(f)
    }
  }
  
  def cnf(form: Formula): Formula = {
    form match {
      case Atom(_) => form
      case Not(v) => Not(cnf(v))
      case And(f, g) => And(cnf(f), cnf(g))
      case Or(f, g) => distr(f, g)
      case Imp(f, g) => cnf(Or(Not(f), g)) //replace implication with other definition via or
    }
  }
  
  def distr(f: Formula, g: Formula): Formula = {
    (f, g) match {
      case (And(f1, f2), _) => And(distr(f1, g), distr(f2, g))
      case (_, And(g1, g2)) => And(distr(g1, f), distr(g2, f))
      case (_, _) => Or(f, g)
    }
  }
  
  def solve(f: Formula): Boolean = {
    f match {
      case And(l, r) => solve(l) && solve(r)
      case Or(l, r) => solve(l) || solve(r) || r.contains(Not(l)) || l.contains(Not(r))
      case Atom(x) => x
      case Not(x) => !solve(x)
      case _ => throw new IllegalArgumentException("Formula is not in CNF")
    }
  }
  
  def main(args: Array[String]){
    val p = true
    val q = false
    val r = true
    val s = false
    
    /*
    val f = And(Or(Atom(p), Atom(q)), Imp(Atom(r), Atom(s)))
    val f2 = Not(And(Not(Or(Atom(p), Atom(r))), Atom(q)))
    
    println(f)
    println(f.implFree())
    println(f.nnf())
    
    println("f2")
    println(f2)
    println(f2.nnf())
    */
    
  }
}
import scala.reflect.runtime.universe._

object ASTSimpleTransform extends Transformer {
  def createPrintln = Apply(Select(Ident("System.out"), newTermName("println")), List(Literal(Constant("bleh"))))
  
  def addPrintln(t: Tree): Block = t match {
    case b :Block => treeCopy.Block(b, createPrintln :: b.stats, b.expr)
    case t => Block(createPrintln, t)
  }
  
  override def transform(tree: Tree) = tree match {
    case defdef @ DefDef(mods, name, tparams, vparamss, tpt, rhs) => 
      treeCopy.DefDef(defdef, mods, name, tparams, vparamss, tpt, addPrintln(rhs))
    case t => super.transform(t)
  }
}
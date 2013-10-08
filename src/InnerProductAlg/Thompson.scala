import scala.language.implicitConversions
object Thompson {
  
  def b(x:Int, n:Int) : Int = x match {
    case 0 => List(2*n, (n+3) / 2, 2*n, n, 2*n, (n-3) / 2, 2*n, n)(n % 8)
    case 1 => List(n/2, 2*n+1, n-1, 2*n+1)(n%4)
    case 2 => List(n, 2*n+4, 2*n-2, (n-1)/2)(n%4)
    case 3 => List(n/2, 2*n+3, 2*n-3, n)(n%4)
    case 4 => List(2*n, n+1, 2*n, (n-1) /2)(n%4)
    case 5 => List(n, 2*n+1, (n+2)/2, 2*n+1, n, 2*n+1,(n-4)/2, 2*n+1)(n%8)
  }
  
  def f(x:Int,y:Int) : Int =  (List(List(0,2,4,5),List(0,1,3,5),List(1,2,3,4),List(1,2,3,4),List(0,2,4,5),List(0,1,3,5))(x))(y)
  
  def evalFunk(a:(Int,Int)=>Int, n:Int, list:List[Int]) : Int = list match {
    case Nil => n
    case x :: xs => a(x,evalFunk(a,n,xs))
  }
  
  def evaluate(n:Int, xs:List[Int]) :List[Int] = {
    def nUpperbound = math.pow(2,n+1).toInt
    List.range(0,nUpperbound).map(m => evalFunk(b,m,xs))
  }
  
  def tuplesByBase(b:Int, n:Int):List[List[Int]] = n match {
    case 0 => List(List())
    case _ => (for (i <- List.range(0,b)) yield tuplesByBase(b,n-1).map(i::_)).flatten
  }
  
  def specialTuples(n:Int):List[List[Int]] = (for (i <- List.range(0,6)) yield tuplesByBase(4,n-1).map(i::_)).flatten
  
  def convertTuplee(x:Int, l:List[Int]):List[Int] = l match {
    case y :: ys => {
      def z = f(x,y)
      z :: convertTuplee(z,ys)
    }
    case Nil => Nil
  }
  
  def convertTuple(l:List[Int]):List[Int] = l match {
    case x :: xs => x :: convertTuplee(x,xs)
    case Nil => ???
  }
  
  def partitions(o:Int):List[(List[Int], List[Int])] = for (xs <- specialTuples(o); ys <- specialTuples(o)) yield (xs,ys) 
  
  def getResult(n:Int, o:Int, p:(List[Int], List[Int])) : Int = p match {
    case (xs,ys) => {
      implicit def bool2int(b:Boolean) = if (b) 1 else 0
      def tss = tuplesByBase(4,n-o)
      def xss = tss.map(xs ++ _).map(convertTuple).map(evaluate(n,_))
      def yss = tss.map(ys ++ _).map(convertTuple).map(evaluate(n,_))
      (for (xs <- xss; ys <- yss) yield (xs==ys):Int).foldLeft(0)(_+_)
    }
  }
  
  def totalSolution(n:Int, o:Int):Int = o match {
    case 0 => ???
    case o => partitions(o).map(getResult(n,o,_)).foldLeft(0)(_+_) 
  }
  
  def main(args:Array[String]) = println(totalSolution(5,1))
}
package src

object mergsort {
	def mergeSort(xs: List[Int]):List[Int] = {
		val n = xs.length / 2
		if (n==0) xs
		else {
			def merge(xs: List[Int], ys:List[Int]):List[Int] = (xs, ys) match {
				case(Nil, ys) => ys
				case(xs, Nil) => xs
				case(x::xs1, y::ys1) =>
					if (x < y) x::merge(xs1,ys)
					else y::merge(xs,ys1)
			}
			val (left, right) = xs splitAt(n)
			merge(mergeSort(left), mergeSort(right))
		}
	}                                         //> mergeSort: (xs: List[Int])List[Int]

  val x = List(5,7,9,8,1,2,3,4)                   //> x  : List[Int] = List(5, 7, 9, 8, 1, 2, 3, 4)
  val y = mergeSort(x)                            //> y  : List[Int] = List(1, 2, 3, 4, 5, 7, 8, 9)
  
  
  
    def mergeSort2[T](pred: (T, T) => Boolean)(xs: Stream[T]): Stream[T] = {
    val m = xs.length / 2
    if (m == 0) xs
    else {
      def merge(ls: Stream[T], rs: Stream[T]): Stream[T] = (ls, rs) match {
        case (Stream.Empty, _) => rs
        case (_, Stream.Empty) => ls
        case (l #:: ls1, r #:: rs1) =>
          if (pred(l, r)) l #:: merge(ls1, rs)
          else r #:: merge(ls, rs1)
      }
      val (l, r) = xs splitAt m
      merge(mergeSort2(pred)(l), mergeSort2(pred)(r))
    }
  }                                               //> mergeSort2: [T](pred: (T, T) => Boolean)(xs: Stream[T])Stream[T]

  def numbers(remain: Int): Stream[Int] =
    if (remain == 0) Stream.Empty
    else Stream.cons(util.Random.nextInt(100), numbers(remain - 1))
                                                  //> numbers: (remain: Int)Stream[Int]

 
  mergeSort2((x: Int, y: Int) => x < y)(numbers(4)).toList
                                                  //> res0: List[Int] = List(55, 58, 71, 72)
  
  numbers(4)                                      //> res1: Stream[Int] = Stream(90, ?)
  
  
}

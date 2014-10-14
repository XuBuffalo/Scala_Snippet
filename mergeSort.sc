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
}
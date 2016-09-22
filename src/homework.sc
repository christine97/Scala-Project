
val l = List(1,2,3,4)

// find average of the list
val x = l.foldLeft(0.0)(_+_)/l.length

//find MIN value
 val min = l.foldLeft(Int.MaxValue)(_ min _)

// find MAX value
val max = l.foldRight(Int.MinValue)(_ max _)


// use traditional methods
 val min2 = l.min


val mult = l.foldLeft(2)(_*_)


val sum = l.foldLeft(0)(_+_)
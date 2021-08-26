module Functions

let flip f x y = f y x

let curry f x y = f (x, y)

let tuple x y = (x, y)

let k x _ = x

let (=>) () g = g

let delay f x  = fun () -> f x

let thunk x () = x 

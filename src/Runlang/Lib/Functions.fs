module Functions

let flip f x y = f y x

let curry f x y = f (x, y)

let tuple x y = (x, y)

let k x y = x

let (=>) () g = g

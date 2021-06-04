module Udecimal

open System

type Udecimal = private Udecimal of decimal

// Same behavior as UINT, maybe this is dumb (probably), but if the code is
// flawed then the uints are also flawed.
let udecimal dec =
    if dec < 0m then
        Udecimal (Decimal.MaxValue + dec)
    else
        Udecimal dec


let sdecimal (Udecimal decimal) = decimal

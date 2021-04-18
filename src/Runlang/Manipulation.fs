module Manipulation

open Interval
open RootList

type Manipulation = Interval list

type ManipulationList = RootList<Manipulation>

type ManipulationScope = 
    | Range of uint * uint
    | EntireList

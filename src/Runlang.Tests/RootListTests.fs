module RootListTests

open FsUnit.Xunit
open Xunit
open RootList

let invalidIndex<'T> : Result<'T, string> = Error "Invalid index"

let cannotRemoveRoot<'T> : Result<'T, string> = 
    Error "Cannot remove root"

let addToRootList list e = RootList.add e list

[<Fact>]
let ``Create root list must return the root element`` () =
    let root = RootList.create 3
    root |> should equal (Root 3)


[<Fact>]
let ``Adding elements must attach new elements to cons`` () =
    let root = RootList.create 3
    let twoElems = RootList.add 2 root
    let threeElems = RootList.add 5 twoElems
    threeElems |> should equal (Cons (5, Cons (2, Root 3)))


[<Fact>]
let ``Length must return the total elements of the root list`` () =
    let root = RootList.create 50
    let length = [1 .. 49] |> List.fold addToRootList root |> RootList.length
    length |> should equal 50

[<Fact>]
let ``Root gets root element of root list`` () =
    let root = RootList.create 50
    let rest = [1 .. 49] |> List.fold addToRootList root
    let rootValue = RootList.root rest
    rootValue |> should equal 50


[<Fact>]
let ``Get at index lower than 0 must return an Error`` () =
    let root = RootList.create 11
    let rest = [1 .. 10] |> List.fold addToRootList root
    let element = RootList.get -3 rest
    element |> should equal invalidIndex<int>


[<Fact>]
let ``Get at index higher than length must return an Error`` () =
    let root = RootList.create 11
    let rest = [1 .. 10] |> List.fold addToRootList root
    let element = RootList.get 12 rest
    element |> should equal invalidIndex<int>


[<Fact>]
let ``Get index inside the list must return corresponding element`` () =
    let root = RootList.create 4
    let rest = [5 .. 15] |> List.fold addToRootList root
    let elements =
        [
            RootList.get 8 rest;
            RootList.get 0 rest;
            RootList.get 3 rest;
        ] |> List.map (fun (Ok value) -> value)
    elements |> should equal [ 12; 4; 7; ]


[<Fact>]
let ``Remove the root element must return an error`` () =
    let root = RootList.create 4
    let removed = RootList.remove 0 root
    removed |> should equal cannotRemoveRoot<RootList<int>>
    

[<Fact>]
let ``Removing and index higher than the length must return an Error`` () =
    let root = RootList.create 11
    let rest = [1 .. 10] |> List.fold addToRootList root
    let removed = RootList.remove 12 rest
    removed |> should equal invalidIndex<RootList<int>>


[<Fact>]
let ``Removing index inside length range must return a list without that element`` () =
    let root = RootList.create 0
    let list = [1 .. 15] |> List.fold addToRootList root
    let (Ok removed) = RootList.remove 12 list
    let expected = [1 .. 11] @ [13 .. 15] |> List.fold addToRootList root 
    removed |> should equal expected

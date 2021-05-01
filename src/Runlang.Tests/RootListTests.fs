module RootListTests

open FsUnit.Xunit
open Xunit
open RootList
open Utils

[<Fact>]
let ``Create root list must return the root element`` () =
    let root = RootList.create 3
    root |> should equal (Root 3)


[<Fact>]
let ``Adding elements must attach new elements to cons`` () =
    let root = RootList.create 3
    let twoElems = RootList.add root 2
    let threeElems = RootList.add twoElems 5
    threeElems |> should equal (Cons (5, Cons (2, Root 3)))


[<Fact>]
let ``Length must return the total elements of the root list`` () =
    let root = RootList.create 50
    let length = [1 .. 49] |> List.fold RootList.add root |> RootList.length
    length |> should equal 50

[<Fact>]
let ``Root gets root element of root list`` () =
    let root = RootList.create 50
    let rest = [1 .. 49] |> List.fold RootList.add root
    let rootValue = RootList.root rest
    rootValue |> should equal 50


[<Fact>]
let ``Get at index lower than 0 must return an Error`` () =
    let root = RootList.create 11
    let rest = [1 .. 10] |> List.fold RootList.add root
    let element = RootList.get -3 rest
    element |> shouldBeError


[<Fact>]
let ``Get at index higher than length must return an Error`` () =
    let root = RootList.create 11
    let rest = [1 .. 10] |> List.fold RootList.add root
    let element = RootList.get 12 rest
    element |> shouldBeError


[<Fact>]
let ``Get index inside the list must return corresponding element`` () =
    let root = RootList.create 4
    let rest = [5 .. 15] |> List.fold RootList.add root
    let elements =
        [
            RootList.get 8 rest;
            RootList.get 0 rest;
            RootList.get 3 rest;
        ] |> List.map (fun value -> ok value)
    elements |> should equal [ 12; 4; 7; ]


[<Fact>]
let ``Remove the root element must return an error`` () =
    let root = RootList.create 4
    let removed = RootList.remove 0 root
    removed |> shouldBeError
    

[<Fact>]
let ``Removing and index higher than the length must return an Error`` () =
    let root = RootList.create 11
    let rest = [1 .. 10] |> List.fold RootList.add root
    let removed = RootList.remove 12 rest
    removed |> shouldBeError


[<Fact>]
let ``Removing index inside length range must return a list without that element`` () =
    let root = RootList.create 0
    let list = [1 .. 15] |> List.fold RootList.add root
    let removed = RootList.remove 12 list |> ok
    let expected = [1 .. 11] @ [13 .. 15] |> List.fold RootList.add root 
    removed |> should equal expected

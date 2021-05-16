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
let ``Top gets the top of the list`` () =
    let root = RootList.create 50
    let rest = [1 .. 49] |> List.fold RootList.add root
    let rootValue = RootList.top rest
    rootValue |> should equal 49

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


[<Fact>]
let ``Copying index inside length range must return a list with that element in head also`` () =
    let root = RootList.create 0
    let list = [1 .. 15] |> List.fold RootList.add root
    let copied = RootList.copy 8 list |> ok
    let expected = [1 .. 15] @ [8] |> List.fold RootList.add root
    copied |> should equal expected


[<Fact>]
let ``Copying index outside length range must return an error`` () =
    let root = RootList.create 11
    let rest = [1 .. 10] |> List.fold RootList.add root
    let copied = RootList.copy 12 rest
    copied |> shouldBeError


[<Fact>]
let ``Moving index inside length range must return a list with that element in head only`` () =
    let root = RootList.create 0
    let list = [1 .. 15] |> List.fold RootList.add root
    let moved = RootList.move 7 list |> ok
    let expected = [1 .. 6] @ [8..15] @ [7] |> List.fold RootList.add root
    moved |> should equal expected


[<Fact>]
let ``Moving index outside length range must return an error`` () =
    let root = RootList.create 11
    let rest = [1 .. 10] |> List.fold RootList.add root
    let moved = RootList.move 12 rest
    moved |> shouldBeError

[<Fact>]
let ``Moving root element must return an error`` () =
    let root = RootList.create 1
    let rest = [1 .. 10] |> List.fold RootList.add root
    let moved = RootList.move 0 rest
    moved |> shouldBeError

[<Fact>]
let ``To list must yield the equivalent list`` () =
    let root = RootList.create 0
    let rest = [1 .. 10] |> List.fold RootList.add root
    let list = RootList.toList rest
    list |> should equal [0..10]

[<Fact>]
let ``From list using a empty list must yield error`` () =
    RootList.fromList [] |> shouldBeError

[<Fact>]
let ``From list using a non empty list must return an equal root list to`` () =
    let list = [1 .. 10] 
    RootList.fromList list |> ok |> RootList.toList |> should equal list

[<Fact>]
let ``Removing range inside length range must return a list without those elements`` () =
    let root = RootList.create 0
    let list = [1 .. 15] |> List.fold RootList.add root
    let removed = RootList.removeRange (Some (7, 12)) list |> ok
    let expected = [1 .. 6] @ [13 .. 15] |> List.fold RootList.add root
    removed |> should equal expected

[<Fact>]
let ``Removing without specifying range should remove everything but root`` () =
    let root = RootList.create 0
    let list = [1 .. 15] |> List.fold RootList.add root
    let removed = RootList.removeRange None list |> ok
    removed |> should equal root

[<Fact>]
let ``Removing range outside length range or with wrong indexing must return an error`` () =
    let root = RootList.create 0
    let list = [1 .. 15] |> List.fold RootList.add root
    let removed = [
        RootList.removeRange (Some (11, 17)) list
        RootList.removeRange (Some (-1, 4)) list
        RootList.removeRange (Some (6, 4)) list
    ]
    removed |> List.iter shouldBeError

[<Fact>]
let ``Removing range from root must return an error`` () =
    let root = RootList.create 0
    let list = [1 .. 15] |> List.fold RootList.add root
    let removed = RootList.removeRange (Some (0, 3)) list
    removed |> shouldBeError

[<Fact>]
let ``Copying range inside length range must return a list with those elements at the head`` () =
    let root = RootList.create 0
    let list = [1 .. 15] |> List.fold RootList.add root
    let copied = RootList.copyRange (Some (7, 12)) list |> ok
    let expected = [1 .. 15] @ [7 .. 12] |> List.fold RootList.add root
    copied |> should equal expected

[<Fact>]
let ``Copying range outside length range or with wrong indexing must return an error`` () =
    let root = RootList.create 0
    let list = [1 .. 15] |> List.fold RootList.add root
    let copied = [
        RootList.copyRange (Some (11, 17)) list
        RootList.copyRange (Some (-1, 4)) list
        RootList.copyRange (Some (6, 4)) list
    ]
    copied |> List.iter shouldBeError

[<Fact>]
let ``Copying range from root must add elements to head`` () =
    let root = RootList.create 0
    let list = [1 .. 15] |> List.fold RootList.add root
    let copied = RootList.copyRange (Some (0, 3)) list |> ok
    let expected = [1 .. 15] @ [0 .. 3] |> List.fold RootList.add root
    copied |> should equal expected

[<Fact>]
let ``Copying without specifying a range must duplicate the list`` () =
    let root = RootList.create 0
    let list = [1 .. 15] |> List.fold RootList.add root
    let copied = RootList.copyRange None list |> ok
    let expected = [1 .. 15] @ [0 .. 15] |> List.fold RootList.add root
    copied |> should equal expected

[<Fact>]
let ``Moving range inside length range must return a list with those elements in head`` () =
    let root = RootList.create 0
    let list = [1 .. 15] |> List.fold RootList.add root
    let moved = RootList.moveRange (Some (7, 12)) list |> ok
    let expected = 
        [1 .. 6] @ [13 .. 15] @ [7 .. 12] |> List.fold RootList.add root
    moved |> should equal expected

[<Fact>]
let ``Moving range outside length range or with wrong indexing must return an error`` () =
    let root = RootList.create 0
    let list = [1 .. 15] |> List.fold RootList.add root
    let moved = [
        RootList.moveRange (Some (11, 17)) list
        RootList.moveRange (Some (-1, 4)) list
        RootList.moveRange (Some (6, 4)) list
    ]
    moved |> List.iter shouldBeError

[<Fact>]
let ``Moving range from root must return an error`` () =
    let root = RootList.create 0
    let list = [1 .. 15] |> List.fold RootList.add root
    let moved = RootList.moveRange (Some (0, 3)) list
    moved |> shouldBeError

[<Fact>]
let ``Moving without specifying a range must be a no op`` () =
    let root = RootList.create 0
    let list = [1 .. 15] |> List.fold RootList.add root
    let moved = RootList.moveRange None list |> ok
    moved |> should equal list

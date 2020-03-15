module Synthesis

let abelar x =
    ((x > 12) && (x < 3097) && (x % 12) = 0)

let area b h =
    match ((b >= 0.0) && (h >= 0.0)) with
    | true -> (0.5 * b * h)
    | false -> failwith "MatchFailException"


let zollo x =
    match (x < 0) with
    | true -> x * -1
    | false -> x * 2

let min x y =
    match (x < y) with
    | true -> x
    | false -> y

let max x y =
    match (x > y) with
    | true -> x
    | false -> y

let ofTime h m s =
    ((h * 3600) + (m * 60) + s)

let toTime timeM =
    match timeM >= 0 with
    | true -> ((timeM / 3600), (timeM % 3600)/60, (timeM % 3600)%60)
    | false-> (0, 0, 0)


let digits num =
         let rec dig num count =
             match ((num/10) = 0) with
             |true -> count
             |false -> (dig (num/10) (count + 1))
         dig num 1

let minmax n =
    let a, b, c, d = n
    min(min a b)(min c d), max(max a b)(max c d)

let isLeap y =
    match y < 1582 with
    |true -> failwith "fail"
    |false -> match (y % 4 = 0 && y % 100 <> 0) || (y % 400 = 0) with
              | true -> true
              | false -> false
   

let month num =
    match num with
    |1 -> ("January", 31)
    |2 -> ("February", 28)
    |3 -> ("March", 31)
    |4 -> ("April", 30)
    |5 -> ("May", 31)
    |6 -> ("June", 30)
    |7 -> ("July", 31)
    |8 -> ("August", 31)
    |9 -> ("September", 30)
    |10 -> ("October", 31)
    |11 -> ("November", 30)
    |12 -> ("December", 31)
    |_ -> failwith "fail"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz n =
    let check i = 
        match i % 3 = 0 && i % 5 = 0 with
            | true -> (1, 1, 1)
            | false -> match (i % 3 = 0, i % 5 = 0) with
                       | true, false -> (1, 0, 0)
                       | false, true -> (0, 1, 0)
                       | _-> (0, 0, 0)
    let rec bizFuzz i n acc = 
        match i > n with
        | true -> acc
        | _-> match acc, check i with
              | (a, b, c), (d, e, f) -> bizFuzz (i + 1) n (a + d, b + e, c+ f)
    bizFuzz 1 n (0, 0, 0)

let monthDay d y =
    let isLeapy = isLeap y
    let rec getD i numOfdays = 
        let x = match isLeapy && i = 2 with | true -> 1 | _-> 0
        match d <= numOfdays + x with
        | true -> match month i with
                  | (months, _) -> months
        | false -> match month (i + 1) with
                   | (_, days) -> getD (i+ 1) (numOfdays + days + x)
    match isLeapy with
    | true -> match 1 <= d && d <= 366 with
              | false -> failwith "fail"
              | true -> getD 1 31
    | false -> match 1 <= d && d <= 366 with
               | false -> failwith "fail"
               | true -> getD 1 31

    
             

let coord _ =
    failwith "Not implemented"
  
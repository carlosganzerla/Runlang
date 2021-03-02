module Grammar

option {

}


type Int60 = private Int60 of int
with
    static member Create value =
    if value >= 60 || value < 0 then
        Error "Value must be between 0 and 59"
     else
       Ok (Int60 value)

    static member Extract (Int60 value) = value


type Time = {
    Hours: int
    Minutes: Base60Int
    Seconds: Base60Int
}


type Distance = Meters of uint | Kilometers of decimal

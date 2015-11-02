type scale = Celsius | Fahrenheit | Kelvin | Rankine | Delisle | Newton | Reaumur | Romer
let fromCelsius x = function
    | Celsius -> x
    | Fahrenheit -> x *. 9. /. 5. +. 32.
    | Kelvin -> x +. 273.15
    | Rankine -> (x +. 273.15) *. 9. /. 5.
    | Delisle -> (100. -. x) *. 3. /. 2.
    | Newton  -> x *. 33. /. 100.
    | Reaumur -> x *. 4. /. 5.
    | Romer -> x *. 21. /. 40. +. 7.5
let toCelsius x = function
    | Celsius -> x
    | Fahrenheit -> (x -. 32.) *. 5. /. 9.
    | Kelvin -> x -. 273.15
    | Rankine -> (x -. 491.67) *. 5. /. 9.
    | Delisle -> (100. -. x) *. 2. /. 3.
    | Newton  -> x *. 100. /. 33.
    | Reaumur -> x *. 5. /. 4.
    | Romer -> (x -. 7.5) *. 40. /. 21.

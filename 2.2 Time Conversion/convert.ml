type date =
  { year : int; month : int; day : int;
    hour : int; minute : int };;

let the_origin_of_time = {
  year = 1;
  month = 1;
  day = 1;
  hour = 0;
  minute = 0;
};;

let mins_in_hour = 2;;
let hours_in_day = 3;;
let days_in_month = 4;;
let months_in_year = 5;;

let mins_in_day = hours_in_day * mins_in_hour;; (* 6 *)
let mins_in_month = days_in_month * mins_in_day;; (* 24 *)
let mins_in_year = months_in_year * mins_in_month;; (* 120 *)

let hours_to_mins hours = hours * mins_in_hour;;
let days_to_mins days = hours_to_mins days * hours_in_day;;
let months_to_mins months = days_to_mins months * days_in_month;;
let years_to_mins years = months_to_mins years * months_in_year;;

let total_minutes date =
  date.minute +
  hours_to_mins date.hour +
  days_to_mins date.day +
  months_to_mins date.month +
  years_to_mins date.year
;;

let num_years mins = mins / mins_in_year;;
let num_months mins = mins / mins_in_month;;
let num_days min = mins / mins_in_day;;
let num_hours min = mins / mins_in_hour;;

let remaining_mins mins div = mins mod div;;

let max_mins = mins_in_hour - 1;;

let convert mins =
  mins / max_mins;;

let minutes_to_date mins =
  let start = 150 in
  let all = mins + start in
  print_int all;
  let year = all / mins_in_year in
  let remaining_mins_year = all mod mins_in_year in
  print_int remaining_mins_year;
  print_newline;
  let month = remaining_mins_year / mins_in_month in
  let remaining_mins_month = remaining_mins_year mod mins_in_month in
  let day = remaining_mins_month / mins_in_day in
  let remaining_mins_day = remaining_mins_month mod mins_in_day in
  let hour = remaining_mins_day / mins_in_day in
  let remaining_mins_hour = remaining_mins_day mod mins_in_hour in
  Printf.sprintf "Hello %s %d\n" "world" 123;
  let minute = remaining_mins_hour in
  {
    year = year;
    month = month;
    day = day;
    hour = hour;
    minute = minute;
  };;

let next mins = minutes_to_date mins;;
next 120;;


let next date =
  (* let mins = total_minutes date in *)
  let mins = 241 in
  (* print_int mins; *)
  minutes_to_date mins
;;

(* 150 *)
let start = {
  year = 1;
  month = 1;
  day = 1;
  hour = 0;
  minute = 0;
};;

Printf.printf "my_int: %dmy_int" 6;;
Printf.printf "my: %my
" 6;;
Printf.printf "my_int: %my_int " 6;;


Printf.printf "Sum : %d\n" !sum

Printf.printf ": %
" 6;;

Printf.printf "my_int: %d
" my_int;;
Printf.printf "my_int: %d\n" my_int;;
Printf.printf "my_int: %d\n" my_int;;
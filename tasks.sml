(*task 1*)
fun is_older (date1: int*int*int, date2: int*int*int) =
 if (#1 date1) > (#1 date2)
 then false
 else if (#2 date1) > (#2 date2)
 then false
 else if (#3 date1) > (#3 date2)
 then false
 else true

fun provided_test1 () = 
    let val date1 = (20,6,2003)
         val date2 = (21,6,2005)
    in
         is_older(date1,date2)
    end


val test1 = provided_test1();

(*task 2*)
fun number_in_month (dates: (int*int*int) list, month: int) =
     List.length (List.filter (fn date => #2 date = month) dates);

fun provided_test2 () = 
    let val dates = [(2004,2,9),(2003,3,23),(2006,4,30),(2006,12,15),(2006,3,1)];
         val month = 3;
    in
         number_in_month(dates,month)
    end

val test2 = provided_test2();

(*task 3*)

fun number_in_months
     (dates: (int*int*int) list, months: int list) = 
          if List.length months = 0 then 0
          else number_in_months(dates, tl months) + number_in_month(dates, hd months);

fun provided_test3 () = 
    let val dates = [(2004,2,9),(2003,3,23),(2006,4,30),(2014,4,12),(2006,12,15),(2006,3,1)];
         val months = [3,4,5];
    in
         number_in_months(dates,months)
    end

val test3 = provided_test3();

(*task 4*)

fun dates_in_month (dates: (int*int*int) list, month: int) =
     List.filter (fn date => #2 date = month) dates;

fun provided_test4 () = 
    let val dates = [(2004,2,9),(2003,3,23),(2006,4,30),(2006,12,15),(2006,3,1)];
         val month = 3;
    in
         dates_in_month(dates,month)
    end

val test4 = provided_test4();

(*task 5*)


fun dates_in_months
     (dates: (int*int*int) list, months: int list) = 
          if List.length months = 0 then []
          else dates_in_months(dates, tl months) @ dates_in_month(dates, hd months);

fun provided_test5 () = 
    let val dates = [(2004,2,9),(2003,3,23),(2006,4,30),(2014,4,12),(2006,12,15),(2006,3,1)];
         val months = [3,4,5];
    in
         dates_in_months(dates,months)
    end

val test5 = provided_test5();

(*task 6*)

fun get_nth (strings: string list, n: int) =
     if n = 1 then hd strings
     else get_nth(tl strings, n - 1 );

fun provided_test6 () = 
    let val strings = ["(1)What","(2)is","(3)the","(4)meaning","(5)of","(6)life,","(6)the","(7)universe",
    "(8)and","(9)everything?"];
         val n = 5;
    in
         get_nth(strings, n)
    end

val test6 = provided_test6();

(*task 7*)
val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];

fun date_to_string(date: int*int*int) = get_nth(months,#2 date) ^ " " ^
     Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date);

fun provided_test7 () = 
     let 
          val date = (2022, 2, 28); 
     in
          date_to_string(date)
     end

val test7 = provided_test7();

(*task 8*)

fun number_before_reaching_sum (sum: int, number_list:int list) = 
     if List.length (number_list) = 0 then 0
     else if hd(number_list) < sum
     then 1 + number_before_reaching_sum(sum,((hd(number_list)
          + hd(tl(number_list)))::(tl(tl(number_list)))))
     else 0;

fun provided_test8 () = 
     let
          val sum = 6;
          val number_list = [1, 1, 3, 2, 1, 1, 1, 1];
     in
          number_before_reaching_sum(sum,number_list)
     end;

val test8 = provided_test8();

(*task 9*)

val months_to_days = [  31, 28,  31,  30,  31,  30,  31,  31,  30,  31,  30,  31];

fun what_month (day: int) = number_before_reaching_sum(day, months_to_days) + 1;

fun provided_test9 () = 
     let
          val day = 68;
     in
          what_month(day)
     end;

val test9 = provided_test9();

(*task 10*)

fun month_range(day1: int, day2: int) = 
if day1 > day2 then []
else what_month(day1) :: month_range(day1 +1, day2);

fun provided_test10 () = 
     let
          val day1 = 25;
          val day2 = 35;
     in
          month_range(day1, day2)
     end;

val test10 = provided_test10();

(*task 11*)

fun get_oldest_date (dates: (int*int*int) list) =
     if List.length dates = 0 then NONE
     else if List.length dates = 1 then SOME(hd dates)
     else if is_older(hd dates, hd(tl dates)) then get_oldest_date(hd(dates)::tl(tl(dates)))
     else get_oldest_date(hd(tl(dates))::tl(tl(dates)));

fun provided_test11 () = 
     let
          val dates = [(12,12,2013),(12,12,2012),(12,12,2010),(12,12,2020),(12,12,2015)];
     in
          get_oldest_date(dates)
     end;

val test11 = provided_test11();

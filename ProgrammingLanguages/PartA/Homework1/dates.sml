(* Programming Languages Part A *)


(* 1. Takes two dates and evaulates as true if first date comes before the second date *)
fun is_older(d1 : int*int*int, d2 : int*int*int) =
    if d1 = d2 then false
    else if #1 d1 < #1 d2 then true
    else if #2 d1 < #2 d2 then true
    else if #3 d1 < #3 d2 then true
    else false
	     

(* 2. Returns the number of dates that are in the month provided *)
fun number_in_month(dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else
	(if #2 (hd dates) = month then 1 else 0) + number_in_month(tl dates, month);

(* 3. Returns the number of dates in the list of dates that are in any of the monthd in the list of months *)
fun number_in_months(dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* 4. Returns a list holding the dates from the argument list of dates that are in the month. *)
fun dates_in_month(dates : (int*int*int) list, month : int) =
    if null dates then []
    else
	if #2 (hd dates) = month
	then (hd dates) :: dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month)

(* 5. Returns a list holding the dates from the arguement list of dates that are in any of the months in the list of months. *)
fun dates_in_months(dates : (int*int*int) list, months : int list) =
    if null dates orelse null months
    then []
    else dates_in_month(dates, hd months) @  dates_in_months(dates, tl months)

(* 6. Returns the nth element of the list *)
fun get_nth(los : string list, n : int) =
    if n = 1 then hd los
    else get_nth(tl los, n-1)
			    
(* 7. Returns the date in the form of Month Day, Year. *)
fun date_to_string(date : (int*int*int)) =
    let
	val dates = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(dates, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

(* 8. Returns an int n such that the first n elements of the list add to less than sum, but the first n+1 elements of the list add to sum or more. *)
fun number_before_reaching_sum(sum : int, numbers : int list) =
    let
	fun count(numbers : int list, accumulator: int, n: int) =
	    if hd numbers + accumulator >= sum then n
	    else count(tl numbers, accumulator + hd numbers, n + 1)
    in
	count(numbers, 0, 0)
    end

(* 9. Returns what month a day of the year is in. *)
fun what_month(day : int) =
    let
	val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum(day, days_in_month) + 1
    end

(* Returns true if value exits in a list. *)
fun exists(lst : int list, n : int) =
    if null lst then false
    else if hd lst = n then true
    else exists(tl lst, n)

(* 10. Returns an int list of months between 2 days. *)
fun month_range(day1 : int, day2 : int) =
    if day1 > day2 then []
    else what_month(day1) :: month_range(day1 + 1, day2)


(* 11. Returns NONE if the list is empty and SOME d if the date d is the oldest date in the list. *)
fun oldest(dates : (int*int*int) list)=
    if null dates then NONE
    else
	let
	    val tail_answer = oldest(tl dates)
	in
	    if isSome tail_answer andalso is_older(valOf tail_answer, hd dates) then tail_answer
	    else SOME (hd dates)
	end


	
    

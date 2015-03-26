-module(converters).
-compile(export_all).

f()-> to_bcd("0034").

to_bcd(String)-> 
	case length(String) rem 2 of
		0->to_bcd(String, []);
		1->to_bcd(["0"|String], [])
	end.
to_bcd([H1,H2|T], Res)->
	R = list_to_integer([H1, H2]),
	to_bcd(T, [R|Res]);
to_bcd([], Res)-> lists:reverse(Res).

to_hex(String)-> 
	case length(String) rem 2 of
		0->to_hex(String, []);
		1->to_hex(["0"|String], [])
	end.
to_hex([H1,H2|T], Res)->
	R = list_to_integer([H1,H2], 16),
	to_hex(T, [R|Res]);
to_hex([], Res)-> lists:reverse(Res).

bin_to_hex(BinString)-> 
	{ok,[Val],[]} = io_lib:fread("~2u", BinString),
	Val. 
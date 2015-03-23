-module(converters).
-compile(export_all).

to_bcd(String)-> 
	case length(String) rem 2 of
		0->to_bcd(String, []);
		1->to_bcd(["0"|String], [])
	end.
to_bcd([H1,H2|T], Res)->
	R = ((H1-48) bsl 4)+(H2-48),
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
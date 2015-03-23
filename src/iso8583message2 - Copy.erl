-module(iso8583message).
%-export([start/0,test/0, f2/1,f3/0]).
-compile(export_all).
-include("field_constants.hrl").
-record(field_type, {type, length}).
-record(field, {name,order_number}).


print_message({MTI, Dict})->print_message(MTI, get_message_structure(MTI), Dict).
print_message(MTI, MessageStructure, Dict)->
	io:format("~n[MTI:~p",[MTI]),
	print_message(MessageStructure, Dict).
print_message([{#field{name=Key,order_number=_},{#field_type{type=_,length=_}}}|Tail], Dict)->
	Value = case dict:find(Key, Dict) of
		{ok, Val}->Val;
		error->""
	end,
	io:format("~n~p:~s",[Key, Value]),	
	print_message(Tail, Dict);
print_message([], _)->io:format("~n]").	

f10()->
	MessageValues = dict:from_list(
			[
			{?MTI, "0210"}
			{?PRIMARY_ACCOUNT_NUMBER, "9826132500000000181"},
			{?PROCESSING_CODE, "000000"},
			{?AMOUNT_TRANSACTION, "000000001000"},
			{?TRANSMISSION_DATE_AND_TIME, "0914225918"},
			{?SYSTEM_TRACE_AUDIT_NUMBER, "000155"},
			{?TIME_LOCAL_TRANSACTION, "235918"},
			{?DATE_LOCAL_TRANSACTION, "0914"},
			{?DATE_EXPIRATION, "0201"},
			{?DATE_SETTLEMENT, "0914"},
			{?MERCHANTS_TYPE, "0000"},
			{?POINT_OF_SERVICE_ENTRY_MODE, "020"},
			{?POINT_OF_SERVICE_CONDITION_CODE, "00"},
			{?AMOUNT_TRANSACTION_FEE, "C00000000"},
			{?ACQUIRING_INSTITUTION_IDENT_CODE, "000002"},
			{?FORWARDING_INSTITUTION_IDENT_CODE, "000002"},
			{?TRACK_2_DATA, "9826132500000000181=02017650000000000"},
			{?RETRIEVAL_REFERENCE_NUMBER, "PCZ257000155"},
			{?CARD_ACCEPTOR_TERMINAL_IDENTIFICACION, "02117986"},
			{?CARD_ACCEPTOR_IDENTIFICATION_CODE, "000000000020553"},
			{?CARD_ACCEPTOR_NAME_LOCATION,
					"Location2              Welwyn Garden01GB"},
			{?CURRENCY_CODE_TRANSACTION, "826"},
			{?RESERVED_NATIONAL, "0000456445|0000|PCZ257000155"},
			{?RESERVED_PRIVATE_USE1, "20010120402C101"}
			]),
			
	Message = generate_message("0200", MessageValues),
	
	io:format("~p~n~n~n~n~n~n~n~n",[MessageValues]),
	
	file:write_file("out.bin", Message),

	{ok, Data} = file:read_file("out.bin"),
	B = binary:bin_to_list(Data,{0,byte_size(Data)}),
	print_message(get_message_values(B)).
	%io:format("~p~n~n~n~n~n~n~n~n",[get_message_values(B)]).
	
f11()->
	MessageValues = dict:from_list(
		[{?PRIMARY_ACCOUNT_NUMBER, "9826132500000000181"},
		{?RESPONSE_CODE, "20"},
		{?ADDITIONAL_RESPONSE_DATA, "1234567890123456789012345"}
		]),
			
	Message = generate_message("0210", MessageValues),
	
	io:format("~p~n~n~n~n~n~n~n~n",[MessageValues]),
	
	
	MMM = iso_message:get_message_values(Message),
	io:format("???????~n"),	
	print_message(MMM),
	io:format("???????~n"),	
	file:write_file("out0210.bin", Message),

	{ok, Data} = file:read_file("out0210.bin"),
	B = binary:bin_to_list(Data,{0,byte_size(Data)}),
	print_message(get_message_values(B)).
	

f9()->
    {ok, Data} = file:read_file("out.bin"),
	B = binary:bin_to_list(Data,{0,byte_size(Data)}),
	get_message_values(B).	

get_message_values(Bytes)->
	MessageBytes = lists:sublist(Bytes, 3, length(Bytes)),
	MTI = lists:sublist(MessageBytes, 1, 4),
	Header = lists:sublist(MessageBytes, 5, 16),
	Bin = lists:append([io_lib:format("~.2B", [B]) || B <- Header]),
	HeaderBinary = lists:append(join_header(Bin)),
	ValuesBytes = lists:sublist(MessageBytes, 21, length(MessageBytes)),
	MessageStructure = rearrange_for_parse(get_message_structure(MTI)),
	{MTI, parse_massage(HeaderBinary, MessageStructure, ValuesBytes)}.

parse_massage(BytesBinary, MessageStructure, ValuesBytes)->
	parse_massage(BytesBinary, MessageStructure, ValuesBytes, 1, dict:new()).
parse_massage([HeadBit|TailBits], MessageStructure, ValuesBytes, Counter, Dict)->
	case HeadBit of
		49 -> 
			if 
				Counter == 1 -> parse_massage(TailBits, MessageStructure, ValuesBytes, Counter+1, Dict);
				true ->
					Field = dict:find(Counter, MessageStructure),
					case Field of
						{ok, Data} ->
									[{#field{name=Key,order_number=_},{#field_type{type=Type,length=Length}}}] = Data,
									{Value, Rest} = case Type of
														var -> get_var_length(Length, ValuesBytes);
														fixed -> get_fixed_length(Length, ValuesBytes)
													end,
									parse_massage(TailBits, MessageStructure, Rest, Counter+1, dict:append(Key, Value, Dict));
						error -> parse_massage(TailBits, MessageStructure, ValuesBytes, Counter+1, Dict)
					end
			end;
		48 -> parse_massage(TailBits, MessageStructure, ValuesBytes, Counter+1, Dict)
	end;
parse_massage([], _, _, _, Dict)-> Dict.

get_var_length(Length, ValuesBytes)->
	BytesWithLength = lists:sublist(ValuesBytes, 1, Length),
	L = list_to_integer(BytesWithLength),
	Value = lists:sublist(ValuesBytes, Length+1, L),
	Rest = lists:sublist(ValuesBytes, Length+L+1, length(ValuesBytes)),	
	{Value, Rest}.

get_fixed_length(Length, ValuesBytes)->
	Value = lists:sublist(ValuesBytes, 1, Length),
	Rest = lists:sublist(ValuesBytes, Length+1, length(ValuesBytes)),	
	{Value, Rest}.	

rearrange_for_parse(List)->	
	rearrange_for_parse(List, dict:new()).
rearrange_for_parse([{#field{name=_,order_number=OrderNumber},{#field_type{type=_,length=_}}} = Head|T], Dict)->
	rearrange_for_parse(T, dict:append(OrderNumber, Head, Dict));
rearrange_for_parse([], Dict)->
	Dict.	
	

join_header(List)-> join_header(List, []).
join_header([H|T], Res)-> join_header(T, [c_z(H)|Res]);		
join_header([], Res)-> lists:reverse(Res).	
	
	
c_z(Str)->
	case length(Str) of
		8 -> Str;
		_-> c_z([48|Str])
	end.
	
	
%=================================================================================================	

generate_message(MTI, MessageValues)->
	WholeMessage = generate_message_without_length(MTI, MessageValues),
	Length = length(WholeMessage),
	lists:append([Length div 256, Length rem 256], WholeMessage).
	
generate_message_without_length(MTI, MessageValues)->
	ISO8385Type = get_message_structure(MTI),
	Bitmap = get_message_bitmap_as_array(ISO8385Type, MessageValues),
	BitmapBytes = stage2(generate_bitmap(Bitmap)),
	MessageBytes = get_message_bytes(ISO8385Type, MessageValues),
	lists:append(MTI, lists:append(BitmapBytes, MessageBytes)).	
	
get_answer_code_for_message(MTI)->
		case MTI of
			"0200" -> "0210"
		end.
	
get_message_structure(MTI)->
	case MTI of
		"0200" -> [
			{#field{name=?MTI,order_number=1},{?CODER_BCD}},
			{#field{name=?PRIMARY_ACCOUNT_NUMBER,order_number=2},{?LLVAR,?CODER_BCD}},		
					{#field{name=?PROCESSING_CODE,order_number=3},{?FIXED,?CODER_BCD,?LENGTH=6}},
					{#field{name=?AMOUNT_TRANSACTION,order_number=4},{#field_type{type=fixed,length=12}}},
					{#field{name=?TRANSMISSION_DATE_AND_TIME,order_number=7},{#field_type{type=fixed,length=10}}},
					{#field{name=?SYSTEM_TRACE_AUDIT_NUMBER,order_number=11},{#field_type{type=fixed,length=6}}},
					{#field{name=?TIME_LOCAL_TRANSACTION,order_number=12},{#field_type{type=fixed,length=6}}},
					{#field{name=?DATE_LOCAL_TRANSACTION,order_number=13},{#field_type{type=fixed,length=4}}},
					{#field{name=?DATE_EXPIRATION,order_number=14},{#field_type{type=fixed,length=4}}},
					{#field{name=?DATE_SETTLEMENT,order_number=15},{#field_type{type=fixed,length=4}}},
					{#field{name=?MERCHANTS_TYPE,order_number=18},{#field_type{type=fixed,length=4}}},
					{#field{name=?POINT_OF_SERVICE_ENTRY_MODE,order_number=22},{#field_type{type=fixed,length=3}}},
					{#field{name=?POINT_OF_SERVICE_CONDITION_CODE,order_number=25},{#field_type{type=fixed,length=2}}},
					{#field{name=?AMOUNT_TRANSACTION_FEE,order_number=28},{#field_type{type=fixed,length=9}}},
					{#field{name=?ACQUIRING_INSTITUTION_IDENT_CODE,order_number=32},{#field_type{type=var,length=2}}},
					{#field{name=?FORWARDING_INSTITUTION_IDENT_CODE,order_number=33},{#field_type{type=var,length=2}}},
					{#field{name=?TRACK_2_DATA,order_number=35},{#field_type{type=var,length=2}}},
					{#field{name=?RETRIEVAL_REFERENCE_NUMBER,order_number=37},{#field_type{type=fixed,length=12}}},
					{#field{name=?CARD_ACCEPTOR_TERMINAL_IDENTIFICACION,order_number=41},{#field_type{type=fixed,length=8}}},
					{#field{name=?CARD_ACCEPTOR_IDENTIFICATION_CODE,order_number=42},{#field_type{type=fixed,length=15}}},
					{#field{name=?CARD_ACCEPTOR_NAME_LOCATION,order_number=43},{#field_type{type=fixed,length=40}}},
					{#field{name=?CURRENCY_CODE_TRANSACTION,order_number=49},{#field_type{type=fixed,length=3}}},
					{#field{name=?RESERVED_NATIONAL,order_number=59},{#field_type{type=var,length=3}}},	
					{#field{name=?RESERVED_PRIVATE_USE1,order_number=123},{#field_type{type=var,length=3}}},	
					{#field{name=?RESERVED_PRIVATE_USE2,order_number=127},{#field_type{type=var,length=3}}}
					];
		"0210" -> [
					{#field{name=?PRIMARY_ACCOUNT_NUMBER,order_number=2},{#field_type{type=var,length=2}}},
					{#field{name=?RESPONSE_CODE,order_number=39},{#field_type{type=fixed,length=2}}},
					{#field{name=?RETRIEVAL_REFERENCE_NUMBER,order_number=37},{#field_type{type=fixed,length=12}}},	
					{#field{name=?ADDITIONAL_RESPONSE_DATA,order_number=44},{#field_type{type=fixed,length=25}}}
					];

		"0420" -> [		
					{#field{name=?PRIMARY_ACCOUNT_NUMBER,order_number=2},{#field_type{type=var,length=2}}},
					{#field{name=?PROCESSING_CODE,order_number=3},{#field_type{type=fixed,length=6}}},
					{#field{name=?AMOUNT_TRANSACTION,order_number=4},{#field_type{type=fixed,length=12}}},
					{#field{name=?TRANSMISSION_DATE_AND_TIME,order_number=7},{#field_type{type=fixed,length=10}}},
					{#field{name=?SYSTEM_TRACE_AUDIT_NUMBER,order_number=11},{#field_type{type=fixed,length=6}}},
					{#field{name=?TIME_LOCAL_TRANSACTION,order_number=12},{#field_type{type=fixed,length=6}}},
					{#field{name=?DATE_LOCAL_TRANSACTION,order_number=13},{#field_type{type=fixed,length=4}}},
					{#field{name=?DATE_EXPIRATION,order_number=14},{#field_type{type=fixed,length=4}}},
					{#field{name=?DATE_SETTLEMENT,order_number=15},{#field_type{type=fixed,length=4}}},
					{#field{name=?MERCHANTS_TYPE,order_number=18},{#field_type{type=fixed,length=4}}},
					{#field{name=?POINT_OF_SERVICE_ENTRY_MODE,order_number=22},{#field_type{type=fixed,length=3}}},
					{#field{name=?POINT_OF_SERVICE_CONDITION_CODE,order_number=25},{#field_type{type=fixed,length=2}}},
					{#field{name=?AMOUNT_TRANSACTION_FEE,order_number=28},{#field_type{type=fixed,length=9}}},
					{#field{name=?ACQUIRING_INSTITUTION_IDENT_CODE,order_number=32},{#field_type{type=var,length=2}}},
					{#field{name=?FORWARDING_INSTITUTION_IDENT_CODE,order_number=33},{#field_type{type=var,length=2}}},
					{#field{name=?TRACK_2_DATA,order_number=35},{#field_type{type=var,length=2}}},
					{#field{name=?RETRIEVAL_REFERENCE_NUMBER,order_number=37},{#field_type{type=fixed,length=12}}},
					{#field{name=?RESPONSE_CODE,order_number=39},{#field_type{type=fixed,length=2}}},					
					{#field{name=?CARD_ACCEPTOR_TERMINAL_IDENTIFICACION,order_number=41},{#field_type{type=fixed,length=8}}},
					{#field{name=?CARD_ACCEPTOR_IDENTIFICATION_CODE,order_number=42},{#field_type{type=fixed,length=15}}},
					{#field{name=?CARD_ACCEPTOR_NAME_LOCATION,order_number=43},{#field_type{type=fixed,length=40}}},
					{#field{name=?CURRENCY_CODE_TRANSACTION,order_number=49},{#field_type{type=fixed,length=3}}},
					{#field{name=?RESERVED_ISO2,order_number=56},{#field_type{type=var,length=3}}},
					{#field{name=?RESERVED_NATIONAL,order_number=59},{#field_type{type=var,length=3}}},	
					{#field{name=?ORIGINAL_DATA_ELEMENTS,order_number=90},{#field_type{type=fixed,length=42}}},					
					{#field{name=?REPLACEMENT_AMOUNTS,order_number=95},{#field_type{type=fixed,length=42}}},					
					{#field{name=?RESERVED_PRIVATE_USE1,order_number=123},{#field_type{type=var,length=3}}},	
					{#field{name=?RESERVED_PRIVATE_USE2,order_number=127},{#field_type{type=var,length=3}}}
					];
		"0430" -> [
					{#field{name=?PRIMARY_ACCOUNT_NUMBER,order_number=2},{#field_type{type=var,length=2}}},
					{#field{name=?RESPONSE_CODE,order_number=39},{#field_type{type=fixed,length=2}}},
					{#field{name=?RESERVED_NATIONAL,order_number=57},{#field_type{type=var,length=3}}}
					]
	end.

get_message_bytes(List, MessageValues)->
	get_message_bytes(List, MessageValues, []).
	
get_message_bytes([{#field{name=Key,order_number=_},{#field_type{type=Type,length=_}}} = Field|T], MessageValues, Bytes)->
		FieldBytes = 
		case dict:find(Key,MessageValues) of
			{ok, Value} ->
					case Type of
						fixed -> lists:append(Bytes, get_fixed_length_field_value(Field, Value));
						var -> lists:append(Bytes, get_variable_length_field_value(Field, Value))
					end;
			error -> Bytes
		end,
	get_message_bytes(T, MessageValues, FieldBytes);
get_message_bytes([], _, Bytes)->
		Bytes.	
		
get_fixed_length_field_value(Field, Value)->
	{#field{name=_,order_number=_},{#field_type{type=_,length=_}}} = Field,
	Value.
	
get_variable_length_field_value(Field, Value)->
	{#field{name=_,order_number=_},{#field_type{type=_,length=Length}}} = Field,
	EncodedLength = format_length(length(Value), Length),
	lists:append(EncodedLength, Value).
	
form_structure(List)->	
	form_structure(List, dict:new()).
form_structure([{#field{name=Key,order_number=_},{#field_type{type=_,length=_}}} = Head|T], Dict)->
	form_structure(T, dict:append(Key, Head, Dict));
form_structure([], Dict)->
	Dict.

get_message_bitmap_as_array(ISO8385Type, Message)->
	get_message_bitmap_as_array(ISO8385Type, Message, [1]).
get_message_bitmap_as_array([{#field{name=Key,order_number=OrderNumber},_}|T], MessageValues, B)->
	Val =  case dict:find(Key, MessageValues) of
				{ok, _} -> 1;
				error -> 0
			end,
	Bytes = 
	case length(B)+1 == OrderNumber of
		true -> [Val|B];
		false -> [Val|fill_zeroes(B, OrderNumber)]
	end,			
	get_message_bitmap_as_array(T, MessageValues, Bytes);
	
get_message_bitmap_as_array([], _, Bytes)->
		lists:reverse(fill_zeroes(Bytes, 129)).	
	
fill_zeroes(Bytes, Finish) when length(Bytes) < Finish-1 ->
	fill_zeroes([0|Bytes], Finish);
fill_zeroes(Bytes, _) ->
	Bytes.		

generate_bitmap(Array)->
	generate_bitmap(Array, []).
generate_bitmap(Array, Res)->
	case Array of
		[0,0,0,0|T] -> 
			generate_bitmap(T, [0|Res]);
		[0,0,0,1|T] -> 
			generate_bitmap(T, [1|Res]);
		[0,0,1,0|T] -> 
			generate_bitmap(T, [2|Res]);
		[0,0,1,1|T] -> 
			generate_bitmap(T, [3|Res]);
		[0,1,0,0|T] -> 
			generate_bitmap(T, [4|Res]);
		[0,1,0,1|T] -> 
			generate_bitmap(T, [5|Res]);
		[0,1,1,0|T] -> 
			generate_bitmap(T, [6|Res]);
		[0,1,1,1|T] -> 
			generate_bitmap(T, [7|Res]);
		[1,0,0,0|T] -> 
			generate_bitmap(T, [8|Res]);
		[1,0,0,1|T] -> 
			generate_bitmap(T, [9|Res]);
		[1,0,1,0|T] -> 
			generate_bitmap(T, [10|Res]);
		[1,0,1,1|T] -> 
			generate_bitmap(T, [11|Res]);
		[1,1,0,0|T] -> 
			generate_bitmap(T, [12|Res]);
		[1,1,0,1|T] -> 
			generate_bitmap(T, [13|Res]);
		[1,1,1,0|T] -> 
			generate_bitmap(T, [14|Res]);
		[1,1,1,1|T] -> 
			generate_bitmap(T, [15|Res]);
		[] -> lists:reverse(Res)
	end.

format_one_digit(Val)->
	48 + Val.
	
format_length(Val, Length)->
	if 
		Length == 2 -> [format_one_digit(Val rem 100 div 10), format_one_digit(Val rem 10)];
		Length == 3 -> [format_one_digit(Val div 100), format_one_digit(Val rem 100 div 10), format_one_digit(Val rem 10)]
	end.	
	
stage2(List)->
	stage2(List, []).
stage2([B0, B1|Tail], Res)->	
	Byte = (B0 bsl 4) bor B1,
	stage2(Tail, [Byte|Res]);
stage2([], Res)->	
		lists:reverse(Res).
				
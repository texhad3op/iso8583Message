-module(iso8583Message).
%-export([start/0,test/0, f2/1,f3/0]).
-compile(export_all).
-include("field_constants.hrl").
-record(field, {name,order_number}).

f13()-> converters:bin_to_hex([48,48,48,48]).
%io_lib:fread("~2u", "1110"). 
%io_lib:format("~.2B", [15]).
%list_to_integer(binary_to_list(X)).

f12()->
	MessageStructure = [
					{#field{name=?LENGTH,order_number=-1},{?NUMERIC, ?CODER_BCD}},
					{#field{name=?MTI,order_number=0},{?NUMERIC, ?CODER_BCD}},
					{#field{name=?_1_BITMAP,order_number=1},{?NUMERIC, ?CODER_HEX}},					
					{#field{name=?_2_PRIMARY_ACCOUNT_NUMBER,order_number=2},{?NUMERIC, ?LLVAR, ?CODER_BCD}},
					{#field{name=?_3_PROCESSING_CODE,order_number=3},{?NUMERIC, ?FIXED,?CODER_BCD,6}},
					{#field{name=?_7_TRANSMISSION_DATE_AND_TIME,order_number=7},{?NUMERIC, ?FIXED,?CODER_BCD,10}},					
					{#field{name=?_11_SYSTEM_TRACE_AUDIT_NUMBER,order_number=11},{?NUMERIC, ?FIXED,?CODER_BCD,6}}					
					],

	MessageValues = dict:from_list(
		[
		{?MTI, "0200"},
		{?_2_PRIMARY_ACCOUNT_NUMBER, "9826132500000000181"},
		{?_3_PROCESSING_CODE, "002000"},
		{?_7_TRANSMISSION_DATE_AND_TIME, "1234567890"},		
		{?_11_SYSTEM_TRACE_AUDIT_NUMBER, "000825"}		
		]),

		Message = generate_message(MessageStructure, MessageValues),
		io:format("Final:~p~n",[Message]),
		
	-7.


rearrange_packager(List)->	
	rearrange_packager(List, dict:new()).
rearrange_packager([{#field{name=_,order_number=OrderNumber},_} = Head|T], Dict)->
	rearrange_packager(T, dict:append(OrderNumber, Head, Dict));
rearrange_packager([], Dict)->
	Dict.
	
generate_message(MessageStructure, MessageValues)->generate_message(MessageStructure, MessageValues, nil).
generate_message(MessageStructure, MessageValues, HeaderAsList)->
	RearrangedStructure = rearrange_packager(MessageStructure),
	Message = generate_internal_message(MessageStructure, MessageValues),
	io:format(">>>>>>>>:Message~p~n",[Message]),	
	EncodedBitmap = get_bitmap(RearrangedStructure, get_message_bitmap_as_array(MessageStructure, MessageValues)),
	io:format(">>>>>>>>:EncodedBitmap~p~n",[EncodedBitmap]),
	MessageAsArray = get_message_as_array(RearrangedStructure, HeaderAsList, EncodedBitmap, Message),
	list_to_binary(MessageAsArray).

	
	
	
get_message_as_array(RearrangedStructure, MessageHeader, Bitmap, Message)->
		MessageWithoutLength = 
			case MessageHeader of
				nil->Bitmap++Message;
				_->MessageHeader++Bitmap++Message
			end,
		MessageLength = generate_length(RearrangedStructure, length(MessageWithoutLength)),
		io:format(">>>>>>>>:MessageLength~p~n",[MessageLength]),
		MessageLength++MessageWithoutLength.		
		
	
generate_internal_message(MessageStructure, MessageValues)-> [4,8].

fill_ascii_zeroes(Bytes, Finish) when length(Bytes) < Finish ->	fill_ascii_zeroes([48|Bytes], Finish);
fill_ascii_zeroes(Bytes, _) -> Bytes.	

generate_length(RearrangedStructure, Val)->
	case dict:find(-1, RearrangedStructure) of
		{ok,[{{_Field,_Length,-1},{_Numeric,Coder}}]} -> 
			case Coder of
				?CODER_ASCII->fill_ascii_zeroes(integer_to_list(Val), 4);			
				?CODER_HEX->
					String = fill_ascii_zeroes(integer_to_list(Val, 16),4),
					converters:to_hex(String);
				?CODER_BCD->
					String = fill_ascii_zeroes(integer_to_list(Val), 4),
					io:format(">>>>>>>>:BCD~p~n",[String]),
					converters:to_bcd(String);
				?CODER_EBCDIC->
					""					
			end;
		_ -> io:format("Message length field format not specified.")
	end.

get_message_bitmap_as_array(ISOStructure, MessageValues)->
	get_message_bitmap_as_array(ISOStructure, MessageValues, [1]).
get_message_bitmap_as_array([{#field{name=Key,order_number=OrderNumber},_}|T], MessageValues, B)->
	case Key of
		?LENGTH->get_message_bitmap_as_array(T, MessageValues, B);
		?MTI->get_message_bitmap_as_array(T, MessageValues, B);	
		?_1_BITMAP->get_message_bitmap_as_array(T, MessageValues, B);	
		_ ->
			Val =  case dict:find(Key, MessageValues) of
						{ok, _} -> 1;
						error -> 0
					end,
			Bytes = case length(B)+1 == OrderNumber of
						true -> [Val|B];
						false -> [Val|fill_zeroes(B, OrderNumber)]
					end,	
			get_message_bitmap_as_array(T, MessageValues, Bytes)	
	end;
get_message_bitmap_as_array([], _, Bytes)->
		lists:reverse(fill_zeroes(Bytes, 129)).	

fill_zeroes(Bytes, Finish) when length(Bytes) < Finish-1 -> fill_zeroes([0|Bytes], Finish);
fill_zeroes(Bytes, _) -> Bytes.	

get_bitmap(RearrangedStructure, Bitmap)->
	Bytes = encode_bitmap(Bitmap),
	io:format(">>Result:~p~n",[Bytes]),
	case dict:find(1, RearrangedStructure) of
		{ok,[{{_Field,_Length, 1},{_Numeric,Coder}}]} -> 
			case Coder of
				?CODER_ASCII->get_bitmap_as_ascii(Bytes);			
				?CODER_HEX->get_bitmap_as_hex(Bytes);
				?CODER_EBCDIC->""					
			end;
		_ -> io:format("Message length field format not specified.")		
	end.
	

	
encode_bitmap(Bitmap) when length(Bitmap) rem 4 == 0 -> encode_bitmap(Bitmap,[]).
encode_bitmap([B1,B2,B3,B4|ReminderBitmap], Result)->	
		Val = [48+B1,48+B2,48+B3,48+B4],
		Converted = converters:bin_to_hex(Val),
		encode_bitmap(ReminderBitmap, [Converted|Result]);
encode_bitmap([], Result) -> lists:reverse(Result).	
		
get_bitmap_as_hex(Bitmap)->	Bitmap.

get_bitmap_as_ascii(Bitmap)->get_bitmap_as_ascii(Bitmap, []).
get_bitmap_as_ascii([H|T], Result)->
		io:format(">>BitmapElement:~p~n",[H+48]),
		get_bitmap_as_ascii(T, [H+48|Result]);	
get_bitmap_as_ascii([], Result)->lists:reverse(Result).
		
-module(iso8583Message).
%-export([start/0,test/0, f2/1,f3/0]).
-compile(export_all).
-include("field_constants.hrl").
-record(field, {name,order_number}).

f12()->
	MessageStructure = [
					{#field{name=?LENGTH,order_number=0},{?NUMERIC, ?CODER_BCD}},
					{#field{name=?MTI,order_number=1},{?NUMERIC, ?CODER_BCD}},
					{#field{name=?_2_PRIMARY_ACCOUNT_NUMBER,order_number=2},{?NUMERIC, ?LLVAR, ?CODER_BCD}},
					{#field{name=?_3_PROCESSING_CODE,order_number=3},{?NUMERIC, ?FIXED,?CODER_BCD,6}}
					],

	MessageValues = dict:from_list(
		[
		{?MTI, "0200"},
		{?_2_PRIMARY_ACCOUNT_NUMBER, "9826132500000000181"},
		{?_3_PROCESSING_CODE, "002000"}
		]),

	get_message_bitmap_as_array(MessageStructure, MessageValues).


f11()->
	MessageStructure = [
					{#field{name=?LENGTH,order_number=0},{?NUMERIC, ?CODER_BCD}},
					{#field{name=?MTI,order_number=1},{?NUMERIC, ?CODER_BCD}},
					{#field{name=?_2_PRIMARY_ACCOUNT_NUMBER,order_number=2},{?NUMERIC, ?LLVAR, ?CODER_BCD}},
					{#field{name=?_3_PROCESSING_CODE,order_number=3},{?NUMERIC, ?FIXED,?CODER_BCD,6}}
					],

	MessageValues = dict:from_list(
		[
		{?MTI, "0200"},
		{?_2_PRIMARY_ACCOUNT_NUMBER, "9826132500000000181"},
		{?_3_PROCESSING_CODE, "002000"}
		]),
		
		Bin = generate_message(MessageStructure, MessageValues).
	
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
	MessageWithHeader = case HeaderAsList of
							nil->Message;
							_->lists:append(HeaderAsList, Message)
						end,
	MessageLength = generate_length(RearrangedStructure, length(Message)),
	list_to_binary([MessageLength|MessageWithHeader]).

generate_internal_message(MessageStructure, MessageValues)-> [4,8].

fill_ascii_zeroes(Bytes, Finish) when length(Bytes) < Finish ->	fill_ascii_zeroes([48|Bytes], Finish);
fill_ascii_zeroes(Bytes, _) -> Bytes.	

generate_length(RearrangedStructure, Val)->
	case dict:find(0, RearrangedStructure) of
		{ok,[{{_Field,_Length,0},{_Numeric,Coder}}]} -> 
			case Coder of
				?CODER_ASCII->fill_ascii_zeroes(integer_to_list(Val), 4);			
				?CODER_HEX->
					String = fill_ascii_zeroes(integer_to_list(Val, 16),4),
					converters:to_hex(String);
				?CODER_BCD->
					String = fill_ascii_zeroes(integer_to_list(Val), 4),
					converters:to_bcd(String);
				?CODER_EBCDIC->
					""					
			end;
		_ -> io:format("Message length field format not specified.")
	end.

get_message_bitmap_as_array(ISOStructure, MessageValues)->
	get_message_bitmap_as_array(ISOStructure, MessageValues, [1]).
get_message_bitmap_as_array([{#field{name=Key,order_number=OrderNumber},_}|T], MessageValues, B)->
	Val =  case dict:find(Key, MessageValues) of
				{ok, _} -> 1;
				error -> 0
			end,
	Bytes = case length(B)+1 == OrderNumber of
				true -> [Val|B];
				false -> [Val|fill_zeroes(B, OrderNumber)]
			end,			
	get_message_bitmap_as_array(T, MessageValues, Bytes);
	
get_message_bitmap_as_array([], _, Bytes)->
		lists:reverse(fill_zeroes(Bytes, 129)).	

fill_zeroes(Bytes, Finish) when length(Bytes) < Finish -> fill_zeroes([0|Bytes], Finish);
fill_zeroes(Bytes, _) -> Bytes.			
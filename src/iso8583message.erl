-module(iso8583Message).
%-export([start/0,test/0, f2/1,f3/0]).
-compile(export_all).
-include("field_constants.hrl").
-record(field, {name,order_number}).

f12()->
%	String = fill_zeroes(integer_to_list(42), 4),
%	io:format("~n>>>~p",[String]),
%	to_hex(String)

	MessageStructure = [
					{#field{name=?LENGTH,order_number=0},{?NUMERIC, ?CODER_BCD}}
					],
	RearrangedStructure = rearrange_for_parse(MessageStructure),
	Bytes = generate_length(RearrangedStructure, 42),
	file:write_file("out.bin", Bytes).
	
	%to_bcd("0042").

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
	
rearrange_for_parse(List)->	
	rearrange_for_parse(List, dict:new()).
rearrange_for_parse([{#field{name=_,order_number=OrderNumber},_} = Head|T], Dict)->
	rearrange_for_parse(T, dict:append(OrderNumber, Head, Dict));
rearrange_for_parse([], Dict)->
	Dict.
	
generate_message(MessageStructure, MessageValues)->generate_message(MessageStructure, MessageValues, nil).
generate_message(MessageStructure, MessageValues, HeaderAsList)->
	RearrangedStructure = rearrange_for_parse(MessageStructure),
	Message = generate_internal_message(MessageStructure, MessageValues),
	MessageWithHeader = case HeaderAsList of
							nil->Message;
							_->lists:append(HeaderAsList, Message)
						end,
	MessageLength = generate_length(RearrangedStructure, length(Message)),
	list_to_binary([MessageLength|MessageWithHeader]).

generate_internal_message(MessageStructure, MessageValues)-> [4,8].

fill_zeroes(Bytes, Finish) when length(Bytes) < Finish ->	fill_zeroes([48|Bytes], Finish);
fill_zeroes(Bytes, _) -> Bytes.	

generate_length(RearrangedStructure, Val)->
	case dict:find(0,RearrangedStructure) of
		{ok,[{{_Field,_Length,0},{_Numeric,Coder}}]} -> 
			case Coder of
				?CODER_ASCII->fill_zeroes(integer_to_list(Val), 4);			
				?CODER_HEX->
					String = fill_zeroes(integer_to_list(Val, 16),4),
					converters:to_hex(String);
				?CODER_BCD->
					String = fill_zeroes(integer_to_list(Val), 4),
					converters:to_bcd(String);
				?CODER_EBCDIC->
					""					
			end;
		_ -> io:format("Message length field format not specified.")
	end.

	 
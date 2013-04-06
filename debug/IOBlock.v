`ifndef _IOBlock_
`define _IOBlock_
//`include "misc.v"

//

module io_selecter(
	SRAM,i,o);

input SRAM;
inout i;
inout o;

tri w;

parameter empty_value = 1'bz;

assign w = SRAM ? o : empty_value;
assign i = w;

endmodule

module IOBlock(
	SRAM,pad,io);


input SRAM;
inout pad;
inout io;

io_selecter to_pad(SRAM,pad,io);
io_selecter to_io(~SRAM,io,pad);


endmodule


`endif//_IOBlock_

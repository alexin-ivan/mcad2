`include "LogicElement.v"
`include "IOBlock.v"

module LogicElement(SRAM,clk,i,o);
input [0:17] SRAM;
input clk,i;
output o;

endmodule

module top(
	SRAM,clk,i,o
);

input [31:0] SRAM;
input clk;
input [0:3] i;
output o;

LogicElement le(SRAM[17:0],clk,i,o);

defparam le . le ="AFDD";

endmodule

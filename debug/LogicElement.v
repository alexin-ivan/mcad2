`ifndef _LogicElement_
`define _LogicElement_
//`include "LUT4.v"
//`include "LE_DFF.v"
//`include "misc.v"


module mux2(d0,d1,sel,q);
input d0,d1,sel;
output q;
assign q = sel ? d1 : d0;
endmodule

module LUT4(SRAM,i,o);

input  [0:15] SRAM;
input  [0:3] i;
output o;

tri [0:8] t1;
tri [0:4] t2;
tri [0:1] t3;

parameter mask = "ASDF";

assign t1[0] = i[0] ? SRAM[0] : SRAM[1];
assign t1[1] = i[0] ? SRAM[2] : SRAM[3];
assign t1[2] = i[0] ? SRAM[4] : SRAM[5];
assign t1[3] = i[0] ? SRAM[6] : SRAM[7];
assign t1[4] = i[0] ? SRAM[8] : SRAM[9];
assign t1[5] = i[0] ? SRAM[10] : SRAM[11];
assign t1[6] = i[0] ? SRAM[12] : SRAM[13];
assign t1[7] = i[0] ? SRAM[14] : SRAM[15];

assign t2[0] = i[1] ? t1[0] : t1[1];
assign t2[1] = i[1] ? t1[2] : t1[3];
assign t2[2] = i[1] ? t1[4] : t1[5];
assign t2[3] = i[1] ? t1[6] : t1[7];

assign t3[0] = i[2] ? t2[0] : t2[1];
assign t3[1] = i[2] ? t2[2] : t2[3];

assign o = i[3] ? t3[0] : t3[1];

endmodule

module LE_DFF(d,clk,q);
input d,clk;
output q;
parameter front=1'b0;

endmodule

module LogicElement(SRAM,clk,i,o);
input [0:SRAM_SIZE] SRAM;
input clk;
input [0:3] i;
output o;

wire lut_out,dff_out;
wire [0:3] lut_in;

parameter SRAM_SIZE = 17;
parameter SRAM_LUT_MASK = 32'b0[14:SRAM_SIZE];

assign lut_in[0:2] = i[0:2];
mux2 com_or_seq_selector(.d0(dff_out),.d1(i[3]),.sel(SRAM[17]),.q(lut_in[3]));
//assign lut_in[3] = SRAM[17] ? i[3] : dff_out;

LUT4 lut(.SRAM(SRAM[0:15]), .i(lut_in), .o(lut_out));
defparam lut.mask=1'b0;
LE_DFF dff(.d(lut_out), .clk(clk), .q(dff_out));
defparam dff.front=1'b1;

assign o = SRAM[16] ? dff_out : lut_out;

endmodule
`endif

//`ifndef _SwitchBox_
//`define _SwitchBox_

// switchBox connection point mux
module swbox_cp_mux(
	SRAM, inputs, o
);

input [0:7] SRAM;
input [0:7] inputs;
output o;

tri q;
wire [0:7] i;

parameter empty_value = 1'bz;
parameter commutator  = "True";
assign i = inputs;

assign q = SRAM[0] ? i[0] : empty_value;
assign q = SRAM[1] ? i[1] : empty_value;
assign q = SRAM[2] ? i[2] : empty_value;
assign q = SRAM[3] ? i[3] : empty_value;
assign q = SRAM[4] ? i[4] : empty_value;
assign q = SRAM[5] ? i[5] : empty_value;
assign q = SRAM[6] ? i[6] : empty_value;
assign q = SRAM[7] ? i[7] : empty_value;

assign o = q;

endmodule

module SwitchBox(
	SRAM,
	li,lo, // left input, left output
	ri,ro, // right
	ti,to, // top
	bi,bo  // bottom	
);

input [0:3] li,ri,ti,bi;
output  [0:3] lo,ro,bo,to;
input [0:63] SRAM;

wire [0:7] h_inputs;
wire [0:7] v_inputs;

assign h_inputs = {li,ri};
assign v_inputs = {ti,bi};

// long lines
assign ro[0:1] = li[0:1];
assign lo[0:1] = ri[0:1];
assign to[0:1] = bi[0:1];
assign bo[0:1] = ti[0:1];

// top output
swbox_cp_mux to2(.SRAM(SRAM[0:7]), .inputs(h_inputs),.o(to[2]));
swbox_cp_mux to3(.SRAM(SRAM[8:15]),.inputs(h_inputs),.o(to[3]));

// bottom output
swbox_cp_mux bo2(.SRAM(SRAM[16:23]),.inputs(h_inputs),.o(bo[2]));
swbox_cp_mux bo3(.SRAM(SRAM[24:31]),.inputs(h_inputs),.o(bo[3]));

// left output
swbox_cp_mux lo2(.SRAM(SRAM[32:39]),.inputs(v_inputs),.o(lo[2]));
swbox_cp_mux lo3(.SRAM(SRAM[40:47]),.inputs(v_inputs),.o(lo[3]));

// right output
swbox_cp_mux ro2(.SRAM(SRAM[48:55]),.inputs(v_inputs),.o(ro[2]));
swbox_cp_mux ro3(.SRAM(SRAM[56:63]),.inputs(v_inputs),.o(ro[3]));


endmodule
//`endif

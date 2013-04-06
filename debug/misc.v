`ifndef _misc_
`define _misc_

module mux2(d0,d1,sel,q);
input d0,d1,sel;
output q;
assign q = sel ? d1 : d0;
endmodule

`endif//_misc_

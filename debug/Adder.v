

module and2(a,b,q);
input a,b;
output q;
endmodule


module Adder(a,b,c,s);
input a,b;
output c,s;

and2 a1 (.a(a),.b(b),.q(c));
and2 a2 (.a(a),.b(b),.q(s));

endmodule

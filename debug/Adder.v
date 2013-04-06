

module and2(\a[0],b,q);
input \a[0],b;
output q;
endmodule


module Adder(a,b,c,s);
input a,b;
output c,s;

and2 a1 (.\a[0](a),.b(b),.q(c));
and2 a2 (.\a[0](a),.b(b),.q(s));

endmodule

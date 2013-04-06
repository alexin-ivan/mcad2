#!/usr/bin/perl -w

#my $num_args=$#ARGV + 1;
my $command=$ARGV[0];
#$command="" if ($num_args == 0);


#my $exe=TestSyntax;

sub do_test {
	my $exe=shift;
	my $fname = shift;
	`./$exe $fname`;
}

sub do_clean {
	@exts= (
		"*.output",
		"*.ast",
		"*.ast.v",
		"*.lex"
	);
	for (@exts) {
		print "Remove $_\n";
		`rm -f $_`;
	}
}

sub do_all {
	my $exe=shift;
	map  {do_test($exe,$_) } `ls *.v`;
}

#do_all($exe) if ( $command eq "" );

if ( $command eq "clean") {
	do_clean();
} else {
	do_all($command);
}


#!/opt/star/bin/perl

sub getQuery {
    if ($qstring = $ENV{'QUERY_STRING'}) {
        foreach (split(/&/, $qstring)) {
            s/%(..)/sprintf("%c", hex($1))/ge;	# unquote %-quoted
            if (/(\S+)=(.*)/) {
                $input{$1} = $2 if $2 ne "";
            } else {
                $input{$_}++;
            }
        }
    }
}

1;

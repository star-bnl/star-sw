#! /usr/bin/env tcsh
foreach d (`ls -1d log_*`)
    set l = ${d}Z;
    if (! -d $l) mkdir $l;
    cd $l;
    set f = `echo $d | sed -e 's/log_//'`;
    foreach k (`ls -1d ../${f}/*.event.root`)
	set b = `basename $k .event.root`;
	cp ../${d}/*/${b}.log.gz .; gunzip ${b}.log.gz;
    end
    cd -;
end
# E o D

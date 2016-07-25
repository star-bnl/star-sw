#! /bin/tcsh -f
set source_dir = $1
set dist_dir = $2
foreach f (`ls $source_dir/*.*`) 
    set b = `basename $f`;
    echo "==============="
    echo "find $b"
    find $dist_dir -name $b
#    set n = `wc -l $l`
#    echo "$b => $l"
#    echo "n = $n"
#    if ("$n" != "1") echo $l
end
# e o d

#! /usr/bin/env tcsh
foreach d (`ls -1d y*/reco*`)
    set b = `basename $d`;
    set c = `echo $b | sed -e 's/reco_//'`;
    mkdir $c; cd $c; ln -s ../$d MuDst; cd -;
end
# E o D

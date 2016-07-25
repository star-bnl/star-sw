foreach d (`ls  -d /star/rcf/test/dev/daq_sl302.ittf/Sat/year_200*/* `)
    set b = `basename $d`
    set dir = `dirname $d`
    set u = `basename $dir`
    mkdir -p $u/$b; cd $u/$b; ln -s $d .; cd -;
end
# EOD

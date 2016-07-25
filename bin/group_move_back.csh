#! /usr/local/bin/tcsh -f
set list = `ls /afs/rhic/.asis/*/usr.local/lib/hepix/shells/site/*sh.old`
foreach f ( $list )
    set s = $f:r
    echo $f "===>" $s
    mv $f $s
end
# e o d

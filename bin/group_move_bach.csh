#! /usr/local/bin/tcsh -f
set list = `ls /afs/rhic/.asis/*/usr.local/lib/hepix/shells/site/*sh.old`
foreach f ( $list )
    echo $f
end
# e o d

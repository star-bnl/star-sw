#!/bin/tcsh

onintr exit

csh -x  $1 &

while (`ps --no-headers -p $!` != "")
        sleep 1
end

exit:
kill -TERM -$!
wait

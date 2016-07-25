#! /usr/local/bin/tcsh -f
set file = ""
set dir  = "/home/starsink/raw"
while ( $#argv > 0 )
        switch ($1)
        case -f:        # set file
        shift; if ( $#argv > 0 ) set file = $1
                breaksw
        case -d:        # set dir
        shift; if ( $#argv > 0 ) set dir = $1
                breaksw
        endsw
        shift
end
if ($file != "") then
ftp -i -v rmds01 2121 <<EOF
cd $dir
dir $file tmp
EOF
more tmp
endif

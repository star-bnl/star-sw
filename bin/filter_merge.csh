#! /usr/local/bin/tcsh -f
#
# Master chain operation for filtering and merging files
#
# H.L. 22/8/98; pmj 9/4/98
#
#====================================================

# This WORKING_DIR variable needs to be changed to appropriate one.

#set WORKING_DIR = ~$USER/production/distribution
set WORKING_DIR=~jacobs/production/hpss_archiving/filter_merge
#set   WORKING_DIR=`pwd`
cd $WORKING_DIR

#----------------------------------------------------------------------

unset top_dir_defined
unset input_list_defined
set ret_status = 0

#----------------------------------------------------------------------

# parse arguments

@ arg_count = 0

if ($#argv == 0) goto LIST

while ($arg_count <= $#argv)
 
   switch ($argv[$arg_count])
 
        case -h:
            goto LIST

        case -d:
            
            @ arg_count++
            set TOP_DIR = $argv[$arg_count]  

	    if ( ! -d $TOP_DIR ) then
               echo "$TOP_DIR is not a directory"
               goto ERROR
            endif

	    echo "Top directory set to $TOP_DIR"
	    set top_dir_defined
	    breaksw

        case -l:

            @ arg_count++
            set INPUT_LIST = $argv[$arg_count]  
            if (! -f $INPUT_LIST ) then
               echo "$INPUT_LIST is not a file"
               goto ERROR
            endif            

            echo "Input list is $INPUT_LIST"
            set input_list_defined
            breaksw

    endsw

    @ arg_count++
 
end

if ( ! $?top_dir_defined && ! $?input_list_defined) then
   echo "Need either -l or -d argument"
   goto LIST
endif

if ($?top_dir_defined && $?input_list_defined) then
   echo "top directory and input list cannot be defined simultaneously"
   goto ERROR
endif
#----------------------------------------------------------------------

echo "Forcing starnew"
#source ${GROUP_DIR}/.starnew

# added by pmj

\rm makefile geant3.def

ln -s $STAR_BIN/makefile makefile
ln -s $STAR/bin/geant3.def geant3.def

#----------------------------------------------------------------------

# scratch directory
set output_dir = /disk1/star/sim/scratch

#=====================================================
# generate list of files on disk

if ( ! $?input_list_defined) then
   set INPUT_LIST = temp_input.list_$$
   $WORKING_DIR/list_files.csh -d $TOP_DIR -o $INPUT_LIST
endif
   
#=====================================================
# check the quality of each event

set olddir = 0
set n_events_max = 30

foreach file (`cat $INPUT_LIST`)

    echo "***************************************************"
    echo "$file is being checked now"

# get absolute path name
# but must strip off ".fz" in name passed t0 kumac
# (filename is used to construct .hb filename also)

    set now=`pwd`
    set dirname = `dirname $file`
    cd  $dirname
    set infilename=`pwd`/`basename $file .fz`
    cd  $now

# creat the catenated file name

    set basedir = `basename $dirname`
    if ($olddir != $basedir) then
       set olddir = $basedir
       set catfile =  $dirname/$basedir"_01".fz
       set catlog =  $dirname/$basedir"_01".log
       touch $catfile; touch $catlog
       @ counter = 1
    endif

# estimate the size of the catfile which should be <= 1 GB

    set temp_filesize = `\ls -l $catfile |  awk '{print $5}'`
    set filesize = `expr $temp_filesize / 1000000`
    if ($filesize > 1000) then
       @ counter++
       if ($counter < 10) then
          set catfile = $dirname/$basedir"_0"$counter.fz
          set catlog =  $dirname/$basedir"_0"$counter.log
       else
          set catfile = $dirname/$basedir"_"$counter.fz
          set catlog =  $dirname/$basedir"_"$counter.log
       endif
       touch $catfile; touch $catlog
    endif

# actually filtering the event

    set outfilename=$output_dir/`basename $file .fz`

# need to read in auxiliary geometry file?

    $WORKING_DIR/query_geometry_status.csh ${infilename}.fz
 
    if ( $status == 1 ) then
        set geofilename = geometry
        echo Geometry file can not be found. Quit !
        goto END
    else
        set geofilename = ""
    endif

    set tempout = temp_out.hb_$$
staf -G 20 -w 0<<EOF
   exec one_file $infilename $outfilename $tempout $geofilename
EOF
   \mv -f $tempout $infilename.hb

# calculate number of events

    cat $outfilename.fz_filt >> $catfile
    \rm $outfilename.fz_filt $file

    echo `basename $file` >> $catlog

end # end of file loop

# check the concatenated file after the file loop

echo "Now concatenating .hb files..."

cd $dirname
@ counter = 0
foreach hbfile (`ls -1 *.log`)
    set hblist = `sed "s/.fz/.hb/" $hbfile`
    @ counter++
    if ($counter < 10) then
       set cathb = $dirname/$basedir"_0"$counter
    else
       set cathb = $dirname/$basedir"_"$counter
    endif

/afs/rhic/asis/sun4x_55/cern/98/bin/pawX11 -w 0<<EOF
   hmerge temp.hb $hblist
EOF
/afs/rhic/asis/sun4x_55/cern/98/bin/pawX11 -w 0<<EOF
   hi/fil 30 temp.hb 0
   hrin 0
   macro/glob/creat nevent \$HINFO(1,'ENTRIES')
   shell \mv -f temp.hb $cathb"_"[nevent]evts.hb
   shell \mv -f $cathb.fz $cathb"_"[nevent]evts.fz
   shell \mv -f $cathb.log $cathb"_"[nevent]evts.log
EOF

\rm -f $hblist

end

\rm -f last.kumac* paw.metafile

#---------------------------------------------------------------------
# move .hb and .log files to AFS area

if ( $?AFS_DIR ) then

   echo "moving .hb and .fz files to $AFS_DIR"
   mv $dirname/*.hb $AFS_DIR
   mv $dirname/*.log $AFS_DIR

endif

#----------------------------------------------------------------------

goto END

ERROR:
set ret_status = 1

LIST:
echo "  Command line arguments are:"
echo "     -h: print this list"
echo "     -d dir: dir is top directory for search"
echo "     -l filename: filename is text file containing list of input files"

END:

if ( -f $INPUT_LIST ) then
   cd $WORKING_DIR
#   \rm -f $INPUT_LIST
endif

exit $ret_status

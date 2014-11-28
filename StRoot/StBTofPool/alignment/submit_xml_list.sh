#!/bin/csh
# this script generates several .xml files out of a input list and submits them to the batch farm
# e.g.: . ./submit_xml_list.sh Full_Split_Lists.txt
# starver DEV   <- was used before
#if ($#argv == 1) 
#then
        set workdir=(/star/u/huck/TofAlignment19GeV/)
        cd $workdir
        starver dev
        set outdir=($workdir"rootout/")
        set lisin=($workdir"lis/")
        set lisend=(.lis)
        set rootend=(.root)
        set filename=(jobTA_)
        set fileend=(.xml)
        set scheddir=($workdir)
        echo $workdir $outdir $lisin $scheddir
        rm *.list
	rm *.csh
        rm *.condor
        rm *.report
        rm *.session.xml
        rm *.log
        rm *.out
        rm *.err
        foreach item (`cat $1`)
          echo FILE name = $item
          set lisfull=$lisin$item$lisend
          set outdirfull=$outdir$item$rootend
          set fullfilename=$scheddir$filename$item$fileend
          set rootarg=($workdir'doMuDstEvents.C\(\"'$lisfull'\",\"'$outdirfull'\"\)')
          echo rootarg = $rootarg
          rm $fullfilename
          echo xmlfile = $fullfilename
          echo '<?xml version="1.0" encoding="utf-8" ?>' > ${fullfilename}
          echo '<job minFilesPerProcess="1" maxFilesPerProcess="1" filesPerHour="9.9">' >> ${fullfilename}
          echo ' <command> ' >> ${fullfilename}
          echo ' cd '$workdir >> ${fullfilename}
          echo ' starver dev ' >> ${fullfilename}
          echo ' root4star -q -b '$rootarg >> ${fullfilename}
          echo ' </command> ' >> ${fullfilename}
          echo ' <stdout URL="file:'$outdir'$JOBID.out" />' >> ${fullfilename}
          echo ' <stderr URL="file:'$outdir'$JOBID.err" />' >> ${fullfilename}
          echo '  <SandBox>' >> ${fullfilename}
          echo '   <Package>' >> ${fullfilename}
          echo '    <File>file:'$workdir'doMuDstEvents.C</File>' >> ${fullfilename}
          echo '   </Package>' >> ${fullfilename}
          echo ' </SandBox>' >> ${fullfilename}
          echo '</job>' >> ${fullfilename}
          star-submit $fullfilename
        end
#endif

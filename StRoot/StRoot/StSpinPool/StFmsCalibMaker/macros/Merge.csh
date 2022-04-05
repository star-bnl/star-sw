#!/bin/csh -f

source /star/u/ckimstar/.login

set Counts = 500
set Source = /star/u/ckimstar/work/tools/merge_hadd.C

set Target = $1
set nFiles = $2 #Merge only certain # of files among the entire set

if ($nFiles == "") then
	set nFiles = -1
endif

#Open the list. Make the one if not exist
#----------------------------------------
if (-f $Target.txt) then
	set nItems = `wc -l ./$Target.txt | awk '{print $1}'`
else
	ls -v ${Target}*.root > $Target.txt
	set nItems = `wc -l ./$Target.txt | awk '{print $1}'`
endif

#Merge
#--------------------------
if ($nItems < $Counts) then

	#Merge all items (# of items < Counts)
	#-------------------------------------
	if ($nFiles == -1) then
		echo Merging $nItems files...
		root4star -l -b -q ''$Source'("'$Target'")'
	else
		echo Merging $nFiles of $nItems files...
		tail -n +1 $Target.txt | head -n $nFiles > ${Target}_${nFiles}Files.txt
		root4star -l -b -q ''$Source'("'${Target}_${nFiles}Files'")'
		rm ${Target}_${nFiles}Files.txt
	endif
	rm $Target.txt

else

	#Make sublists, then merge each segments
	#---------------------------------------
	echo Merging $nItems files...
	@ nSub = ($nItems / $Counts)
	set Loop = 0

	while ($Loop <= $nSub)
		@ LineS = ($Counts * $Loop + 1)
		if ($Loop != $nSub) then
			tail -n +$LineS $Target.txt | head -n $Counts > ${Target}_Sub${Loop}.txt
		else
			@ LineR = $nItems - ($LineS - 1)
			tail -n +$LineS $Target.txt | head -n $LineR > ${Target}_Sub${Loop}.txt
		endif
		root4star -l -b -q ''$Source'("'${Target}_Sub{$Loop}'")'
		@ Loop++
	end

	ls merged_${Target}_Sub* > ${Target}_SubAll.txt
	root4star -l -b -q ''$Source'("'${Target}_SubAll'")'
	mv merged_${Target}_SubAll.root merged_${Target}.root

	rm $Target.txt
	rm ${Target}_Sub*.txt
	rm ${Target}_SubAll.txt
	rm merged_${Target}_Sub*.root

endif

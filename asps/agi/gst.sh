#!/bin/csh 
#set list = "car" #  
set list = "comis deccc dzdoc geant hadr"
foreach dir ( $list )
        switch ($dir)
        case car:        # 
# 	        shift; 
		rm -r gst; /cern/pro/bin/cmz -l install.kumac;
		car2cvs -v -p CERNLIB_ -kstar_known.inc  gst.car
		rm -r  gst/TITLE.gst gst/Makefile* gst/*/Make* gst/boot 
		rm -r gst/*/README gst/INSTALL gst/_kumacs 
		rm gst/*.in
                breaksw
        case deccc:
		mv gst/deccc/dumsgi6.F gst/deccc/dumsgi6.c
        case comis:
		cd gst/$dir; #shift; 
		set LIBLIST = "`ls *.F *.c`"
		foreach file ($LIBLIST)
			echo $file
			sed -e '/\#include \"sys\/CERNLIB_machine\.h\"/d' \
			-e '/* ===============================================/d' \
	 		-e 's/\#include \"pilot\.h\"/\#include \"comis\/pilot\.h\"/g' \
                        $file > tmp.$file
			diff $file tmp.$file
			mv tmp.$file $file
		end
		cd ../../
                breaksw
        case dzdoc:
		cd gst/$dir; #shift; 
		set LIBLIST = "`ls *.F *.c`"
		foreach file ($LIBLIST)
			echo $file
			sed -e '/\#include \"sys\/CERNLIB_machine\.h\"/d' \
	 		-e 's/\#include \"pilot\.h\"/\#include \"dzdoc\/pilot\.h\"/g' \
                        $file > tmp.$file
			diff $file tmp.$file
			mv tmp.$file $file
# #include "packlib/zebra/zebra/zvfaut.inc" in ZFATAL
		end
		cd ../../
                breaksw
        case geant:
        case hadr:
		cd gst/$dir; #shift; 
		set LIBLIST = "`ls *.F *.c`"
		foreach file ($LIBLIST)
			echo $file
			sed -e '/\#include \"sys\/CERNLIB_machine\.h\"/d' \
	 		-e 's/\#include \"pilot\.h\"/\#include \"geant321\/pilot\.h\"/g' \
                        $file > tmp.$file
			diff $file tmp.$file
			mv tmp.$file $file
		end
		cd ../../
                breaksw
	default
		breaksw
	endsw
end




#! /bin/tcsh -f
foreach f (`ls *.*`)
    rm ${f}; cvs update -p -r v08Mar06 ${f} > ${f}
end
# e o d 

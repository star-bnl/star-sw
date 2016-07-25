# !/bin/tcsh -f
setenv STARFPE NO
foreach d (`ls -1d i*`)
  cd $d;
  ls -1 "*F.root"
  if ( $? )   root.exe -q -b TpcT.C+
  ls -1  "*Fit.root"
  if ( $? ) root.exe -q  *F.root FitTpcT.C
  cd ..
end
# EOD

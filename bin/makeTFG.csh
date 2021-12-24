foreach f (`ls -1d *.*`)
  diff -D__TFG__VERSION__ $STAR_PATH/adev/StRoot/StMuDSTMaker/COMMON/${f} ${f} > ${f}.new.C
end

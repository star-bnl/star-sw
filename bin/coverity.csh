setenv COVPATH /afs/rhic.bnl.gov/x8664_sl6/app/coverity-7.6.0
setenv PATH ${COVPATH}/bin:${PATH}
cov-build --dir ./ cons -k
# cov-analyze --dir ./  --all -j auto
# cov-format-errors --dir ./  --filesort -x  --title "Bla" --html-output $MYDIR/html

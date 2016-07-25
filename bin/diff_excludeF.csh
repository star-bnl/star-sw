#! /usr/local/bin/tcsh -f
#diff -ruwbB --exclude=CVS --exclude=.svn --exclude=doc --exclude=.consign --exclude=".\#*" -I '^//' -I '^/\*' -I 'rdm' -I 'perev' -I 'fisyak' -I '*** empty log message' $1 $2
#diff -ruwbB --exclude=CVS --exclude=.svn --exclude=doc --exclude=.consign --exclude=".\#*" \
# -I '^//.*' -I '^/\*.*' -I '^\*.* rdm .*' -I '^\*.* perev .*' -I '^\*.* fisyak .*' -I '^\*.* *** empty log message .*' $1 $2
diff -ruwbB --exclude=CVS --exclude=.svn --exclude=doc --exclude=.consign --exclude=".\#*" \
--ignore-matching-lines='^\*' \
--ignore-matching-lines='^c ' \
--ignore-matching-lines='^C ' \
--ignore-matching-lines='^\#include' \
$1 $2

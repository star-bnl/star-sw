#! /bin/tcsh -f
#diff -ruwbB --exclude=CVS --exclude=.svn --exclude=doc --exclude=.consign --exclude=".\#*" -I '^//' -I '^/\*' -I 'rdm' -I 'perev' -I 'fisyak' -I '*** empty log message' $1 $2
#diff -ruwbB --exclude=CVS --exclude=.svn --exclude=doc --exclude=.consign --exclude=".\#*" \
# -I '^//.*' -I '^/\*.*' -I '^\*.* rdm .*' -I '^\*.* perev .*' -I '^\*.* fisyak .*' -I '^\*.* *** empty log message .*' $1 $2
diff -ruwbB --exclude=CVS --exclude=.svn --exclude=doc --exclude=.consign --ignore-matching-lines='^*' \
--ignore-matching-lines='\$Id:' --ignore-matching-lines='^ * Revision' \
#--ignore-matching-lines='^ *$' \
$1 $2
#--ignore-matching-lines='^\* \$Id\:' \
#--ignore-matching-lines='^\* Revision' \
#--ignore-matching-lines='^\* *** empty log message *' \

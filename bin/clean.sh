#!/bin/sh -f

ignores='/CVS/'

for f in $* ; do

  # 
  # Ensure that we only consider text files
  #

  filetype=`file ${f} | grep 'text'`
  if test $? = 0; then
    t=`echo ${f} | egrep -v -e ${ignores}`

    #
    # Ignore some file patterns (here CVS files)
    #

    if test $? = 0; then
      t=`od -c ${f} | grep '\\\\n'`

      #
      # Handle various formatting styles
      #

      if test $? = 0; then

        # There are \n separators and there MAY be \r in addition

        echo " Convert ${f}" 1>&2

        sed -e 's#[ ]$#{trailingspace}'${id}'#' ${f} | \
         tr '\r' ' ' | \
         sed -e 's#[ ]$##' -e 's#{trailingspace}'${id}'# #' >t$$; mv t$$ $f
      else

        # There are only \r separators

        echo " Convert ${f}" 1>&2

        cat ${f} | \
         tr '\r' '\n' >t$$; mv t$$ $f
      fi

    fi
  fi

done


	SUBROUTINE STRCONCAT(ARG,JARG,NARGS,COM)

	IMPLICIT NONE

*  Inputs:
	CHARACTER*(*) ARG(*) !Array of parsed-arguments.
	INTEGER JARG         !Element of ARG with which to start.
	INTEGER NARGS        !Number of arguments in ARG.
*  Output:
	CHARACTER*(*) COM    !Re-combined (concatenated) string of ARGs.

*  Functional description:
*	Recombine the arguments, previously parsed from a single line into
*	ARG, as with STRPARSE, into a single line.  General, this is used
*	to recombine everything but the first argument, so that an otherwise
*	intact command string can be passed "down the line", stripped of its
*	"routing" command.  Arguments are concatenated with intervening spaces.
*	The intervening spaces could be tabs, but not if ARG and NARGS come
*	from STRPARSE.

	INTEGER LCOM,KARG,SPT

	LCOM=LEN(COM) !Get length of string COM.

	COM=ARG(JARG) !First one passed goes right in.

*	Do all the remaining arguments:
	DO KARG=JARG+1,NARGS
	  CALL STREND(COM,SPT) !Point to last non-blank.
	  SPT=SPT+2 !Point to first character after blank.
	  IF (SPT.LE.2) THEN !Nothing -- all blanks.
*	    This shouldn't happen if ARG and NARGS came from STRPARSE:
	    COM=ARG(KARG) !This one goes right in.
	  ELSE IF (SPT.LT.LCOM) THEN !Put next one in right after one blank:
	    COM(SPT:)=ARG(KARG)
	  END IF
	END DO

	RETURN
	END

	INTEGER FUNCTION STR_NEXTARG(ARG,NARGS,JARG)

	IMPLICIT NONE

*  Inputs:
	CHARACTER*(*) ARG(*)
	INTEGER NARGS

*  Input/output:
	INTEGER JARG

*  Returns:
*	STR_NEXTARG_SUCCESS_RC if JARG points at the next argument.
*	STR_NEXTARG_NOARGS_RC if no arguments remain;  JARG is invalid.

*  Functional description:
*	Advances JARG to point at the next argument, if any remain.

	INCLUDE 'str_ret_inc'

	JARG=JARG+1 !Point to next argument
	IF (JARG.GT.NARGS) THEN !Error -- no args left.
	  STR_NEXTARG=STR_NEXTARG_NOARGS_RC
	ELSE !Success -- JARG points at the next argument:
	  STR_NEXTARG=STR_NEXTARG_SUCCESS_RC
	END IF

	RETURN
	END

	INTEGER FUNCTION STR_NEXTARG_ALLOWEQ(ARG,NARGS,JARG)

	IMPLICIT NONE

*  Inputs:
	CHARACTER*(*) ARG(*)
	INTEGER NARGS
*  Input/output:
	INTEGER JARG

*  Returns:
*	STR_NEXTARG_SUCCESS_RC if JARG points at the next argument, skipping
*	an equals sign "=" if encountered.
*	STR_NEXTARG_NOARGS_RC if no arguments remain;  JARG is invalid.

	INCLUDE 'str_ret_inc'

	JARG=JARG+1 !Point to next argument
	IF (JARG.GT.NARGS) THEN !Error -- no args left.
	  STR_NEXTARG_ALLOWEQ=STR_NEXTARG_NOARGS_RC
	ELSE IF (ARG(JARG).EQ.'=') THEN
*	  Skip over an equals sign:
	  JARG=JARG+1
	  IF (JARG.GT.NARGS) THEN !Error -- no args left.
	    STR_NEXTARG_ALLOWEQ=STR_NEXTARG_NOARGS_RC
	  ELSE !Success -- JARG points at the next argument:
	    STR_NEXTARG_ALLOWEQ=STR_NEXTARG_SUCCESS_RC
	  END IF
	ELSE !Success -- JARG points at the next argument:
	  STR_NEXTARG_ALLOWEQ=STR_NEXTARG_SUCCESS_RC
	END IF

	RETURN
	END

	INTEGER FUNCTION STR_NEXTARG_REQEQ(ARG,NARGS,JARG)

	IMPLICIT NONE

*  Inputs:
	CHARACTER*(*) ARG(*)
	INTEGER NARGS
*  Input/output:
	INTEGER JARG

*  Returns:
*	STR_NEXTARG_SUCCESS_RC if JARG points at the next argument, skipping
*	a required equals sign "=".
*	STR_NEXTARG_NOARGS_RC if no arguments remain;  JARG is invalid.
*	STR_NEXTARG_NOEQ_RC if no equals sign was found.  JARG points at
*	where the equals sign was expected.

	INCLUDE 'str_ret_inc'

	JARG=JARG+1 !Point to next argument
	IF (JARG.GT.NARGS) THEN !Error -- no args left.
	  STR_NEXTARG_REQEQ=STR_NEXTARG_NOARGS_RC
	ELSE IF (ARG(JARG).EQ.'=') THEN
*	  Skip over the required equals sign:
	  JARG=JARG+1
	  IF (JARG.GT.NARGS) THEN !Error -- no args left.
	    STR_NEXTARG_REQEQ=STR_NEXTARG_NOARGS_RC
	  ELSE !Success -- JARG points at the next argument:
	    STR_NEXTARG_REQEQ=STR_NEXTARG_SUCCESS_RC
	  END IF
	ELSE !No equals sign -- error:
	  STR_NEXTARG_REQEQ=STR_NEXTARG_NOEQ_RC
	END IF

	RETURN
	END

	LOGICAL FUNCTION STR_NUMARG(ARG,RADIX)

	IMPLICIT NONE

*  Inputs:
	CHARACTER*(*) ARG
	INTEGER RADIX        !The base in which translation would
	                     !be done: 2, 8, 10, 16.

*  Returns:
*	True if ARG is a number, in base RADIX.
*	False if ARG is a not number, in base RADIX.

*  Functional description:
*	Determine whether ARG is a candidate number-value.

	INTEGER I,L,R

	CHARACTER*(*) RADIX_2_LIST
	PARAMETER (RADIX_2_LIST='01%') !Allow % as the binary radix indicator.
	CHARACTER*(*) RADIX_8_LIST
	PARAMETER (RADIX_8_LIST='01234567Oo"') !Allow Oo" for radix indicators.
	CHARACTER*(*) RADIX_10_LIST
	PARAMETER (RADIX_10_LIST='0123456789E+-$.') !Allow $ for a radix indicator.
	CHARACTER*(*) RADIX_16_LIST
	PARAMETER (RADIX_16_LIST='0123456789AaBbCcDdEeFfHhXx') !Allow HhXx for radix indicators.


	STR_NUMARG=.FALSE. !Return false on first bad character.

	CALL STREND(ARG,L) !L points at last non-blank.

	R=RADIX !Make a local copy, for possible mod.

	IF (INDEX('HhXx',ARG(:1)).GT.0) THEN !Forced radix 16:
	  R=16
	ELSE IF (INDEX('$',ARG(:1)).GT.0) THEN !Forced radix 10:
	  R=10
	ELSE IF (INDEX('Oo"',ARG(:1)).GT.0) THEN !Forced radix 8:
	  R=8
	ELSE IF (INDEX('%',ARG(:1)).GT.0) THEN !Forced radix 2:
	  R=2
	END IF


	IF (R.EQ.10) THEN
	  DO I=1,L
	    IF (INDEX(RADIX_10_LIST,ARG(I:I)).LE.0) RETURN
	  END DO
	ELSE IF (R.EQ.16) THEN
	  DO I=1,L
	    IF (INDEX(RADIX_16_LIST,ARG(I:I)).LE.0) RETURN
	  END DO
	ELSE IF (R.EQ.8) THEN
	  DO I=1,L
	    IF (INDEX(RADIX_8_LIST,ARG(I:I)).LE.0) RETURN
	  END DO
	ELSE IF (R.EQ.2) THEN
	  DO I=1,L
	    IF (INDEX(RADIX_2_LIST,ARG(I:I)).LE.0) RETURN
	  END DO
	ELSE !Unknown radix.
	  RETURN
	END IF

	STR_NUMARG=.TRUE.
	RETURN
	END


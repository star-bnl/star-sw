*
        SUBROUTINE STRCONCAT(ARG,JARG,NARGS,COM)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) ARG(*) !Array of parsed-arguments.
        INTEGER JARG         !Element of ARG with which to start.
        INTEGER NARGS        !Number of arguments in ARG.
*  Output:
        CHARACTER*(*) COM    !Re-combined (concatenated) string of ARGs.

*  Functional description:
*       Recombine the arguments, previously parsed from a single line into
*       ARG, as with STRPARSE, into a single line.  General, this is used
*       to recombine everything but the first argument, so that an otherwise
*       intact command string can be passed "down the line", stripped of its
*       "routing" command.  Arguments are concatenated with intervening spaces.
*       The intervening spaces could be tabs, but not if ARG and NARGS come
*       from STRPARSE.

        INTEGER LCOM,KARG,SPT

        LCOM=LEN(COM) !Get length of string COM.

        COM=ARG(JARG) !First one passed goes right in.

*       Do all the remaining arguments:
        DO KARG=JARG+1,NARGS
          CALL STREND(COM,SPT) !Point to last non-blank.
          SPT=SPT+2 !Point to first character after blank.
          IF (SPT.LE.2) THEN !Nothing -- all blanks.
*           This shouldn't happen if ARG and NARGS came from STRPARSE:
            COM=ARG(KARG) !This one goes right in.
          ELSE IF (SPT.LT.LCOM) THEN !Put next one in right after one blank:
            COM(SPT:)=ARG(KARG)
          END IF
        END DO

        RETURN
        END
*
        LOGICAL FUNCTION STRDBL(S,RADIX,D_MIN,D_MAX,D,ERRMSG)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) S !String containing a numerical argument.
        INTEGER RADIX   !RADIX, base 2,8,10 or 16, for the conversion.
        DOUBLE PRECISION D_MIN,D_MAX !Min. and Max. for acceptable result.

*  Input/Output:
        DOUBLE PRECISION D    !Decoded floating value, from char. rep.
*                              May be specified at call time to be a default
*                              value, used in case of error.

*  Output:
        CHARACTER*(*) ERRMSG  !Possible error message, if fail.

*  Return values:
*       .TRUE. if a successful conversion occurred,
*       .FALSE. if not.

*  Description:
*       "Read" a double precision number from the character string S.
*       Convert according to the radix indicated in S by
*       one of the (prefix) radix indicators:
*       " or O ==> octal    X or H ==> hex    % ==> binary   $ ==> decimal.
*       If no prefix is used, the default radix is used, specified by
*       RADIX as 2, 8 or 16 (anything else is taken as 10).
*       The result, if successful, is in D.  D_MIN and D_MAX specify the
*       acceptable range of D.
*       If D_MAX .le. D_MIN, the limits are ignored.

        DOUBLE PRECISION UNREAL
        INTEGER UNINT
        INTEGER IERR
        LOGICAL BAD_VAL

        INTEGER STRUNCODE

500     FORMAT('STRDBL-E0 Illegal representation of a number.')
501     FORMAT('STRDBL-E1 'E15.7' outside range ('E15.7','E15.7')')
        STRDBL=.FALSE. !Assume no good until success.
        BAD_VAL=.FALSE. !Assume no "bad value" until "bad".
        IERR=STRUNCODE(S,RADIX,UNINT,UNREAL)
        IF (IERR.LT.0) THEN !Illegal representation.
          WRITE(ERRMSG,500)
          UNREAL=D !Use default.
        ELSE IF (D_MAX.LE.D_MIN) THEN !No limits -- success:
          STRDBL=.TRUE.
        ELSE IF (UNREAL.GT.D_MAX) THEN !Illegal value.
          BAD_VAL=.TRUE.
        ELSE IF (UNREAL.LT.D_MIN) THEN !Illegal value.
          BAD_VAL=.TRUE.
        ELSE !Success:
          STRDBL=.TRUE.
        END IF
        IF (BAD_VAL) THEN !Bad value.
          WRITE(ERRMSG,501) UNREAL,D_MIN,D_MAX
          UNREAL=D !Retain "default".
        END IF
        D=UNREAL
        RETURN
        END
*
        LOGICAL FUNCTION STRINT(S,RADIX,I_MIN,I_MAX,I,ERRMSG)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) S !String containing a numerical argument.
        INTEGER RADIX   !RADIX, base 2,8,10 or 16, for the conversion.
        INTEGER I_MIN,I_MAX  !Min. and Max. for acceptable result.

*  Input/Output:
        INTEGER I       !Decoded integer value, from char. representation.
*                        May be specified at call time to be a default
*                        value, used in case of error.

*  Output:
        CHARACTER*(*) ERRMSG  !Possible error message, if fail.

*  Return values:
*       .TRUE. if a successful conversion occurred,
*       .FALSE. if not.

*  Description:
*       "Read" an integer number from the character string S.
*       Convert according to the radix indicated in S by
*       one of the (prefix) radix indicators:
*       " or O ==> octal    X or H ==> hex    % ==> binary   $ ==> decimal.
*       If no prefix is used, the default radix is used, specified by
*       RADIX as 2, 8 or 16 (anything else is taken as 10).
*       The result, if successful, is in I.  I_MIN and I_MAX specify the
*       acceptable range of I.
*       If I_MAX .le. I_MIN, the limits are ignored.

        DOUBLE PRECISION UNREAL
        INTEGER UNINT
        INTEGER IERR
        LOGICAL BAD_VAL

        INTEGER STRUNCODE

500     FORMAT(' STRINT-E0-Illegal representation of a number.')
501     FORMAT(' STRINT-E1-Number:'I11' outside valid range.')

        STRINT=.FALSE. !Assume no good until success.
        BAD_VAL=.FALSE. !Assume no "bad value" until "bad".
        IERR=STRUNCODE(S,RADIX,UNINT,UNREAL)
        IF (IERR.LE.0) THEN !Illegal representation.
          WRITE(ERRMSG,500)
          UNINT=I !Use default.
        ELSE IF (I_MAX.LE.I_MIN) THEN !No limits -- success:
          STRINT=.TRUE.
        ELSE IF (UNINT.GT.I_MAX) THEN !Illegal value.
          BAD_VAL=.TRUE.
        ELSE IF (UNINT.LT.I_MIN) THEN !Illegal value.
          BAD_VAL=.TRUE.
        ELSE !Success:
          STRINT=.TRUE.
        END IF
        IF (BAD_VAL) THEN !Bad value.
          WRITE(ERRMSG,501) UNINT
          UNINT=I !Retain "default".
        END IF
        I=UNINT
        RETURN
        END
*
        SUBROUTINE STRPARSE(S,NARGS_max,NARGS,ARG,DARG,IARG,VDARG,VIARG)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) S !Character string containing one or more "arguments"
*                        separated from one another by "separators".
        INTEGER NARGS_max !Maximum number of arguments available to be filled.

*  Outputs:
        INTEGER NARGS !Number of arguments filled;  .le. NARGS_max.
        CHARACTER*(*) ARG(*) !Filled with NARGS char. arguments from S.
        DOUBLE PRECISION DARG(*) !ARGs translated as D.P. numbers, if possible.
        INTEGER IARG(*) !ARGs translated as integer numbers, if possible.
        LOGICAL VDARG(*) !Set true for each ARG with valid D.P. number.
        LOGICAL VIARG(*) !Set true for each ARG with valid integer number.

*  Description:
*       Parse out up to NARGS_max argument from S and into ARG, DARG and IARG.
*       Argument separators are:
*       space, tab, comma, and equal-sign.  The equal-sign is a special
*       separator in that it is also returned as an argument.  This permits
*       checking externally to STRPARSE that equal signs occur where they
*       are supposed to.  To keep spaces or "=" in a single argument, embed
*       the whole argument in double quotes "".

*       Define these values & arrays in the calling routine:
*       INTEGER NARGS_max
*       INTEGER ARG_length
*       PARAMETER (NARGS_max=40) !40 is an example.
*       PARAMETER (ARG_length=80) !80 is an example.
*       INTEGER IARG(NARGS_max)
*       DOUBLE PRECISION DARG(NARGS_max) !This MUST be D.P.
*       CHARACTER*(ARG_length) ARG(NARGS_max)
*       LOGICAL VDARG(NARGS_max),VIARG(NARGS_max)


*       Call STRPARSE_RADIX with a default radix of 10 (decimal):

        CALL STRPARSE_RADIX
     1            (S,NARGS_max,10,NARGS,ARG,DARG,IARG,VDARG,VIARG)

        RETURN
        END
*
        SUBROUTINE STRPARSE_RADIX
     1            (S,NARGS_max,RADIX,NARGS,ARG,DARG,IARG,VDARG,VIARG)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) S !Character string containing one or more "arguments"
                        !separated from one another by "separators".
        INTEGER NARGS_max !Maximum number of arguments available to be filled.
        INTEGER RADIX   !Default radix of integers appearing in arguments.

*  Outputs:
        INTEGER NARGS !Number of arguments filled;  .le. NARGS_max.
        CHARACTER*(*) ARG(*) !Filled with NARGS char. arguments from S.
        DOUBLE PRECISION DARG(*) !ARGs translated as D.P. numbers, if possible.
        INTEGER IARG(*) !ARGs translated as integer numbers, if possible.
        LOGICAL VDARG(*) !Set true for each ARG with valid D.P. number.
        LOGICAL VIARG(*) !Set true for each ARG with valid integer number.

*  Description:
*       Parse out up to NARGS_max argument from S and into ARG, DARG and IARG.
*       Argument separators are:
*       space, tab, comma, and equal-sign.  The equal-sign is a special
*       separator in that it is also returned as an argument.  This permits
*       checking externally to STRPARSE_RADIX that equal signs occur where they
*       are supposed to.  To keep spaces or "=" in a single argument, embed
*       the whole argument in double quotes "".

*       Define these values & arrays in the calling routine:
*       INTEGER NARGS_max
*       INTEGER ARG_length
*       PARAMETER (NARGS_max=40) !40 is an example.
*       PARAMETER (ARG_length=80) !80 is an example.
*       INTEGER IARG(NARGS_max)
*       DOUBLE PRECISION DARG(NARGS_max) !This MUST be D.P.
*       CHARACTER*(ARG_length) ARG(NARGS_max)
*       LOGICAL VDARG(NARGS_max),VIARG(NARGS_max)

        INTEGER I,J,SPT

        CHARACTER*80 M80 !Dummy message string for STRINT & STRDBL.

        LOGICAL STRPARSEN,STRINT,STRDBL,STR_NUMARG

        SPT=1
        NARGS=0
        DO I=1,Nargs_max
          J=SPT !Preserve SPT.
          ARG(I)=' [ No Argument ] '
          VIARG(I)=.FALSE.
          VDARG(I)=.FALSE.
          DARG(I)=0.D0
          IARG(I)=0
          IF (STRPARSEN(S,J,ARG(I))) THEN
            SPT=J !J becomes new SPT.
            NARGS=NARGS+1
*           Try to make an integer, base "RADIX", & D.P. real:
            IF (STR_NUMARG(ARG(NARGS),RADIX)) THEN !Possible number value:
              IF (STRINT(ARG(NARGS),RADIX,0,0,IARG(NARGS),M80)) THEN !Good:
                VIARG(NARGS)=.TRUE.
              END IF
              IF (STRDBL(ARG(NARGS),10,0.D0,0.D0,DARG(NARGS),M80)) THEN !Good:
                VDARG(NARGS)=.TRUE.
              END IF
            END IF !STR_NUMARG(ARG(NARGS),RADIX)
          END IF !STRPARSEN(S,J,ARG(I))
        END DO !I=1,Nargs_max

        RETURN
        END
*
        SUBROUTINE STRPARSEF(CIN,CDEF,COUT)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) CIN !Character string containing a file name, which
*                          may or may not include a file "type" field.
*                          A "type" field are those characters following
*                          the first period "." in a file name, not counting
*                          periods inside VMS-style directory-specifications.
        CHARACTER*(*) CDEF !Character string containing a specified default
*                           file "type" field.  The period is not included.
*  Output:
        CHARACTER*(*) COUT !If CIN contains a "type" field, then COUT is just
*                           a copy of CIN.  Otherwise, COUT is CIN, appended
*                           with a period and then with CDEF.

*  Description:
*       Parse out a file name from CIN and put it in COUT.  If no
*       file type is specified, append the default file type, CDEF.
*       If there is an error with the lengths, (ie, not enough space
*       in COUT), COUT is returned as "strparsef.err".

        CHARACTER*1 NULL

        INTEGER DPT,NPT,PPT,SPT,LIN,LOUT,LDEF,RBPT,START
        DATA NULL/'000'O/

*       Obtain the lengths of the character strings:
        LIN=LEN(CIN)
        NPT=INDEX(CIN,NULL) !Locate a possible null, for back-compatibility.
        IF (NPT.LE.0) NPT=LIN+1 !No null - "pretend" null follows string.
        LIN=NPT-1 !Use this as the effective length.
        SPT=INDEX(CIN,' ') !Point to 1st space.
        IF (SPT.LE.0) SPT=LIN+1 !No space - "pretend" space follows string.
        IF (SPT.LT.LIN) LIN=SPT-1 !If space preceeds end, use space-pointer.
*       LIN now is the "effective" length of CIN.

        LOUT=LEN(COUT)
        IF (LIN.GT.LOUT) GO TO 5000 !Error -- output string too short.

        COUT=' ' !Blank out COUT.

*       Protect against possible VMS-style directories containing periods:
        RBPT=INDEX(CIN,']')
        IF (RBPT.GT.0) THEN !There's a directory spec here:
          START=RBPT+1
          IF (START.GT.LIN) START=LIN
        ELSE !No directory spec:
          START=1
        END IF

*       Determine whether CIN already has a file-type specified:
        DPT=INDEX(CIN(START:LIN),'.')
        IF (DPT.LE.0) THEN !File type is not specified.
          LDEF=LEN(CDEF) !Length of default file type.
          NPT=INDEX(CDEF,NULL) !Locate a possible null, for back-compatibility.
          IF (NPT.LE.0) NPT=LDEF+1 !No null - "pretend" null follows string.
          LDEF=NPT-1 !Use this as the effective length.
          SPT=INDEX(CDEF,' ') !Point to 1st space.
          IF (SPT.LE.0) SPT=LDEF+1 !No space - "pretend" space follows string.
          IF (SPT.LT.LDEF) LDEF=SPT-1 !If space preceeds end, use space-pntr.
*         LDEF now is the "effective" length of CDEF.
          IF ((LIN+LDEF+1).GT.LOUT) GO TO 5000 !Not enough room.
          PPT=INDEX(CDEF,'.')
          IF (PPT.EQ.1) THEN !"." is included in the default type:
            COUT=CIN(1:LIN)//CDEF(1:LDEF)
          ELSE !"." is not included in the default type:
            COUT=CIN(1:LIN)//'.'//CDEF(1:LDEF)
          END IF
        ELSE !File type is specified.
          COUT=CIN(1:LIN) !Move the already-complete file-spec into COUT.
        END IF

        RETURN

*       Error - insert error-name and return:
5000    COUT='strparsef.err'
        RETURN

        END
*
        LOGICAL FUNCTION STRPARSEN(S,SPT,D)

        IMPLICIT NONE

*  Input:
        CHARACTER*(*) S !Character string containing one or more "arguments"
*                        separated from one another by "separators".

*  Input/Output:
        INTEGER SPT     !Index to a starting point in S, advanced to the next
*                        starting point after each call to STRPARSEN.

*  Output:
        CHARACTER*(*) D !Returned with the single argument from S, indexed by
*                        SPT upon the call to STRPARSEN.

*  Return values:
*       Return .TRUE. if a next (now, this) argument exists, .FALSE. otherwise.

*  Description:
*       Parse the next argument out of S and into D.
*       Leave SPT pointing at the space after the argument parsed,
*       or at SMAX.  Call STRPARSEN in a loop until it returns false,
*       giving it a different D each time (D is usually a dimensioned
*       array);  this parses out the arguments in S.  Separators are:
*       space, tab, comma, and equal-sign.  The equal-sign is a special
*       separator in that it is also returned as an argument.  This permits
*       checking externally to STRPARSEN that equal signs occur where they
*       are supposed to.  To keep spaces or "=" in a single argument, embed
*       the whole argument in double quotes "".

        LOGICAL SEP_FL
        INTEGER I
        INTEGER SLEN,DLEN

        CHARACTER*1 QUOTE_CHAR
        LOGICAL QUOTE

        SLEN=LEN(S)
        DLEN=LEN(D)

        STRPARSEN=.FALSE.

        IF (SPT.GT.SLEN) RETURN !Argument list exhausted.

        D=' ' !Blank out D.

*       Find first character (at beginning or after sep.).
*       Might be already positioned, or might need to skip several sep. chars.:
        SEP_FL=.TRUE. !Force initial loop entry.
        DO WHILE (SEP_FL.AND.(SPT.LE.SLEN))
*         Space,tab,comma,newline:
          IF (INDEX('   ,\n',S(SPT:SPT)).LE.0) THEN
*           SPT not pointing at separator (exit this loop):
            SEP_FL=.FALSE.
          ELSE !Points at a separator (continue this loop):
            SPT=SPT+1 !Next character.
          END IF
        END DO

*       SPT now points at begining of next (now current!) field:
*       Copy to D until sep. or end:
        IF (S(SPT:SPT).EQ.'=') THEN !Single character, self-separating "=":
          D(1:1)='='
          STRPARSEN=.TRUE. !Set true if at least one character is moved.
          SEP_FL=.TRUE. !Self-separating - stop here.
          SPT=SPT+1
        END IF

        I=1
        QUOTE=.FALSE.
        QUOTE_CHAR=' '
        DO WHILE ((.NOT.SEP_FL).AND.(SPT.LE.SLEN).AND.(I.LE.DLEN))
          IF (QUOTE) THEN !Nested in quotes.
            IF (S(SPT:SPT).EQ.QUOTE_CHAR) THEN !End-quote.
              SPT=SPT+1 !Skip, don't copy the quote.
              QUOTE=.FALSE.
            ELSE !Still in the quote:
              STRPARSEN=.TRUE. !Set true if at least one character is moved.
              D(I:I)=S(SPT:SPT)
              I=I+1
              SPT=SPT+1
            END IF
          ELSE IF (INDEX('"''',S(SPT:SPT)).GT.0) THEN !SPT points at quote.
            QUOTE=.TRUE.
            QUOTE_CHAR=S(SPT:SPT) !Use this to look for matching end-quote.
            SPT=SPT+1 !Skip, don't copy the quote.

*         Space,tab,comma,newline:
          ELSE IF (INDEX('      ,\n',S(SPT:SPT)).GT.0) THEN !SPT points at a separator:
            SEP_FL=.TRUE.
          ELSE IF (S(SPT:SPT).EQ.'=') THEN !Self-separating character "=".
            SEP_FL=.TRUE.
          ELSE
            STRPARSEN=.TRUE. !Set true if at least one character is moved.
            D(I:I)=S(SPT:SPT)
            I=I+1
            SPT=SPT+1
          END IF
        END DO
        RETURN
        END
*
        INTEGER FUNCTION STRUNCODE(S,REQRAD,UNINT,UNREAL)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) S         !Character string containing representation.
        INTEGER REQRAD          !Requested radix for conversion - 2,8,10,16.

*  Outputs:
        INTEGER UNINT           !Result as an integer, if possible.
        DOUBLE PRECISION UNREAL !Result as a floating value, unless error.

*  Description:
*       Decode the character string in S as a number.
*       Put the result in UNREAL and UNINT, if possible.
*       Return STRUNCODE = :
*       -1      error - nothing was uncoded.
*        0      a real number was decoded, but the result was too big
*               to be converted into integer form.
*        1      a real number was decoded, and exists in integer form, too.

*       To set the default conversion radix to other than 10,
*       set REQRAD to 2, 8, or 16 - otherwise it's assumed decimal.
*       Numbers of radices 2, 8 and 16 are allowed, the radix being
*       indicated by prefixing the ASCII representation of the number
*       with a '%' for radix 2, '"' or 'O' for radix 8, and 'H' or 'X'
*       for radix 16.  The prefix and the number may not be separated
*       by any character, such as a space.
*       If radix 10 is desired and the default radix isn't 10, use the
*       decimal radix indicator '$'.

        REAL RIMAX !Maximum integer value.

        INTEGER END,DEFRAD,FINFLG,NPT,START,PPT,NCHAR,NDCHAR,RADID
        INTEGER IBYTE,DPT,ACCUM,SGNPOS,I,J,SLEN
        INTEGER MAXCHR(3),SGNBIT(3),RADIX(3),MAXFST(3)

        LOGICAL SIGN

        CHARACTER*1 MAXNUM(3),CBYTE,RADCHAR
        CHARACTER*10 FORMAT
        CHARACTER*80 C80

        DATA RADIX/8,16,2/MAXNUM/'7','F','1'/

        DATA MAXCHR/11,8,32/MAXFST/3,15,1/SGNBIT/2,8,1/SGNPOS/31/
*       DATA RIMAX/2147483647./ !Exact value.
        DATA RIMAX/2.14648E9/ !"Safe" underestimate - watch round-offs!!

        DATA FORMAT/' '/


        SLEN=LEN(S) !Get the character-string length.

        DEFRAD=10 !Default radix.
        IF (REQRAD.EQ.16) DEFRAD=16
        IF (REQRAD.EQ.8) DEFRAD=8
        IF (REQRAD.EQ.2) DEFRAD=2

        FINFLG=0
        UNINT=0
        UNREAL=0.D0

        CALL STREND(S,END) !Point at "end" (last non-blank).

        IF (END.LE.0) THEN
          STRUNCODE=-1 !Error.
          RETURN
        END IF

        IF (END.GT.SLEN) END=SLEN !Force a safe end.
        C80=S(1:END)//' '  !Make a copy.
        CALL STRCAPS(C80(:END)) !Convert to caps.

*       Have the end of the character group; END points at the last non-bl.
*       Is a radix specified with the representation (overides REQRAD)?
        RADCHAR=C80(1:1) !1st character is where radix specifier would be.
        RADID=INDEX('"OXH%$',RADCHAR) !Octal,Octal,Hex,Hex,Binary,Decimal.
        IF (RADID.LE.0) THEN !There's no radix indicator;  use default:
          START=1
          NCHAR=END
*         Check the default radix:
          IF (DEFRAD.EQ.8) RADID=1
          IF (DEFRAD.EQ.16) RADID=2
          IF (DEFRAD.EQ.2) RADID=3
        ELSE !Special radix indicator:
          START=2 !Skip radix indicator.
          IF (END.LE.1) THEN
            STRUNCODE=-1 !Error.
            RETURN
          END IF
          NCHAR=END-1
          IF (RADID.EQ.6) RADID=0 !Decimal (prefix "$").  Set to zero.
*         This is just a quick and dirty remap:
          RADID=(RADID+1)/2 !2 indicators for each radix ('cept B & D).
        END IF

        IF (RADID.LE.0) THEN !Decimal conversion:
*         Decimal radix - either by default or by indicator:
          PPT=INDEX(C80(START:END),'.')
          IF (PPT.LE.0) THEN !No decimal point.
            NDCHAR=0
          ELSE
            NDCHAR=END-PPT !# of chars. in the decimal field.
          END IF

*         Make the format statement:
          WRITE(FORMAT,101,ERR=5000) NCHAR,NDCHAR
101       FORMAT('(D'I2'.'I2')')

*         Try to decode the character group as a real number:
          READ(C80(START:END),FORMAT,ERR=5000) UNREAL

          STRUNCODE=0 !Indicate success as a real number.

          IF (UNREAL.GT.RIMAX) RETURN !Not an integer.
          IF (UNREAL.LT.-(RIMAX+1.)) RETURN !Not an integer.

*         Obtain the integer value, too:
          UNINT=NINT(UNREAL)
          STRUNCODE=1 !Indicate successful integer conversion.
          RETURN

        END IF !RADID.LE.0


*       Number should be converted according to a non-decimal radix:

        IF ((NCHAR.LE.0).OR.(NCHAR.GT.MAXCHR(RADID))) THEN
          GO TO 5000 !error.
        END IF

        SIGN=.FALSE. !The sign bit flag (not used as a sign bit, here).
        ACCUM=0 !The result will be accumulated here.
*       Cycle through the digits  *****************************
        DO DPT=1,NCHAR
          CBYTE=C80(DPT+START-1:DPT+START-1)
          IBYTE=ICHAR(CBYTE) !For arithmetic opeations.
          IF (LGT(CBYTE,MAXNUM(RADID))) THEN
            GO TO 5000 !Illegal character.
          ELSE IF (IBYTE.LE.0) THEN
            GO TO 5000 !Illegal character.
          ELSE IF (RADID.EQ.2) THEN !Hex.
*           Special treatment for hex:
            IF (LGT(CBYTE,'9').AND.LLT(CBYTE,'A')) THEN
              GO TO 5000 !Illegal char.
            ELSE IF (LGE(CBYTE,'A')) THEN
              IBYTE=(IBYTE-ICHAR('A'))+ICHAR('9')+1 !ASCII gap betw. 9 and A.
            END IF
          END IF
          IBYTE=IBYTE-ICHAR('0') !Convert ASCII into a number.
          IF (IBYTE.LT.0) THEN
            GO TO 5000 !Error.
          END IF
          IF (DPT.LE.1) THEN !The first digit.
*           The first digit may need special attention.
            IF (NCHAR.LT.MAXCHR(RADID)) THEN !Nothing special.
            ELSE IF (IBYTE.LT.SGNBIT(RADID)) THEN !Nothing special.
            ELSE IF (IBYTE.GT.MAXFST(RADID)) THEN !Error.
              GO TO 5000 !Illegal 1st digit.
            ELSE !Sign bit must be used (MSB of 16 bit unsigned integer).
*             (Be careful not to do arithmetic with sign bit.)
              SIGN=.TRUE.
              IBYTE=IBYTE-SGNBIT(RADID)
            END IF
          END IF
          ACCUM=ACCUM*RADIX(RADID)+IBYTE
        END DO

        UNREAL=FLOAT(ACCUM)
        IF (SIGN) THEN
          ACCUM=IOR(ACCUM,'80000000'X)
          UNREAL=UNREAL+2.D0**31
        END IF
        UNINT=ACCUM

*       Done:
        STRUNCODE=1 !Indicate successful integer conversion.
        RETURN

*       Errors:
5000    CONTINUE
        STRUNCODE=-1
        RETURN
        END
*
        INTEGER FUNCTION STR_NEXTARG(ARG,NARGS,JARG)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) ARG(*)
        INTEGER NARGS

*  Input/output:
        INTEGER JARG

*  Returns:
*       STR_NEXTARG_SUCCESS_RC if JARG points at the next argument.
*       STR_NEXTARG_NOARGS_RC if no arguments remain;  JARG is invalid.

*  Functional description:
*       Advances JARG to point at the next argument, if any remain.

        INCLUDE 'str_parse_inc'

        JARG=JARG+1 !Point to next argument
        IF (JARG.GT.NARGS) THEN !Error -- no args left.
          STR_NEXTARG=STR_NEXTARG_NOARGS_RC
        ELSE !Success -- JARG points at the next argument:
          STR_NEXTARG=STR_NEXTARG_SUCCESS_RC
        END IF

        RETURN
        END
*
        INTEGER FUNCTION STR_NEXTARG_ALLOWEQ(ARG,NARGS,JARG)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) ARG(*)
        INTEGER NARGS
*  Input/output:
        INTEGER JARG

*  Returns:
*       STR_NEXTARG_SUCCESS_RC if JARG points at the next argument, skipping
*       an equals sign "=" if encountered.
*       STR_NEXTARG_NOARGS_RC if no arguments remain;  JARG is invalid.

        INCLUDE 'str_parse_inc'

        JARG=JARG+1 !Point to next argument
        IF (JARG.GT.NARGS) THEN !Error -- no args left.
          STR_NEXTARG_ALLOWEQ=STR_NEXTARG_NOARGS_RC
        ELSE IF (ARG(JARG).EQ.'=') THEN
*         Skip over an equals sign:
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
*
        INTEGER FUNCTION STR_NEXTARG_REQEQ(ARG,NARGS,JARG)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) ARG(*)
        INTEGER NARGS
*  Input/output:
        INTEGER JARG

*  Returns:
*       STR_NEXTARG_SUCCESS_RC if JARG points at the next argument, skipping
*       a required equals sign "=".
*       STR_NEXTARG_NOARGS_RC if no arguments remain;  JARG is invalid.
*       STR_NEXTARG_NOEQ_RC if no equals sign was found.  JARG points at
*       where the equals sign was expected.

        INCLUDE 'str_parse_inc'

        JARG=JARG+1 !Point to next argument
        IF (JARG.GT.NARGS) THEN !Error -- no args left.
          STR_NEXTARG_REQEQ=STR_NEXTARG_NOARGS_RC
        ELSE IF (ARG(JARG).EQ.'=') THEN
*         Skip over the required equals sign:
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
*
        LOGICAL FUNCTION STR_NUMARG(ARG,RADIX)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) ARG
        INTEGER RADIX        !The base in which translation would
                             !be done: 2, 8, 10, 16.

*  Returns:
*       True if ARG is a number, in base RADIX.
*       False if ARG is a not number, in base RADIX.

*  Functional description:
*       Determine whether ARG is a candidate number-value.

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
*

*       File:  str_file.f

*       This set of routines provides some platform-independent
*       file-access capabilities, such as openning, reading,
*       writing and simple dialogs.

*
        LOGICAL FUNCTION STRGETCOM(FUNIT,S,EPT)

        IMPLICIT NONE

*  Input:
        INTEGER FUNIT !FORTRAN logical unit from which to command-lines.

*  Outputs:
        CHARACTER*(*) S !If successfull, contains the first non-blank command
*                        line read from FUNIT.  STRGETCOM keeps reading lines
*                        from FUNIT until it gets one that is not a blank or
*                        a pure comment.  Any comments are stripped off and
*                        are not sent out with S.
        INTEGER EPT !The index to the last non-blank/tab in S.

*  Return-value:
*       .true.  ==> command line successfully read in.
*       .false. ==> EOF encountered on logical unit FUNIT.

*  Description:
*       Get first non-comment, non-blank line, from logical unit FUNIT, and
*       put it in S.  Remove comments and set EPT to point at the last
*       non-blank character. Return the FUNCTION value as .true. for success,
*       .false. for EOF.  Ignores form feeds.
*       No null is appended.

        CHARACTER*1 FF
        CHARACTER*132 S132

        DATA FF/'014'O/

201     FORMAT(A132)

1000    STRGETCOM=.FALSE. !In case of EOF.
        EPT=0
        DO WHILE (EPT.LT.1) !Skip over any pure-comment lines.
          READ(FUNIT,201,END=2000) S132
          IF (S132(1:1).EQ.FF) GO TO 1000 !Restart.
          CALL STRIP(S132,EPT) !Strip off comments/find last non-blank.
        END DO
        S=S132 !Copy over as much as will fit.
        STRGETCOM=.TRUE.

2000    RETURN

        END
*
        SUBROUTINE STRLIST(MSG,LINES,LUN)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) MSG(*) !Character string to be output.
        INTEGER LINES !Number of lines in MSG to output.
        INTEGER LUN !Logical unit on which to output (terminal).

*  Description:
*       Output a one-or-more-line message on a terminal-emulating
*       listing-file with machine-independent carriage-control as if
*       on a terminal.

        INCLUDE 'str_file_inc'

        INTEGER I

        DO I=1,LINES
          WRITE(LUN,100) MSG(I)
        END DO
100     FORMAT(A)

        STRLIST_NOCR_ACTIVE=.FALSE.

        RETURN
        END
*
        SUBROUTINE STRLIST_NOCR(MSG,LUN)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) MSG !Character string to be output.
        INTEGER LUN !Logical unit on which to output (terminal).

*  Description:
*       Emulate output of a one-line message without a following
*       carriage return on a terminal-emulating listing-file.
*       Actual outputing is deferred until a STRLIST_NOLF
*       call is made, at which time the message specified in that
*       latter call is appended to previously-specified output
*       and then output.  A call to STRLIST will wipe out any
*       characters from a STRLIST_NOCR or STRLIST_NOLFCR call.
*       Any existing characters in the previously-specified output
*       is overwritten.

        INCLUDE 'str_file_inc'

        STRLIST_NOCR_LINE=MSG
        STRLIST_NOCR_ACTIVE=.TRUE.

        RETURN
        END
*
        SUBROUTINE STRLIST_NOLF(MSG,LUN)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) MSG !Character string to be output.
        INTEGER LUN !Logical unit on which to output (terminal).

*  Description:
*       Output a one-line message without a preceding line feed
*       on a terminal-emulating listing file.  MSG is appended
*       to any previously specified output from a call to either
*       STRLIST_NOCR or STRLIST_NOLFCR before it is output.

        INCLUDE 'str_file_inc'


        INTEGER EPT

        IF (STRLIST_NOCR_ACTIVE) THEN !Append MSG to the existing line:

          CALL STREND(STRLIST_NOCR_LINE,EPT) !Find the current end.
          IF (EPT.LE.0) THEN !Nothing to append to:
            STRLIST_NOCR_LINE=MSG
          ELSE !Append MSG to exisitng line:
            STRLIST_NOCR_LINE=STRLIST_NOCR_LINE(:EPT)//MSG
          END IF

        END IF

        CALL STREND(STRLIST_NOCR_LINE,EPT) !Find the current end.
        IF (EPT.GT.0) THEN !Ouput only if there's something to output.
          WRITE(LUN,100) STRLIST_NOCR_LINE(:EPT)
        END IF
100     FORMAT(A)

        STRLIST_NOCR_ACTIVE=.FALSE.

        RETURN
        END
*
        SUBROUTINE STRLIST_NOLFCR(MSG,LUN)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) MSG !Character string to be output.
        INTEGER LUN !Logical unit on which to output (terminal).

*  Description:
*       Output a one-line message without a preceding line feed
*       or a following carriage-return on a terminal-emulating
*       listing-file.
*       Actual outputing is deferred until a STRLIST_NOLF
*       call is made, at which time the message specified in that
*       latter call is appended to previously-specified output
*       and then output.  A call to STRLIST will wipe out any
*       characters from a STRLIST_NOCR or STRLIST_NOLFCR call.
*       MSG is appended to any existing characters in
*       previously-specified output in a STRLIST_NOCR or STRLIST_NOLFCR
*       call.

*       It is illegal to call this routine before a call to any one of
*       STRLIST, STRLIST_NOCR or STRLIST_NOLF.

        INCLUDE 'str_file_inc'

        INTEGER EPT

        IF (STRLIST_NOCR_ACTIVE) THEN !Append MSG to the existing line:

          CALL STREND(STRLIST_NOCR_LINE,EPT) !Find the current end.
          IF (EPT.LE.0) THEN !Nothing to append to:
            STRLIST_NOCR_LINE=MSG
          ELSE !Append MSG to exisitng line:
            STRLIST_NOCR_LINE=STRLIST_NOCR_LINE(:EPT)//MSG
          END IF

        END IF

        STRLIST_NOCR_ACTIVE=.TRUE.


        RETURN
        END
*
        LOGICAL FUNCTION STROPEN(LUN,FILENAME,COMMANDS)

        IMPLICIT NONE

*  Inputs:
        INTEGER LUN !Logical unit on which to open file.
        CHARACTER*(*) FILENAME !Name of file.
        CHARACTER*(*) COMMANDS !Command line, through which
                               !OPEN options are selected.

*  Return values:
*       .TRUE. for a successful open on FILENAME.
*       .FALSE. for a failed open.


*  Description:
*       Attempts to open the specified file in a machine-independent fashion
*       using the OPEN parameters specified in COMMANDS.  COMMANDS is a
*       character string containing all the "usual" OPEN parameters, except
*       for the UNIT=lun (FORTRAN Logical Unit) and FILE=filename, which are passed
*       separately, and without the usual single-quotes (apostrophes) normally
*       around literal arguments.


        INTEGER NARGS_max
        PARAMETER (NARGS_max=30)
        INTEGER IARG(NARGS_max)
        DOUBLE PRECISION DARG(NARGS_max)
        CHARACTER*80 ARG(NARGS_max)
        LOGICAL VIARG(NARGS_max)
        LOGICAL VDARG(NARGS_max)

        INTEGER JARG,NARGS
        INTEGER I

        LOGICAL Open_success
        LOGICAL MACHINE_SPECIFIC

        CHARACTER*40 STATUS_CARG
        CHARACTER*40 ACCESS_CARG
        CHARACTER*40 FORM_CARG
        LOGICAL RECL_FLAG
        INTEGER RECL_IARG
        LOGICAL CARRIAGECONTROL_FLAG
        CHARACTER*40 CARRIAGECONTROL_CARG
        LOGICAL RECORDTYPE_FLAG
        CHARACTER*40 RECORDTYPE_CARG
        LOGICAL INITIALSIZE_FLAG
        INTEGER INITIALSIZE_IARG
        LOGICAL BLOCKSIZE_FLAG
        INTEGER BLOCKSIZE_IARG
        LOGICAL MAXREC_FLAG
        INTEGER MAXREC_IARG
        LOGICAL READONLY_FLAG


        LOGICAL STROPEN_NAT,STRINT

*       write(6,300) lun,filename,commands
*300    format(' STROPEN-D0 LUN:'I11'  filename/commands:'/' 'A/' 'A)


*       Parse out the commands:
        CALL STRPARSE
     1      (COMMANDS,NARGS_max,NARGS,ARG,DARG,IARG,VDARG,VIARG)

        IF (NARGS.LE.0) THEN !No arguments -- simple open:
          Open_success=.FALSE.
          OPEN(UNIT=LUN,FILE=FILENAME,STATUS='UNKNOWN',ERR=3)
          Open_success=.TRUE.
3         CONTINUE
          STROPEN=Open_success
*         write(6,301) Open_success
*301    format(' STROPEN-D1  Plain OPEN, success:'L1)
          RETURN
        END IF

*       Convert all args to all-caps:
        DO I=1,NARGS
          CALL STRCAPS(ARG(I))
        END DO

*       Standard defaults:
        STATUS_CARG='UNKNOWN'
        FORM_CARG='FORMATTED'
        ACCESS_CARG='SEQUENTIAL'
        RECL_FLAG=.FALSE.

        MACHINE_SPECIFIC=.FALSE.

*       VMS & SGI specials:
        CARRIAGECONTROL_FLAG=.FALSE.
        CARRIAGECONTROL_CARG='LIST'
        MAXREC_FLAG=.FALSE.

*       VMS specials:
        RECORDTYPE_FLAG=.FALSE.
        RECORDTYPE_CARG='VARIABLE' !Also in SGI, but unnecessary.
        INITIALSIZE_FLAG=.FALSE.
        INITIALSIZE_IARG=0
        BLOCKSIZE_FLAG=.FALSE.
        READONLY_FLAG=.FALSE.


        DO JARG=1,NARGS

          IF (ARG(JARG)  .EQ.'READONLY') THEN

            READONLY_FLAG=.TRUE.
            MACHINE_SPECIFIC=.TRUE.

          ELSE IF (JARG.GT.NARGS-2) THEN !Not enough args left to proceed.

          ELSE IF ( (ARG(JARG)  .EQ.'STATUS')
     1        .AND. (ARG(JARG+1).EQ.  '='   ) ) THEN

            STATUS_CARG=ARG(JARG+2)

          ELSE IF ( (ARG(JARG)  .EQ.'ACCESS')
     1        .AND. (ARG(JARG+1).EQ.  '='   ) ) THEN

            ACCESS_CARG=ARG(JARG+2)

          ELSE IF ( (ARG(JARG)  .EQ.'FORM')
     1        .AND. (ARG(JARG+1).EQ.'='   ) ) THEN

            FORM_CARG=ARG(JARG+2)

          ELSE IF ( (ARG(JARG)  .EQ.'RECL')
     1        .AND. (ARG(JARG+1).EQ.'='   ) ) THEN


            IF (VIARG(JARG+2)) THEN !Valid integer in argument:
              MACHINE_SPECIFIC=.TRUE. !RECL is in machine-units.
              RECL_FLAG=.TRUE.
              RECL_IARG=IARG(JARG+2)
            END IF

          ELSE IF ( (ARG(JARG)  .EQ.'CARRIAGECONTROL')
     1        .AND. (ARG(JARG+1).EQ.  '='            ) ) THEN

            MACHINE_SPECIFIC=.TRUE.
            CARRIAGECONTROL_FLAG=.TRUE.
            CARRIAGECONTROL_CARG=ARG(JARG+2)

          ELSE IF ( (ARG(JARG)  .EQ.'RECORDTYPE')
     1        .AND. (ARG(JARG+1).EQ.  '='       ) ) THEN

            MACHINE_SPECIFIC=.TRUE.
            RECORDTYPE_FLAG=.TRUE.
            RECORDTYPE_CARG=ARG(JARG+2)

          ELSE IF ( (ARG(JARG)  .EQ.'INITIALSIZE')
     1        .AND. (ARG(JARG+1).EQ.  '='        ) ) THEN

            IF (VIARG(JARG+2)) THEN !Valid integer in argument:
              MACHINE_SPECIFIC=.TRUE.
              INITIALSIZE_FLAG=.TRUE.
              INITIALSIZE_IARG=IARG(JARG+2)
            END IF

          ELSE IF ( (ARG(JARG)  .EQ.'BLOCKSIZE')
     1        .AND. (ARG(JARG+1).EQ.  '='       ) ) THEN

            IF (VIARG(JARG+2)) THEN !Valid integer in argument:
              MACHINE_SPECIFIC=.TRUE.
              BLOCKSIZE_FLAG=.TRUE.
              BLOCKSIZE_IARG=IARG(JARG+2)
            END IF

          ELSE IF ( (ARG(JARG)  .EQ.'MAXREC')
     1        .AND. (ARG(JARG+1).EQ.  '='       ) ) THEN

            IF (VIARG(JARG+2)) THEN !Valid integer in argument:
              MACHINE_SPECIFIC=.TRUE.
              MAXREC_FLAG=.TRUE.
              MAXREC_IARG=IARG(JARG+2)
            END IF

          ELSE !Unrecognized command.
          END IF

        END DO !JARG=1,NARGS-2

        IF (MACHINE_SPECIFIC) THEN

          Open_success=STROPEN_NAT(LUN,FILENAME
     1   ,STATUS_CARG,ACCESS_CARG,FORM_CARG
     1   ,RECL_FLAG,RECL_IARG
     1   ,CARRIAGECONTROL_FLAG,CARRIAGECONTROL_CARG
     1   ,RECORDTYPE_FLAG,RECORDTYPE_CARG
     1   ,INITIALSIZE_FLAG,INITIALSIZE_IARG
     1   ,BLOCKSIZE_FLAG,BLOCKSIZE_IARG
     1   ,MAXREC_FLAG,MAXREC_IARG
     1   ,READONLY_FLAG
     1   )

*         write(6,302) Open_success
*302    format(' STROPEN-D2  Native OPEN, success:'L1)

        ELSE

          Open_success=.FALSE.
          OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,ERR=2)
          Open_success=.TRUE.
2         CONTINUE

*         write(6,304) Open_success,STATUS_CARG,ACCESS_CARG,FORM_CARG
*     1               ,FILENAME
*304    format(' STROPEN-D4  no-RECL OPEN, success:'L1/
*     1        '             STATUS:'A/
*     1        '             ACCESS:'A/
*     1        '             FORM:  'A/
*     1        '             FILE:  'A)

        END IF

        STROPEN=Open_success
        RETURN
        END

*
        LOGICAL FUNCTION STROPENVER(LUN,FILENAME,COMMANDS
     1    ,VLIMIT,VCHAR,VERSION,FILE_NAME_OUT)

        IMPLICIT NONE

*  Inputs:
        INTEGER LUN !Logical unit on which to open file.
        CHARACTER*(*) FILENAME !Name of file.
        CHARACTER*(*) COMMANDS !Command line, through which
                               !OPEN options are selected.

        INTEGER VLIMIT !Caller-specified limit to the version number.
                       !When VERSION exceeds this, STROPENVER fails.

        CHARACTER*1 VCHAR !One-character version-field delimiter, caller
                          !specified.  eg, ";".

*  Outputs:
        INTEGER VERSION !The version number of a successfully opened file.

        CHARACTER*(*) FILE_NAME_OUT !Same as FILENAME, but with a version
                                    !number appended after an underscore.
                                    !Returns blank if an error occurs.
                                    !If VERSION is 0, the returned file name
                                    !is exactly the specified file name;
                                    !no version-field delimiter or digits
                                    !are appended.

*  Return values:
*       .TRUE. for a successful open on: FILENAME//VCHAR//VERSION.
*       .FALSE. for a failed open.


*  Description:
*       Combines the functions of STRVER and STROPEN, providing a simple
*       way to open version-appended files in a machine-independent fashion,
*       using the OPEN parameters specified in COMMANDS.  COMMANDS is a
*       character string containing all the "usual" OPEN parameters, except
*       for the UNIT=lun (FORTRAN Logical Unit) and FILE=filename, which are passed
*       separately, and without the usual single-quotes (apostrophes) normally
*       around literal arguments.


        CHARACTER*200 Local_filename !Make it good and long.

        LOGICAL VALID,SUCCESS
        LOGICAL STRVER,STROPEN



        VALID=.TRUE.
        SUCCESS=.FALSE.
        VERSION=0
        DO WHILE ((.NOT.SUCCESS).AND.VALID)
          VALID=STRVER(FILENAME,Vlimit,Vchar,VERSION,Local_filename)
          IF (VALID) THEN !Valid name, i.e.,  VERSION .le. Vlimit:
            SUCCESS=STROPEN(LUN,Local_filename,COMMANDS)
          END IF
        END DO

        FILE_NAME_OUT=Local_filename

*       write(6,301) len(file_name_out),file_name_out
*301    format(' STROPENVER-D1 LEN(FILE_NAME_OUT):'I11' FILE_NAME_OUT:'/
*     1        ' 'A)


        STROPENVER=SUCCESS

        END
*
        LOGICAL FUNCTION STRQUERY(Q,D,INLUN,OUTLUN,A)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) Q,D !Question & default answer.
        INTEGER INLUN !FORTRAN logical unit from which input is taken.
        INTEGER OUTLUN !FORTRAN logical unit to which the questions are put.

*  Output:
        CHARACTER*(*) A !Answer to the question.

*  Description:
*       Ask the operator, found on INLUN and OUTLUN, for the answer to the
*       .le. 80 character question Q.  The default .le. 80 character answer
*       is specified by the calling program in D.  The answer is returned
*       in A.  The last non-blank answer is returned.  STRQUERY does not
*       exit until a blank answer or EOF is specified.
*       If OUTLUN is 0, then nothing is output and the first input from
*       INCOM is accepted & returned.

*       STRQUERY = .TRUE. for normal return,
*       STRQUERY = .FALSE. for EOF on INLUN.

        INTEGER LQ,LD,LA,L
        CHARACTER*80 C80,B80

101     FORMAT(' 'A/' 'A)
102     FORMAT(' 'A' [ 'A' ] '$)
201     FORMAT(A)

        IF (OUTLUN.LE.0) THEN
          READ(INLUN,201,END=2000) A
          STRQUERY=.TRUE.
          RETURN
        END IF

        LQ=LEN(Q)
        LD=LEN(D)
        LA=LEN(A)
        L=LA+LD+LQ
        B80=D !Default answer.
        C80='*' !Dummy to make DO WHILE go at least once.

        DO WHILE (C80.NE.' ')
          A=B80 !Get current answer.
          IF (L.LT.73) THEN !Use short version:
            WRITE(OUTLUN,102) Q,A
            READ(INLUN,201,END=2000) C80
            B80=C80 !Make next current answer.
          ELSE IF ((LQ.GT.80).OR.(LA.GT.80)) THEN !Truncated long version:
            WRITE(OUTLUN,101) Q(1:MIN(80,LQ)),A(1:MIN(80,LA))
            READ(INLUN,201,END=2000) C80
            B80=C80 !Make next current answer.
          ELSE !Normal long version:
            WRITE(OUTLUN,101) Q,A
            READ(INLUN,201,END=2000) C80
            B80=C80 !Make next current answer.
          END IF
        END DO
        STRQUERY=.TRUE.
        RETURN

2000    STRQUERY=.FALSE.
        RETURN
        END
*
        LOGICAL FUNCTION STRQUERY1(Q,D,INLUN,OUTLUN,A)

        IMPLICIT NONE

*  Inputs:
        CHARACTER*(*) Q,D !Question & default answer.
        INTEGER INLUN !FORTRAN logical unit from which input is taken.
        INTEGER OUTLUN !FORTRAN logical unit to which the questions are put.

*  Output:
        CHARACTER*(*) A !Answer to the question.

*  Description:
*       Ask the operator, found on INLUN and OUTLUN, for the answer to the
*       .le. 80 character question Q.  The default .le. 80 character answer
*       is specified by the calling program in D.  The answer is returned
*       in A.  The first non-blank answer is returned.
*       If OUTLUN is 0, then nothing is output and the first input from
*       INCOM is accepted & returned.

*       STRQUERY1 = .TRUE. for normal return,
*       STRQUERY1 = .FALSE. for EOF on INLUN.

        INTEGER LQ,LD,LA,L
        CHARACTER*80 C80,B80

101     FORMAT( ' ' A / ' ' A )
102     FORMAT( ' ' A ' [' A '] ' $ )
103     FORMAT( ' ' A )
104     FORMAT( ' ' A ' ' $ )
201     FORMAT( A )

        IF (OUTLUN.LE.0) THEN
          READ(INLUN,201,END=2000) A
          STRQUERY1=.TRUE.
          RETURN
        END IF

        LQ=LEN(Q)
        LD=LEN(D)
        LA=LEN(A)
        L=LA+LD+LQ
        B80=D !Default answer.

        A=B80 !Get current answer.
        IF (L.LT.77) THEN !Use short version:
          IF ((LD.LE.0).OR.(D.EQ.' ')) THEN !No default answer:
            WRITE(OUTLUN,104) Q
          ELSE !Display default answer along with the query:
            WRITE(OUTLUN,102) Q,A
          END IF
          READ(INLUN,201,END=2000) C80
          B80=C80 !Make next current answer.
        ELSE IF ((LQ.GT.80).OR.(LA.GT.80)) THEN !Truncated long version:
          IF ((LD.LE.0).OR.(D.EQ.' ')) THEN !No default answer:
            WRITE(OUTLUN,103) Q(1:MIN(80,LQ))
          ELSE !Display default answer along with the query:
            WRITE(OUTLUN,101) Q(1:MIN(80,LQ)),A(1:MIN(80,LA))
          END IF
          READ(INLUN,201,END=2000) C80
          B80=C80 !Make next current answer.
        ELSE !Normal long version:
          IF ((LD.LE.0).OR.(D.EQ.' ')) THEN !No default answer:
            WRITE(OUTLUN,103) Q
          ELSE !Display default answer along with the query:
            WRITE(OUTLUN,101) Q,A
          END IF
          READ(INLUN,201,END=2000) C80
          B80=C80 !Make next current answer.
        END IF
        IF (C80.NE.' ') THEN !Non-blank response.
          A=C80 !Get current answer.
        END IF
        STRQUERY1=.TRUE.
        RETURN

2000    STRQUERY1=.FALSE.
        RETURN
        END
*
        LOGICAL FUNCTION STRVER
     1    (FILE_NAME_IN,VLIMIT,VCHAR,VNEXT,FILE_NAME_OUT)

        IMPLICIT NONE

*       Inputs:

        CHARACTER*(*) FILE_NAME_IN !Unadulterated file name.  Terminated
                                   !with a space or specified in quotes.

        INTEGER VLIMIT !Caller-specified limit to the version number.
                       !If, VNEXT exceeds this STRVER fails.

        CHARACTER*1 VCHAR !One-character version-field delimiter, caller
*                          specified.  eg, ";".

*  Input/Output:
        INTEGER VNEXT !The next version number.  Initialized by caller,
                      !incremented by STRVER each call.

*  Output:
        CHARACTER*(*) FILE_NAME_OUT !Same as FILE_NAME_IN, but with a version
                                    !number appended after an underscore.
                                    !Returns blank if an error occurs.
                                    !If VNEXT is 0, the returned file name
                                    !is exactly the specified file name;
                                    !no underscore or digits are appended.
*  Return values:
*       .true.  -- If a name was successfully generated.
*       .false. -- If a name was not generated, which would occur if VNEXT
*                  is specified as negative, or if VNEXT exceeds VLIMIT, or
*                  if FILE_NAME_IN is blank.

*  Description:
*       Make file "version" numbers & append it to the file name.
*       The file name constructed is "hypothetical", that is, it may
*       or may not work.  It is intended to be used in a FORTRAN OPEN
*       statement as the value to the "FILE=" keyword.  The OPEN
*       statement should specify an error return;  if the STRVER-
*       constructed filename fails in the OPEN, STRVER should be
*       called again, and it will return a new file name, with the
*       next version number, and the process repeated until the OPEN
*       succeeds or until VNEXT is higher than the calling routine's
*       specified limit, VLIMIT.


        INTEGER Len_in,Len_out,First_space,Len_ver
        CHARACTER*10 VERSION
        CHARACTER*132 Local_filename


        Len_in=LEN(FILE_NAME_IN)
        Len_out=LEN(FILE_NAME_OUT)
        First_space=INDEX(FILE_NAME_IN,' ')-1 !Use the space as a term. char.
        IF (First_space.LE.0) First_space=Len_in
        Len_in=MIN(Len_in,First_space) !Use position of 1st space or length.

        IF (VNEXT.LT.0) THEN
          Local_filename=' '
          STRVER=.false.
          RETURN
        ELSE IF (VNEXT.GT.VLIMIT) THEN
          Local_filename=' '
          STRVER=.false.
          RETURN
        ELSE IF (VNEXT.EQ.0) THEN
          Len_ver=0
          VERSION=' '
        ELSE IF (VNEXT.LT.10) THEN
          Len_ver=2
          WRITE(VERSION,100) VCHAR,VNEXT
100       FORMAT(A1,I1)
        ELSE IF (VNEXT.LT.100) THEN
          Len_ver=3
          WRITE(VERSION,101) VCHAR,VNEXT
101       FORMAT(A1,I2.2)
        ELSE IF (VNEXT.LT.1 000) THEN
          Len_ver=4
          WRITE(VERSION,102) VCHAR,VNEXT
102       FORMAT(A1,I3.3)
        ELSE IF (VNEXT.LT.10 000) THEN
          Len_ver=5
          WRITE(VERSION,103) VCHAR,VNEXT
103       FORMAT(A1,I4.4)
        ELSE IF (VNEXT.LT.100 000) THEN
          Len_ver=6
          WRITE(VERSION,104) VCHAR,VNEXT
104       FORMAT(A1,I5.5)
        ELSE IF (VNEXT.LT.1 000 000) THEN
          Len_ver=7
          WRITE(VERSION,105) VCHAR,VNEXT
105       FORMAT(A1,I6.6)
        ELSE IF (VNEXT.LT.10 000 000) THEN
          Len_ver=8
          WRITE(VERSION,106) VCHAR,VNEXT
106       FORMAT(A1,I7.7)
        ELSE IF (VNEXT.LT.100 000 000) THEN
          Len_ver=9
          WRITE(VERSION,107) VCHAR,VNEXT
107       FORMAT(A1,I8.8)
        ELSE IF (VNEXT.LT.1 000 000 000) THEN
          Len_ver=10
          WRITE(VERSION,108) VCHAR,VNEXT
108       FORMAT(A1,I9.9)
        ELSE
          Local_filename=' '
          STRVER=.false.
          RETURN
        END IF
        IF ((Len_ver+Len_in).GT.Len_out) THEN !Not enough room:
          Local_filename=' '
          STRVER=.false.
          RETURN
        END IF

        Local_filename=FILE_NAME_IN(1:Len_in)//VERSION
        FILE_NAME_OUT=Local_filename

*       WRITE(6,301) Len_in,FILE_NAME_IN(1:Len_in)
*     1             ,VERSION,LEN(FILE_NAME_OUT)
*     1             ,Local_filename
*301    FORMAT(' STRVER-D1-------------------------------------'/
*     1        ' Len_in:'I11' FILE_NAME_IN(1:Len_in):'/
*     1        ' 'A/
*     1        ' VERSION:'A'   Len(file_name_out):'I11' out-file:'/
*     1        ' 'A)


        Vnext=Vnext+1
        STRVER=.true.
        RETURN
        END
*

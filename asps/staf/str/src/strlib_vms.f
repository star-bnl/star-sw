*       VMS versions.


        SUBROUTINE STRCPU(TCPU)

        IMPLICIT NONE

*  Output:
        INTEGER TCPU

*  Description:
*       Obtain the total CPU time since counter-initialization:

        INTEGER IERR,QCPU(2)
        INTEGER LIB$STAT_TIMER
        IERR=LIB$STAT_TIMER(2,QCPU) !Don't care about errors here.
        TCPU=QCPU(1)

        RETURN
        END





        SUBROUTINE STRCPUQ(TCPU) !Quad-version (64-bits).

        IMPLICIT NONE

*  Output argument:
        INTEGER TCPU(2) !VMS uses 64 bits, others may or may not.

*  Description:
*       Obtain the total CPU time since counter-initialization:

        INTEGER IERR
        INTEGER LIB$STAT_TIMER
        IERR=LIB$STAT_TIMER(2,TCPU) !Don't care about errors here.

        RETURN
        END





        SUBROUTINE STRCPU0

*  Description:
*       Initialize CPU elapsed time counter.

        CALL LIB$INIT_TIMER
        RETURN
        END





        INTEGER FUNCTION STRCPUTPS()

        IMPLICIT NONE

*  Description:
*       Return Native cpu clock ticks-per-second.

        STRCPUTPS=100 !On VMS, it's 100 per second.

        RETURN
        END





        SUBROUTINE STRDATE(YEAR,MONTH,DAY)

        IMPLICIT NONE

*  Output arguments:
        INTEGER YEAR,MONTH,DAY !Year is 4-digit, eg, 1991.

*  Description:
*       Return the integer-date.

        CALL IDATE(MONTH,DAY,YEAR) !Get the numerical date.
        IF (YEAR.LT.1900) YEAR=YEAR+1900 !If it's missing, add the century.
        RETURN
        END





        SUBROUTINE STRDEC_ENDIAN(I32)

*  Description:
*       Compatibility routine.  See STRDEC_ENDIAN_HALF.

        IMPLICIT NONE

        INTEGER I32

        CALL STRDEC_ENDIAN_HALF(I32)

        RETURN
        END





        SUBROUTINE STRDEC_ENDIAN_BYTE(I32)

*  Description:
*       Swap the 4 8-bit bytes in the 32-bit (long) word I32,
*       if needed, to make the big/little endian business come out
*       DEC-style.  On VMS (here), nothing is done.

        IMPLICIT NONE

        INTEGER I32

        RETURN
        END





        SUBROUTINE STRDEC_ENDIAN_BYTES(Nwords,Block)

*  Description:
*       Swap the 4 8-bit bytes in the 32-bit (long) word I32,
*       if needed, to make the big/little endian business come out
*       DEC-style.  On VMS (here), nothing is done.

        IMPLICIT NONE

*  Input:
        INTEGER Nwords   !Number of 32-bit words in Block to reorder.

*  Input/Output:
        INTEGER Block(*) !Block of 32-bit words in to be reordered.

*       No-op, on VMS!  (Bye-bye.)

        RETURN
        END





        SUBROUTINE STRDEC_ENDIAN_HALF(I32)

*  Description:
*       Swap the two 16-bit half-words in the 32-bit (long) word I32,
*       if needed, to make the big/little endian business come out
*       DEC-style.  On VMS (here), nothing is done.

        IMPLICIT NONE

        INTEGER I32

        RETURN
        END





        INTEGER FUNCTION STRDEC_IBITS(DATA,BIT0,BITS)

*  Description:
*       Do an IBITS, but make it act like DEC.

        IMPLICIT NONE

        INTEGER DATA,BIT0,BITS

        STRDEC_IBITS=IBITS(DATA,BIT0,BITS) !This is VMS - same thing.

        RETURN
        END





        SUBROUTINE STRFLUSH( LUN )

        IMPLICIT NONE

*  Input:
        INTEGER LUN !FORTRAN Logical Unit of device (file) to be "flushed".

*  Description:
*       Do whatever platform-dependent operations are necessary to cause all
*       pending output for the specified LUN to be sent to the actual device
*       or file represented by the LUN, "flushing" out that device's buffer.

        RETURN !No-op under VMS.

        END





        SUBROUTINE STRMOVE(COUNT,SOURCE,DEST)

        IMPLICIT NONE

*  Inputs:
        INTEGER COUNT
        INTEGER SOURCE(*)

*  Output:
        INTEGER DEST(*)

*  Description:
*       VAX interface routine to LIB$MOVC3.  COUNT is a 32-bit word-count,
*       not a byte-count.

        INTEGER BYTES_TO_TRANSFER,BYTES_THIS_TRANSFER,I

        BYTES_TO_TRANSFER=COUNT*4
        I=1
        DO WHILE (BYTES_TO_TRANSFER.GT.0)
          BYTES_THIS_TRANSFER=MIN(BYTES_TO_TRANSFER,65532)
          CALL LIB$MOVC3
        1     (BYTES_THIS_TRANSFER,SOURCE(I),DEST(I))
          BYTES_TO_TRANSFER=BYTES_TO_TRANSFER-BYTES_THIS_TRANSFER
          I=I+BYTES_THIS_TRANSFER/4
        END DO

        RETURN
        END





        SUBROUTINE STRMSEC(MSECS)

        IMPLICIT NONE

*  Input:
        INTEGER MSECS !Millisecs.

*  Description:
*       Standard interface routine to return milliseconds since midnight.

        REAL SECNDS
        MSECS=INT(1000.*SECNDS(0.)) !Milliseconds since midnight
        RETURN
        END





        LOGICAL FUNCTION STRMSEC_DELAY(MSECS)

        IMPLICIT NONE

*  Input:
        INTEGER MSECS !Delay in milliseconds.

*  Description:
*       Standard interface routine to delay the specified time in
*       milliseconds.
*       Returns .TRUE. for success, .FALSE. for failure, such as
*       would occur for an illegally long, or negative time.

        CHARACTER*15 C15
        INTEGER DAYS,HOURS,MINS,SECS,MILLS,BAL
        INTEGER Tbinary(2)
        LOGICAL Success
        LOGICAL SYS$BINTIM,SYS$SETIMR,SYS$WAITFR


        STRMSEC_DELAY=.FALSE. !In case of failure, just return false.

        IF (MSECS.LT.0) THEN !Illegal time:
          RETURN
        END IF !MSECS.LT.0

        DAYS =INT(MSECS/(24*3600*100)) !DAYS will be no more than 248.
        BAL  =MSECS-DAYS*(24*3600*100)
        HOURS=INT(BAL/(3600*100))
        BAL  =BAL-HOURS*(3600*100)
        MINS =INT(BAL/(60*100))
        BAL  =BAL-MINS*(60*100)
        SECS =INT(BAL/100)
        MILLS=BAL-SECS*100

*       This is probably the safest way to get DEC's 64-bit "binary time";
*       by going ASCII first:
        WRITE(C15,100) DAYS,HOURS,MINS,SECS,MILLS
100     FORMAT(I3' 'I2.2':'I2.2':'I2.2'.'I2.2)

*       Convert ASCII to binary time:
        Success=SYS$BINTIM(C15,Tbinary)
        IF (.NOT.Success) THEN
          RETURN
        END IF

*       Set VMS's timer:
        Success=SYS$SETIMR(%VAL(0),Tbinary,,) !Event flag 0.
        IF (.NOT.Success) THEN
          RETURN
        END IF

*       Wait for the relevant flag:
        Success=SYS$WAITFR(%VAL(0))           !Event flag 0.
        IF (.NOT.Success) THEN
          RETURN
        END IF

        STRMSEC_DELAY=.TRUE.

        RETURN
        END





        SUBROUTINE STRNET_ENDIAN_BYTE(I32)

        IMPLICIT NONE

*  Input/Output:
        INTEGER I32

*  Description:
*       Swap the 4 8-bit bytes in the 32-bit (long) word I32,
*       if needed, to make the big/little endian business come out
*       Network-style.  On SGI (here), the swapping is done.

        INTEGER I4
        INTEGER*1 I1(4),I1SAVE
        EQUIVALENCE (I1,I4)

        I4=I32

*       Swap outter two:
        I1SAVE=I1(1)
        I1(1)=I1(4)
        I1(4)=I1SAVE

*       Swap inner two:
        I1SAVE=I1(2)
        I1(2)=I1(3)
        I1(3)=I1SAVE

        I32=I4

        RETURN
        END





        SUBROUTINE STRNET_ENDIAN_BYTES(Nwords,Block)

        IMPLICIT NONE

*  Input:
        INTEGER Nwords   !Number of 32-bit words in Block to reorder.

*  Input/Output:
        INTEGER Block(*) !Block of 32-bit words in to be reordered.

*  Description:
*       Swap the 4 8-bit bytes in the 32-bit (long) word I32,
*       if needed, to make the big/little endian business come out
*       Network-style.  On SGI (here), the swapping is done.

        INTEGER Iword

        INTEGER I4
        INTEGER*1 I1(4),I1SAVE
        EQUIVALENCE (I1,I4)

        DO Iword=1,Nwords

          I4=Block(Iword)

*         Swap outter two:
          I1SAVE=I1(1)
          I1(1)=I1(4)
          I1(4)=I1SAVE

*         Swap inner two:
          I1SAVE=I1(2)
          I1(2)=I1(3)
          I1(3)=I1SAVE

          Block(Iword)=I4

        END DO !Iword=1,Nwords

        RETURN
        END





        SUBROUTINE STRNET_ENDIAN_HALF(I32)

        IMPLICIT NONE

*  Input/Output:
        INTEGER I32

*  Description:
*       Swap the two 16-bit half-words in the 32-bit (long) word I32,
*       if needed, to make the big/little endian business come out
*       Network-style.  On SGI (here), the swapping is done.

        INTEGER I4
        INTEGER*2 I2(2),I2SAVE
        EQUIVALENCE (I2,I4)

        I4=I32
        I2SAVE=I2(1)
        I2(1)=I2(2)
        I2(2)=I2SAVE
        I32=I4

        RETURN
        END





        LOGICAL FUNCTION STROPEN_NAT( LUN, FILENAME
     1                 , STATUS_CARG, ACCESS_CARG, FORM_CARG
     2                 , RECL_FLAG  , RECL_IARG
     3                 , CARRIAGECONTROL_FLAG, CARRIAGECONTROL_CARG
     4                 , RECORDTYPE_FLAG , RECORDTYPE_CARG
     5                 , INITIALSIZE_FLAG, INITIALSIZE_IARG
     6                 , BLOCKSIZE_FLAG  , BLOCKSIZE_IARG
     7                 , MAXREC_FLAG     , MAXREC_IARG
     1                 , READONLY_FLAG           )

        IMPLICIT NONE

*  Input arguments:
        INTEGER       LUN         !Logical unit on which to open file.
        CHARACTER*(*) FILENAME    !Name of file.
        CHARACTER*(*) STATUS_CARG !"STATUS=" character argument
        CHARACTER*(*) ACCESS_CARG !"ACCESS=" character argument
        CHARACTER*(*) FORM_CARG   !"FORM=" character argument
        LOGICAL       RECL_FLAG   !RECL specified/not flag.
        INTEGER       RECL_IARG   !"RECL=" integer argument;  Record-length, bytes.
        LOGICAL       CARRIAGECONTROL_FLAG !CARRIAGECONTROL specified/not flag.
        CHARACTER*(*) CARRIAGECONTROL_CARG !"CARRIAGECONTROL=" char. argument
        LOGICAL       RECORDTYPE_FLAG      !RECORDTYPE specified/not flag.
        CHARACTER*(*) RECORDTYPE_CARG      !"RECORDTYPE=" character argument.
        LOGICAL       INITIALSIZE_FLAG     !INITIALSIZE specified/not flag.
        INTEGER       INITIALSIZE_IARG     !"INITIALSIZE=" integer argument.
        LOGICAL       BLOCKSIZE_FLAG       !BLOCKSIZE specified/not flag.
        INTEGER       BLOCKSIZE_IARG       !"BLOCKSIZE=" integer argument.
        LOGICAL       MAXREC_FLAG          !MAXREC specified/not flag.
        INTEGER       MAXREC_IARG          !"MAXREC=" integer argument.
        LOGICAL       READONLY_FLAG        !Whether OPEN is to be READONLY.

*  Description:
*       Provides a machine-dependent (native) OPEN routine, intended
*       to be called only from STROPEN (qv).  That such a thing as this
*       is needed is sad indeed.

        INTEGER RECL_JARG

        LOGICAL Open_success

        IF (FORM_CARG.EQ.'UNFORMATTED') THEN !Convert bytes to longwords:
          RECL_JARG=(RECL_IARG+3)/4
        ELSE !Stick with bytes:
          RECL_JARG=RECL_IARG
        END IF

        Open_success=.FALSE.

        IF ( RECL_FLAG .AND. BLOCKSIZE_FLAG .AND. MAXREC_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND. INITIALSIZE_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN !All extras:

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( BLOCKSIZE_FLAG .AND. MAXREC_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND. INITIALSIZE_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN !All extras but RECL:

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. MAXREC_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND. INITIALSIZE_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN !All extras but BLOCKSIZE:

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. BLOCKSIZE_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND. INITIALSIZE_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN !All extras but MAXREC:

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. BLOCKSIZE_FLAG .AND. MAXREC_FLAG .AND.
     1       INITIALSIZE_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN !All extras but CARRIAGECONTROL:

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. BLOCKSIZE_FLAG .AND. MAXREC_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN !All extras but INITIALSIZE:

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. BLOCKSIZE_FLAG .AND. MAXREC_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND. INITIALSIZE_FLAG
     2          ) THEN !All extras but RECORDTYPE:

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF (  MAXREC_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND. INITIALSIZE_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN !All extras but RECL & BLOCKSIZE:

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( BLOCKSIZE_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND. INITIALSIZE_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN !All extras but RECL & MAXREC:

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( BLOCKSIZE_FLAG .AND. MAXREC_FLAG .AND.
     1       INITIALSIZE_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN !All extras but RECL & C.C.

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( BLOCKSIZE_FLAG .AND. MAXREC_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN !All extras but RECL & INIT.

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( BLOCKSIZE_FLAG .AND. MAXREC_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND. INITIALSIZE_FLAG
     2       ) THEN !All extras but RECL & RECORDTYPE:

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND. INITIALSIZE_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN !All extras but BLOCKSIZE & MAXREC:

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. MAXREC_FLAG .AND.
     1       INITIALSIZE_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN !All extras but BLOCKSIZE & C.C.:

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. MAXREC_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN !All extras but BLOCKSIZE & INIT.:

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. MAXREC_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND. INITIALSIZE_FLAG
     2       ) THEN !All extras but BLOCKSIZE & RECORDTYPE:

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. BLOCKSIZE_FLAG .AND.
     1       INITIALSIZE_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN !All extras but MAXREC & C.C.:

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. BLOCKSIZE_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN !All extras but MAXREC & INIT:

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. BLOCKSIZE_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND. INITIALSIZE_FLAG
     2       ) THEN !All extras but MAXREC & RECORDTYPE:

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. BLOCKSIZE_FLAG .AND. MAXREC_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN !All extras but C.C. & INIT:

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. BLOCKSIZE_FLAG .AND. MAXREC_FLAG .AND.
     1       INITIALSIZE_FLAG
     2       ) THEN !All extras but C.C. & RECORDTYPE:

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. BLOCKSIZE_FLAG .AND. MAXREC_FLAG .AND.
     1       CARRIAGECONTROL_FLAG
     2       ) THEN !All extras but INITIALSIZE & RECORDTYPE:

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. BLOCKSIZE_FLAG .AND. MAXREC_FLAG
     1          ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. BLOCKSIZE_FLAG .AND.
     1       CARRIAGECONTROL_FLAG
     2          ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. BLOCKSIZE_FLAG
     1       .AND. INITIALSIZE_FLAG
     1          ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. BLOCKSIZE_FLAG .AND.
     1       RECORDTYPE_FLAG
     2          ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. MAXREC_FLAG .AND.
     1       CARRIAGECONTROL_FLAG
     2          ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. MAXREC_FLAG .AND.
     1       INITIALSIZE_FLAG
     2          ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. MAXREC_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND. INITIALSIZE_FLAG
     2          ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND.
     1       INITIALSIZE_FLAG .AND. RECORDTYPE_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( BLOCKSIZE_FLAG .AND. MAXREC_FLAG .AND.
     1       CARRIAGECONTROL_FLAG
     2          ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( BLOCKSIZE_FLAG .AND. MAXREC_FLAG .AND.
     1       INITIALSIZE_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( BLOCKSIZE_FLAG .AND. MAXREC_FLAG .AND.
     1       RECORDTYPE_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( BLOCKSIZE_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND. INITIALSIZE_FLAG
     2          ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( BLOCKSIZE_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( BLOCKSIZE_FLAG .AND.
     1       INITIALSIZE_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( MAXREC_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND. INITIALSIZE_FLAG
     1          ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( MAXREC_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( MAXREC_FLAG .AND.
     1       INITIALSIZE_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF (
     1       CARRIAGECONTROL_FLAG .AND. INITIALSIZE_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. BLOCKSIZE_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. MAXREC_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. CARRIAGECONTROL_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. INITIALSIZE_FLAG )THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG .AND. RECORDTYPE_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     8        ,ERR=1)
          END IF

        ELSE IF ( BLOCKSIZE_FLAG .AND. MAXREC_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( BLOCKSIZE_FLAG .AND. CARRIAGECONTROL_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( BLOCKSIZE_FLAG .AND. INITIALSIZE_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( BLOCKSIZE_FLAG .AND. RECORDTYPE_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( MAXREC_FLAG .AND. CARRIAGECONTROL_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( MAXREC_FLAG .AND. INITIALSIZE_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( MAXREC_FLAG .AND. RECORDTYPE_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( CARRIAGECONTROL_FLAG .AND. INITIALSIZE_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( CARRIAGECONTROL_FLAG .AND. RECORDTYPE_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     8        ,ERR=1)
          END IF

        ELSE IF ( INITIALSIZE_FLAG .AND. RECORDTYPE_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECL_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     8        ,ERR=1)
          END IF

        ELSE IF ( BLOCKSIZE_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     6        ,BLOCKSIZE=BLOCKSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( MAXREC_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     7        ,MAXREC=MAXREC_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( CARRIAGECONTROL_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     3        ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     8        ,ERR=1)
          END IF

        ELSE IF ( INITIALSIZE_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     5        ,INITIALSIZE=INITIALSIZE_IARG
     8        ,ERR=1)
          END IF

        ELSE IF ( RECORDTYPE_FLAG ) THEN

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     4        ,RECORDTYPE=RECORDTYPE_CARG
     8        ,ERR=1)
          END IF

        ELSE !None of the "extras" are specified:

          IF (READONLY_FLAG) THEN
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,READONLY
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     8        ,ERR=1)
          ELSE
            OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     8        ,ERR=1)
          END IF

        END IF

        Open_success=.TRUE.
1       CONTINUE

*       write(6,301) Open_success,RECL_JARG,INITIALSIZE_IARG
*     1             ,READONLY_FLAG
*301    format(' STROPEN_NAT(VMS)-D1  Native OPEN, success:'L1
*     1        '  RECL='I11' INIT='I11/
*     1        '            READONLY:'L1)

        STROPEN_NAT=Open_success

        RETURN
        END





        SUBROUTINE STRTIME(HOURS,MINS,SECS)
        IMPLICIT NONE
        INTEGER HOURS,MINS,SECS
        BYTE BTIME(8)
        INTEGER I4
        BYTE I1(4)
        EQUIVALENCE(I1,I4)
        CHARACTER*8 C8
        INTEGER H,M,S,I

        CALL TIME(BTIME) !Get the 8-byte ASCII time.
*       BTIME has this:  hh:mm:ss
*       Convert to character:
        DO I=1,8
          I4=0
          I1(1)=BTIME(I)
          C8(I:I)=CHAR(I4)
        END DO

        HOURS=0
        MINS=0
        SECS=0
        READ(C8,101,ERR=1) H,M,S
101     FORMAT(I2,1X,I2,1X,I2)
        HOURS=H
        MINS=M
        SECS=S
1       CONTINUE
        RETURN
        END





        SUBROUTINE STR_FLOAT_IEEE_TO_HOST( Ireal )

        IMPLICIT NONE

        INTEGER Ireal !Integer-cast of a IEEE-style floating number.

*  Description:

*       Convert Ireal, which is passed to this routine as an integer-
*       casting of an IEEE-style floating number, into a VAX
*       floating number (see STR_FLOAT_HOST_TO_IEEE for more details).

        INTEGER Ieee, Ivax

        INTEGER I4
        INTEGER*2 I2(2), J2
        EQUIVALENCE ( I4, I2 )

        LOGICAL SIGN
        INTEGER Ifraction

        Ieee = Ireal !Work with a copy.

        IF (IAND( Ieee, '80000000'X ) .NE. 0 ) THEN !Sign bit is set.
          SIGN   = .TRUE.
          Ieee = IAND( Ieee, '7FFFFFFF'X ) !Clear the sign bit.
        ELSE
          SIGN   = .FALSE.
        END IF

        IF      (Ieee.GE.'7F000000'X) THEN !Infinities become VAX "NAN".
          Ivax='80000000'X                 !(NAN ==> Not A Number)
          SIGN=.FALSE.                     !No sign information.

        ELSE IF (Ieee.GE.'00800000'X) THEN !"Normal" range.
          Ivax=Ieee+'01000000'X            !Add 2 to the exponent.

        ELSE IF (Ieee.GE.'00400000'X) THEN !Given Ireal has zero exponent,
                                           !but with full precision.
          Ifraction=ISHFT( Ieee, 1 )       !Shift the fraction left 1 bit.
          Ifraction=IAND( Ifraction, '007FFFFF'X) !Lose the high bit of the
                                            !fraction, which had been shifted
                                            !into the exponent.
          Ivax=Ifraction+'01000000'X        !The exponent is 2.

        ELSE IF (Ieee.GE.'00200000'X) THEN  !Given Ireal has zero exponent,
                                            !and suffers a loss of precision.
          Ivax=ISHFT( Ieee, 2 )             !Shift the fraction left 2 bits.
                                            !The hob becomes an exponent of 1.

        ELSE                                !Everything else is a VAX zero:
          Ireal=0.
          RETURN                            !No more work needed here.

        END IF

        IF (SIGN) THEN !Set the sign bit.
          Ivax = IOR( Ivax, '80000000'X )
        END IF

*       Swap the halves on Ivax:

        I4=Ivax
        J2=I2(2)
        I2(2)=I2(1)
        I2(1)=J2

        Ireal=I4

        RETURN
        END

        SUBROUTINE STR_FLOAT_HOST_TO_IEEE( Ireal )

        IMPLICIT NONE

        INTEGER Ireal !Integer-cast of a DEC-style floating number.

*  Description:

*       Convert Ireal, which is passed to this routine as an integer
*       cast of a DEC-style floating number, into an IEEE-style floating
*       number:
*
*                              DEC-style
*
*       _31_____________  16_15_________14___________7_6______________0_
*       |fraction (F2) ...  | sign (S) | exponent (E) | fraction (F1)  |
*       |-------------------|----------|--------------|----------------|
*                        LSB                          MSB
*
*       With the usual:  x = (+/-) f * 2**k
*       "+/-" is "+" if S (bit 15) is 0, and "-" if not.
*       "f" is always positive, and:  1 .GT. f   1/2  .LE. f
*       The leading bit of "f" is always omitted (the "hidden" bit).
*
*                  F1(shifted up 16 bits) + F2(shifted down 16 bits) + 2**23
*       "f"  =   ------------------------------------------------------------
*                                          2**24
*
*       Note that the MSB of "f" is bit 6, from F1.
*
*       "k" is:   E(shifted down 7 bits)
*
*       Note that bit 14 (in E) is the sign bit of E.  For E=0, S=0, "x" is
*       defined to be zero.  For E=0, S=1, "x" is a reserved operand - illegal.

        INTEGER Ieee, Ivax

        INTEGER I4
        INTEGER*2 I2(2), J2
        EQUIVALENCE ( I4, I2 )

        LOGICAL SIGN
        INTEGER Ifraction

*       Swap the halves on Ireal:

        I4=Ireal
        J2=I2(2)
        I2(2)=I2(1)
        I2(1)=J2

        Ivax=I4


        IF (IAND( Ivax, '80000000'X ) .NE. 0 ) THEN !Sign bit is set.
          IF (Ivax .EQ. '80000000'X) THEN !VAX NAN.
            Ireal = '7FFFFFFF'X !Make an IEEE NAN & it's done.
            RETURN
          END IF
          SIGN   = .TRUE.
          Ivax = IAND( Ivax, '7FFFFFFF'X ) !Clear the sign bit.
        ELSE
          SIGN   = .FALSE.
        END IF

        IF      (Ivax.GE.'01800000'X) THEN !"Normal" range.
          Ieee=Ivax-'01000000'X            !Subtract 2 from the exponent.

        ELSE IF (Ivax.GE.'01000000'X) THEN !Given Ivax has exponent 2;
                                           !Ieee will have exponent 0,
                                           !with full precision.
          Ifraction=IAND( Ivax, '007FFFFF'X) !Lose the exponent.
          Ifraction=IOR( Ifraction, '00800000'X) !Set the hidden bit.
          Ieee=ISHFT( Ifraction, -1 )      !Shift the fraction right 1 bit.

        ELSE IF (Ivax.GE.'00800000'X) THEN !Given Ivax has exponent 1;
                                           !Ieee will have exponent 0,
                                           !with loss of precision.
          Ifraction=IAND( Ivax, '007FFFFF'X) !Lose the exponent.
          Ifraction=IOR( Ifraction, '00800000'X) !Set the hidden bit.
          Ieee=ISHFT( Ifraction, -2 )      !Shift the fraction right 2 bits.

        ELSE                               !Everything else is a VAX zero:
          Ireal=0.                         !Which forces Ieee to zero (set Ireal).
          RETURN                           !No more work needed here.

        END IF

        IF (SIGN) THEN !Set the sign bit -- this may make a VAX NAN.
          Ieee = IOR( Ieee, '80000000'X )
        END IF

*       Just to be safe (ie, has this programmer missed something?),
*       do another NAN-check etc:
        IF (Ieee.EQ.'80000000'X) THEN !This is a VAX NAN:
          Ieee = '7F800000'X !Make an IEEE NAN & it's done.
        END IF

        Ireal = Ieee !Return the IEEE conversion.

        RETURN
        END

        SUBROUTINE STR_FLOAT_VAX_TO_HOST( Ireal )

        IMPLICIT NONE

        INTEGER Ireal !Integer-cast of a DEC-style floating number.

*  Description:
*       No-op under VMS!

*       Convert Ireal, which is passed to this routine as an integer
*       cast of a DEC-style floating number, into a Host-style floating
*       number:
*
*       _31_____________  16_15_________14___________7_6______________0_
*       |fraction (F2) ...  | sign (S) | exponent (E) | fraction (F1)  |
*       |-------------------|----------|--------------|----------------|
*                        LSB                          MSB
*
*       With the usual:  x = (+/-) f * 2**k
*       "+/-" is "+" is S (bit 15) is 0, and "-" if not.
*       "f" is always positive, and:  1 .GT. f   1/2  .LE. f
*       The leading bit of "f" is always omitted (the "hidden" bit).
*
*                  F1(shifted up 16 bits) + F2(shifted down 16 bits) + 2**23
*       "f"  =   ------------------------------------------------------------
*                                          2**24
*
*       Note that the MSB of "f" is bit 6, from F1.
*
*       "k" is:   E(shifted down 7 bits)
*
*       Note that bit 14 (in E) is the sign bit of E.  For E=0, S=0, "x" is
*       defined to be zero.  For E=0, S=1, "x" is a reserved operand - illegal.


        RETURN !No-op under VMS.
        END

        SUBROUTINE STR_FLOAT_HOST_TO_VAX( Ireal )

        IMPLICIT NONE

        INTEGER Ireal !Integer-cast of a Host-style floating number.

*  Description:
*       No-op under VMS!

*       Convert Ireal, which is passed to this routine as an integer
*       cast of a Host-style floating number, into a DEC-style floating
*       number (see STR_FLOAT_DEC_TO_HOST for more details).

        RETURN !No-op under VMS.
        END

        SUBROUTINE STR_Sleep( Seconds )

        IMPLICIT NONE

*  Input:
        REAL Seconds !Seconds to go to sleep, then wake up.

*  Description:
*       Platform-independent call to interface platform-dependent
*       system call to wait for the specified interval in seconds
*       and then wake up and continue, exiting this rouine
*       normally.  Note that on some systems (not VMS) the time-
*       interval has one-second granuality (ie, seconds is taken
*       as an integer), and the minimum time to wait on such
*       systems is requested to be 1 for "Seconds" > 0.
*       Furthermore, on those systems which take integer-valued
*       "Seconds", because of the granularity, the actual
*       interval of sleep may be less than one second, right
*       down to essentially zero.

        REAL Local_Seconds

        Local_Seconds = Seconds !Some meager protection.

        CALL LIB$WAIT( Local_Seconds )

        RETURN
        END

        SUBROUTINE STR_Time1970( Seconds_Since_1970 )

        IMPLICIT NONE

*  Output:
        INTEGER Seconds_Since_1970
*  Description:
*       Get the number of seconds since 1-Jan-1970, 00:00:00 GMT of the current local time.

        INTEGER Hour, Min, Sec
        INTEGER Year, Month, Day

*       The VMS version (this) is a little kludgey, but it works.


        CALL STRTIME( Hour, Min, Sec )
        CALL STRDATE( Year, Month, Day )

        CALL STR_MakeTime1970( Year, Month, Day, Hour, Min, Sec, Seconds_Since_1970 )

        RETURN
        END


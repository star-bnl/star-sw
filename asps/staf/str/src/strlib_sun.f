

*       SUN versions.

        SUBROUTINE STRCPU(TCPU)
        INTEGER TCPU
        INTEGER STRCPUUSER
        INTEGER NATCPU_T0
        COMMON/NATCPU/NATCPU_T0


        TCPU=STRCPUUSER()-NATCPU_T0

        RETURN
        END

        SUBROUTINE STRCPUQ(TCPU) !Quad (64-bit) version.
        INTEGER TCPU(2)
        INTEGER STRCPUUSER
        INTEGER NATCPU_T0
        COMMON/NATCPU/NATCPU_T0


        TCPU(1)=STRCPUUSER()-NATCPU_T0
        TCPU(2)=0

        RETURN
        END

        SUBROUTINE STRCPU0
*       "Initialize" CPU elapsed time counter.
        IMPLICIT NONE
        INTEGER NATCPU_T0
        COMMON/NATCPU/NATCPU_T0
        INTEGER STRCPUUSER
        NATCPU_T0=STRCPUUSER()
        RETURN
        END

*       This is now replaced with a C routine which inquires the system
*       with the sysconf call.
*       INTEGER FUNCTION STRCPUTPS()
*
*       IMPLICIT NONE
*
**  Return value:
**      Native cpu clock ticks-per-second.
*
*       STRCPUTPS=60 !On SUN, it's 60 per second (for "times", via str's "mclock").
*
*       RETURN
*       END

        SUBROUTINE STRDate( Year, Month, Day )

        IMPLICIT NONE

*  Outputs:
        INTEGER Year, Month, Day !Year is 4-digit, eg, 1991.

*  Brief Description: Handles a call to the SGI time routine.

*  Description:
*       Return the integer date.

        INTEGER Itime, Days, Hour, Secs
        INTEGER Leap_Cycle

        CHARACTER*4 Zone_Name
        INTEGER     Zone

        INTEGER Month_Days(12)

        INTEGER TIME !System routine.

*       Table of months' days:
        DATA Month_Days / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
*                        Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
*                        Note:  Feb's entry is overwritten as needed (not constant)!


        CALL STRTime_Get_Current_Zone( Zone, Zone_Name ) !Get it as set by application or from system.

*       "TIME" gives seconds since 01-Jan-1970, midnight.
        Itime = TIME()

        Days  = Itime / 86400
        Secs  = Itime - Days * 86400
        Hour  = Secs  / 3600

        Hour = Hour - Zone !Convert UT to the current time zone.
        IF ( Hour .LT. 0 ) THEN
          Hour = Hour + 24
          Days = Days -  1
        ELSE IF ( Hour .GE. 24 ) THEN
          Hour = Hour - 24
          Days = Days +  1
        END IF

*       Assume 1-JAN-1970 is Day Zero.
        Year       = 1970
        Leap_Cycle = 2
        DO WHILE ( Days .GE. 366 )
          Days = Days - 365
          Year = Year + 1
          Leap_Cycle = Leap_Cycle+1
*         Subtract one from days on year after leap:
          IF ( Leap_Cycle .EQ. 1 ) THEN !Year after leap.
            Days = Days - 1
          END IF
*         Recycle back to zero every 4 years:
          IF ( Leap_Cycle .EQ. 4 ) THEN !Leap.
            Leap_Cycle = 0
          END IF
        END DO

        IF ( Leap_Cycle .EQ. 0 ) THEN !Leap year:
          Month_Days(2) = 29
        ELSE !Non-leap year:
          Month_Days(2) = 28
          IF ( Days .EQ. 365 ) THEN !Last day plus one of non-leap year:
            Days = 0 !Make it 1st day of next year.
            Year = Year + 1 !Next year.
          END IF
        END IF

        Month = 1
        DO WHILE ( ( Days .GE. Month_Days(Month) ) .AND. (Month.LE.12) )
          Days  = Days - Month_Days(Month)
          Month = Month + 1
        END DO
        Day = Days + 1

        RETURN
        END





        SUBROUTINE STRDEC_ENDIAN(I32)

        IMPLICIT NONE

*  Input/Output:
        INTEGER I32

*  Compatibility routine.  See STRDEC_ENDIAN_HALF.

        CALL STRDEC_ENDIAN_HALF(I32)

        RETURN
        END

        SUBROUTINE STRDEC_ENDIAN_BYTE(I32)

        IMPLICIT NONE

*  Input/Output:
        INTEGER I32

*  Description:
*       Swap the 4 8-bit bytes in the 32-bit (long) word I32,
*       if needed, to make the big/little endian business come out
*       DEC-style.  On SUN (here), the swapping is done.

        INTEGER I4
        BYTE I1(4),I1SAVE
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

        SUBROUTINE STRDEC_ENDIAN_BYTES(Nwords,Block)

        IMPLICIT NONE

*  Input:
        INTEGER Nwords   !Number of 32-bit words in Block to reorder.

*  Input/Output:
        INTEGER Block(*) !Block of 32-bit words in to be reordered.

*  Description:
*       Swap the 4 8-bit bytes in the 32-bit (long) word I32,
*       if needed, to make the big/little endian business come out
*       DEC-style.  On SUN (here), the swapping is done.

        INTEGER Iword

        INTEGER I4

        BYTE I1(4),I1SAVE
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

        SUBROUTINE STRDEC_ENDIAN_HALF(I32)

        IMPLICIT NONE

*  Input/Output:
        INTEGER I32

*  Description:
*       Swap the two 16-bit half-words in the 32-bit (long) word I32,
*       if needed, to make the big/little endian business come out
*       DEC-style.  On SUN (here), the swapping is done.

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

        INTEGER FUNCTION STRDEC_IBITS(DATA,BIT0,BITS)

*       Do an IBITS, but make it act like DEC.

        IMPLICIT NONE

        INTEGER DATA,BIT0,BITS

        STRDEC_IBITS=IBITS(DATA,31-BIT0,BITS) !This is SUN - reversed.

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
*       Intended for use on ASCII-type text files, as written by FORTRAN
*       formatted-writes.

        CALL FLUSH( LUN )

        RETURN

        END

        SUBROUTINE STRMOVE(COUNT,SOURCE,DEST)

*       SUN interface routine to (equivalent) to LIB$MOVC3.
*       COUNT is a 32-bit word-count, not a byte-count.

        IMPLICIT NONE

        INTEGER COUNT
        INTEGER SOURCE(*),DEST(*)

        INTEGER I

        DO I=1,COUNT
          DEST(I)=SOURCE(I)
        END DO

        RETURN
        END
*
        SUBROUTINE STRMSEC(MSECS)
*       Standard interface routine to return milliseconds since midnight.
        IMPLICIT NONE
        INTEGER MSECS
        INTEGER HOUR,MIN,SEC
        CALL STRTIME(HOUR,MIN,SEC)
        SEC=SEC+60*(MIN+60*HOUR)
        MSECS=1000*SEC !Milliseconds since midnight (only whole secs.).
        RETURN
        END

        LOGICAL FUNCTION STRMSEC_DELAY(MSECS)

*       Standard interface routine to delay the specified time in
*       milliseconds.
*       Returns .TRUE. for success, .FALSE. for failure, such as
*       would occur for an illegally long, or negative time.

        IMPLICIT NONE
        INTEGER MSECS

        INTEGER DAYS,HOURS,MINS,SECS,MILLS,BAL


        STRMSEC_DELAY=.FALSE. !In case of failure, just return false.

        IF (MSECS.LT.0) THEN !Illegal time:
          RETURN
        END IF !MSECS.LT.0

        if (.false.) then !turn this off, temporarily:

        DAYS =INT(MSECS/(24*3600*100)) !DAYS will be no more than 248.
        BAL  =MSECS-DAYS*(24*3600*100)
        HOURS=INT(BAL/(3600*100))
        BAL  =BAL-HOURS*(3600*100)
        MINS =INT(BAL/(60*100))
        BAL  =BAL-MINS*(60*100)
        SECS =INT(BAL/100)
        MILLS=BAL-SECS*100

        else !and temporarily, do this:

          do mills=1,msecs
        DAYS =INT(MSECS/(24*3600*100)) !DAYS will be no more than 248.
        BAL  =MSECS-DAYS*(24*3600*100)
        HOURS=INT(BAL/(3600*100))
        BAL  =BAL-HOURS*(3600*100)
        MINS =INT(BAL/(60*100))
        BAL  =BAL-MINS*(60*100)
        SECS =INT(BAL/100)
          end do

        end if

        STRMSEC_DELAY=.TRUE.

        RETURN
        END

        LOGICAL FUNCTION STROPEN_NAT(LUN,FILENAME
     1   ,STATUS_CARG,ACCESS_CARG,FORM_CARG
     1   ,RECL_FLAG,RECL_IARG
     1   ,CARRIAGECONTROL_FLAG,CARRIAGECONTROL_CARG
     1   ,RECORDTYPE_FLAG,RECORDTYPE_CARG
     1   ,INITIALSIZE_FLAG,INITIALSIZE_IARG
     1   ,BLOCKSIZE_FLAG,BLOCKSIZE_IARG
     1   ,MAXREC_FLAG,MAXREC_IARG
     1   ,READONLY_FLAG
     1   )

        IMPLICIT NONE

*  Inputs:
        INTEGER LUN !Logical unit on which to open file.
        CHARACTER*(*) FILENAME !Name of file.
        CHARACTER*(*) STATUS_CARG !"STATUS=" character argument
        CHARACTER*(*) ACCESS_CARG !"ACCESS=" character argument
        CHARACTER*(*) FORM_CARG !"FORM=" character argument
        LOGICAL RECL_FLAG !RECL specified/not flag.
        INTEGER RECL_IARG !"RECL=" integer argument;  Record-length, bytes.
        LOGICAL CARRIAGECONTROL_FLAG !CARRIAGECONTROL specified/not flag.
        CHARACTER*(*) CARRIAGECONTROL_CARG !"CARRIAGECONTROL=" char. argument
        LOGICAL RECORDTYPE_FLAG !RECORDTYPE specified/not flag.
        CHARACTER*(*) RECORDTYPE_CARG !"RECORDTYPE=" character argument.
        LOGICAL INITIALSIZE_FLAG !INITIALSIZE specified/not flag.
        INTEGER INITIALSIZE_IARG !"INITIALSIZE=" integer argument -- not used.
        LOGICAL BLOCKSIZE_FLAG !BLOCKSIZE specified/not flag -- not used.
        INTEGER BLOCKSIZE_IARG !"BLOCKSIZE=" integer argument -- not used.
        LOGICAL MAXREC_FLAG !MAXREC specified/not flag.
        INTEGER MAXREC_IARG !"MAXREC=" integer argument.
        LOGICAL READONLY_FLAG !No-op on SUN.

*  Description:
*       Provides a machine-dependent (native) OPEN routine, intended
*       to be called only from STROPEN (qv).

*  Note:
*       The SG FORTRAN manual seems to indicate that the RECORDTYPE specifier
*       is completely useless, ie, "If ... specified, ... must be the
*       appropriate default value...".  In other words, specifying it
*       may only lead to trouble, if the "appropriate default" value isn't
*       used.  This routine now checks this, and if it's not set to VARIABLE,
*       with FORM set to UNFORMATTED, then ACCESS is set to DIRECT;
*       RECORDTYPE itself is not set.

        INTEGER RECL_JARG

        CHARACTER*32 ACCESS_SARG !"ACCESS=" character argument.
        INTEGER logic_block
        CHARACTER*80 Local_filename

        LOGICAL Open_success

        ACCESS_SARG=ACCESS_CARG !Local copy for substitution.
        Local_filename=FILENAME !Local copy for substitution.

        IF (.NOT.RECL_FLAG) THEN !No RECL specified:
        ELSE IF (FORM_CARG.EQ.'UNFORMATTED') THEN !Convert bytes to longwords:
          RECL_JARG=(RECL_IARG+3)/4
          IF (RECORDTYPE_FLAG.AND.(RECORDTYPE_CARG.NE.'VARIABLE')) THEN
*           Work around an SUN bug:
            ACCESS_SARG='DIRECT'
          END IF
        ELSE !Stick with bytes:
          RECL_JARG=RECL_IARG
        END IF

        Open_success=.FALSE.

        IF ( RECL_FLAG .AND. MAXREC_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN !All extras:

          logic_block=1
          OPEN(UNIT=LUN,FILE=Local_filename,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_SARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
*     3       ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
*     4       ,RECORDTYPE=RECORDTYPE_CARG
*     7       ,MAXREC=MAXREC_IARG
     8        ,ERR=1)

        ELSE IF ( MAXREC_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN !All extras but RECL:

          logic_block=2
          OPEN(UNIT=LUN,FILE=Local_filename,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_SARG,FORM=FORM_CARG
*     3       ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
*     4       ,RECORDTYPE=RECORDTYPE_CARG
*     7       ,MAXREC=MAXREC_IARG
     8        ,ERR=1)

        ELSE IF ( RECL_FLAG .AND.
     1       CARRIAGECONTROL_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN !All extras but MAXREC:

          logic_block=3
          OPEN(UNIT=LUN,FILE=Local_filename,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_SARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
*     3       ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
*     4       ,RECORDTYPE=RECORDTYPE_CARG
     8        ,ERR=1)

        ELSE IF ( RECL_FLAG .AND. MAXREC_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN !All extras but CARRIAGECONTROL:

          logic_block=4
          OPEN(UNIT=LUN,FILE=Local_filename,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_SARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
*     4       ,RECORDTYPE=RECORDTYPE_CARG
*     7       ,MAXREC=MAXREC_IARG
     8        ,ERR=1)

        ELSE IF ( RECL_FLAG .AND. MAXREC_FLAG .AND.
     1       CARRIAGECONTROL_FLAG
     2          ) THEN !All extras but RECORDTYPE:

          logic_block=5
          OPEN(UNIT=LUN,FILE=Local_filename,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_SARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
*     3       ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
*     7       ,MAXREC=MAXREC_IARG
     8        ,ERR=1)

        ELSE IF ( RECL_FLAG .AND. MAXREC_FLAG
     2       ) THEN !All extras but C.C. & RECORDTYPE:

          logic_block=6
          OPEN(UNIT=LUN,FILE=Local_filename,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_SARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
*     7       ,MAXREC=MAXREC_IARG
     8        ,ERR=1)

        ELSE IF ( RECL_FLAG .AND.
     1       CARRIAGECONTROL_FLAG
     2          ) THEN

          logic_block=7
          OPEN(UNIT=LUN,FILE=Local_filename,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_SARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
*     3       ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     8        ,ERR=1)

        ELSE IF ( RECL_FLAG .AND.
     1       RECORDTYPE_FLAG ) THEN

          logic_block=8
          OPEN(UNIT=LUN,FILE=Local_filename,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_SARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
*     4       ,RECORDTYPE=RECORDTYPE_CARG
     8        ,ERR=1)

        ELSE IF ( MAXREC_FLAG .AND.
     1       CARRIAGECONTROL_FLAG
     2       ) THEN

          logic_block=9
          OPEN(UNIT=LUN,FILE=Local_filename,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_SARG,FORM=FORM_CARG
*     3       ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
*     7       ,MAXREC=MAXREC_IARG
     8        ,ERR=1)

        ELSE IF ( MAXREC_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN

          logic_block=10
          OPEN(UNIT=LUN,FILE=Local_filename,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_SARG,FORM=FORM_CARG
*     4       ,RECORDTYPE=RECORDTYPE_CARG
*     7       ,MAXREC=MAXREC_IARG
     8        ,ERR=1)

        ELSE IF (
     1       CARRIAGECONTROL_FLAG .AND.
     2       RECORDTYPE_FLAG ) THEN

          logic_block=11
          OPEN(UNIT=LUN,FILE=Local_filename,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_SARG,FORM=FORM_CARG
*     3       ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
*     4       ,RECORDTYPE=RECORDTYPE_CARG
     8        ,ERR=1)

        ELSE IF ( RECL_FLAG )THEN

          logic_block=12
          OPEN(UNIT=LUN,FILE=Local_filename,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_SARG,FORM=FORM_CARG
     2        ,RECL=RECL_JARG
     8        ,ERR=1)

        ELSE IF ( MAXREC_FLAG ) THEN

          logic_block=13
          OPEN(UNIT=LUN,FILE=Local_filename,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_SARG,FORM=FORM_CARG
*     7       ,MAXREC=MAXREC_IARG
     8        ,ERR=1)

        ELSE IF ( CARRIAGECONTROL_FLAG ) THEN

          logic_block=14
          OPEN(UNIT=LUN,FILE=Local_filename,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_SARG,FORM=FORM_CARG
*     3       ,CARRIAGECONTROL=CARRIAGECONTROL_CARG
     8        ,ERR=1)

        ELSE IF ( RECORDTYPE_FLAG ) THEN

          logic_block=15
          OPEN(UNIT=LUN,FILE=Local_filename,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_SARG,FORM=FORM_CARG
*     4       ,RECORDTYPE=RECORDTYPE_CARG
     8        ,ERR=1)

        ELSE !None of the "extras" are specified:

          logic_block=16
          OPEN(UNIT=LUN,FILE=Local_filename,STATUS=STATUS_CARG
     1        ,ACCESS=ACCESS_SARG,FORM=FORM_CARG
     8        ,ERR=1)

        END IF

        Open_success=.TRUE.
1       CONTINUE

***************************** * *  72  * * *****************************
*       write(6,301) Open_success,lun,logic_block,Local_filename
*     1   ,status_carg,access_carg,form_carg
*     1   ,recl_flag,recl_jarg
*     1   ,carriagecontrol_flag,carriagecontrol_carg
*     1   ,recordtype_flag,recordtype_carg
*
*
*301    format(' STROPEN_NAT(SUN)-D1  Native OPEN, success:'L1' lun:'I3
*     1     ' logic block # 'I3/
*     1     ' file:'/' 'A/' status:'A/' access:'A/' form:'A/
*     1     ' recl flag:'L1' recl:'I11/
*     1     ' car.ctl. flag:'L1'  car.crl:'A/
*     1     ' recordtype? 'L1'  recordtype:'A)
***************************** * *  72  * * *****************************

        STROPEN_NAT=Open_success

        RETURN
        END
*
        SUBROUTINE STRTIME(HOUR,MIN,SEC)

        IMPLICIT NONE

        INTEGER HOUR,MIN,SEC
        INTEGER ITIME,SECS,DAYS

        INTEGER     Zone
        CHARACTER*4 Zone_Name

        INTEGER TIME
        ITIME=TIME() !Seconds since 1-jan-1970.
        DAYS=ITIME/86400
        SECS=ITIME-DAYS*86400
        HOUR=SECS/3600
        MIN=(SECS-HOUR*3600)/60
        SEC=(SECS-HOUR*3600-MIN*60)

        CALL STRTime_Get_Current_Zone( Zone, Zone_Name )

        HOUR = HOUR - Zone !Convert UT to the current time zone.
        IF ( HOUR .LT. 0 ) THEN
          HOUR = HOUR + 24
        ELSE IF ( HOUR .GE. 24 ) THEN
          HOUR = HOUR - 24
        END IF

        RETURN
        END

        SUBROUTINE STR_FLOAT_IEEE_TO_HOST( Ireal )

        IMPLICIT NONE

        INTEGER Ireal !Integer-cast of an IEEE-style floating number.

*  Description:
*       No-op under SUN!

*       Convert Ireal, which is passed to this routine as an integer
*       cast of a Host-style floating number, into a DEC-style floating
*       number (see STR_FLOAT_HOST_TO_IEEE for more details).


        RETURN !No-op under SUN.
        END

        SUBROUTINE STR_FLOAT_HOST_TO_IEEE( Ireal )

        IMPLICIT NONE

        INTEGER Ireal !Integer-cast of a Host-style floating number.

*  Description:
*       No-op under SUN!

*       Convert Ireal, which is passed to this routine as an integer
*       cast of a Host-style floating number, into an IEEE-style floating
*       number:
*                              DEC-style
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


        RETURN !No-op under SUN.
        END

        SUBROUTINE STR_FLOAT_HOST_TO_VAX( Ireal )

        IMPLICIT NONE

        INTEGER Ireal !Integer-cast of a Host-style floating number.

*  Description:

*       This is the IEEE version, which works for any host using the
*       IEEE floating-representation.

*       Convert Ireal, which is passed to this routine as an integer-
*       casting of a Host-style (IEEE) floating number, into a DEC-style
*       floating number (see STR_FLOAT_DEC_TO_HOST for more details).

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

        SUBROUTINE STR_FLOAT_VAX_TO_HOST( Ireal )

        IMPLICIT NONE

        INTEGER Ireal !Integer-cast of a DEC-style floating number.

*  Description:

*       This is the generic version, which works anywhere on 32-bit
*       floating numbers.

*       Convert Ireal, which is passed to this routine as an integer
*       cast of a DEC-style floating number, into a Host-style floating
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

        SUBROUTINE STR_Sleep( Seconds )

        IMPLICIT NONE

*  Input:
        REAL Seconds !Seconds to go to sleep, then wake up.

*  Description:
*       Platform-independent call to interface platform-dependent
*       system call to wait for the specified interval in seconds
*       and then wake up and continue, exiting this rouine
*       normally.  Note that on some systems (SGI, not VMS) the
*       time-interval has one-second granuality (ie, seconds is
*       taken as an integer), and the minimum time to wait on
*       such systems is requested to be 1 for "Seconds" > 0.
*       Furthermore, on those systems which take integer-valued
*       "Seconds", because of the granularity, the actual
*       interval of sleep may be less than one second, right
*       down to essentially zero.

        INTEGER Local_Seconds !On SGI, this needs to be an integer.

        IF ( Seconds .LE. 0 ) THEN !Just return quickly.
          RETURN
        ELSE     !Ensure one sec. if Seconds > 0 is requested.
          Local_Seconds = NINT( Seconds + 0.5 )
        END IF

        CALL sleep( Local_Seconds )

        RETURN
        END

        SUBROUTINE STR_Time1970( Seconds_Since_1970 )
        IMPLICIT NONE
*  Output:
        INTEGER Seconds_Since_1970
*  Description:
*       Get the number of seconds since 1-Jan-1970, 00:00:00 GMT.
        INTEGER TIME
*       Is this right on SUN?
        Seconds_Since_1970 = TIME() !Seconds since 1-jan-1970.
        RETURN
        END


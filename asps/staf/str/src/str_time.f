*       File: str_time.f

*       This is a general package which provides platform-independent
*       real-time and CPU-time utilities.  The code in this file
*       is platform-independent, and works in conjunction with
*       platform-dependent code to make a platform-independent
*       interface to some system utilities.



*
        SUBROUTINE STRASCCPU( TCPU_TIME, DCPU_TIME
     1                      , TCPU_ASC , DCPU_ASC
     1                      ,  ASC_DATE, ASC_WEEKDAY, ASC_TIME )

        IMPLICIT NONE

*  Input/Output:
        INTEGER TCPU_TIME(2) !Total "absolute" CPU time since last
                             !call to STRCPU0.  Input value is used
                             !to get DCPU_TIME.

*  Outputs:
        INTEGER DCPU_TIME(2) !CPU time-difference between input and
                             !output values of TCPU_TIME.
        CHARACTER*17 TCPU_ASC !ASCII representation of TCPU_TIME.
        CHARACTER*17 DCPU_ASC !ASCII representation of DCPU_TIME.
        CHARACTER*9 ASC_DATE  !ASCII representation of real date.
        CHARACTER*9 ASC_WEEKDAY !ASCII representation of weekday.
        CHARACTER*8 ASC_TIME  !ASCII representation of real time.

*  Description:
*       Return absolute (since last STRCPU0 call) and delta CPU
*       times, and 9 character ASCII date, 8 character ASCII time,
*       and 9 character ASCII weekday.

        LOGICAL QUAD_MODE !Switch to control 64-bit arithmetic (on/off).
        PARAMETER (QUAD_MODE=.FALSE.) !Off.
*       PARAMETER (QUAD_MODE=.TRUE.) !On.

        INTEGER CPU_TIME(2)

        CHARACTER*9 CDATE
        CHARACTER*8 CTIME

        INTEGER I,MONTH,DAY,YEAR

        LOGICAL SN,SP


        CALL STRASCDATE(CDATE)
        ASC_DATE=CDATE

        CALL STRASCTIME(CTIME)
        ASC_TIME=CTIME

        CALL STRDATE(YEAR,MONTH,DAY)
        CALL STRWEEKDAY(YEAR,MONTH,DAY,ASC_WEEKDAY)

        CALL STRCPU(CPU_TIME) !Get "absolute" CPU time, since last STRCPU0 call.

        IF (.NOT.QUAD_MODE) THEN
          DCPU_TIME(1)=CPU_TIME(1)-TCPU_TIME(1)
          DCPU_TIME(2)=0
          TCPU_TIME(1)=CPU_TIME(1)
          TCPU_TIME(2)=0
        ELSE

*         Quad-word subtraction:
          IF (CPU_TIME(1).LT.0) THEN
            SP=.TRUE.
            CPU_TIME(1)=IAND(CPU_TIME(1),'7FFFFFFF'X)
          ELSE
            SP=.FALSE.
          END IF
          IF (TCPU_TIME(1).LT.0) THEN
            TCPU_TIME(1)=IAND(TCPU_TIME(1),'7FFFFFFF'X)
            SN=.TRUE.
          ELSE
            SN=.FALSE.
          END IF

          DCPU_TIME(1)=CPU_TIME(1)-TCPU_TIME(1)
          DCPU_TIME(2)=CPU_TIME(2)-TCPU_TIME(2)

          IF (DCPU_TIME(1).LT.0) THEN !Negative result (Clear sign or borrow):
            IF (SP.AND.(.NOT.SN)) THEN !SP only:
              DCPU_TIME(1)=IAND(DCPU_TIME(1),'7FFFFFFF'X) !Clear sign bit.
            ELSE IF (SN.AND.(.NOT.SP)) THEN !SN only:
              DCPU_TIME(1)=IAND(DCPU_TIME(1),'7FFFFFFF'X) !Clear sign bit.
              DCPU_TIME(2)=DCPU_TIME(2)-1 !And borrow.
            ELSE !Neither or both:
              DCPU_TIME(2)=DCPU_TIME(2)-1 !Borrow.
            END IF
          ELSE !Non-negative result:
            IF (SP.AND.(.NOT.SN)) THEN !SP only:
              DCPU_TIME(1)=IOR(DCPU_TIME(1),'80000000'X) !Set sign bit.
            ELSE IF (SN.AND.(.NOT.SP)) THEN !SN only:
              DCPU_TIME(1)=IOR(DCPU_TIME(1),'80000000'X) !Set sign bit.
              DCPU_TIME(2)=DCPU_TIME(2)-1 !And borrow.
            ELSE !Neither or both (do nothing):
            END IF
          END IF

          TCPU_TIME(1)=CPU_TIME(1)
          TCPU_TIME(2)=CPU_TIME(2)

        END IF !.NOT.QUAD_MODE


        CALL STRCPUCNV(TCPU_TIME,TCPU_ASC)
        CALL STRCPUCNV(DCPU_TIME,DCPU_ASC)

        RETURN
        END
*
        SUBROUTINE STRASCDATE(C9)

        IMPLICIT NONE

*  Output:
        CHARACTER*9 C9 !9-character ASCII date:  dd-mmm-yy

*  Description:
*       Return 9-character ASCII date.

        INTEGER YEAR,MONTH,DAY,IM
        CHARACTER*3 CM(13)
        DATA CM/'Jan','Feb','Mar','Apr','May','Jun'
     1         ,'Jul','Aug','Sep','Oct','Nov','Dec'
     2         ,'Err'/
        CALL STRDATE(YEAR,MONTH,DAY)
        IM=MONTH
        IF (IM.LE.0) IM=13
        IF (IM.GT.12) IM=13
        IF (DAY.LE.0) IM=13
        IF (DAY.GT.31) IM=13
        WRITE(C9,101) DAY,CM(IM),YEAR-1900
101     FORMAT(I2.2'-'A3'-'I2)
        RETURN
        END
*
        SUBROUTINE STRASCDateTime( TIM_ASC )

        IMPLICIT NONE

*  Output:
        CHARACTER*23 TIM_ASC !Standard time & date: dd-mmm-yy hh:mm:ss zone
*                             (Last four characters are time-zone.)

*  Description:
*       Return standard 23 character ASCII years, months, days,
*       hours, minutes and seconds.

        INTEGER     Zone
        CHARACTER*4 Zone_Name

        CHARACTER*3 TIM_AMON(12)
        DATA TIM_AMON/'Jan','Feb','Mar','Apr','May','Jun'
     1               ,'Jul','Aug','Sep','Oct','Nov','Dec'/

        INTEGER YEAR,MONTH,DAY,HOURS,MINS,SECS

*       Get the standard date and time:
        CALL STRDATE(YEAR,MONTH,DAY)
        IF (YEAR.GT.1900) YEAR=YEAR-1900 !Subtract off the upper 2 digits.
        CALL STRTIME(HOURS,MINS,SECS) !Time comes back in current zone.

        TIM_ASC='No date or time'
        IF (MONTH.GT.12) RETURN
        IF (MONTH.LE.0) RETURN

        CALL STRTime_Get_Current_Zone( Zone, Zone_Name )

        WRITE(TIM_ASC,101)
     1     DAY,TIM_AMON(MONTH),YEAR
     2    ,HOURS,MINS,SECS, Zone_Name
101     FORMAT(I2'-'A3'-'I2' 'I2.2':'I2.2':'I2.2' 'A4)

        RETURN
        END
*
        SUBROUTINE STRASCELA(TELA_TIME,DELA_TIME,TELA_ASC,DELA_ASC,
     1                       ASC_DATE,ASC_WEEKDAY,ASC_TIME)

        IMPLICIT NONE

*  Input/Output:
        INTEGER TELA_TIME(2) !Total "absolute" elapsed time since last
                             !call to STRELA0.  Input value is used
                             !to get DELA_TIME.

*  Outputs:
        INTEGER DELA_TIME(2) !ELA time-difference between input and
                             !output values of TELA_TIME.
        CHARACTER*17 TELA_ASC !ASCII representation of TELA_TIME.
        CHARACTER*17 DELA_ASC !ASCII representation of DELA_TIME.
        CHARACTER*9 ASC_DATE  !ASCII representation of real date.
        CHARACTER*9 ASC_WEEKDAY !ASCII representation of weekday.
        CHARACTER*8 ASC_TIME  !ASCII representation of real time.

*  Description:
*       Return absolute (since last STRELA0 call) and delta ELA
*       times, and 9 character ASCII date, 8 character ASCII time,
*       and 9 character ASCII weekday.

        INTEGER ELA_TIME(2)

        CHARACTER*9 CDATE
        CHARACTER*8 CTIME

        INTEGER I,MONTH,DAY,YEAR

        LOGICAL SN,SP


        CALL STRASCDATE(CDATE)
        ASC_DATE=CDATE

        CALL STRASCTIME(CTIME)
        ASC_TIME=CTIME

        CALL STRDATE(YEAR,MONTH,DAY)
        CALL STRWEEKDAY(YEAR,MONTH,DAY,ASC_WEEKDAY)

        CALL STRELA(ELA_TIME) !Get "absolute" ELA time, since last STRELA0 call.

        DELA_TIME(1)=ELA_TIME(1)-TELA_TIME(1) !msecs
        DELA_TIME(2)=ELA_TIME(2)-TELA_TIME(2) !days

*       Check for (and fix, if not negative days) negative msecs:
        IF ((DELA_TIME(1).LT.0).AND.(DELA_TIME(2).GT.0)) THEN
          DELA_TIME(2)=DELA_TIME(2)-1 !Subtract off one day.
          DELA_TIME(1)=DELA_TIME(1)+24*60*60*1000 !Add back a day, in msec.
        END IF

*       Update the total elapsed time:
        TELA_TIME(1)=ELA_TIME(1) !msecs
        TELA_TIME(2)=ELA_TIME(2) !days

        CALL STRELACNV(TELA_TIME,TELA_ASC)
        CALL STRELACNV(DELA_TIME,DELA_ASC)

        RETURN
        END
*
        SUBROUTINE STRASCTIME(C8)

        IMPLICIT NONE

*  Output:
        CHARACTER*8 C8 !8-character ASCII time:  hh:mm:ss

*  Description:
*       Return 8-character ASCII time.

        INTEGER HOUR,MIN,SEC
        CALL STRTIME(HOUR,MIN,SEC)
        WRITE(C8,101) HOUR,MIN,SEC
101     FORMAT(I2.2':'I2.2':'I2.2)
        RETURN
        END
*
        SUBROUTINE STRCPUCNV(CPU_TIME,CPU_ASC)

        IMPLICIT NONE

*  Input:
        INTEGER CPU_TIME(2) !CPU clock ticks of some caller-timed-interval.
*  Output:
        CHARACTER*(*) CPU_ASC !ddddd_hh:mm:ss.tt (ASCII cpu time; "_" = blank)

*  Description:
*       Convert specified CPU time to 17 character ASCII days, hours, minutes,
*       seconds and ticks.
*       If the passed string isn't long enough, a truncation is performed, and
*       the last character in the passed string is set to "*".

        LOGICAL QUAD_MODE !Switch to control 64-bit arithmetic (on/off).
        PARAMETER (QUAD_MODE=.FALSE.) !Off.
*       PARAMETER (QUAD_MODE=.TRUE.) !On.

        INTEGER TPS !Ticks per second.
        INTEGER CPU_DAYS,CPU_HOURS,CPU_MINS,CPU_SECS,CPU_TICKS
        INTEGER CPU_BAL
        INTEGER L
        CHARACTER*17 Elapsed_CPU_ASCII

        INTEGER STRCPUTPS

        L=LEN(CPU_ASC)
        IF (L.LE.0) THEN !Intercept bogus passed strings here.
          RETURN
        END IF

        TPS=STRCPUTPS() !Get native cpu clock ticks-per-second.

        CPU_DAYS=CPU_TIME(1)/(TPS*3600*24) !TPS(ticks/sec), 3600 sec/hr, etc.
        CPU_BAL=CPU_TIME(1)-CPU_DAYS*(TPS*3600*24)
        IF (QUAD_MODE) THEN !64-bit arithmetic, if enabled:
          CPU_DAYS=CPU_DAYS+CPU_TIME(2) !VMS relic; valid? Affects long jobs.
        END IF
        CPU_HOURS=CPU_BAL/(TPS*3600)
        CPU_BAL=CPU_BAL-CPU_HOURS*(TPS*3600)
        CPU_MINS=CPU_BAL/(TPS*60)
        CPU_BAL=CPU_BAL-CPU_MINS*(TPS*60)
        CPU_SECS=CPU_BAL/TPS
        CPU_BAL=CPU_BAL-CPU_SECS*(TPS)
*       Map 0-59 to 0-5900 or 0-99 to 0-9900:
        CPU_TICKS=CPU_BAL*100
        CPU_TICKS=CPU_TICKS/TPS !Map 0-5900 to 0-98 or 0-9900 to 0-99.

        WRITE(Elapsed_CPU_ASCII,101)
     1     CPU_DAYS,CPU_HOURS,CPU_MINS,CPU_SECS,CPU_TICKS
101     FORMAT(I5' 'I2.2':'I2.2':'I2.2'.'I2.2)

        CPU_ASC=Elapsed_CPU_ASCII
        IF (L.LT.17) THEN !Indicate a truncation:
          CPU_ASC(L:L)='*'
        END IF

        RETURN
        END
*
        SUBROUTINE STRCPUASC(CPU_ASC)

        IMPLICIT NONE

*  Output:
        CHARACTER*17 CPU_ASC !ddddd_hh:mm:ss.tt (ASCII cpu time; "_" = blank)

*  Description:
*       Return CPU time, since last STRCPU0 call, as a 17 character ASCII days,
*       hours, minutes, seconds and ticks string.

        INTEGER CPU_TIME(2) !CPU clock ticks of some caller-timed-interval.

        CALL STRCPU(CPU_TIME) !Get "absolute" CPU time, since last STRCPU0 call.
        CALL STRCPUCNV(CPU_TIME,CPU_ASC) !Convert to ASCII string.

        RETURN
        END
*
        SUBROUTINE STRCPUTXT(TEXT,LINE_OUT)

        IMPLICIT NONE

*  Input:
        CHARACTER*(*) TEXT !Line of text, truncated if > 80 chars.

*  Output:
        CHARACTER*(*) LINE_OUT !Contains TEXT, appended with CPU info.

*  Description:
*       Return a character string (LINE_OUT) consisting of a line of
*       input text, with CPU time-usage info appended.

        INTEGER TLEN

        CHARACTER*80 H80
        CHARACTER*132 C132

        CHARACTER*9 CDATE
        CHARACTER*8 CTIME

        INTEGER CPU_TIME(2),CPU(5),CPU_TICKS,CPU_SECS,CPU_MINS
        INTEGER CPU_HOURS,CPU_DAYS,CPU_BAL

        CHARACTER*17 CPU_ASC

        CALL STRCPU(CPU_TIME)
        CALL STRCPUCNV(CPU_TIME,CPU_ASC)
        CALL STRASCDATE(CDATE) !Get the 9-byte ASCII date.
        CALL STRASCTIME(CTIME) !Get the 8-byte ASCII time.

        TLEN=MIN(LEN(TEXT),80)
        CALL STRCLEAN(TEXT,TLEN,H80) !Remove any non-printables.

        WRITE(C132,101) H80(:TLEN),CPU_ASC,CDATE,CTIME
101     FORMAT(A' 'A17' 'A9' 'A8)

        LINE_OUT=C132 !Buffer through C132 -- prevent write errors.

        RETURN
        END
*
        SUBROUTINE STRDAYSINCE(YEAR_NEW,MONTH_NEW,DAY_NEW
     1                        ,YEAR_OLD,MONTH_OLD,DAY_OLD,DAYSINCE)

        IMPLICIT NONE

*  Inputs:
        INTEGER YEAR_NEW,MONTH_NEW,DAY_NEW,YEAR_OLD,MONTH_OLD,DAY_OLD

*  Output:
        INTEGER DAYSINCE

*  Description:
*       Return the total days from the "new" date since the "old" date.

        INTEGER I,LEAPS,IYEAR

        INTEGER LENGTH(12)
        DATA LENGTH/31,28,31,30,31,30,31,31,30,31,30,31/


*       Accumulate the total number of days from a fixed reference:
*       (Fixed reference is necessary to get leaps right.)

        IYEAR=YEAR_NEW-1900
        LEAPS=IYEAR/4
        IF (IYEAR.EQ.(LEAPS*4)) THEN !Leap year.
          IF (MONTH_NEW.LE.2) THEN !Jan. & Feb. unaffected in leap years.
            LEAPS=LEAPS-1
          END IF
        END IF
        DAYSINCE=IYEAR*365+LEAPS
        DO I=1,MONTH_NEW-1
          DAYSINCE=DAYSINCE+LENGTH(I)
        END DO
        DAYSINCE=DAYSINCE+DAY_NEW

*       Subtract off the number of days in the old date, from same fixed ref:
        IYEAR=YEAR_OLD-1900
        LEAPS=IYEAR/4
        IF (IYEAR.EQ.(LEAPS*4)) THEN !Leap year.
          IF (MONTH_OLD.LE.2) THEN !Jan. & Feb. unaffected in leap years.
            LEAPS=LEAPS-1
          END IF
        END IF
        DAYSINCE=DAYSINCE-(IYEAR*365+LEAPS)
        DO I=1,MONTH_OLD-1
          DAYSINCE=DAYSINCE-LENGTH(I)
        END DO
        DAYSINCE=DAYSINCE-DAY_OLD

        RETURN
        END
*
        SUBROUTINE STRELA(TELA)

        IMPLICIT NONE

*  Output:
        INTEGER TELA(2) !(1) is msecs, (2) is days.

*  Description:
*       Return the elapsed time, in days and msecs, since STRELA0 was called.

        INTEGER NATELA_T0
        COMMON/NATELA/NATELA_T0(2)

        INTEGER MSECS,YEAR,MONTH,DAY,DAYSINCE

*       Obtain the total elapsed time since counter-initialization:
        CALL STRMSEC(MSECS)
        TELA(1)=MSECS-NATELA_T0(1)
        CALL STRDATE(YEAR,MONTH,DAY)
*       Convert to days since 1-JAN-1900:
        CALL STRDAYSINCE(YEAR,MONTH,DAY,1900,1,1,DAYSINCE)
        TELA(2)=DAYSINCE-NATELA_T0(2)

*       Check for (and fix, if not negative days) negative msecs:
        IF ((TELA(1).LT.0).AND.(TELA(2).GT.0)) THEN
          TELA(2)=TELA(2)-1 !Subtract off one day.
          TELA(1)=TELA(1)+24*60*60*1000 !Add back a day, in msec.
        END IF

        RETURN
        END
*
        SUBROUTINE STRELADEL(TELA, DELA)

        IMPLICIT NONE

*  Input/Output:
        INTEGER TELA(2) !(1) is msecs, (2) is days -- since STRELA0.

*  Output:
        INTEGER DELA(2) !(1) is msecs, (2) is days -- since given TELA.

*  Description:
*       Return the elapsed time, in days and msecs, since STRELA0 was called,
*       in TELA.  Return the elapsed time since TELA was previously set in
*       DELA.

        INTEGER NATELA_T0
        COMMON/NATELA/NATELA_T0(2)

        INTEGER MSECS,YEAR,MONTH,DAY,DAYSINCE
        INTEGER ELA(2)

*       Obtain the total elapsed time since counter-initialization:
        CALL STRELA( ELA )

        DELA(1) = ELA(1)-TELA(1)
        DELA(2) = ELA(2)-TELA(2)

*       Check for (and fix, if not negative days) negative msecs:
        IF ((DELA(1).LT.0).AND.(DELA(2).GT.0)) THEN
          DELA(2)=DELA(2)-1 !Subtract off one day.
          DELA(1)=DELA(1)+24*60*60*1000 !Add back a day, in msec.
        END IF

        TELA(1) = ELA(1)
        TELA(2) = ELA(2)

        RETURN
        END
*
        SUBROUTINE STRELAASC(ELA_ASC)

        IMPLICIT NONE

*  Output (should be CHARACTER*18 or more):
        CHARACTER*(*) ELA_ASC !ddddd_hh:mm:ss.mmm (ASCII elapsed time; "_" = blank)

*  Description:
*       Return the elapsed time (since STRELA0 was last called) as a 17 character
*       ASCII days, hours, minutes, seconds and milliseconds string.

        INTEGER ELA_TIME(2) !Elapsed time, msecs & days.

        CALL STRELA(ELA_TIME) !Get the two-word elapsed time, 64-bit milliseconds.
        CALL STRELACNV(ELA_TIME,ELA_ASC) !Convert it to 17-character ASCII.

        RETURN
        END
*
        SUBROUTINE STRELACNV(ELA_TIME,ELA_ASC)

        IMPLICIT NONE

*  Input:
        INTEGER ELA_TIME(2) !Elapsed time, msecs & days.
*  Output (should be CHARACTER*18 or more):
        CHARACTER*(*) ELA_ASC !ddddd_hh:mm:ss.mmm (ASCII elapsed time; "_" = blank)

*  Description:
*       Convert specified elapsed time to 18 character ASCII days, hours, minutes,
*       seconds and milliseconds.
*       If the passed string isn't long enough, a truncation is performed, and
*       the last character in the passed string is set to "*".

        INTEGER ELA_DAYS,ELA_HOURS,ELA_MINS,ELA_SECS,ELA_MSECS
        INTEGER ELA_BAL
        INTEGER L
        CHARACTER*18 Elapsed_Time_ASCII

        L=LEN(ELA_ASC) !Get the length of the passed string.
        IF (L.LE.0) THEN !Intercept bogus arguments here.
          RETURN
        END IF

        ELA_DAYS=ELA_TIME(2)

        ELA_HOURS=ELA_TIME(1)/(1000*60*60) !msec ==> hours.
        ELA_BAL=ELA_TIME(1)-ELA_HOURS*(1000*60*60) !Remainder, in msec.
        ELA_MINS=ELA_BAL/(1000*60)         !msec ==> mins.
        ELA_BAL=ELA_BAL-ELA_MINS*(1000*60) !Remainder, in msec.
        ELA_SECS=ELA_BAL/1000              !msec ==> sec
        ELA_MSECS=ELA_BAL-ELA_SECS*1000    !Remainder, in msec (done).

        WRITE(Elapsed_Time_ASCII,101)
     1     ELA_DAYS,ELA_HOURS,ELA_MINS,ELA_SECS,ELA_MSECS
101     FORMAT(I5' 'I2.2':'I2.2':'I2.2'.'I3.3)

        ELA_ASC = Elapsed_Time_ASCII !Copy all or as much as fits.

        IF (L.LT.18) THEN
          ELA_ASC(L:L)='*' !Indicate a truncation.
        END IF

        RETURN

        END
*
        SUBROUTINE STRELA0

        IMPLICIT NONE

*  Brief description: Initialize elapsed real time counter.

        INTEGER NATELA_T0
        COMMON/NATELA/NATELA_T0(2)

        INTEGER MSECS,YEAR,MONTH,DAY, DAYSINCE

        CALL STRMSEC(MSECS)
        NATELA_T0(1)=MSECS
        CALL STRDATE(YEAR,MONTH,DAY)
*       Convert to days since 1-JAN-1900:
        CALL STRDAYSINCE(YEAR,MONTH,DAY,1900,1,1,DAYSINCE)
        NATELA_T0(2)=DAYSINCE

        RETURN
        END
*
        SUBROUTINE STRTIMCNV(TIME,ASC)

        IMPLICIT NONE

*  Input:
        INTEGER TIME(2) !TIME(1) is seconds since midnight, 1-jan-1970.
                        !TIME(2) is (additional) days since 1-jan-1970.
*  Output:
        CHARACTER*23 ASC !Standard time & date: dd-mmm-yy hh:mm:ss zone
                         !(Last four characters are time-zone.)

*  Brief Description:  Convert seconds since 1970 to 23-char ASCII date-time.

*  Description:
*       Convert TIME to 23 character ASCII years, months, days,
*       hours, minutes and seconds.

        INTEGER HOURS,MINS,SECS
        INTEGER DAYS,DAY,MON,YEARS,YEAR,LEAPS
        INTEGER BAL

        INTEGER     Zone
        CHARACTER*4 Zone_Name

        INTEGER MONTH_DAYS(12)
        CHARACTER*3 AMON(12)
        DATA MONTH_DAYS/31,28,31,30,31,30,31,31,30,31,30,31/
        DATA AMON/'Jan','Feb','Mar','Apr','May','Jun'
     1           ,'Jul','Aug','Sep','Oct','Nov','Dec'/

        ASC = ' ' !Blank this out for starters.

        DAYS  = TIME(1) / (24*60*60)
        BAL   = TIME(1) - DAYS*(24*60*60)
        DAYS  = DAYS + TIME(2)
        HOURS = BAL/(60*60)
        BAL   = BAL - HOURS*(60*60)
        MINS  = BAL/60
        BAL   = BAL - MINS*60
        SECS  = BAL

        LEAPS = ( DAYS + 366 + 365 - 31 - 29 ) / ( 366 + 3*365 ) !Start from 1-mar-1968 for leap-counting.

*       Assume TIME=0 occurs at 1-jan-1970;  1996 is 7 leaps later.
        IF (LEAPS.GE.8) LEAPS=LEAPS-1 !Skip a leap year in 2000 -- next skip in 2400.

        YEARS=(DAYS-LEAPS)/365   !Year-zero is 1970, here.
        YEAR=MOD((YEARS+70),100) !Give year in century.
        DAY=DAYS-LEAPS-365*YEARS !Day in year.

*       Make DAY the day in the month:
        MON=1
        DO WHILE (  (MON.LT.12)  .AND.
     1              ( (DAY-MONTH_DAYS(MON)) .GE. 0 )  )
          DAY=DAY-MONTH_DAYS(MON)
          MON=MON+1
        END DO

        DAY = DAY + 1 !1st day of month is 1, not 0.

        CALL STRTime_Get_Current_Zone( Zone, Zone_Name )

        WRITE(ASC,101) DAY,AMON(MON),YEAR,HOURS,MINS,SECS, Zone_Name
101     FORMAT(I2'-'A3'-'I2' 'I2.2':'I2.2':'I2.2' 'A4)

        RETURN
        END
*
        SUBROUTINE STRTIMETXT(TEXT,LINE_OUT)

        IMPLICIT NONE

*  Input:
        CHARACTER*(*) TEXT !Line of text, truncated if > 80 chars.

*  Output:
        CHARACTER*(*) LINE_OUT !Contains TEXT, appended with date/time info.

*  Description:
*       Return a character string (LINE_OUT) consisting of a line of
*       input text, with date, weekday and time info appended.

        INTEGER TLEN

        CHARACTER*80 H80
        CHARACTER*132 C132

        CHARACTER*9 CDATE
        CHARACTER*8 CTIME

        INTEGER MONTH,DAY,YEAR,LEAPS,KEY

        CHARACTER*9 WEEKDAY(7)
        INTEGER CODE(12)
        LOGICAL LEAP

        DATA CODE/1,4,4,7,2,5,7,3,6,1,4,6/
        DATA WEEKDAY/'Sunday','Monday','Tuesday','Wednesday',
     1    'Thursday','Friday!','Saturday'/

        CALL STRASCDATE(CDATE)
        CALL STRDATE(YEAR,MONTH,DAY)
        YEAR=YEAR-1900
        LEAPS=YEAR/4
        LEAP=.FALSE.
        IF (YEAR.EQ.(LEAPS*4)) LEAP=.TRUE.
        IF (MONTH.GT.2) LEAP=.FALSE. !Only Jan. & Feb. are odd in leap years.
        KEY=YEAR+LEAPS+CODE(MONTH)+DAY
        IF (LEAP) KEY=KEY-1 !Sub one if it's leap year but not yet March.
        KEY=MOD(KEY,7)
        IF (KEY.EQ.0) KEY=7
        CALL STRASCTIME(CTIME)

        TLEN=MIN(LEN(TEXT),80)

        CALL STRCLEAN(TEXT,TLEN,H80) !Remove any non-printables.
        C132=' ' !Blank this out.
        WRITE(C132,101) H80(:TLEN),WEEKDAY(KEY),CDATE,CTIME

        LINE_OUT=C132 !Buffer through C132 -- prevent write errors.

101     FORMAT(A'  :  'A9', 'A9,2X,A8)

        RETURN
        END
*
        SUBROUTINE STRTime_Get_Current_Zone( Zone, Zone_Name )

        IMPLICIT NONE

        INCLUDE 'str_time_inc'

*  Outputs:
        INTEGER       Zone      !Time zone offset, in {-12, 12}.
        CHARACTER*(*) Zone_Name !Zone name (eg, "EST", "PDT", etc.)

*  Brief Description: Get the time zone.

*  Description:
*       Return the current time zone, as set in str.  If not set,
*       set str's current zone to the host's local time zone.


        LOGICAL Initialized
        SAVE    Initialized

        DATA    Initialized / .FALSE. /


*       Initialization of time zone may occur externally, so a gauntlet of tests is needed:
*       (Doing it with two independent values is safe & eliminates the need for BLOCKDATAs.)
        IF ( Initialized )                             THEN !Initialized; Skip this.
        ELSE IF ( Current_Zone .LT. -12 )                THEN !Not initialized:
          Initialized = .FALSE.
        ELSE IF ( Current_Zone .GT.  12 )                THEN !Not initialized:
          Initialized = .FALSE.
        ELSE IF ( Initialized_1 .NE. Initialized_1_P ) THEN !Not initialized:
          Initialized = .FALSE.
        ELSE IF ( Initialized_2 .NE. Initialized_2_P ) THEN !Not initialized:
          Initialized = .FALSE.
        ELSE                                                !Initialized.
          Initialized = .TRUE.
        END IF

        IF ( .NOT. Initialized ) THEN !Set it to the local time zone:
*         Hardwire for the moment:
          Current_Zone      = 4
          Current_Zone_Name = 'EDT '
*         Current_Zone      = 5
*         Current_Zone_Name = 'EST '
          Initialized = .TRUE.
          Initialized_1 = Initialized_1_P
          Initialized_2 = Initialized_2_P
        END IF

        Zone      = Current_Zone
        Zone_Name = Current_Zone_Name

        RETURN
        END




*
        SUBROUTINE STRTime_Set_Current_Zone( Zone, Zone_Name )

        IMPLICIT NONE

        INCLUDE 'str_time_inc'

*  Inputs:
        INTEGER       Zone      !Time zone offset, in {-12, 12}.
        CHARACTER*(*) Zone_Name !Zone name (eg, "EST", "PDT", etc.)

*  Brief Description:  Set str's current time zone.

        Current_Zone      = Zone
        Current_Zone_Name = Zone_name

*       Double zone-initialized values -- indicate it's initialized:
        Initialized_1 = Initialized_1_P
        Initialized_2 = Initialized_2_P

        RETURN
        END




*
        SUBROUTINE STRWEEKDAY(YEAR,MONTH,DAY,ASC_WEEKDAY)

        IMPLICIT NONE

*  Inputs:
        INTEGER YEAR,MONTH,DAY

*  Output:
        CHARACTER*9 ASC_WEEKDAY !ASCII representation of weekday.

*  Description:
*       Return 9 character ASCII weekday corresponding to the specified
*       year/month/day.

        INTEGER I,LEAPS,KEY,IYEAR

        CHARACTER*9 WEEKDAY(7)
        INTEGER CODE(12)
        LOGICAL LEAP
        DATA WEEKDAY/'Sunday','Monday','Tuesday','Wednesday',
     1    'Thursday','Friday!','Saturday'/
        DATA CODE/1,4,4,7,2,5,7,3,6,1,4,6/


        IYEAR=YEAR-1900
        LEAPS=IYEAR/4
        LEAP=.FALSE.
        IF (IYEAR.EQ.(LEAPS*4)) LEAP=.TRUE.
        IF (MONTH.GT.2) LEAP=.FALSE. !Only Jan. & Feb. are odd in leap years.
        KEY=IYEAR+LEAPS+CODE(MONTH)+DAY
        IF (LEAP) KEY=KEY-1 !Sub one if it's leap year but not yet March.
        KEY=MOD(KEY,7)
        IF (KEY.EQ.0) KEY=7
        ASC_WEEKDAY=WEEKDAY(KEY) !9 character weekday.

        RETURN
        END
*
        SUBROUTINE STR_MakeTime1970( Year, Month, Day, Hour, Min, Sec, Seconds_Since_1970 )

        IMPLICIT NONE

*  Inputs:
        INTEGER Year, Month, Day
        INTEGER Hour, Min, Sec

*  Output:
        INTEGER Seconds_Since_1970

*  Description:
*       Get the number of seconds since 1-Jan-1970, 00:00:00 GMT, of the specified date-time.

        INTEGER Year_1970, Month_1970, Day_1970
        INTEGER Days_Since_1970
        INTEGER Seconds_Since_Today

*       The beginning date of 1970:
        DATA Year_1970 / 1970 /
        DATA Month_1970 / 1 /
        DATA Day_1970   / 1 /


*       Get number of days since 1970 began:
        CALL STRDAYSINCE( Year, Month, Day, Year_1970, Month_1970, Day_1970, Days_Since_1970 )

        Seconds_Since_Today = ( Hour * 60 + Min ) * 60 + Sec !Today's contribution.
        Seconds_Since_1970  = Days_Since_1970 * 24 * 3600 + Seconds_Since_Today
        RETURN
        END
*


	SUBROUTINE STRASCCPU(TCPU_TIME,DCPU_TIME,TCPU_ASC,DCPU_ASC,
     1	                     ASC_DATE,ASC_WEEKDAY,ASC_TIME)

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
*	Return absolute (since last STRCPU0 call) and delta CPU
*	times, and 9 character ASCII date, 8 character ASCII time,
*	and 9 character ASCII weekday.

	LOGICAL QUAD_MODE !Switch to control 64-bit arithmetic (on/off).
	PARAMETER (QUAD_MODE=.FALSE.) !Off.
*	PARAMETER (QUAD_MODE=.TRUE.) !On.

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

*	  Quad-word subtraction:
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
*	Return 9-character ASCII date.

	INTEGER YEAR,MONTH,DAY,IM
	CHARACTER*3 CM(13)
	DATA CM/'Jan','Feb','Mar','Apr','May','Jun'
     1	       ,'Jul','Aug','Sep','Oct','Nov','Dec'
     2	       ,'Err'/
	CALL STRDATE(YEAR,MONTH,DAY)
	IM=MONTH
	IF (IM.LE.0) IM=13
	IF (IM.GT.12) IM=13
	IF (DAY.LE.0) IM=13
	IF (DAY.GT.31) IM=13
	WRITE(C9,101) DAY,CM(IM),YEAR-1900
101	FORMAT(I2.2'-'A3'-'I2)
	RETURN
	END
*
	SUBROUTINE STRASCDATETIME(TIM_ASC)

	IMPLICIT NONE

*  Output:
	CHARACTER*23 TIM_ASC !Standard time & date: dd-mmm-yy hh:mm:ss bbbb
*                             (Last five characters are blank.)

*  Description:
*	Return standard 23 character ASCII years, months, days,
*	hours, minutes and seconds.

	CHARACTER*3 TIM_AMON(12)
	DATA TIM_AMON/'Jan','Feb','Mar','Apr','May','Jun'
     1	             ,'Jul','Aug','Sep','Oct','Nov','Dec'/

	INTEGER YEAR,MONTH,DAY,HOURS,MINS,SECS

*	Get the standard date and time:
	CALL STRDATE(YEAR,MONTH,DAY)
	IF (YEAR.GT.1900) YEAR=YEAR-1900 !Subtract off the upper 2 digits.
	CALL STRTIME(HOURS,MINS,SECS)

	TIM_ASC='No date or time'
	IF (MONTH.GT.12) RETURN
	IF (MONTH.LE.0) RETURN

	WRITE(TIM_ASC,101)
     1	   DAY,TIM_AMON(MONTH),YEAR
     2	  ,HOURS,MINS,SECS
101	FORMAT(I2'-'A3'-'I2' 'I2.2':'I2.2':'I2.2)

	RETURN
	END
*
	SUBROUTINE STRASCELA(TELA_TIME,DELA_TIME,TELA_ASC,DELA_ASC,
     1	                     ASC_DATE,ASC_WEEKDAY,ASC_TIME)

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
*	Return absolute (since last STRELA0 call) and delta ELA
*	times, and 9 character ASCII date, 8 character ASCII time,
*	and 9 character ASCII weekday.

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

*	Check for (and fix, if not negative days) negative msecs:
	IF ((DELA_TIME(1).LT.0).AND.(DELA_TIME(2).GT.0)) THEN
	  DELA_TIME(2)=DELA_TIME(2)-1 !Subtract off one day.
	  DELA_TIME(1)=DELA_TIME(1)+24*60*60*1000 !Add back a day, in msec.
	END IF

*	Update the total elapsed time:
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
*	Return 8-character ASCII time.

	INTEGER HOUR,MIN,SEC
	CALL STRTIME(HOUR,MIN,SEC)
	WRITE(C8,101) HOUR,MIN,SEC
101	FORMAT(I2.2':'I2.2':'I2.2)
	RETURN
	END
*
	SUBROUTINE STRCAP(BPT,EPT,C)

	IMPLICIT NONE

*  Inputs:
	INTEGER BPT,EPT !Range in string C in which conversion is requested.

*  Input/output:
	CHARACTER*(*) C !String to be converted to all capitals.

*  Description:
*	Convert a specified range in a character string to all caps.
*	Provided for back compatibility.  Preferred usage is STRCAPS.

	CALL STRCAPS(C(BPT:EPT))

	RETURN
	END
*
	SUBROUTINE STRCAPS(C)

	IMPLICIT NONE

*  Input/output:
	CHARACTER*(*) C !String to be converted to all capitals.

*  Description:
*	Convert a character string to all caps.

	CHARACTER*1 NULL
	INTEGER I,L
	DATA NULL/'000'O/

	L=LEN(C)
	DO I=1,L
	  IF (C(I:I).EQ.NULL) THEN !Keep from running past short ASCIZ strings.
	    RETURN
	  ELSE IF (C(I:I).GE.'a' .AND. C(I:I).LE.'z') THEN !Convert to capital.
	    C(I:I)=CHAR(ICHAR(C(I:I))-ICHAR('a')+ICHAR('A'))
	  END IF
	END DO
	RETURN
	END
*
	SUBROUTINE STRCLEAN(TEXT,LENGTH,CTEXT)

	IMPLICIT NONE

*  Inputs:
	CHARACTER*(*) TEXT !The character string to be cleaned-up.
	INTEGER LENGTH     !The length of TEXT to be cleaned-up.
*  Output:
	CHARACTER*(*) CTEXT !The cleaned-up text.

*  Description:
*	Copy all or as much as will fit of specified input text to an
*	output character string, and replace any non-printable characters in
*	with blanks (clean text).  Terminate the copying if input string has
*	a null, and pad the rest of the output string with blanks.

	CHARACTER*1 NULL
	INTEGER I,J,L,K
	DATA NULL/'000'O/

	L=LEN(TEXT) !The length of the specified argument.
	K=MIN(LENGTH,LEN(CTEXT)) !The output length.

	DO I=1,K
	  IF (I.GT.L) THEN !Input exhausted - pad with blanks:
	    DO J=I,K !Blank out the rest.
	      CTEXT(J:J)=' '
	    END DO
	    RETURN
	  ELSE IF (TEXT(I:I).EQ.NULL) THEN
*	    Keep from running past short ASCIZ strings.
	    DO J=I,K !Blank out the rest.
	      CTEXT(J:J)=' '
	    END DO
	    RETURN
	  ELSE IF (TEXT(I:I).LT.' ' .OR. TEXT(I:I).GT.'~') THEN !Non-printable:
	    CTEXT(I:I)=' ' !Blank it out.
	  ELSE !Printable:
	    CTEXT(I:I)=TEXT(I:I)
	  END IF
	END DO
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
*	Convert specified CPU time to 17 character ASCII days, hours, minutes,
*	seconds and ticks.
*	If the passed string isn't long enough, a truncation is performed, and
*	the last character in the passed string is set to "*".

	LOGICAL QUAD_MODE !Switch to control 64-bit arithmetic (on/off).
	PARAMETER (QUAD_MODE=.FALSE.) !Off.
*	PARAMETER (QUAD_MODE=.TRUE.) !On.

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
*	Map 0-59 to 0-5900 or 0-99 to 0-9900:
	CPU_TICKS=CPU_BAL*100
	CPU_TICKS=CPU_TICKS/TPS !Map 0-5900 to 0-98 or 0-9900 to 0-99.

	WRITE(Elapsed_CPU_ASCII,101)
     1	   CPU_DAYS,CPU_HOURS,CPU_MINS,CPU_SECS,CPU_TICKS
101	FORMAT(I5' 'I2.2':'I2.2':'I2.2'.'I2.2)

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
*	Return CPU time, since last STRCPU0 call, as a 17 character ASCII days,
*	hours, minutes, seconds and ticks string.

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
*	Return a character string (LINE_OUT) consisting of a line of
*	input text, with CPU time-usage info appended.

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
101	FORMAT(A' 'A17' 'A9' 'A8)

	LINE_OUT=C132 !Buffer through C132 -- prevent write errors.

	RETURN
	END
*
	SUBROUTINE STRDAYSINCE(YEAR_NEW,MONTH_NEW,DAY_NEW
     1	                      ,YEAR_OLD,MONTH_OLD,DAY_OLD,DAYSINCE)

	IMPLICIT NONE

*  Inputs:
	INTEGER YEAR_NEW,MONTH_NEW,DAY_NEW,YEAR_OLD,MONTH_OLD,DAY_OLD

*  Output:
	INTEGER DAYSINCE

*  Description:
*	Return the total days from the "new" date since the "old" date.

	INTEGER I,LEAPS,IYEAR

	INTEGER LENGTH(12)
	DATA LENGTH/31,28,31,30,31,30,31,31,30,31,30,31/


*	Accumulate the total number of days from a fixed reference:
*	(Fixed reference is necessary to get leaps right.)

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

*	Subtract off the number of days in the old date, from same fixed ref:
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
*	.TRUE. if a successful conversion occurred,
*	.FALSE. if not.

*  Description:
*	"Read" a double precision number from the character string S.
*	Convert according to the radix indicated in S by
*	one of the (prefix) radix indicators:
*	" or O ==> octal    X or H ==> hex    % ==> binary   $ ==> decimal.
*	If no prefix is used, the default radix is used, specified by
*	RADIX as 2, 8 or 16 (anything else is taken as 10).
*	The result, if successful, is in D.  D_MIN and D_MAX specify the
*	acceptable range of D.
*	If D_MAX .le. D_MIN, the limits are ignored.

	DOUBLE PRECISION UNREAL
	INTEGER UNINT
	INTEGER IERR
	LOGICAL BAD_VAL

	INTEGER STRUNCODE

500	FORMAT('STRDBL-E0 Illegal representation of a number.')
501	FORMAT('STRDBL-E1 'E15.7' outside range ('E15.7','E15.7')')
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
	SUBROUTINE STRELA(TELA)

	IMPLICIT NONE

*  Output:
	INTEGER TELA(2) !(1) is msecs, (2) is days.

*  Description:
*	Return the elapsed time, in days and msecs, since STRELA0 was called.

	INTEGER NATELA_T0
	COMMON/NATELA/NATELA_T0(2)

	INTEGER MSECS,YEAR,MONTH,DAY,DAYSINCE

*	Obtain the total elapsed time since counter-initialization:
	CALL STRMSEC(MSECS)
	TELA(1)=MSECS-NATELA_T0(1)
	CALL STRDATE(YEAR,MONTH,DAY)
*	Convert to days since 1-JAN-1900:
	CALL STRDAYSINCE(YEAR,MONTH,DAY,1900,1,1,DAYSINCE)
	TELA(2)=DAYSINCE-NATELA_T0(2)

*	Check for (and fix, if not negative days) negative msecs:
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
*	Return the elapsed time, in days and msecs, since STRELA0 was called,
*	in TELA.  Return the elapsed time since TELA was previously set in
*	DELA.

	INTEGER NATELA_T0
	COMMON/NATELA/NATELA_T0(2)

	INTEGER MSECS,YEAR,MONTH,DAY,DAYSINCE
	INTEGER ELA(2)

*	Obtain the total elapsed time since counter-initialization:
	CALL STRELA( ELA )

	DELA(1) = ELA(1)-TELA(1)
	DELA(2) = ELA(2)-TELA(2)

*	Check for (and fix, if not negative days) negative msecs:
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
*	Return the elapsed time (since STRELA0 was last called) as a 17 character
*	ASCII days, hours, minutes, seconds and milliseconds string.

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
*	Convert specified elapsed time to 18 character ASCII days, hours, minutes,
*	seconds and milliseconds.
*	If the passed string isn't long enough, a truncation is performed, and
*	the last character in the passed string is set to "*".

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
     1	   ELA_DAYS,ELA_HOURS,ELA_MINS,ELA_SECS,ELA_MSECS
101	FORMAT(I5' 'I2.2':'I2.2':'I2.2'.'I3.3)

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
*	Convert to days since 1-JAN-1900:
	CALL STRDAYSINCE(YEAR,MONTH,DAY,1900,1,1,DAYSINCE)
	NATELA_T0(2)=DAYSINCE

	RETURN
	END
*
	SUBROUTINE STREND(S,EPT)

	IMPLICIT NONE

*  Input:
	CHARACTER*(*) S
*  Output:
	INTEGER EPT !Index to the last non-blank character in S.

*  Description:
*	Find the string-end -- set EPT to point at the last non-blank in S,
*	or 0 if all blanks.  Tabs are regarded as blanks.
*	No null is appended.

	INTEGER L,I

	L=LEN(S) !Length of string S.
*	Search for the end of the line:
	EPT=L !No trailing blanks confirmed yet (reset value).
	DO I=1,L
	  IF ((S(I:I).NE.' ').AND.(S(I:I).NE.'	')) THEN !Non-blank/tab.
	    EPT=L !Reset to last character in S.
	  ELSE IF (EPT.EQ.L) THEN !First blank/tab since last reset.
	    EPT=I-1 !A trailing blank?  Point to previous character.
	  END IF
	END DO

	RETURN
	END
*
	SUBROUTINE STRGETCHR(LONG,SEL,CHR)

	IMPLICIT NONE

*  Inputs:
	INTEGER LONG !Contains a long-word (4 bytes) -- the "source".
	INTEGER SEL  !A number, 1 to 4, indicating one of LONG's 4 bytes.
*  Output:
	CHARACTER*1 CHR !Contains the character-representation of the selected
*                        byte from LONG.

*  Description:
*	Get a character out of LONG, a 32 bit word.  The character
*	is the SELth byte of LONG, and goes into CHR.  The selection
*	is made in a machine-independent fashion.

	INTEGER I4
	BYTE I1(4)
	EQUIVALENCE (I1,I4)
	INTEGER J4
	BYTE J1(4)
	EQUIVALENCE (J1,J4)

	J4=LONG         !Gain byte-access.
	CALL STRDEC_ENDIAN_BYTE(J4) !Make J1(SEL) select bytes like a VAX.
	I4=0            !Start with all four bytes of I4 zeroed.
	I1(1) = J1(SEL) !Overwrite the selected byte (other 3 are zero).

	CALL STRDEC_ENDIAN_BYTE(I4) !Make it DEC-like (byte-ordering).
	                            !ie, have the CHAR function work on
	                            !what was put into I1(1).

	CHR=CHAR(I4)

	RETURN
	END
*
	SUBROUTINE STRGETCHRS( Buffer, Nchrs, Nwords, CHRS )

	IMPLICIT NONE

*  Inputs:
	INTEGER Buffer(*) !Longword-buffer from which CHRS is copied.
	INTEGER Nchrs !Number of characters to copy.

*  Outputs:
	INTEGER Nwords !Number of longwords copied from Buffer.
	CHARACTER*(*) CHRS !Character-string to be copied from Buffer.

*  Description:
*	Copy Nchrs characters from Buffer into CHRS.
*	Return the number of longwords accessed in Buffer; should
*	be (Nchrs+3)/4 .

	INTEGER I, J
	INTEGER SEL

	I=1
	J=1
	Nwords=0
	SEL=4 !Set this in case of funny Nchrs or Buffer_size.
	CHRS=' ' !Start out with all blanks.

	DO WHILE ( I.LE.Nchrs )
	  SEL=MOD(I-1,4)+1 !Cycles: 1, 2, 3, 4, 1, ...
	  CALL STRGETCHR( Buffer(J), SEL, CHRS(I:I) ) !Machine-independent order.
	  IF      (SEL.EQ.1) THEN !Broke ground on a new word.
	    Nwords=Nwords+1
	  ELSE IF (SEL.GE.4) THEN !Will break ground on a new word on the next char.
	    J=J+1
	  END IF
	  I=I+1 !Next character.
	END DO !WHILE ( I.LE.Nchrs )


	RETURN
	END
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
*	.true.  ==> command line successfully read in.
*	.false. ==> EOF encountered on logical unit FUNIT.

*  Description:
*	Get first non-comment, non-blank line, from logical unit FUNIT, and
*	put it in S.  Remove comments and set EPT to point at the last
*	non-blank character. Return the FUNCTION value as .true. for success,
*	.false. for EOF.  Ignores form feeds.
*	No null is appended.

	CHARACTER*1 FF
	CHARACTER*132 S132

	DATA FF/'014'O/

201	FORMAT(A132)

1000	STRGETCOM=.FALSE. !In case of EOF.
	EPT=0
	DO WHILE (EPT.LT.1) !Skip over any pure-comment lines.
	  READ(FUNIT,201,END=2000) S132
	  IF (S132(1:1).EQ.FF) GO TO 1000 !Restart.
	  CALL STRIP(S132,EPT) !Strip off comments/find last non-blank.
	END DO
	S=S132 !Copy over as much as will fit.
	STRGETCOM=.TRUE.

2000	RETURN

	END
*
	SUBROUTINE STRINSERT(SOURCE,START,END,DEST,DPT)

	IMPLICIT NONE

*  Inputs:
	CHARACTER*(*) SOURCE !Source string.
	INTEGER START,END    !Index-range in SOURCE to be copied.
*  Outputs:
	CHARACTER*(*) DEST   !Destination string.
	INTEGER DPT          !Starting index in DEST -- SOURCE is copied to
*                             to DEST starting at DEST(DPT:DPT), and continues
*                             until the SOURCE range is exhausted or until
*                             DEST runs out of space.  DPT is returned to be
*                             the index of the first character in DEST
*                             following the characters copied over from SOURCE,
*                             unless DEST ran out of room, in which case
*                             DPT is set to zero.

*  Description:
*	Insert characters in SOURCE (from START to END)
*	to DEST (from DPT to .LE. LEN(DEST). Set DPT to point after
*	the last character inserted.
*	No null is appended.

	INTEGER SPT,DMAX

	DMAX=LEN(DEST)

	IF (DPT.LE.0) RETURN !Error, from some previous manipulation.
*	Move:
	DO SPT=START,END
	  IF (DPT.GT.DMAX) GO TO 5000 !Fatal error.
	  DEST(DPT:DPT)=SOURCE(SPT:SPT)
	  DPT=DPT+1
	END DO
	RETURN !Done (success).

*	Error; DPT is too high (Fatal. DEST is zeroed):
5000	DPT=0
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
*	.TRUE. if a successful conversion occurred,
*	.FALSE. if not.

*  Description:
*	"Read" an integer number from the character string S.
*	Convert according to the radix indicated in S by
*	one of the (prefix) radix indicators:
*	" or O ==> octal    X or H ==> hex    % ==> binary   $ ==> decimal.
*	If no prefix is used, the default radix is used, specified by
*	RADIX as 2, 8 or 16 (anything else is taken as 10).
*	The result, if successful, is in I.  I_MIN and I_MAX specify the
*	acceptable range of I.
*	If I_MAX .le. I_MIN, the limits are ignored.

	DOUBLE PRECISION UNREAL
	INTEGER UNINT
	INTEGER IERR
	LOGICAL BAD_VAL

	INTEGER STRUNCODE

500	FORMAT(' STRINT-E0-Illegal representation of a number.')
501	FORMAT(' STRINT-E1-Number:'I11' outside valid range.')

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
	SUBROUTINE STRIP(S,EPT)

	IMPLICIT NONE

*  Input/output:
	CHARACTER*(*) S  !Character string containing possible trailing
*                         blanks and comments (comments indicated by "!").
*                         S is returned with trailing comments blanked out.
*  Output:
	INTEGER EPT      !Index to the last non-blank character in S, after
*                         comments have been removed.

*  Description:
*	Routine to strip off comments (and trailing blanks) at the end
*	of "command" lines, where comments are denoted by the usual
*	exclamation mark (!).  Leave EPT pointing at last non-blank.
*	No null is appended.

	INTEGER L,I

	L=LEN(S)
	I=INDEX(S,'!')
	IF (I.LE.0) THEN !No comment.
	ELSE IF (I.LE.L) THEN !Strip off the comment.
	  S(I:L)=' '
	END IF
	CALL STREND(S,EPT) !Set EPT to string-end (last non-blank/tab).
	RETURN
	END
*
	SUBROUTINE STRLIST(MSG,LINES,LUN)

	IMPLICIT NONE

*  Inputs:
	CHARACTER*(*) MSG(*) !Character string to be output.
	INTEGER LINES !Number of lines in MSG to output.
	INTEGER LUN !Logical unit on which to output (terminal).

*  Description:
*	Output a one-or-more-line message on a terminal-emulating
*	listing-file with machine-independent carriage-control as if
*	on a terminal.

	INCLUDE 'strlib_inc'

	INTEGER I

	DO I=1,LINES
	  WRITE(LUN,100) MSG(I)
	END DO
100	FORMAT(A)

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
*	Emulate output of a one-line message without a following
*	carriage return on a terminal-emulating listing-file.
*	Actual outputing is deferred until a STRLIST_NOLF
*	call is made, at which time the message specified in that
*	latter call is appended to previously-specified output
*	and then output.  A call to STRLIST will wipe out any
*	characters from a STRLIST_NOCR or STRLIST_NOLFCR call.
*	Any existing characters in the previously-specified output
*	is overwritten.

	INCLUDE 'strlib_inc'

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
*	Output a one-line message without a preceding line feed
*	on a terminal-emulating listing file.  MSG is appended
*	to any previously specified output from a call to either
*	STRLIST_NOCR or STRLIST_NOLFCR before it is output.

	INCLUDE 'strlib_inc'


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
100	FORMAT(A)

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
*	Output a one-line message without a preceding line feed
*	or a following carriage-return on a terminal-emulating
*	listing-file.
*	Actual outputing is deferred until a STRLIST_NOLF
*	call is made, at which time the message specified in that
*	latter call is appended to previously-specified output
*	and then output.  A call to STRLIST will wipe out any
*	characters from a STRLIST_NOCR or STRLIST_NOLFCR call.
*	MSG is appended to any existing characters in
*	previously-specified output in a STRLIST_NOCR or STRLIST_NOLFCR
*	call.

*	It is illegal to call this routine before a call to any one of
*	STRLIST, STRLIST_NOCR or STRLIST_NOLF.

	INCLUDE 'strlib_inc'

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
*	.TRUE. for a successful open on FILENAME.
*	.FALSE. for a failed open.


*  Description:
*	Attempts to open the specified file in a machine-independent fashion
*	using the OPEN parameters specified in COMMANDS.  COMMANDS is a
*	character string containing all the "usual" OPEN parameters, except
*	for the UNIT=lun (FORTRAN Logical Unit) and FILE=filename, which are passed
*	separately, and without the usual single-quotes (apostrophes) normally
*	around literal arguments.


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

*	write(6,300) lun,filename,commands
*300	format(' STROPEN-D0 LUN:'I11'  filename/commands:'/' 'A/' 'A)


*	Parse out the commands:
	CALL STRPARSE
     1	    (COMMANDS,NARGS_max,NARGS,ARG,DARG,IARG,VDARG,VIARG)

	IF (NARGS.LE.0) THEN !No arguments -- simple open:
	  Open_success=.FALSE.
	  OPEN(UNIT=LUN,FILE=FILENAME,STATUS='UNKNOWN',ERR=3)
	  Open_success=.TRUE.
3	  CONTINUE
	  STROPEN=Open_success
*	  write(6,301) Open_success
*301	format(' STROPEN-D1  Plain OPEN, success:'L1)
	  RETURN
	END IF

*	Convert all args to all-caps:
	DO I=1,NARGS
	  CALL STRCAPS(ARG(I))
	END DO

*	Standard defaults:
	STATUS_CARG='UNKNOWN'
	FORM_CARG='FORMATTED'
	ACCESS_CARG='SEQUENTIAL'
	RECL_FLAG=.FALSE.

	MACHINE_SPECIFIC=.FALSE.

*	VMS & SGI specials:
	CARRIAGECONTROL_FLAG=.FALSE.
	CARRIAGECONTROL_CARG='LIST'
	MAXREC_FLAG=.FALSE.

*	VMS specials:
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
     1	      .AND. (ARG(JARG+1).EQ.  '='   ) ) THEN

	    STATUS_CARG=ARG(JARG+2)
	  
	  ELSE IF ( (ARG(JARG)  .EQ.'ACCESS')
     1	      .AND. (ARG(JARG+1).EQ.  '='   ) ) THEN

	    ACCESS_CARG=ARG(JARG+2)
	  
	  ELSE IF ( (ARG(JARG)  .EQ.'FORM')
     1	      .AND. (ARG(JARG+1).EQ.'='   ) ) THEN

	    FORM_CARG=ARG(JARG+2)
	  
	  ELSE IF ( (ARG(JARG)  .EQ.'RECL')
     1	      .AND. (ARG(JARG+1).EQ.'='   ) ) THEN


	    IF (VIARG(JARG+2)) THEN !Valid integer in argument:
	      MACHINE_SPECIFIC=.TRUE. !RECL is in machine-units.
	      RECL_FLAG=.TRUE.
	      RECL_IARG=IARG(JARG+2)
	    END IF
	  
	  ELSE IF ( (ARG(JARG)  .EQ.'CARRIAGECONTROL')
     1	      .AND. (ARG(JARG+1).EQ.  '='            ) ) THEN

	    MACHINE_SPECIFIC=.TRUE.
	    CARRIAGECONTROL_FLAG=.TRUE.
	    CARRIAGECONTROL_CARG=ARG(JARG+2)
	  
	  ELSE IF ( (ARG(JARG)  .EQ.'RECORDTYPE')
     1	      .AND. (ARG(JARG+1).EQ.  '='       ) ) THEN

	    MACHINE_SPECIFIC=.TRUE.
	    RECORDTYPE_FLAG=.TRUE.
	    RECORDTYPE_CARG=ARG(JARG+2)
	  
	  ELSE IF ( (ARG(JARG)  .EQ.'INITIALSIZE')
     1	      .AND. (ARG(JARG+1).EQ.  '='        ) ) THEN

	    IF (VIARG(JARG+2)) THEN !Valid integer in argument:
	      MACHINE_SPECIFIC=.TRUE.
	      INITIALSIZE_FLAG=.TRUE.
	      INITIALSIZE_IARG=IARG(JARG+2)
	    END IF

	  ELSE IF ( (ARG(JARG)  .EQ.'BLOCKSIZE')
     1	      .AND. (ARG(JARG+1).EQ.  '='       ) ) THEN

	    IF (VIARG(JARG+2)) THEN !Valid integer in argument:
	      MACHINE_SPECIFIC=.TRUE.
	      BLOCKSIZE_FLAG=.TRUE.
	      BLOCKSIZE_IARG=IARG(JARG+2)
	    END IF

	  ELSE IF ( (ARG(JARG)  .EQ.'MAXREC')
     1	      .AND. (ARG(JARG+1).EQ.  '='       ) ) THEN

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
     1	 ,STATUS_CARG,ACCESS_CARG,FORM_CARG
     1	 ,RECL_FLAG,RECL_IARG
     1	 ,CARRIAGECONTROL_FLAG,CARRIAGECONTROL_CARG
     1	 ,RECORDTYPE_FLAG,RECORDTYPE_CARG
     1	 ,INITIALSIZE_FLAG,INITIALSIZE_IARG
     1	 ,BLOCKSIZE_FLAG,BLOCKSIZE_IARG
     1	 ,MAXREC_FLAG,MAXREC_IARG
     1	 ,READONLY_FLAG
     1   )

*	  write(6,302) Open_success
*302	format(' STROPEN-D2  Native OPEN, success:'L1)

	ELSE

	  Open_success=.FALSE.
	  OPEN(UNIT=LUN,FILE=FILENAME,STATUS=STATUS_CARG
     1	      ,ACCESS=ACCESS_CARG,FORM=FORM_CARG
     2	      ,ERR=2)
	  Open_success=.TRUE.
2	  CONTINUE

*	  write(6,304) Open_success,STATUS_CARG,ACCESS_CARG,FORM_CARG
*     1	              ,FILENAME
*304	format(' STROPEN-D4  no-RECL OPEN, success:'L1/
*     1	       '             STATUS:'A/
*     1	       '             ACCESS:'A/
*     1	       '             FORM:  'A/
*     1	       '             FILE:  'A)

	END IF

	STROPEN=Open_success
	RETURN
	END

*
	LOGICAL FUNCTION STROPENVER(LUN,FILENAME,COMMANDS
     1	  ,VLIMIT,VCHAR,VERSION,FILE_NAME_OUT)

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
*	.TRUE. for a successful open on: FILENAME//VCHAR//VERSION.
*	.FALSE. for a failed open.


*  Description:
*	Combines the functions of STRVER and STROPEN, providing a simple
*	way to open version-appended files in a machine-independent fashion,
*	using the OPEN parameters specified in COMMANDS.  COMMANDS is a
*	character string containing all the "usual" OPEN parameters, except
*	for the UNIT=lun (FORTRAN Logical Unit) and FILE=filename, which are passed
*	separately, and without the usual single-quotes (apostrophes) normally
*	around literal arguments.


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

*	write(6,301) len(file_name_out),file_name_out
*301	format(' STROPENVER-D1 LEN(FILE_NAME_OUT):'I11' FILE_NAME_OUT:'/
*     1	       ' 'A)

***************************** * *  72  * * *****************************

	STROPENVER=SUCCESS

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
*	Parse out up to NARGS_max argument from S and into ARG, DARG and IARG.
*	Argument separators are:
*	space, tab, comma, and equal-sign.  The equal-sign is a special
*	separator in that it is also returned as an argument.  This permits
*	checking externally to STRPARSE that equal signs occur where they
*	are supposed to.

*	Define these values & arrays in the calling routine:
*	INTEGER NARGS_max
*	INTEGER ARG_length
*	PARAMETER (NARGS_max=40) !40 is an example.
*	PARAMETER (ARG_length=80) !80 is an example.
*	INTEGER IARG(NARGS_max)
*	DOUBLE PRECISION DARG(NARGS_max) !This MUST be D.P.
*	CHARACTER*(ARG_length) ARG(NARGS_max)
*	LOGICAL VDARG(NARGS_max),VIARG(NARGS_max)


*	Call STRPARSE_RADIX with a default radix of 10 (decimal):

	CALL STRPARSE_RADIX
     1	          (S,NARGS_max,10,NARGS,ARG,DARG,IARG,VDARG,VIARG)

	RETURN
	END
*
	SUBROUTINE STRPARSE_RADIX
     1	          (S,NARGS_max,RADIX,NARGS,ARG,DARG,IARG,VDARG,VIARG)

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
*	Parse out up to NARGS_max argument from S and into ARG, DARG and IARG.
*	Argument separators are:
*	space, tab, comma, and equal-sign.  The equal-sign is a special
*	separator in that it is also returned as an argument.  This permits
*	checking externally to STRPARSE_RADIX that equal signs occur where they
*	are supposed to.

*	Define these values & arrays in the calling routine:
*	INTEGER NARGS_max
*	INTEGER ARG_length
*	PARAMETER (NARGS_max=40) !40 is an example.
*	PARAMETER (ARG_length=80) !80 is an example.
*	INTEGER IARG(NARGS_max)
*	DOUBLE PRECISION DARG(NARGS_max) !This MUST be D.P.
*	CHARACTER*(ARG_length) ARG(NARGS_max)
*	LOGICAL VDARG(NARGS_max),VIARG(NARGS_max)

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
*	    Try to make an integer, base "RADIX", & D.P. real:
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
*	Parse out a file name from CIN and put it in COUT.  If no
*	file type is specified, append the default file type, CDEF.
*	If there is an error with the lengths, (ie, not enough space
*	in COUT), COUT is returned as "strparsef.err".

	CHARACTER*1 NULL

	INTEGER DPT,NPT,PPT,SPT,LIN,LOUT,LDEF,RBPT,START
	DATA NULL/'000'O/

*	Obtain the lengths of the character strings:
	LIN=LEN(CIN)
	NPT=INDEX(CIN,NULL) !Locate a possible null, for back-compatibility.
	IF (NPT.LE.0) NPT=LIN+1 !No null - "pretend" null follows string.
	LIN=NPT-1 !Use this as the effective length.
	SPT=INDEX(CIN,' ') !Point to 1st space.
	IF (SPT.LE.0) SPT=LIN+1 !No space - "pretend" space follows string.
	IF (SPT.LT.LIN) LIN=SPT-1 !If space preceeds end, use space-pointer.
*	LIN now is the "effective" length of CIN.

	LOUT=LEN(COUT)
	IF (LIN.GT.LOUT) GO TO 5000 !Error -- output string too short.

	COUT=' ' !Blank out COUT.

*	Protect against possible VMS-style directories containing periods:
	RBPT=INDEX(CIN,']')
	IF (RBPT.GT.0) THEN !There's a directory spec here:
	  START=RBPT+1
	  IF (START.GT.LIN) START=LIN
	ELSE !No directory spec:
	  START=1
	END IF

*	Determine whether CIN already has a file-type specified:
	DPT=INDEX(CIN(START:LIN),'.')
	IF (DPT.LE.0) THEN !File type is not specified.
	  LDEF=LEN(CDEF) !Length of default file type.
	  NPT=INDEX(CDEF,NULL) !Locate a possible null, for back-compatibility.
	  IF (NPT.LE.0) NPT=LDEF+1 !No null - "pretend" null follows string.
	  LDEF=NPT-1 !Use this as the effective length.
	  SPT=INDEX(CDEF,' ') !Point to 1st space.
	  IF (SPT.LE.0) SPT=LDEF+1 !No space - "pretend" space follows string.
	  IF (SPT.LT.LDEF) LDEF=SPT-1 !If space preceeds end, use space-pntr.
*	  LDEF now is the "effective" length of CDEF.
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

*	Error - insert error-name and return:
5000	COUT='strparsef.err'
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
*	Return .TRUE. if a next (now, this) argument exists, .FALSE. otherwise.

*  Description:
*	Parse the next argument out of S and into D.
*	Leave SPT pointing at the space after the argument parsed,
*	or at SMAX.  Call STRPARSEN in a loop until it returns false,
*	giving it a different D each time (D is usually a dimensioned
*	array);  this parses out the arguments in S.  Separators are:
*	space, tab, comma, and equal-sign.  The equal-sign is a special
*	separator in that it is also returned as an argument.  This permits
*	checking externally to STRPARSEN that equal signs occur where they
*	are supposed to.

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

*	Find first character (at beginning or after sep.).
*	Might be already positioned, or might need to skip several sep. chars.:
	SEP_FL=.TRUE. !Force initial loop entry.
	DO WHILE (SEP_FL.AND.(SPT.LE.SLEN))
*	  Space,tab,comma:
	  IF (INDEX(' 	,',S(SPT:SPT)).LE.0) THEN
*	    SPT not pointing at separator (exit this loop):
	    SEP_FL=.FALSE.
	  ELSE !Points at a separator (continue this loop):
	    SPT=SPT+1 !Next character.
	  END IF
	END DO

*	SPT now points at begining of next (now current!) field:
*	Copy to D until sep. or end:
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

*	  Space,tab,comma:
	  ELSE IF (INDEX(' 	,',S(SPT:SPT)).GT.0) THEN !SPT points at a separator:
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
	SUBROUTINE STRPUTCHR(CHR,SEL,LONG)

	IMPLICIT NONE

*  Inputs:
	CHARACTER*1 CHR !Contains the character-representation of the selected
*                        byte to be inserted into LONG.
	INTEGER SEL  !A number, 1 to 4, indicating one of LONG's 4 bytes.

*  Output:
	INTEGER LONG !Contains a long-word (4 bytes) -- the "sink".

*  Description:
*	Put a character into LONG, a 32 bit word.  The character becomes
*	is the SELth byte of LONG, and from CHR.  The selection
*	is made in a machine-independent fashion.

	INTEGER I4
	BYTE I1(4)
	EQUIVALENCE (I1,I4)
	INTEGER J4
	BYTE J1(4)
	EQUIVALENCE (J1,J4)

	I4=ICHAR(CHR) !Make it an integer.
	CALL STRDEC_ENDIAN_BYTE(I4) !Make it DEC-like (byte-ordering).
	                            !ie, want the desired byte to be in I1(1).

	J4=LONG         !Make a copy.
	CALL STRDEC_ENDIAN_BYTE(J4) !Make it DEC-like (byte-ordering).
	J1(SEL) = I1(1) !Overwrite the selected DEC-byte.
	CALL STRDEC_ENDIAN_BYTE(J4) !Put it back the way it was.
	LONG=J4         !Put the result back here.

	RETURN
	END
*
	SUBROUTINE STRPUTCHRS( CHRS, Nchrs, Buffer_size, Nwords, Buffer )

	IMPLICIT NONE

*  Inputs:
	CHARACTER*(*) CHRS !Character-string to be copied to Buffer.
	INTEGER Nchrs !The number of characters from CHRS to copy.
	INTEGER Buffer_size !Size, in 32-bit words, of Buffer.

*  Outputs:
	INTEGER Nwords !Number of longwords in Buffer, from the copy.
	INTEGER Buffer(*) !Longword-buffer into which CHRS is copied.

*  Description:
*	Copy Nchrs characters from CHRS into Buffer;  pad 0, 1, 2 or 3
*	bytes in the last word in Buffer with spaces, if the Nchrs is
*	not a multiple of 4.

	INTEGER I, J
	INTEGER SEL

	I=1
	J=1
	Nwords=0
	SEL=4 !Set this in case of funny Nchrs or Buffer_size.

	DO WHILE ( (J.LE.Buffer_size) .AND. (I.LE.Nchrs) )
	  SEL=MOD(I-1,4)+1 !Cycles: 1, 2, 3, 4, 1, ...
	  CALL STRPUTCHR( CHRS(I:I), SEL, Buffer(J) ) !Machine-independent order.
	  IF      (SEL.EQ.1) THEN !Broke ground on a new word.
	    Nwords=Nwords+1
	  ELSE IF (SEL.GE.4) THEN !Will break ground on a new word on the next char.
	    J=J+1
	  END IF
	  I=I+1 !Next character.
	END DO !WHILE ( (J.LE.Buffer_size) .AND. (I.LE.Nchrs) )

*	Pad with 0, 1, 2 or 3 spaces, as needed:
*	(This loop is skipped if first loop is skipped.)
	SEL = SEL+1 !Next byte, of four in a longword, not yet used.
	DO WHILE (SEL.LE.4) !Done if SEL is now 5.
	  CALL STRPUTCHR( ' ', SEL, Buffer(Nwords) )
	  SEL=SEL+1
	END DO !WHILE (SEL.LE.4)


	RETURN
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
*	Ask the operator, found on INLUN and OUTLUN, for the answer to the
*	.le. 80 character question Q.  The default .le. 80 character answer
*	is specified by the calling program in D.  The answer is returned
*	in A.  The last non-blank answer is returned.  STRQUERY does not
*	exit until a blank answer or EOF is specified.
*	If OUTLUN is 0, then nothing is output and the first input from
*	INCOM is accepted & returned.

*	STRQUERY = .TRUE. for normal return,
*	STRQUERY = .FALSE. for EOF on INLUN.

	INTEGER LQ,LD,LA,L
	CHARACTER*80 C80,B80

101	FORMAT(' 'A/' 'A)
102	FORMAT(' 'A' [ 'A' ] '$)
201	FORMAT(A)

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

2000	STRQUERY=.FALSE.
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
*	Ask the operator, found on INLUN and OUTLUN, for the answer to the
*	.le. 80 character question Q.  The default .le. 80 character answer
*	is specified by the calling program in D.  The answer is returned
*	in A.  The first non-blank answer is returned.
*	If OUTLUN is 0, then nothing is output and the first input from
*	INCOM is accepted & returned.

*	STRQUERY1 = .TRUE. for normal return,
*	STRQUERY1 = .FALSE. for EOF on INLUN.

	INTEGER LQ,LD,LA,L
	CHARACTER*80 C80,B80

101	FORMAT(' 'A/' 'A)
102	FORMAT(' 'A' [ 'A' ] '$)
103	FORMAT(' 'A)
104	FORMAT(' 'A' '$)
201	FORMAT(A)

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
	IF (L.LT.73) THEN !Use short version:
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

2000	STRQUERY1=.FALSE.
	RETURN
	END
*
	SUBROUTINE STRTIMCNV(TIME,ASC)

	IMPLICIT NONE

*  Input:
	INTEGER TIME(2) !TIME(1) is seconds since midnight, 1-jan-1970.
	                !TIME(2) is (additional) days since 1-jan-1970.
*  Output:
	CHARACTER*23 ASC !ASCII representation of TIME: dd-mmm-yy_hh:mm:ss_____
                         !("_" = "blank" -- last five characters are blank.)

*  Brief Description:  Convert seconds since 1970 to 23-char ASCII date-time.

*  Description:
*	Convert TIME to 23 character ASCII years, months, days,
*	hours, minutes and seconds.

	INTEGER HOURS,MINS,SECS
	INTEGER DAYS,DAY,MON,YEARS,YEAR,LEAPS
	INTEGER BAL

	INTEGER MONTH_DAYS(12)
	CHARACTER*3 AMON(12)
	DATA MONTH_DAYS/31,28,31,30,31,30,31,31,30,31,30,31/
	DATA AMON/'Jan','Feb','Mar','Apr','May','Jun'
     1	         ,'Jul','Aug','Sep','Oct','Nov','Dec'/

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

*	Assume TIME=0 occurs at 1-jan-1970;  1996 is 7 leaps later.
	IF (LEAPS.GE.8) LEAPS=LEAPS-1 !Skip a leap year in 2000 -- next skip in 2400.

	YEARS=(DAYS-LEAPS)/365   !Year-zero is 1970, here.
	YEAR=MOD((YEARS+70),100) !Give year in century.
	DAY=DAYS-LEAPS-365*YEARS !Day in year.

*	Make DAY the day in the month:
	MON=1
	DO WHILE (  (MON.LT.12)  .AND.
     1	            ( (DAY-MONTH_DAYS(MON)) .GT. 0 )  )
	  DAY=DAY-MONTH_DAYS(MON)
	  MON=MON+1
	END DO

	DAY = DAY + 1 !1st day of month is 1, not 0.

	WRITE(ASC,101) DAY,AMON(MON),YEAR,HOURS,MINS,SECS
101	FORMAT(I2'-'A3'-'I2' 'I2.2':'I2.2':'I2.2)

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
*	Return a character string (LINE_OUT) consisting of a line of
*	input text, with date, weekday and time info appended.

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
     1	  'Thursday','Friday!','Saturday'/

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

101	FORMAT(A'  :  'A9', 'A9,2X,A8)

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
*	Decode the character string in S as a number.
*	Put the result in UNREAL and UNINT, if possible.
*	Return STRUNCODE = :
*	-1	error - nothing was uncoded.
*	 0	a real number was decoded, but the result was too big
*		to be converted into integer form.
*	 1	a real number was decoded, and exists in integer form, too.

*	To set the default conversion radix to other than 10,
*	set REQRAD to 2, 8, or 16 - otherwise it's assumed decimal.
*	Numbers of radices 2, 8 and 16 are allowed, the radix being
*	indicated by prefixing the ASCII representation of the number
*	with a '%' for radix 2, '"' or 'O' for radix 8, and 'H' or 'X'
*	for radix 16.  The prefix and the number may not be separated
*	by any character, such as a space.
*	If radix 10 is desired and the default radix isn't 10, use the
*	decimal radix indicator '$'.

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
*	DATA RIMAX/2147483647./ !Exact value.
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

*	Have the end of the character group; END points at the last non-bl.
*	Is a radix specified with the representation (overides REQRAD)?
	RADCHAR=C80(1:1) !1st character is where radix specifier would be.
	RADID=INDEX('"OXH%$',RADCHAR) !Octal,Octal,Hex,Hex,Binary,Decimal.
	IF (RADID.LE.0) THEN !There's no radix indicator;  use default:
	  START=1
	  NCHAR=END
*	  Check the default radix:
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
*	  This is just a quick and dirty remap:
	  RADID=(RADID+1)/2 !2 indicators for each radix ('cept B & D).
	END IF

	IF (RADID.LE.0) THEN !Decimal conversion:
*	  Decimal radix - either by default or by indicator:
	  PPT=INDEX(C80(START:END),'.')
	  IF (PPT.LE.0) THEN !No decimal point.
	    NDCHAR=0
	  ELSE
	    NDCHAR=END-PPT !# of chars. in the decimal field.
	  END IF

*	  Make the format statement:
	  WRITE(FORMAT,101,ERR=5000) NCHAR,NDCHAR
101	  FORMAT('(D'I2'.'I2')')

*	  Try to decode the character group as a real number:
	  READ(C80(START:END),FORMAT,ERR=5000) UNREAL

	  STRUNCODE=0 !Indicate success as a real number.

	  IF (UNREAL.GT.RIMAX) RETURN !Not an integer.
	  IF (UNREAL.LT.-(RIMAX+1.)) RETURN !Not an integer.

*	  Obtain the integer value, too:
	  UNINT=NINT(UNREAL)
	  STRUNCODE=1 !Indicate successful integer conversion.
	  RETURN

	END IF !RADID.LE.0


*	Number should be converted according to a non-decimal radix:

	IF ((NCHAR.LE.0).OR.(NCHAR.GT.MAXCHR(RADID))) THEN
	  GO TO 5000 !error.
	END IF

	SIGN=.FALSE. !The sign bit flag (not used as a sign bit, here).
	ACCUM=0 !The result will be accumulated here.
*	Cycle through the digits  *****************************
	DO DPT=1,NCHAR
	  CBYTE=C80(DPT+START-1:DPT+START-1)
	  IBYTE=ICHAR(CBYTE) !For arithmetic opeations.
	  IF (LGT(CBYTE,MAXNUM(RADID))) THEN
	    GO TO 5000 !Illegal character.
	  ELSE IF (IBYTE.LE.0) THEN
	    GO TO 5000 !Illegal character.
	  ELSE IF (RADID.EQ.2) THEN !Hex.
*	    Special treatment for hex:
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
*	    The first digit may need special attention.
	    IF (NCHAR.LT.MAXCHR(RADID)) THEN !Nothing special.
	    ELSE IF (IBYTE.LT.SGNBIT(RADID)) THEN !Nothing special.
	    ELSE IF (IBYTE.GT.MAXFST(RADID)) THEN !Error.
	      GO TO 5000 !Illegal 1st digit.
	    ELSE !Sign bit must be used (MSB of 16 bit unsigned integer).
*	      (Be careful not to do arithmetic with sign bit.)
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

*	Done:
	STRUNCODE=1 !Indicate successful integer conversion.
	RETURN

*	Errors:
5000	CONTINUE
	STRUNCODE=-1
	RETURN
	END
*
	LOGICAL FUNCTION STRVER
     1	  (FILE_NAME_IN,VLIMIT,VCHAR,VNEXT,FILE_NAME_OUT)

	IMPLICIT NONE

*	Inputs:

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
*	.true.  -- If a name was successfully generated.
*	.false. -- If a name was not generated, which would occur if VNEXT
*	           is specified as negative, or if VNEXT exceeds VLIMIT, or
*	           if FILE_NAME_IN is blank.

*  Description:
*	Make file "version" numbers & append it to the file name.
*	The file name constructed is "hypothetical", that is, it may
*	or may not work.  It is intended to be used in a FORTRAN OPEN
*	statement as the value to the "FILE=" keyword.  The OPEN
*	statement should specify an error return;  if the STRVER-
*	constructed filename fails in the OPEN, STRVER should be
*	called again, and it will return a new file name, with the
*	next version number, and the process repeated until the OPEN
*	succeeds or until VNEXT is higher than the calling routine's
*	specified limit, VLIMIT.


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
100	  FORMAT(A1,I1)
	ELSE IF (VNEXT.LT.100) THEN
	  Len_ver=3
	  WRITE(VERSION,101) VCHAR,VNEXT
101	  FORMAT(A1,I2.2)
	ELSE IF (VNEXT.LT.1 000) THEN
	  Len_ver=4
	  WRITE(VERSION,102) VCHAR,VNEXT
102	  FORMAT(A1,I3.3)
	ELSE IF (VNEXT.LT.10 000) THEN
	  Len_ver=5
	  WRITE(VERSION,103) VCHAR,VNEXT
103	  FORMAT(A1,I4.4)
	ELSE IF (VNEXT.LT.100 000) THEN
	  Len_ver=6
	  WRITE(VERSION,104) VCHAR,VNEXT
104	  FORMAT(A1,I5.5)
	ELSE IF (VNEXT.LT.1 000 000) THEN
	  Len_ver=7
	  WRITE(VERSION,105) VCHAR,VNEXT
105	  FORMAT(A1,I6.6)
	ELSE IF (VNEXT.LT.10 000 000) THEN
	  Len_ver=8
	  WRITE(VERSION,106) VCHAR,VNEXT
106	  FORMAT(A1,I7.7)
	ELSE IF (VNEXT.LT.100 000 000) THEN
	  Len_ver=9
	  WRITE(VERSION,107) VCHAR,VNEXT
107	  FORMAT(A1,I8.8)
	ELSE IF (VNEXT.LT.1 000 000 000) THEN
	  Len_ver=10
	  WRITE(VERSION,108) VCHAR,VNEXT
108	  FORMAT(A1,I9.9)
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

*	WRITE(6,301) Len_in,FILE_NAME_IN(1:Len_in)
*     1	            ,VERSION,LEN(FILE_NAME_OUT)
*     1	            ,Local_filename
*301	FORMAT(' STRVER-D1-------------------------------------'/
*     1	       ' Len_in:'I11' FILE_NAME_IN(1:Len_in):'/
*     1	       ' 'A/
*     1	       ' VERSION:'A'   Len(file_name_out):'I11' out-file:'/
*     1	       ' 'A)


	Vnext=Vnext+1
	STRVER=.true.
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
*	Return 9 character ASCII weekday corresponding to the specified
*	year/month/day.

	INTEGER I,LEAPS,KEY,IYEAR

	CHARACTER*9 WEEKDAY(7)
	INTEGER CODE(12)
	LOGICAL LEAP
	DATA WEEKDAY/'Sunday','Monday','Tuesday','Wednesday',
     1	  'Thursday','Friday!','Saturday'/
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
	LOGICAL FUNCTION STRWID(BEG,WIDTH,END,S)

	IMPLICIT NONE

*  Inputs:
	INTEGER BEG   !Index to S -- make a gap in S starting here.
	INTEGER WIDTH !Width of gap to make in S.
	INTEGER END   !Effective end-of-string in S (can't use LEN(S)).

*  Input/Output:
	CHARACTER*(*) S !String to be "widenned".

*  Description:
*	Move everything in S, from and including BEG to END, up by WIDTH.
*	Effectively widen or create a gap in S. WIDTH is not negative.
*	BEG is left to point at the first position of the new gap.
*	Returns .true. for success, .false. for failure.

	INTEGER SPT,L

	L=LEN(S)
	IF ((END+WIDTH).GT.L) THEN !Not enough room in string.
	  STRWID=.FALSE. !Failure.
	  RETURN
	END IF
	DO SPT=END,BEG,-1 !Work backwards -- it's easier this way.
	  S(SPT+WIDTH:SPT+WIDTH)=S(SPT:SPT)
	END DO
	STRWID=.TRUE. !Success.
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
*	Get the number of seconds since 1-Jan-1970, 00:00:00 GMT, of the specified date-time.

	INTEGER Year_1970, Month_1970, Day_1970
	INTEGER Days_Since_1970
	INTEGER Seconds_Since_Today

*	The beginning date of 1970:
	DATA Year_1970 / 1970 /
	DATA Month_1970 / 1 /
	DATA Day_1970   / 1 /


*	Get number of days since 1970 began:
	CALL STRDAYSINCE( Year, Month, Day, Year_1970, Month_1970, Day_1970, Days_Since_1970 )

	Seconds_Since_Today = ( Hour * 60 + Min ) * 60 + Sec !Today's contribution.
	Seconds_Since_1970  = Days_Since_1970 * 24 * 3600 + Seconds_Since_Today
	RETURN
	END


*
	SUBROUTINE STR_Axis_Tick( Xmin, Xmax, Max_Nticks, Nticks, Xtick, Nlabels, Xlabel, Clabel, Iscale, Cexponent )

	IMPLICIT NONE

*  Inputs:
	REAL    Xmin
	REAL    Xmax
	INTEGER Max_Nticks !Maximum number of ticks.

*  Outputs:
	INTEGER       Nticks    !Number of ticks.
	REAL          Xtick(*)  !Coordinate of each tick.
	INTEGER       Nlabels   !No. of labels.
	REAL          Xlabel(*) !Coordinate of each label
	CHARACTER*(*) Clabel(*) !Each label.
	INTEGER       Iscale    !Log-10 of scale factor of resulting output tick-labels.
	CHARACTER*(*) Cexponent !Log-10 of scale factor as left-justified text.

*  Description:
*	Fill the output arrays with sensible axis tick-positions and labels, corresponding
*	to the given range and maximum number of ticks.


	INTEGER     Nticks_skip_alternate
	PARAMETER ( Nticks_skip_alternate = 6 )

	INTEGER Nticks_all
	INTEGER IXformat
	INTEGER Itick
	REAL XRmin, XRmax
	REAL DXtick
	REAL XP

	CHARACTER*132 M132(2)
	CHARACTER*6 C6
	CHARACTER*3 C3
	CHARACTER*6 Xformat(4)

	REAL XS, XSmax(3)

	REAL DigErrEven , DigErrOdd
	REAL DigErrEven1, DigErrOdd1
	REAL DigErrEven2, DigErrOdd2

	LOGICAL Label_On, ODD, Even_Odd_Selection_Made, First_is_Odd
	LOGICAL Toggling, Toggling_From_One

	INTEGER IDT1
	SAVE    IDT1
	INTEGER IDE1, IDE2, IDE3, IDE4
	SAVE    IDE1, IDE2, IDE3, IDE4

	LOGICAL MSG_Enabled

	DATA    IDT1 / 0 /
	DATA    IDE1, IDE2, IDE3, IDE4 / 0, 0, 0, 0 /

*	The 4th entry is "just in case" (it doesn't fix anything but prevents a possible crash):
	DATA Xformat/ '(F6.2)', '(F6.1)', '(F6.0)', '(F6.0)' /
	DATA XSmax  /  100.0  , 1000.0  , 10000.0  /


*	Tick marks:
	XRmin = Xmin
	XRmax = Xmax

*	Auto-scale/rescale, and get the number of ticks to use for X:
	CALL STR_Scale_Tick( XRmin, XRmax, Nticks_All, Iscale ) !Range scaled to (-1,1).
	Iscale = Iscale - 1 !Shift labels from (-1,1) to (-10,10)

	IF ( Nticks_All .GT. 1 ) THEN
	  DXtick = ( XRmax - XRmin ) / FLOAT( Nticks_All - 1 )
	ELSE !Protect against the bizarre (shouldn't happen):
	  CALL Message( 'STR_Axis_Tick-E3  One or fewer axis ticks!  Bug?', 1, IDE3 )
	  Nticks_All = 2
	  DXtick = XRmax - XRmin
	END IF

	IF ( MSG_Enabled( 'STR_Axis_Tick-T1', IDT1 ) ) THEN
	  WRITE(M132,301) Xmin, Xmax, Nticks_All, Iscale, DXtick, XRmin, XRmax
301	FORMAT( 'STR_Axis_Tick-T1 (1) Xmin:' E15.8 '  Xmax:' E15.8 ' Nticks_All:' I3 ' Iscale:' I3 /
     1	        '                   DXtick:' E15.8 ' XRmin:' E15.8 '      XRmax:' E15.8 )
	  CALL Message( M132, 2, IDT1 )
	END IF

	Nticks = 0 !Count only "visible" ticks:
	DO Itick=1, Nticks_All
	  XP = FLOAT( Itick - 1 ) * DXtick + XRmin
	  IF      ( XP .LT. Xmin ) THEN !Don't plot this one.
	  ELSE IF ( XP .GT. Xmax ) THEN !Don't plot this one.
	  ELSE IF ( Nticks .GE. Max_Nticks ) THEN !No more space.
	    CALL Message( 'STR_Axis_Tick-E2  Insufficient tick-array space;  increase Max_Nticks.', 1, IDE2 )
	  ELSE !Plot this one:
	    Nticks = Nticks + 1
	    Xtick( Nticks ) = XP
	  END IF
	END DO


	IF ( Nticks .GT. Nticks_skip_alternate ) THEN !Don't skip every other tick if only a few ticks.
	  Toggling = .TRUE. !Toggle -- every other tick gets labelled.
	  Toggling_from_one = .FALSE.

C	  Number X axis every other tick -- but is even or odd better?

	  XP = XRmin !1st is defined as odd -- no meaning attached, otherwise.
	  DigErrOdd1 = ABS( FLOAT( INT( XP/10.**Iscale ) )
     1	                            - ( XP/10.**Iscale )   )
	  XP = XRmin + 2.*DXtick !Try the next one, too.
	  DigErrOdd2 = ABS( FLOAT( INT( XP/10.**Iscale ) )
     1	                            - ( XP/10.**Iscale )   )

	  DigErrOdd  = MIN( DigErrOdd1, DigErrOdd2 ) !Take the better one.


	  XP = XRmin + DXtick
	  DigErrEven1 = ABS( FLOAT( INT( XP/10.**Iscale ) )
     1	                             - ( XP/10.**Iscale )   )
	  XP = XRmin + 3.*DXtick !Try the next one, too.
	  DigErrEven2 = ABS( FLOAT( INT( XP/10.**Iscale ) )
     1	                             - ( XP/10.**Iscale )   )

	  DigErrEven = MIN( DigErrEven1, DigErrEven2 ) !Take the better one.


	  IF ( MSG_Enabled( 'STR_Axis_Tick-T1', IDT1 ) ) THEN
	    WRITE( M132, 303 ) DigErrEven, DigErrOdd
	    CALL MESSAGE( M132, 1, IDT1 )
	  END IF
303	FORMAT( 'STR_Axis_Tick-T1 (3) DigErrEven:' E15.8 '  DigErrOdd:' E15.8 )


	  IF ( ABS( DigErrOdd - DigErrEven ) .LT. 0.0005 ) THEN !Either is just as good.

*	    If the X range includes zero, pick odd or even to put a label at zero (!!):
	    IF ( (Xmin.LE.0.) .AND. (Xmax.GE.0.) ) THEN !Zero is included -- center up on zero:
	      Odd = .TRUE. !Start odd, then toggle each time.
	      DO Itick = 1, Nticks_All
*	        X in WorldCoords:
	        XP = FLOAT( Itick - 1 ) * DXtick + XRmin
	        IF      ( XP .LT. Xmin ) THEN !Wouldn't plot this one.
	        ELSE IF ( XP .GT. Xmax ) THEN !Wouldn't plot this one.
	        ELSE !(Would try to) Plot this one:
	          IF ( ABS(XP) .LT. 0.0005 ) THEN !Close enough to zero:
	            First_is_Odd = Odd !Start this way: odd or not odd.
	            Even_Odd_Selection_Made = .TRUE.
	          END IF
	        END IF
	        Odd = .NOT. Odd
	      END DO

*	    If the X range includes one, and not zero, pick odd or even to put a label at one:
	    ELSE IF ( (Xmin.LE.1.) .AND. (Xmax.GE.1.) ) THEN !One is included -- center up on one:
	      Odd = .TRUE. !Start odd, then toggle each time.
	      DO Itick = 1, Nticks_All
*	        X in WorldCoords:
	        XP = FLOAT( Itick - 1) * DXtick + XRmin
	        IF      ( XP .LT. Xmin ) THEN !Wouldn't plot this one.
	        ELSE IF ( XP .GT. Xmax ) THEN !Wouldn't plot this one.
	        ELSE !(Would try to) Plot this one:
	          IF ( ABS(XP-1.0) .LT. 0.0005 ) THEN !Close enough to one:
	            First_is_Odd = Odd !Start this way: odd or not odd.
	            Toggling_from_one = .TRUE.
	            Even_Odd_Selection_Made = .TRUE.
	          END IF
	        END IF
	        Odd = .NOT. Odd
	      END DO

	    END IF !(Xmin.LE.0.).AND.(Xmax.GE.0)

	  ELSE IF (DigErrOdd .LT. DigErrEven) THEN !Do Odd ticks (1st tick, etc.):
	    First_is_Odd=.TRUE.
	    Even_Odd_Selection_Made=.TRUE.
	  ELSE !Do Even ticks (2nd tick, etc.):
	    First_is_Odd=.FALSE.
	    Even_Odd_Selection_Made=.TRUE.
	  END IF

	  IF (.NOT.Even_Odd_Selection_Made) THEN !The question is still not resolved:
	    First_is_Odd=.TRUE.
	  END IF !.NOT.Even_Odd_Selection_Made

	ELSE !Always output the tick labels -- not enough to skip:
	  Toggling=.FALSE. !Don't toggle -- Each tick gets labelled.
	  Toggling_from_one=.FALSE.
	  First_is_Odd=.TRUE.

	END IF

*	Adjust the X scale to be divisible by three and choose one of three formats:
	IF (Iscale.EQ.0) THEN
	  IXformat=1
	ELSE IF (Iscale.EQ.3) THEN !Special case -- allow thousands:
	  IXformat=3
	  Iscale=0 !Don't scale.
	ELSE IF (Iscale.GT.0) THEN
	  IXformat=MOD(Iscale,3)+1
	  IF (IXformat.LE.0) THEN
	    WRITE(M132,501) IXformat, Iscale
	    CALL MESSAGE(M132,1,-1)
	    IXformat=1
	  ELSE
	    Iscale =3*((Iscale)/3)
	  END IF
	ELSE
	  IXformat=MOD(-Iscale+1,3)+1
	  IF (IXformat.LE.0) THEN
	    WRITE(M132,501) IXformat, Iscale
	    CALL MESSAGE(M132,1,-1)
	    IXformat=1
	  ELSE
	    Iscale =3*((Iscale-2)/3)
	  END IF
	END IF
501	FORMAT( 'STR_Axis_Tick-E1  Invalid format index:'I11'  Iscale:'I11)


	IF ( MSG_Enabled( 'STR_Axis_Tick-T1', IDT1 ) ) THEN
	  WRITE(M132,304) Iscale, IXformat, Xformat(IXformat)
	  CALL MESSAGE(M132,1,IDT1)
	END IF
304	FORMAT( 'STR_Axis_Tick-T1 (4) Iscale:' I3'  IXformat:' I2'  Xformat[' A '].' )


	Label_On = First_is_Odd
	Nlabels = 0
	DO Itick = 1, Nticks_All
*	  X in WorldCoords:
	  XP = FLOAT( Itick - 1 ) * DXtick + XRmin
	  IF      ( XP .LT. Xmin ) THEN !Don't plot this one.
	  ELSE IF ( XP .GT. Xmax ) THEN !Don't plot this one.
	  ELSE IF ( Nlabels .GE. Max_Nticks ) THEN
	  ELSE !(try to) Plot this one:
	    IF (Label_On) THEN
	      Nlabels = Nlabels + 1
	      XS = XP/(10.**Iscale)
	      IF ( ABS(XS) .GE. XSmax(IXformat) ) THEN !This protects against rare "overflows".
	        WRITE(C6,Xformat(IXformat+1)) XS
	      ELSE                                !"Normal" case:
	        WRITE(C6,Xformat(IXformat)) XS
	      END IF
	      Xlabel( Nlabels ) = XP
	      Clabel( Nlabels ) = C6
	    END IF
	  END IF
	  IF (Toggling) THEN !Toggling is enabled -- every other tick gets labelled.
	    Label_On=.NOT.Label_On
	  END IF
	END DO


*	Make the X scale exponent:
	IF (Iscale.NE.0) THEN !Only put up the scale if not unity:
	  IF (Iscale.LT.-9) THEN
	    WRITE(C3,'(I3)') Iscale
	  ELSE IF (Iscale.LT.0) THEN
	    WRITE(C3,'(I2)') Iscale
	  ELSE IF (Iscale.LT.10) THEN
	    WRITE(C3,'(I1)') Iscale
	  ELSE IF (Iscale.LT.100) THEN
	    WRITE(C3,'(I2)') Iscale
	  ELSE
	    C3='big'
	  END IF

	  Cexponent = C3

	END IF !Iscale.NE.0



	RETURN

	END




*
	SUBROUTINE STR_Scale_Tick(Amin, Amax, Nticks, Iscale)

	IMPLICIT NONE

*  Input/outputs:
	REAL Amin !Lower Bound
	REAL Amax !Upper Bound

*  Outputs:
	INTEGER Nticks
	INTEGER Iscale !The tens-exponent of the scaled range.

*  Description:
*	Automatically determine the number (and spacing) of ticks to 
*	use on a scale with range (Amin, Amax).  The range may be slightly
*	extended, if necessary.  Iscale is a power of ten which, when applied
*	to Amin and Amax, reduces the range to (-10,10) or less, but not less
*	than (-1,1).


	INTEGER IS0,ISL,IS,ISR
	REAL AS,AS0,ASL,DIFF,EXTEND

	INTEGER Ntick_patterns_P
	PARAMETER (Ntick_patterns_P=12)
	INTEGER DTICKS(Ntick_patterns_P)

*	DATA DTICKS/ 9,  7, 11, 7,  9, 11, 6, 7,  8,  9/ !Don't include the ends.
	DATA DTICKS/11,  9, 13, 9, 11, 13, 8, 9, 10, 11
     1	           ,12, 13 / !Include the ends.


	DIFF=Amax-Amin !Declared range.

	IF (DIFF.LT.0.010) THEN !Special action:
	  EXTEND=(0.010-DIFF)/2. !Extend upper & lower bounds by this.
	  Amin=Amin-EXTEND
	  Amax=Amax+EXTEND
	  DIFF=0.010
	END IF

	AS=LOG10(DIFF) !Orders-of-magnitude spanned.
	IS=INT(AS)     !Integer orders-of-magnitude (scale).
	IF (FLOAT(IS).LT.AS) IS=IS+1 !Ensure it's greater than or equal to AS.

*	Get "cleanly extended" upper limit:
	ASL=Amax/(10.**IS) !Scaled upper limit: (-1.0,1.0)
	ISL=INT(ASL*10.)   !Integer scaled upper limit: (-10,10)
	IF (FLOAT(ISL)/10..LT.ASL) ISL=ISL+1 !Ensure integer exceeds real.
	ASL=FLOAT(ISL)*10.**IS/10.  !Cleanly-extended upper limit.

*	Get "cleanly extended" lower limit:
	AS0=Amin/(10.**IS) !Scaled lower limit: (-1.0,1.0)
	IS0=INT(AS0*10.)   !Integer scaled lower limit: (-10,10)
	IF (FLOAT(IS0)/10..GT.AS0) IS0=IS0-1 !Ensure integer does not exceed real.
	AS0=FLOAT(IS0)*10.**IS/10.  !Cleanly-extended lower limit.

	ISR=ISL-IS0  !Scaled-integer-range

	IF (ISR.LE.0) THEN
	  Nticks=5
	ELSE IF (ISR.LE.Ntick_patterns_P) THEN
	  Nticks=DTICKS(ISR)
	ELSE
	  Nticks=ISR-1
	  DO WHILE (Nticks.GT.11)
	    Nticks=Nticks/2
	  END DO
	END IF

*	Return the cleanly-extended range:
	Amin=AS0
	Amax=ASL

*	Return the scale (exponent):
	Iscale=IS

	RETURN
	END

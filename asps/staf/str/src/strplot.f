
	SUBROUTINE STR_Scale_Tick(Amin, Amax, Nticks, Iscale)

	IMPLICIT NONE

*  Input/outputs:
	REAL Amin !Lower Bound
	REAL Amax !Upper Bound

*  Outputs:
	INTEGER Nticks
	INTEGER Iscale !The tens-exponent of the scaled range.

*  Functional Description:
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


*       IBM 3090 versions.

        REAL FUNCTION RAN(ISEED)
*       This is meant to provide VMS look-alike random number generator.
*       ISEED isn't used at all - VMS ISEED and IBM SEED are different things.
        IMPLICIT NONE
        INTEGER ISEED
        REAL SEED
        SAVE SEED
        REAL RAND
        DATA SEED/0.5/

*       CALL SRAND(SEED)
*       SEED=RAND()
        RAN=SEED

        RETURN
        END

        SUBROUTINE STRCPU(TCPU)
        IMPLICIT NONE
        INTEGER TCPU
        DOUBLE PRECISION NATCPU_T0
        COMMON/NATCPU/NATCPU_T0
        DOUBLE PRECISION CPU_TIME
        INTEGER RCODE

        CALL CPUTIME(CPU_TIME,RCODE) !These times come in usec.

        TCPU=(CPU_TIME-NATCPU_T0)/10000. !Convert to 100 ticks/sec.

        RETURN
        END

        SUBROUTINE STRCPUQ(TCPU) !Quad (64-bit) version.
        IMPLICIT NONE
        INTEGER TCPU(2)
        DOUBLE PRECISION NATCPU_T0
        COMMON/NATCPU/NATCPU_T0
        DOUBLE PRECISION CPU_TIME
        INTEGER RCODE

        CALL CPUTIME(CPU_TIME,RCODE) !These times come in usec.

        TCPU(1)=(CPU_TIME-NATCPU_T0)/10000. !Convert to 100 ticks/sec.
        TCPU(2)=0

        RETURN
        END

        SUBROUTINE STRCPU0
*       "Initialize" CPU elapsed time counter.
        IMPLICIT NONE
        DOUBLE PRECISION NATCPU_T0
        COMMON/NATCPU/NATCPU_T0
        DOUBLE PRECISION CPU_TIME
        INTEGER RCODE

        CALL CPUTIME(CPU_TIME,RCODE) !These times come in usec.
        NATCPU_T0=CPU_TIME !Don't convert here (to 1/100 sec).
        RETURN
        END

        INTEGER FUNCTION STRCPUTPS()

        IMPLICIT NONE

*  Return value:
*       Native cpu clock ticks-per-second.

        STRCPUTPS=100 !On IBM 3090, it's 100 per second.

        RETURN
        END

        SUBROUTINE STRDATE(YEAR,MONTH,DAY)
*       Handle a call to an IBM 3090 time routine -- 4-digit year.
        IMPLICIT NONE
        INTEGER YEAR,MONTH,DAY
        INTEGER NOW(8),HOUR,LAST_DAY(12)
        DATA LAST_DAY/31,28,31,30,31,30,31,31,30,31,30,31/

        CALL DATIM(NOW)
        IF (NOW(1).LT.0) THEN !Error -- return this noteworthy date:
          YEAR=0
          MONTH=1
          DAY=1
        ELSE
          YEAR=NOW(8) !4 digit year.
          MONTH=NOW(7)
          DAY=NOW(6)

*         If the bleeding system would only return local time, this would
*         be done.  But it doesn't and therefore this isn't:
          HOUR=NOW(5)-5 !Convert to EST, from UT (GMT).

*         Now this must check for negative hours and then the whole stinking
*         Gregorian calender must be invoked:
          IF (HOUR.LT.0) THEN !Must go back a day:
*           HOUR=HOUR+24 !Don't really need this any more -- comment out.
            DAY=DAY-1
            IF (DAY.LE.0) THEN !Must go back a month:
              MONTH=MONTH-1
              IF (MONTH.LE.0) THEN !Must go back a year:
                MONTH=12
                YEAR=YEAR-1
              END IF
              DAY=LAST_DAY(MONTH)
              IF (MONTH.EQ.2) THEN !Watch out for February in a leap year!
*               The Gregorian calender -- leaps @4, except @100 unless @400:
                IF (MOD(YEAR,4).EQ.0) THEN !Could be a leap year:
                  IF (MOD(YEAR,100).EQ.0) THEN !Maybe not a leap year:
                    IF (MOD(YEAR,400).EQ.0) THEN !It's a leap year:
                      DAY=29
                    END IF
                  ELSE !It's a leap year:
                    DAY=29
                  END IF
                END IF
              END IF
            END IF
          END IF
        END IF

        RETURN
        END

        SUBROUTINE STRDEC_ENDIAN(I32)

*       Swap the two 16-bit half-words in the 32-bit (long) word I32,
*       if needed, to make the big/little endian business come out
*       DEC-style.  On IBM 3090 (here), the swapping is done.

        IMPLICIT NONE

        INTEGER I32
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

        SUBROUTINE STRDEC_ENDIAN_BYTE(I32)

*       Swap the 4 8-bit bytes in the 32-bit (long) word I32,
*       if needed, to make the big/little endian business come out
*       DEC-style.  On IBM 3090 (here), the swapping is done.

        IMPLICIT NONE

        INTEGER I32
        INTEGER I4
        LOGICAL*1 I1(4),I1SAVE
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

        INTEGER FUNCTION STRDEC_IBITS(DATA,BIT0,BITS)

*       Do an IBITS, but make it act like DEC.

        IMPLICIT NONE

        INTEGER DATA,BIT0,BITS,DATA_HI,SHIFT_UP,SHIFT_DOWN

*       Shift up so that first bit out of range is shifted off into the bucket:
        SHIFT_UP=31-(BIT0+BITS-1)
*       Shift it back down so that BIT0 is shifted down to bit position 0:
        SHIFT_DOWN=(BIT0-31)-SHIFT_UP

*       The actual shifts:
        DATA_HI=ISHFT(DATA,SHIFT_UP)
        STRDEC_IBITS=ISHFT(DATA_HI,SHIFT_DOWN)


        RETURN
        END

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

        SUBROUTINE STRTIME(HOUR,MIN,SEC)
        IMPLICIT NONE
        INTEGER HOUR,MIN,SEC
        INTEGER NOW(8)

        CALL DATIM(NOW)
        IF (NOW(1).LT.0) THEN !Error -- return midnight:
          HOUR=0
          MIN=0
          SEC=0
        ELSE
          HOUR=NOW(5)-5 !Convert from UT (GMT) to EST.
          IF (HOUR.LT.0) HOUR=HOUR+24 !Yesterday, still.
          MIN=NOW(4)
          SEC=NOW(3)
        END IF
        RETURN
        END


C-----------------------------------------------------------------------
C.....This is a subroutine to set the values of the HWBMCH Common Block
C.....John Novak September 6, 2012, 2:59pm EST
C-----------------------------------------------------------------------
      SUBROUTINE setbeams(PARTA,PARTB)
      IMPLICIT NONE
      CHARACTER*8 PARTA,PARTB,PART1,PART2
      COMMON/HWBMCH/PART1,PART2
      PART1=PARTA
      PART2=PARTB
      END

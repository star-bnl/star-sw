      SUBROUTINE TU_CPI2( i2in, i2out )
      IMPLICIT NONE
C   Input arguments  (One to a line with definition after ! ) 
C     i2in = input I*2
C   Output arguments : 
C     i2out = output I*2
C   Functional Description : 
C     i2out = i2in
C   Created  3-Feb-1993   Doug Olson
C   Copyright 1993    Lawrence Berkeley Laboratory
C   Error conditions : 
C----------------------------------------------------------------------
      INTEGER*2 i2in, i2out
C
      i2out = i2in
C
      RETURN
      END

      SUBROUTINE TU_CPI4( i4in, i4out )
      IMPLICIT NONE
C   Input arguments  (One to a line with definition after ! ) 
C     i4in = input I*4
C   Output arguments : 
C     i4out = output I*4
C   Functional Description : 
C     i4out = i4in
C   Created  3-Feb-1993   Doug Olson
C   Copyright 1993    Lawrence Berkeley Laboratory
C   Error conditions : 
C----------------------------------------------------------------------
      INTEGER*4 i4in, i4out
C
      i4out = i4in
C
      RETURN
      END

      SUBROUTINE TU_CPR4( r4in, r4out )
      IMPLICIT NONE
C   Input arguments  (One to a line with definition after ! ) 
C     r4in = input Real*4
C   Output arguments : 
C     r4out = output Real*4
C   Functional Description : 
C     r4out = r4in
C   Created  3-Feb-1993   Doug Olson
C   Copyright 1993    Lawrence Berkeley Laboratory
C   Error conditions : 
C----------------------------------------------------------------------
      REAL*4 r4in, r4out
C
      r4out = r4in
C
      RETURN
      END

      SUBROUTINE TU_CPR8( r8in, r8out )
      IMPLICIT NONE
C   Input arguments  (One to a line with definition after ! ) 
C     r8in = input Real*8
C   Output arguments : 
C     r8out = output Real*8
C   Functional Description : 
C     r8out = r8in
C   Created  3-Feb-1993   Doug Olson
C   Copyright 1993    Lawrence Berkeley Laboratory
C   Error conditions : 
C----------------------------------------------------------------------
      REAL*8 r8in, r8out
C
      r8out = r8in
C
      RETURN
      END


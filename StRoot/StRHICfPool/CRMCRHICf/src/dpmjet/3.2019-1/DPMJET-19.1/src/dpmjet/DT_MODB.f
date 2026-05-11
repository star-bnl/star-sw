
      SUBROUTINE DT_MODB(B,Nidx)
 
C***********************************************************************
C Sampling of impact parameter of collision.                           *
C    B          impact parameter    (output)                           *
C    NIDX       index of projectile/target material             (input)*
C Based on the original version by Shmakov et al.                      *
C This version dated 21.04.95 is revised by S. Roesler                 *
C Last change  5.5.2012 by S. Roesler.                                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION B , bb , DT_RNDM , ONE , ra , rb , TINY15 , TWO , 
     &                 x0 , x1 , x2 , y , y0 , y1 , y2 , ZERO
      INTEGER i0 , i1 , i2 , Nidx , ntarg
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,TINY15=1.0D-15,ONE=1.0D0,TWO=2.0D0)
 
      LOGICAL left , lfirst
 
C central particle production, impact parameter biasing
      INCLUDE 'inc/dtimpa'
C emulsion treatment
      INCLUDE 'inc/dtcomp'
C Glauber formalism: parameters
      INCLUDE 'inc/dtglam'
C Glauber formalism: cross sections
      INCLUDE 'inc/dtglxs'
 
      DATA lfirst/.TRUE./
 
Cc                                                            mar'04 (26-02:10)
Cc -----------------------------------------------------------------------------
Cc  here we call into the pre-processed Glauber parametrization as derived for
Cc   dpmjet-II.5x - the called routine is a carbon-copy of this subroutine with
Cc  minor changes
Cc  also, by selecting KKMAT == -2 all the other routines in connection with
Cc  the Glauber formalism usually called are skipped
Cc
      IF ( Nidx.EQ.-2 ) THEN
         CALL FL_MODB(B,Nidx,y)
         RETURN
      END IF
Cc
Cc -----------------------------------------------------------------------------
Cc
 
 
      ntarg = ABS(Nidx)
      IF ( Nidx.LE.-1 ) THEN
         ra = RASh(1)
         rb = RBSh(ntarg)
      ELSE
         ra = RASh(ntarg)
         rb = RBSh(1)
      END IF
 
      IF ( ICEntr.NE.2 ) THEN
 50      y = DT_RNDM(bb)
         i0 = 1
         i2 = NSIteb
 100     i1 = (i0+i2)/2
         left = ((BSIte(0,1,ntarg,i0)-y)*(BSIte(0,1,ntarg,i1)-y))
     &          .LT.ZERO
         IF ( left ) THEN
            i2 = i1
         ELSE
            i0 = i1
         END IF
         IF ( i2-i0.LT.2 ) THEN
            i1 = i2 + 1
            IF ( i1.GT.NSIteb ) i1 = i0 - 1
         ELSE IF ( i2-i0.EQ.2 ) THEN
            i1 = i0 + 1
         ELSE
            GOTO 100
         END IF
         x0 = DBLE(i0-1)*BSTep(ntarg)
         x1 = DBLE(i1-1)*BSTep(ntarg)
         x2 = DBLE(i2-1)*BSTep(ntarg)
         y0 = BSIte(0,1,ntarg,i0)
         y1 = BSIte(0,1,ntarg,i1)
         y2 = BSIte(0,1,ntarg,i2)
C  80    CONTINUE
         B = x0*(y-y1)*(y-y2)/((y0-y1)*(y0-y2)+TINY15) + x1*(y-y0)
     &       *(y-y2)/((y1-y0)*(y1-y2)+TINY15) + x2*(y-y0)*(y-y1)
     &       /((y2-y0)*(y2-y1)+TINY15)
C*sr 5.4.98: shift B by half the bin width to be in agreement with BPROD
         B = B + 0.5D0*BSTep(ntarg)
         IF ( B.LT.ZERO ) B = x1
         IF ( B.GT.BMAx(ntarg) ) B = BMAx(ntarg)
         IF ( ICEntr.LT.0 ) THEN
            IF ( lfirst ) THEN
               lfirst = .FALSE.
               IF ( ICEntr.LE.-100 ) THEN
                  BIMin = 0.0D0
               ELSE
                  XSFrac = 0.0D0
               END IF
               CALL DT_GETBXS(XSFrac,BIMin,BIMax,ntarg)
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99010) RASh(1) , RBSh(ntarg)
     &              , BMAx(ntarg) , BIMin , BIMax , XSFrac*100.0D0 , 
     &              XSFrac*XSPro(1,1,ntarg)
99010          FORMAT (/,1X,'DT_MODB:      Biasing in impact parameter',
     &                 /,15X,'---------------------------'/,/,4X,
     &                 'average radii of proj / targ :',F10.3,' fm /',
     &                 F7.3,' fm',/,4X,'corresp. b_max (4*(r_p+r_t)) :',
     &                 F10.3,' fm',/,/,21X,'b_lo / b_hi :',F10.3,
     &                 ' fm /',F7.3,' fm',/,5X,'percentage of',
     &                 ' cross section :',F10.3,' %',/,5X,
     &                 'corresponding cross section :',F10.3,' mb',/)
            END IF
            IF ( ABS(BIMax-BIMin).LT.1.0D-3 ) THEN
               B = BIMin
            ELSE IF ( (B.LT.BIMin) .OR. (B.GT.BIMax) ) THEN
               GOTO 50
            END IF
         END IF
      ELSE IF ( ra.EQ.rb ) THEN
         bb = DT_RNDM(B)*(0.3D0*ra)**2
         B = SQRT(bb)
      ELSE IF ( ra.LT.rb ) THEN
         bb = DT_RNDM(B)*1.4D0*(rb-ra)**2
         B = SQRT(bb)
      ELSE IF ( ra.GT.rb ) THEN
         bb = DT_RNDM(B)*1.4D0*(ra-rb)**2
         B = SQRT(bb)
      END IF
 
      END SUBROUTINE


 
 
      SUBROUTINE FL_MODB(B,Nidx,Y)
Cc   ------------------------------
Cc
Cc                                                      mar'04 (26-02:48)
Cc -----------------------------------------------------------------------------
Cc
Cc  kept the logic of the original modb() - just replaced the BSITE array by
Cc  our own Barr and we also fill RBSH/RASH from the info() returned at the
Cc  same time (don't know it that information is even needed ouside of here)
Cc
Cc -----------------------------------------------------------------------------
Cc
 
C***********************************************************************
C Sampling of impact parameter of collision.                           *
C    B          impact parameter    (output)                           *
C    NIDX       index of projectile/target material             (input)*
C Based on the original version by Shmakov et al.                      *
C This version dated 21.04.95 is revised by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION B , bb , BGCms , BGLab , BLAb , DT_RNDM , EPRoj , 
     &                 GACms , GALab , ONE , PPCm , PPRoj , ra , rb , 
     &                 TINY15 , TWO , UMO , x0 , x1 , x2
      DOUBLE PRECISION Y , y0 , y1 , y2 , ZERO
      INTEGER i0 , i1 , i2 , IBProj , IBTarg , IJProj , IJTarg , IP , 
     &        IPZ , IT , ITZ , Nidx , ntarg
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,TINY15=1.0D-15,ONE=1.0D0,TWO=2.0D0)
 
      LOGICAL left , lfirst
      INCLUDE 'inc/dtimpa'
 
C emulsion treatment
      INCLUDE 'inc/dtcomp'
      INCLUDE 'inc/dtglam'
C Glauber formalism: cross sections
      INCLUDE 'inc/dtglxs'
 
Cc
Cc                                                              mar'04 18-08:28
Cc -----------------------------------------------------------------------------
C properties of interacting particles
      COMMON /DTPRTA/ IT , ITZ , IP , IPZ , IJProj , IBProj , IJTarg , 
     &                IBTarg
C Lorentz-parameters of the current interaction
      COMMON /DTLTRA/ GACms(2) , BGCms(2) , GALab , BGLab , BLAb , UMO , 
     &                PPCm , EPRoj , PPRoj
 
      REAL*8 INFo(4) , BARr(200)
      COMMON /TONI  / INFo , BARr
Cc -----------------------------------------------------------------------------
Cc
      DATA lfirst/.TRUE./
 
Cc
Cc -----------------------------------------------------------------------------
      CALL GLAUBR(PPRoj,UMO,IBProj,IT,IP,INFo,BARr)
Cc -----------------------------------------------------------------------------
Cc
      ntarg = ABS(Nidx)
 
      ra = INFo(3)
      rb = INFo(4)
 
      IF ( ICEntr.NE.2 ) THEN
 50      Y = DT_RNDM(bb)
         i0 = 1
         i2 = 80  ! <---
 100     i1 = (i0+i2)/2
         left = ((BARr(i0)-Y)*(BARr(i1)-Y)).LT.ZERO
         IF ( left ) THEN
            i2 = i1
         ELSE
            i0 = i1
         END IF
         IF ( i2-i0.LT.2 ) THEN
            i1 = i2 + 1
            IF ( i1.GT.80 ) i1 = i0 - 1
                                  ! <---
         ELSE IF ( i2-i0.EQ.2 ) THEN
            i1 = i0 + 1
         ELSE
            GOTO 100
         END IF
         x0 = DBLE(i0-1)*INFo(2)
         x1 = DBLE(i1-1)*INFo(2)
         x2 = DBLE(i2-1)*INFo(2)
         y0 = BARr(i0)
         y1 = BARr(i1)
         y2 = BARr(i2)
C  80    CONTINUE
         B = x0*(Y-y1)*(Y-y2)/((y0-y1)*(y0-y2)+TINY15) + x1*(Y-y0)
     &       *(Y-y2)/((y1-y0)*(y1-y2)+TINY15) + x2*(Y-y0)*(Y-y1)
     &       /((y2-y0)*(y2-y1)+TINY15)
C*sr 5.4.98: shift B by half the bin width to be in agreement with BPROD
         bb = B
         B = B + 0.5D0*INFo(2)
         IF ( B.LT.ZERO ) B = x1
         IF ( B.GT.INFo(1) ) B = INFo(1)
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
Cc
Cc                                                              mar'04 18-08:28
Cc -----------------------------------------------------------------------------
      RASh(1) = ra
      RBSh(ntarg) = rb
 
C     write(0,1) B,IP,IT,IJPROJ,info(3),info(4)
C   1 format('  -- fl_modb --',f13.6,' - ',3i5,' - ',4f13.6)
Cc -----------------------------------------------------------------------------
Cc
      END SUBROUTINE

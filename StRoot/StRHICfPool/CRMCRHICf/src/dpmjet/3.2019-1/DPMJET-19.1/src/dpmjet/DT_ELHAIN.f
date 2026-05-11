
      SUBROUTINE DT_ELHAIN(Ip,Pla,Elab,Cx,Cy,Cz,It,Irej)
 
C***********************************************************************
C Elastic hadron-hadron scattering.                                    *
C This is a revised version of the original.                           *
C This version dated 03.04.98 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION amp2 , amt2 , BHIB , BHIM , BLOWB , BLOWM , 
     &                 cplabp , ctcms , ctlabp , ctlabt , Cx , Cy , Cz , 
     &                 DT_RNDM , ecm , ecmp , ekin , Elab , ENNTHR , 
     &                 fmax
      INTEGER Ip , Irej , It , kproj
      DOUBLE PRECISION OHALF , ONE , pcm , PHIH , Pla , plab , pllabt , 
     &                 PLOWH , r , rr , s , splabp , stlabp , stlabt , 
     &                 t , TINY10 , tmax , tslope , TWO , ZERO
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TWO=2.0D0,ONE=1.0D0,OHALF=0.5D0,ZERO=0.0D0,
     &           TINY10=1.0D-10)
 
      PARAMETER (ENNTHR=3.5D0)
      PARAMETER (PLOWH=0.01D0,PHIH=9.0D0,BLOWB=0.05D0,BHIB=0.2D0,
     &           BLOWM=0.1D0,BHIM=2.0D0)
 
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C final state from HADRIN interaction
      INCLUDE 'inc/hnfspa'
 
C     DATA TSLOPE /10.0D0/
 
      Irej = 0
 
 
 100  plab = SQRT((Elab-AAM(Ip))*(Elab+AAM(Ip)))
      ekin = Elab - AAM(Ip)
C   kinematical quantities in cms of the hadrons
      amp2 = AAM(Ip)**2
      amt2 = AAM(It)**2
      s = amp2 + amt2 + TWO*Elab*AAM(It)
      ecm = SQRT(s)
      ecmp = OHALF*ecm + (amp2-amt2)/(TWO*ecm)
      pcm = SQRT((ecmp-AAM(Ip))*(ecmp+AAM(Ip)))
 
C nucleon-nucleon scattering at E_kin<3.5: use DT_TSAMCS(HETC-KFA)
      IF ( ((Ip.EQ.1) .OR. (Ip.EQ.8)) .AND. ((It.EQ.1) .OR. (It.EQ.8))
     &     .AND. (ekin.LT.ENNTHR) ) THEN
C   TSAMCS treats pp and np only, therefore change pn into np and
C   nn into pp
         IF ( It.EQ.1 ) THEN
            kproj = Ip
         ELSE
            kproj = 8
            IF ( Ip.EQ.8 ) kproj = 1
         END IF
         CALL DT_TSAMCS(kproj,ekin,ctcms)
         t = TWO*pcm**2*(ctcms-ONE)
 
C very crude treatment otherwise: sample t from exponential dist.
      ELSE
C   momentum transfer t
         tmax = TWO*TWO*pcm**2
         rr = (plab-PLOWH)/(PHIH-PLOWH)
         IF ( IIBar(Ip).NE.0 ) THEN
            tslope = BLOWB + rr*(BHIB-BLOWB)
         ELSE
            tslope = BLOWM + rr*(BHIM-BLOWM)
         END IF
         fmax = EXP(-tslope*tmax) - ONE
         r = DT_RNDM(rr)
         t = LOG(ONE+r*fmax+TINY10)/tslope
         IF ( t.GT.ZERO ) t = LOG(ONE+r*fmax)/tslope
      END IF
 
C   target hadron in Lab after scattering
      ELRh(2) = (TWO*amt2-t)/(TWO*AAM(It))
      PLRh(2) = SQRT(ABS(ELRh(2)-AAM(It))*(ELRh(2)+AAM(It)))
C        WRITE(*,*)'ELHAIN: T,PLRH(2) ',T,PLRH(2)
      IF ( PLRh(2).LE.TINY10 ) GOTO 100
C   projectile hadron in Lab after scattering
      ELRh(1) = Elab + AAM(It) - ELRh(2)
      PLRh(1) = SQRT(ABS(ELRh(1)-AAM(Ip))*(ELRh(1)+AAM(Ip)))
C   scattering angle of projectile in Lab
      ctlabp = (t-TWO*amp2+TWO*Elab*ELRh(1))/(TWO*plab*PLRh(1))
      stlabp = SQRT((ONE-ctlabp)*(ONE+ctlabp))
      CALL DT_DSFECF(splabp,cplabp)
C   direction cosines of projectile in Lab
      CALL DT_STTRAN(Cx,Cy,Cz,ctlabp,stlabp,splabp,cplabp,CXRh(1),
     &               CYRh(1),CZRh(1))
C   scattering angle of target in Lab
      pllabt = plab - ctlabp*PLRh(1)
      ctlabt = pllabt/PLRh(2)
      stlabt = SQRT((ONE-ctlabt)*(ONE+ctlabt))
C   direction cosines of target in Lab
      CALL DT_STTRAN(Cx,Cy,Cz,ctlabt,stlabt,-splabp,-cplabp,CXRh(2),
     &               CYRh(2),CZRh(2))
C   fill /HNFSPA/
      IRH = 2
      ITRh(1) = Ip
      ITRh(2) = It
 
      END SUBROUTINE

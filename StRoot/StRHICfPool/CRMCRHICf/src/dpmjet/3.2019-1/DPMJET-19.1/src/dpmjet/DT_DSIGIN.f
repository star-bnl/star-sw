
      SUBROUTINE DT_DSIGIN(Ire,Plab,N,Ie,Amt,Amn,Ecm,Si,Itar)
 
      IMPLICIT NONE
      DOUBLE PRECISION Amn , amn2 , Amt , amt2 , dec , decc , decm , 
     &                 delim , dete , Ecm , ecm1 , ecmo , eklim , Plab , 
     &                 Si , wdk , wkk , wok
      INTEGER IDT_IEFUND , Ie , ielim , iiki , Ire , Itar , N
      SAVE 
 
C particle properties (BAMJET index convention),
C (dublicate of DTPART for HADRIN)
      INCLUDE 'inc/hnablt'
      INCLUDE 'inc/hnredv'
      INCLUDE 'inc/hnreac'
 
      Ie = IDT_IEFUND(Plab,Ire)
      IF ( Ie.LE.IEIi(Ire) ) Ie = Ie + 1
      Amt = AMH(Itar)
      Amn = AMH(N)
      amn2 = Amn*Amn
      amt2 = Amt*Amt
      Ecm = SQRT(amn2+amt2+2.0D0*Amt*SQRT(amn2+Plab**2))
      ecmo = UMO(Ie)
      ecm1 = UMO(Ie-1)
      decm = ecmo - ecm1
      dec = ecmo - Ecm
      iiki = IKIi(Ire) + 1
      eklim = -THResh(iiki)
      wok = SIIn(Ie)
      wdk = wok - SIIn(Ie-1)
      IF ( Ecm.GT.ecmo ) wdk = 0.0D0
      ielim = IDT_IEFUND(eklim,Ire)
      delim = UMO(ielim) + eklim + 1.D-16
      dete = (Ecm-(ecmo-eklim)*0.5D0)*2.0D0
      IF ( delim*delim.LE.dete*dete ) THEN
         decc = decm
      ELSE
         decc = delim
      END IF
      wkk = wok - wdk*dec/(decc+1.D-9)
      IF ( wkk.LT.0.0D0 ) wkk = 0.0D0
      Si = wkk + 1.D-12
      IF ( -eklim.GT.Ecm ) Si = 1.D-14
      END SUBROUTINE


      SUBROUTINE PHO_PREVNT(Npart)
C**********************************************************************
C
C     print all information of event generation and history
C
C     input:        NPART  -1   minimal output: process IDs
C                           0   additional output of /POEVT1/
C                           1   additional output of /POSTRG/
C                           2   additional output of /HEPEVT/
C                               (call LULIST(1))
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION amfs , ba , ch , p0s , pxs , pys , pzs
      INTEGER ibarfs , ichas , ih , imul , imulc , in , IPHO_BAR3 , 
     &        IPHO_CHR3 , j , Npart
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  general process information
      INCLUDE 'inc/poprcs'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  nucleon-nucleus / nucleus-nucleus interface to DPMJET
      INCLUDE 'inc/pohdfl'
 
      CHARACTER*15 PHO_PNAME
 
      IF ( LPRi.GT.4 .AND. Npart.GE.0 ) WRITE (LO,'(/)')
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1PE10.3)')
     &                         'PHO_PREVNT: c.m. energy' , ECM
      CALL PHO_SETPAR(-2,ih,Npart,0.D0)
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,A,/1X,I10,10I6)')
     &                         'EV-CALL,ISPOM,IHPOM,ISREG,IHDIR,KSTRG,'
     &                        , 'KHTRG,KSLOO,KHLOO,KSDPO,KHDPO' , 
     &                        KEVent , KSPom , KHPom , KSReg , KHDir , 
     &                        KSTrg , KHTrg , KSLoo , KHLoo , KSDpo , 
     &                        KHDpo
      IF ( LPRi.GT.4 ) WRITE (LO,'(6X,A,I4,4I3)')
     &                         'PROCESS-ID,IDNODF,IDIFF1,IDIFF2,IDDPOM'
     &                        , IPRoce , IDNodf , IDIfr1 , IDIfr2 , 
     &                        IDDpom
 
      IF ( IPAmdl(13).GT.0 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)')
     &         'PHO_PREVNT: DPMJET special settings:'
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,1P,4E11.3)')
     &         'ECMN,PCMN,SECM,SPCM' , ECMn , PCMn , SECm , SPCm
         IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,1P,2E11.3)') 'XPSUB,XTSUB' , 
     &        XPSub , XTSub
      END IF
 
      IF ( Npart.LT.0 ) RETURN
 
      IF ( Npart.GE.1 ) CALL PHO_PRSTRG
 
      IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A)') 'COMMON /POEVT1/:'
      ichas = 0
      ibarfs = 0
      imulc = 0
      imul = 0
      IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,A,/,1X,A,A)') 
     &        '   NO  IST    NAME         MO-1 MO-2 DA-1 DA-2  CHA  BAR'
     &        , '  IH1  IH2  CO1  CO2' , 
     &        '========================================================'
     &        , '===================='
      DO ih = 1 , NHEp
         ch = DBLE(IPHO_CHR3(ih,2)/3.D0)
         ba = DBLE(IPHO_BAR3(ih,2)/3.D0)
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2I5,1X,A15,4I5,2F5.1,2I5,2I5)')
     &        ih , ISThep(ih) , PHO_PNAME(ih,2) , JMOhep(1,ih) , 
     &        JMOhep(2,ih) , JDAhep(1,ih) , JDAhep(2,ih) , ch , ba , 
     &        IPHist(1,ih) , IPHist(2,ih) , ICOlor(1,ih) , ICOlor(2,ih)
         IF ( ABS(ISThep(ih)).EQ.1 ) THEN
            ichas = ichas + IPHO_CHR3(ih,2)
            ibarfs = ibarfs + IPHO_BAR3(ih,2)
         END IF
         IF ( ABS(ISThep(ih)).EQ.1 ) THEN
            IF ( IPHO_CHR3(ih,2).NE.0 ) imulc = imulc + 1
            imul = imul + 1
         END IF
      END DO
      IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2(3X,A,I3),2X,2(5X,A,I5))')
     &                         'sum charge:' , ichas/3 , 'baryon:' , 
     &                        ibarfs/3 , 'chr.mult:' , imulc , 
     &                        'tot.mult:' , imul
 
      IF ( LPRi.GT.4 ) WRITE (LO,99010)
 
C   5 FORMAT(2X,8H NUMBER ,8H STATUS ,8H IDENT. ,
C    &  8H 1.MOTH.,8H 2.MOTH.,8H 1.DAUG.,8H L.DAUG.,
C    &  8H CHARGE ,8H BARYON ,/)
C   6 FORMAT(7I8,2F8.3)
99010 FORMAT (/,2X,' NR STAT NAME        X-MOMENTA',
     &        ' Y-MOMENTA Z-MOMENTA  ENERGY    MASS     PT',/,2X,
     &        '-------------------------------',
     &        '--------------------------------------------')
      pxs = 0.D0
      pys = 0.D0
      pzs = 0.D0
      p0s = 0.D0
      DO in = 1 , NHEp
         IF ( (ABS(PHEp(3,in)).LT.99999.D0) .AND. 
     &        (PHEp(4,in).LT.99999.D0) ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,99020) in , ISThep(in) , 
     &           PHO_PNAME(in,2) , (PHEp(j,in),j=1,5) , 
     &           SQRT(PHEp(1,in)**2+PHEp(2,in)**2)
99020       FORMAT (I5,I4,1X,A15,2F8.3,3F10.3,F8.3)
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,99030) in , ISThep(in) , 
     &           PHO_PNAME(in,2) , (PHEp(j,in),j=1,5) , 
     &           SQRT(PHEp(1,in)**2+PHEp(2,in)**2)
99030       FORMAT (I5,I4,1X,A15,2F8.2,2F10.1,F10.3,F8.3)
         END IF
         IF ( ABS(ISThep(in)).EQ.1 ) THEN
            pxs = pxs + PHEp(1,in)
            pys = pys + PHEp(2,in)
            pzs = pzs + PHEp(3,in)
            p0s = p0s + PHEp(4,in)
         END IF
      END DO
      amfs = p0s**2 - pxs**2 - pys**2 - pzs**2
      amfs = SIGN(SQRT(ABS(amfs)),amfs)
      IF ( p0s.LT.99999.D0 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,99040) '      sum:    ' , pxs , 
     &        pys , pzs , p0s , amfs
C   9 FORMAT(I10,14X,5F10.3)
99040    FORMAT (10X,A14,1X,2F8.3,3F10.3)
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,99050) '      sum:    ' , pxs , 
     &        pys , pzs , p0s , amfs
99050    FORMAT (10X,A14,1X,2F8.2,2F10.1,F10.3)
      END IF
      IF ( LPRi.GT.4 ) WRITE (LO,'(//)')
 
      IF ( Npart.GE.2 ) CALL PYLIST(1)
 
      END SUBROUTINE

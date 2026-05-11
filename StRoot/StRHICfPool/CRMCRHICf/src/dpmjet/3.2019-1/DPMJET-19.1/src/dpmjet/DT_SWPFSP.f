
      SUBROUTINE DT_SWPFSP(Idx,Lfsp,Lrnl)
 

#ifdef FOR_FLUKA
      INCLUDE '(DIMPAR)'
      INCLUDE '(PAREVT)'
#else
      IMPLICIT NONE
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
      LOGICAL LEVprt
      PARAMETER ( LEVprt =  .False. )
#endif
      DOUBLE PRECISION BOG , costh , ek , ONE , PI , pminus , pplus , 
     &                 pt2 , ptotcm , TINY14 , TWO , TWOPI , ZERO
      INTEGER Idx , inut , istrnl
      SAVE 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TWO=2.0D0,TINY14=1.0D-14)
      PARAMETER (TWOPI=6.283185307179586476925286766559D+00,
     &           PI=TWOPI/TWO,BOG=TWOPI/360.0D0)
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'

C Lorentz-parameters of the current interaction
      INCLUDE 'inc/dtltra'
C flags for input different options
      INCLUDE 'inc/dtflg1'

C temporary storage for one final state particle
      INCLUDE 'inc/dtfspa'

      LOGICAL Lfsp , Lrnl
 
      Lfsp = .FALSE.
      Lrnl = .FALSE.
      istrnl = 1000
      MULdef = 1
      IF ( LEVprt ) istrnl = 1001
 
      IF ( ABS(ISThkk(Idx)).EQ.1 ) THEN
         IST = ISThkk(Idx)
         IDPdg = IDHkk(Idx)
         LFRag = .FALSE.
         IF ( IDHkk(Idx).LT.80000 ) THEN
            IDBjt = IDBam(Idx)
            IBAry = IIBar(IDBjt)
            ICHar = IICh(IDBjt)
            AMAss = AAM(IDBjt)
         ELSE IF ( IDHkk(Idx).EQ.80000 ) THEN
            IDBjt = 0
            IBAry = IDRes(Idx)
            ICHar = IDXres(Idx)
            AMAss = PHKk(5,Idx)
            inut = IBAry - ICHar
            IF ( (ICHar.EQ.1) .AND. (inut.EQ.1) ) IDBjt = 207
            IF ( (ICHar.EQ.1) .AND. (inut.EQ.2) ) IDBjt = 208
            IF ( (ICHar.EQ.2) .AND. (inut.EQ.1) ) IDBjt = 209
            IF ( (ICHar.EQ.2) .AND. (inut.EQ.2) ) IDBjt = 210
            IF ( IDBjt.EQ.0 ) LFRag = .TRUE.
         ELSE
            GOTO 99999
         END IF
         PE = PHKk(4,Idx)
         PX = PHKk(1,Idx)
         PY = PHKk(2,Idx)
         PZ = PHKk(3,Idx)
         pt2 = PX**2 + PY**2
         PT = SQRT(pt2)
         PTOt = SQRT(pt2+PZ**2)
         SINthe = PT/MAX(PTOt,TINY14)
         COSthe = PZ/MAX(PTOt,TINY14)
         IF ( COSthe.GT.ONE ) THEN
            THEta = ZERO
         ELSE IF ( COSthe.LT.-ONE ) THEN
            THEta = TWOPI/2.0D0
         ELSE
            THEta = ACOS(COSthe)
         END IF
         EKIn = PE - AMAss
C*sr 15.4.96 new E_t-definition
         IF ( IBAry.GT.0 ) THEN
            ET = EKIn*SINthe
         ELSE IF ( IBAry.LT.0 ) THEN
            ET = (EKIn+TWO*AMAss)*SINthe
         ELSE
            ET = PE*SINthe
         END IF
C*
         XLAb = PZ/MAX(PPRoj,TINY14)
C        XLAB   = PE/MAX(EPROJ,TINY14)
         BETa = SQRT
     &          (ABS((ONE-AMAss/MAX(PE,TINY14))*(ONE+AMAss/MAX(PE,TINY14
     &          ))))
         pplus = PE + PZ
         pminus = PE - PZ
         IF ( pminus.GT.TINY14 ) THEN
            YY = 0.5D0*LOG(ABS(pplus/pminus))
         ELSE
            YY = 100.0D0
         END IF
         IF ( (THEta.GT.TINY14) .AND. ((PI-THEta).GT.TINY14) ) THEN
            ETA = -LOG(TAN(THEta/TWO))
         ELSE
            ETA = 100.0D0
         END IF
         IF ( IFRame.EQ.1 ) THEN
            CALL DT_LTNUC(PZ,PE,PZCms,EECms,3)
            pplus = EECms + PZCms
            pminus = EECms - PZCms
            IF ( (pplus*pminus).GT.TINY14 ) THEN
               YYCms = 0.5D0*LOG(ABS(pplus/pminus))
            ELSE
               YYCms = 100.0D0
            END IF
            ptotcm = SQRT(pt2+PZCms**2)
            costh = PZCms/MAX(ptotcm,TINY14)
            IF ( costh.GT.ONE ) THEN
               THEcms = ZERO
            ELSE IF ( costh.LT.-ONE ) THEN
               THEcms = TWOPI/2.0D0
            ELSE
               THEcms = ACOS(costh)
            END IF
            IF ( (THEcms.GT.TINY14) .AND. ((PI-THEcms).GT.TINY14) ) THEN
               ETAcms = -LOG(TAN(THEcms/TWO))
            ELSE
               ETAcms = 100.0D0
            END IF
            XF = PZCms/MAX(PPCm,TINY14)
            THEcms = THEcms/BOG
         ELSE
            PZCms = PZ
            EECms = PE
            YYCms = YY
            ETAcms = ETA
            XF = XLAb
            THEcms = THEta/BOG
         END IF
         THEta = THEta/BOG
 
C set flag for "grey/black"
         LGRey = .FALSE.
         LBLack = .FALSE.
         ek = EKIn
         IF ( IDHkk(Idx).EQ.80000 ) ek = EKIn/DBLE(IBAry)
         IF ( MULdef.EQ.1 ) THEN
C  EMU01-Def.
            IF ( ((IDBjt.EQ.1) .AND. (ek.GT.26.0D-3) .AND. 
     &           (ek.LE.375.0D-3)) .OR. 
     &           ((IDBjt.EQ.13) .AND. (ek.GT.12.0D-3) .AND. 
     &           (ek.LE.56.0D-3)) .OR. 
     &           ((IDBjt.EQ.14) .AND. (ek.GT.12.0D-3) .AND. 
     &           (ek.LE.56.0D-3)) .OR. 
     &           ((IDBjt.EQ.15) .AND. (ek.GT.20.0D-3) .AND. 
     &           (ek.LE.198.0D-3)) .OR. 
     &           ((IDBjt.EQ.16) .AND. (ek.GT.20.0D-3) .AND. 
     &           (ek.LE.198.0D-3)) .OR. 
     &           ((IDBjt.NE.1) .AND. (IDBjt.NE.13) .AND. (IDBjt.NE.14)
     &           .AND. (IDBjt.NE.15) .AND. (IDBjt.NE.16) .AND. 
     &           (BETa.GT.0.23D0) .AND. (BETa.LE.0.70D0)) )
     &           LGRey = .TRUE.
            IF ( ((IDBjt.EQ.1) .AND. (ek.LE.26.0D-3)) .OR. 
     &           ((IDBjt.EQ.13) .AND. (ek.LE.12.0D-3)) .OR. 
     &           ((IDBjt.EQ.14) .AND. (ek.LE.12.0D-3)) .OR. 
     &           ((IDBjt.EQ.15) .AND. (ek.LE.20.0D-3)) .OR. 
     &           ((IDBjt.EQ.16) .AND. (ek.LE.20.0D-3)) .OR. 
     &           ((IDBjt.NE.1) .AND. (IDBjt.NE.13) .AND. (IDBjt.NE.14)
     &           .AND. (IDBjt.NE.15) .AND. (IDBjt.NE.16) .AND. 
     &           (BETa.LE.0.23D0)) ) LBLack = .TRUE.
         ELSE
C  common Def.
            IF ( (BETa.GT.0.23D0) .AND. (BETa.LE.0.70D0) )
     &           LGRey = .TRUE.
            IF ( BETa.LE.0.23D0 ) LBLack = .TRUE.
         END IF
         Lfsp = .TRUE.
      ELSE IF ( ABS(ISThkk(Idx)).EQ.istrnl ) THEN
         IST = ISThkk(Idx)
         IDPdg = IDHkk(Idx)
         LFRag = .TRUE.
         IDBjt = 0
         IBAry = IDRes(Idx)
         ICHar = IDXres(Idx)
         AMAss = PHKk(5,Idx)
         PE = PHKk(4,Idx)
         PX = PHKk(1,Idx)
         PY = PHKk(2,Idx)
         PZ = PHKk(3,Idx)
         pt2 = PX**2 + PY**2
         PT = SQRT(pt2)
         PTOt = SQRT(pt2+PZ**2)
         SINthe = PT/MAX(PTOt,TINY14)
         COSthe = PZ/MAX(PTOt,TINY14)
         IF ( COSthe.GT.ONE ) THEN
            THEta = ZERO
         ELSE IF ( COSthe.LT.-ONE ) THEN
            THEta = TWOPI/2.0D0
         ELSE
            THEta = ACOS(COSthe)
         END IF
         EKIn = PE - AMAss
C*sr 15.4.96 new E_t-definition
C        ET     = PE*SINTHE
         ET = EKIn*SINthe
C*
         IF ( (THEta.GT.TINY14) .AND. ((PI-THEta).GT.TINY14) ) THEN
            ETA = -LOG(TAN(THEta/TWO))
         ELSE
            ETA = 100.0D0
         END IF
         THEta = THEta/BOG
         Lrnl = .TRUE.
      END IF
 
99999 END SUBROUTINE

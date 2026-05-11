
      SUBROUTINE DT_EVENTD(Irej)
 
C***********************************************************************
C Quasi-elastic neutrino nucleus scattering.                           *
C This version dated 29.04.00 is written by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION chklev , DT_RNDM , dum , ekin , ONE , pe , px , 
     &                 py , pz , rtyp , SQTINF , TINY5 , TWO , ZERO , 
     &                 zfrac
      INTEGER i , id , idbj , IDT_ICIHAD , idum , idx , ifound , Irej , 
     &        irej1 , ltyp , nhkk0 , nlines , nuctop , nuctyp
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TWO=2.0D0,TINY5=1.0D-5)
      PARAMETER (SQTINF=1.0D+15)
 
      LOGICAL lfirst
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C flags for input different options
      INCLUDE 'inc/dtflg1'
 
      INCLUDE 'inc/pyjets'
 
C properties of interacting particles
      INCLUDE 'inc/dtprta'
C Lorentz-parameters of the current interaction
      INCLUDE 'inc/dtltra'
C nuclear potential
      INCLUDE 'inc/dtnpot'
C steering flags for qel neutrino scattering modules
      INCLUDE 'inc/qneuto'
      INCLUDE 'inc/qnpol'
 
      INTEGER PYK
 
      DATA lfirst/.TRUE./
 
      Irej = 0
 
      IF ( lfirst ) THEN
         lfirst = .FALSE.
         CALL DT_MASS_INI
      END IF
 
C JETSET parameter
      CALL DT_INITJS(0)
 
C interacting target nucleon
      ltyp = NEUtyp
      IF ( NEUdec.GT.9 ) THEN
         rtyp = DT_RNDM(rtyp)
         zfrac = DBLE(ITZ)/DBLE(IT)
         IF ( rtyp.LE.zfrac ) THEN
            nuctyp = 2212
            nuctop = 1
         ELSE
            nuctyp = 2112
            nuctop = 2
         END IF
      ELSE IF ( (ltyp.EQ.1) .OR. (ltyp.EQ.3) .OR. (ltyp.EQ.5) ) THEN
         nuctyp = 2112
         nuctop = 2
      ELSE
         nuctyp = 2212
         nuctop = 1
      END IF
 
C select first nucleon in list with matching id and reset all other
C nucleons which have been marked as "wounded" by ININUC
      ifound = 0
      DO i = 1 , NHKk
         IF ( (IDHkk(i).EQ.nuctyp) .AND. (ifound.EQ.0) ) THEN
            ISThkk(i) = 12
            ifound = 1
            idx = i
         ELSE
            IF ( ISThkk(i).EQ.12 ) ISThkk(i) = 14
         END IF
      END DO
      IF ( ifound.EQ.0 ) STOP 
     &                 ' EVENTD: interacting target nucleon not found! '
 
C correct position of proj. lepton: assume position of target nucleon
      DO i = 1 , 4
         VHKk(i,1) = VHKk(i,idx)
         WHKk(i,1) = WHKk(i,idx)
      END DO
 
C load initial momenta for conservation check
      IF ( LEMcck ) THEN
         CALL DT_EVTEMC(ZERO,ZERO,PPRoj,EPRoj,1,idum,idum)
         CALL DT_EVTEMC(PHKk(1,idx),PHKk(2,idx),PHKk(3,idx),PHKk(4,idx),
     &                  2,idum,idum)
      END IF
 
C quasi-elastic scattering
      IF ( NEUdec.LT.9 ) THEN
         CALL DT_QEL_POL(EPRoj,ltyp,PHKk(1,idx),PHKk(2,idx),PHKk(3,idx),
     &                   PHKk(4,idx),PHKk(5,idx))
C  CC event on p or n
      ELSE IF ( NEUdec.EQ.10 ) THEN
         CALL DT_GEN_DELTA(EPRoj,ltyp,nuctop,1,PHKk(1,idx),PHKk(2,idx),
     &                     PHKk(3,idx),PHKk(4,idx),PHKk(5,idx))
C  NC event on p or n
      ELSE IF ( NEUdec.EQ.11 ) THEN
         CALL DT_GEN_DELTA(EPRoj,ltyp,nuctop,2,PHKk(1,idx),PHKk(2,idx),
     &                     PHKk(3,idx),PHKk(4,idx),PHKk(5,idx))
      END IF
 
C get final state particles from Lund-common and write them into HKKEVT
      NPOint(1) = NHKk + 1
      NPOint(4) = NHKk + 1
 
      nlines = PYK(0,1)
 
      nhkk0 = NHKk + 1
      DO i = 4 , nlines
         IF ( K(i,1).EQ.1 ) THEN
            id = K(i,2)
            px = P(i,1)
            py = P(i,2)
            pz = P(i,3)
            pe = P(i,4)
            CALL DT_EVTPUT(1,id,1,idx,px,py,pz,pe,0,0,0)
            idbj = IDT_ICIHAD(id)
            ekin = PHKk(4,NHKk) - PHKk(5,NHKk)
            IF ( (idbj.EQ.1) .OR. (idbj.EQ.8) ) THEN
               IF ( ekin.LE.EPOt(2,idbj) ) ISThkk(NHKk) = 16
            END IF
            VHKk(1,NHKk) = VHKk(1,idx)
            VHKk(2,NHKk) = VHKk(2,idx)
            VHKk(3,NHKk) = VHKk(3,idx)
            VHKk(4,NHKk) = VHKk(4,idx)
C           IF (I.EQ.4) THEN
C              WHKK(1,NHKK) = POLARX(1)
C              WHKK(2,NHKK) = POLARX(2)
C              WHKK(3,NHKK) = POLARX(3)
C              WHKK(4,NHKK) = POLARX(4)
C           ELSE
            WHKk(1,NHKk) = WHKk(1,idx)
            WHKk(2,NHKk) = WHKk(2,idx)
            WHKk(3,NHKk) = WHKk(3,idx)
            WHKk(4,NHKk) = WHKk(4,idx)
C           ENDIF
            IF ( LEMcck ) CALL DT_EVTEMC(-px,-py,-pz,-pe,2,idum,idum)
         END IF
      END DO
 
      IF ( LEMcck ) THEN
         chklev = TINY5
         CALL DT_EVTEMC(dum,dum,dum,chklev,-1,778,irej1)
         IF ( irej1.NE.0 ) CALL DT_EVTOUT(4)
      END IF
 
C transform momenta into cms (as required for inc etc.)
      DO i = nhkk0 , NHKk
         IF ( ISThkk(i).EQ.1 ) THEN
            CALL DT_LTNUC(PHKk(3,i),PHKk(4,i),pz,pe,3)
            PHKk(3,i) = pz
            PHKk(4,i) = pe
         END IF
      END DO
 
      END SUBROUTINE

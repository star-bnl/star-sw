*
* $Id: gfmfin.F,v 1.2 1998/04/16 16:59:32 fisyak Exp $
*
* $Log: gfmfin.F,v $
* Revision 1.2  1998/04/16 16:59:32  fisyak
* 2nd pass with gstar
*
*
#include "geant321/pilot.h"
#if defined(CERNLIB_HADRON)
*CMZ :          16/12/97  13.31.05  by  Pavel Nevski
*CMZ :  3.21/02 29/03/94  15.41.47  by  S.Giani
*-- Author :
      SUBROUTINE GFMFIN
#include "geant321/gcbank.inc"
#include "geant321/gccuts.inc"
#include "geant321/gcjloc.inc"
#include "geant321/gcflag.inc"
#include "geant321/gckine.inc"
#include "geant321/gcking.inc"
#include "geant321/gcmate.inc"
#include "geant321/gcphys.inc"
#include "geant321/gctrak.inc"
#include "geant321/gsecti.inc"
#include "geant321/gctmed.inc"
#include "geant321/gcunit.inc"
*KEEP,DIMPAR.
*                                                                     *
*=== dimpar ==========================================================*
*                                                                     *
*---------------------------------------------------------------------*
*                                                                     *
*      DIMPAR: included in any routine                                *
*                                                                     *
*          Mxxrgn = maximum number of regions                         *
*          Mxxmdf = maximum number of media in Fluka                  *
*          Mxxmde = maximum number of media in Emf                    *
*          Mfstck = stack dimension in Fluka                          *
*          Mestck = stack dimension in Emf                            *
*          Nallwp = number of allowed particles                       *
*          Mpdpdx = number of particle types for which EM dE/dx pro-  *
*                   cesses (ion,pair,bremss) have to be computed      *
*          Icomax = maximum number of materials for compounds (equal  *
*                   to the sum of the number of materials for every   *
*                   compound )                                        *
*          Nstbis = number of stable isotopes recorded in common iso- *
*                   top                                               *
*                                                                     *
*---------------------------------------------------------------------*
*                                                                     *
      PARAMETER ( MXXRGN = 500  )
      PARAMETER ( MXXMDF = 50   )
      PARAMETER ( MXXMDE = 18   )
      PARAMETER ( MFSTCK = 1000 )
      PARAMETER ( MESTCK = 100  )
      PARAMETER ( NALLWP = 39   )
      PARAMETER ( MPDPDX = 8    )
      PARAMETER ( ICOMAX = 120  )
      PARAMETER ( NSTBIS = 304  )
 
*KEEP,FINUCT.
      DOUBLE PRECISION CXR, CYR, CZR, TKI, PLR, WEI, TV, TVCMS,
     &                 TVRECL, TVHEAV, TVBIND
 
*KEEP,FINUC.
*
*=== finuc ============================================================*
*
*----------------------------------------------------------------------*
*     Last change on 5 april 1990     by Alfredo Ferrari, INFN Milan   *
*     include file: finuc copy                   created 26/11/86 by pa*
*     note: if changed change finuc2 copy also                         *
*     changes: none                                                    *
*     included in the following subroutines or functions: not updated  *
*                                                                      *
*     description of the common block(s) and variable(s)               *
*                                                                      *
*     /finuc/ is the storage for secondaries created in event          *
*        np        = number of secondaries                             *
*        kpart(ip) = type of the secondary ip                          *
*        cxr(ip)   = direction cosine of the secondary ip              *
*                    with respect to x-axis                            *
*        cyr(ip)   = direction cosine of the secondary ip              *
*                    with respect to y-axis                            *
*        czr(ip)   = direction cosine of the secondary ip              *
*                    with respect to z-axis                            *
*        tki(ip)   = kinetic energy of secondary ip                    *
*        plr(ip)   = momentum of the secondary ip                      *
*        wei(ip)   = weight of the secondary ip                        *
*        tv        = excitation energy                                 *
*        tvcms     = actual excitation energy of the residual nucleus  *
*        tvrecl    = recoil kinetic energy of the residual nucleus     *
*        tvheav    = recoil kinetic energies of heavy (2-H, 3-H, 3-He, *
*                    4-He) fragments after evaporation                 *
*        tvbind    = approximate energy wasted in nuclear binding      *
*                    effects
*----------------------------------------------------------------------*
*
      PARAMETER (MXP=999)
      COMMON / FKFINU / CXR (MXP), CYR (MXP), CZR (MXP), TKI (MXP),
     &                 PLR (MXP), WEI (MXP), TV, TVCMS, TVRECL, TVHEAV,
     &                 TVBIND, NP0, NP, KPART (MXP)
 
*KEND.
      REAL RNDM(1)
      DOUBLE PRECISION AOCMBM, AMSS , ZTAR, RHO , ZLIN, ZLEL, ZLRAD,
     +ZUL
      COMMON / FKMAPA / AOCMBM (MXXMDF), AMSS (MXXMDF), ZTAR   (MXXMDF),
     +                  RHO    (MXXMDF), ZLIN (MXXMDF), ZLEL   (MXXMDF),
     +                  ZLRAD  (MXXMDF), ZUL  (MXXMDF), MEDIUM (MXXRGN),
     +                  MULFLG (MXXMDF),IFCOMP(MXXMDF), MSSNUM (MXXMDF),
     +                  NREGS, NMATF, MTBSNM
*KEEP,PART2T.
      DOUBLE PRECISION AAM, GA, TAU, AAMDSC
 
*KEEP,PART2.
*
*=== part2 ============================================================*
*
*----------------------------------------------------------------------*
*     Include file: part2 copy        Revised on 20-7-90 by A. Ferrari *
*     Note: see also part copy and part3 copy                          *
*     Changes: none                                                    *
*     Included in the following subroutines or functions: not updated  *
*                                                                      *
*     Description of the common block(s) and variable(s)               *
*                                                                      *
*         Kptoip = conversion from part to paprop numbering            *
*         Iptokp = conversion from paprop to part numbering            *
*                                                                      *
*----------------------------------------------------------------------*
*
      PARAMETER (IDMAX8=183)
      CHARACTER*8  ANAME
      COMMON / FKPART / AAM  (IDMAX8), GA     (IDMAX8), TAU    (IDMAX8),
     &                AAMDSC (IDMAX8), IICH   (IDMAX8), IIBAR  (IDMAX8),
     &                K1     (IDMAX8), K2     (IDMAX8), KPTOIP (IDMAX8),
     &                IPTOKP (39)
      COMMON / FKCHPA / ANAME (IDMAX8)
 
*KEEP,COMCONT.
      DOUBLE PRECISION AOCMBC, CONTNT, SUMACN, CABINX, CABELX,
     &                 ANXNOR, ELXNOR, CONA13, SUMZMF
 
*KEEP,COMCON.
*
*=== comcon ===========================================================*
*
*----------------------------------------------------------------------*
*                                                                      *
*     include file: comcon copy                 created 26/11/86 by pa *
*                                                                      *
*     Last change  on  30-oct-92   by   Alfredo Ferrari, INFN - Milan  *
*     included in the following subroutines or functions: not updated  *
*                                                                      *
*     description of the common block(s) and variable(s)               *
*                                                                      *
*                                                                      *
*     /comcon/ contains information about compounds                    *
*         icompl(imat) = number of materials in a compound             *
*         matnum(i)    = material numbers (compounds consist of the    *
*                        materials)                                    *
*         aocmbc(i)    = atomic densities in barn**-1 cm**-1           *
*                        (Atoms Over Cm times Barn for Compounds)      *
*         cona13(i)    = A^1/3 of the constituents                     *
*         sumzmf(i)    = cumulative z^2 for multiple scattering        *
*         contnt(i)    = partial densities                             *
*         sumacn(i)    = cumulative atom contents of the compounds     *
*         cabinx(i)    = cumulative at.cont.*inelastic cross section   *
*         cabelx(i)    = cumulative at.cont.*  elastic cross section   *
*         anxnor       = total inelastic cross section                 *
*         elxnor       = total   elastic cross section                 *
*----------------------------------------------------------------------*
*
      COMMON /FKCOMC/ AOCMBC (ICOMAX), CONA13 (ICOMAX), SUMZMF (ICOMAX),
     &                CONTNT (ICOMAX), SUMACN (ICOMAX), CABINX (ICOMAX),
     &                CABELX (ICOMAX), ANXNOR, ELXNOR,  ICOMPL (MXXMDF),
     &                MATNUM (ICOMAX)
 
*KEEP,FHEAVYT.
      DOUBLE PRECISION CXHEAV, CYHEAV, CZHEAV, TKHEAV, PHEAVY,
     &                 WHEAVY, AMHEAV
 
*KEEP,FHEAVY.
*
*=== fheavy ===========================================================*
*
*----------------------------------------------------------------------*
*     Last change on 9 March 1993     by Alfredo Ferrari, INFN Milan   *
*     include file: fheavy copy       created 5/4/90 by A. Ferrari     *
*     changes: none                                                    *
*     included in the following subroutines or functions: not updated  *
*                                                                      *
*     description of the common block(s) and variable(s)               *
*                                                                      *
*     /fheavy/ is the storage for heavy secondaries created in the     *
*              nuclear evaporation                                     *
*        npheav     = number of secondaries                            *
*        kheavy(ip) = type of the secondary ip                         *
*                   ( 3 = deuteron, 4 = 3-H, 5 = 3-He, 6 = 4-He,       *
*                     7-12 = "Heavy" fragment specified by Ibheav and  *
*                     Icheav )                                         *
*        cxheav(ip) = direction cosine of the secondary ip             *
*                     with respect to x-axis                           *
*        cyheav(ip) = direction cosine of the secondary ip             *
*                     with respect to y-axis                           *
*        czheav(ip) = direction cosine of the secondary ip             *
*                     with respect to z-axis                           *
*        tkheav(ip) = kinetic energy of secondary ip                   *
*        pheavy(ip) = momentum of the secondary ip                     *
*        wheavy(ip) = weight of the secondary ip                       *
*        amheav(ip) = atomic masses of the six types of evaporated     *
*                     particles                                        *
*        anheav(kp) = name of the kp-type heavy particle               *
*        icheav(kp) = charge of the kp-type heavy particle             *
*        ibheav(kp) = mass number of the kp-type heavy particle        *
*   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   *
*   !!! There is now the possibility to produce up to 6 "heavy" !!!!   *
*   !!! fragments besides the residual nucleus recorded in      !!!!   *
*   !!! Resnuc: they are identified by indeces 7-12, of course  !!!!   *
*   !!! the corresponding physical properties (Z,A,m..) must be !!!!   *
*   !!! updated every time they are produced                    !!!!   *
*   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   *
*----------------------------------------------------------------------*
*
      PARAMETER ( MXHEAV = 100 )
      CHARACTER*8 ANHEAV
      COMMON / FKHEAV / CXHEAV (MXHEAV), CYHEAV (MXHEAV),
     &                  CZHEAV (MXHEAV), TKHEAV (MXHEAV),
     &                  PHEAVY (MXHEAV), WHEAVY (MXHEAV),
     &                  AMHEAV  ( 12 ) ,
     &                  KHEAVY (MXHEAV), ICHEAV  ( 12 ) ,
     &                  IBHEAV  ( 12 ) , NPHEAV
      COMMON / FKHEAC / ANHEAV  ( 12 )
 
*KEEP,PAPROP.
*
*=== paprop ===========================================================*
*
*----------------------------------------------------------------------*
*     include file: paprop copy                   created 26/11/86 by p*
*     changes: on  16 december 1992 by Alfredo Ferrari                 *
*     included in the following subroutines or functions: not updated  *
*                                                                      *
*     description of the common block(s) and variable(s)               *
*                                                                      *
*     /paprop/ contains particle properties                            *
*        btype  = literal name of the particle                         *
*        am     = particle mass in gev                                 *
*        ichrge = electric charge of the particle                      *
*        iscore = explanations for the scored distribution             *
*        genpar = names of the generalized particles                   *
*        ijdisc = list of the particle types to be discarded           *
*        thalf  = half life of the particle in sec                     *
*        biasdc = decay biasing factors                                *
*        biasin = inelastic interaction biasing factors                *
*        lhadro = flag for hadrons                                     *
*        jspinp = particle spin (in units of 1/2)                      *
*        lbsdcy = logical flag for biased decay: if .true. the biasing *
*                 factor is used as an upper limit to the decay length *
*        lprbsd = logical flag for biased decay: if .true. the biasing *
*                 factor is applied only to primaries                  *
*        lprbsi = logical flag for inelastic interaction biasing: if   *
*                 .true. the biasing factor is applied only to prima-  *
*                 ries                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*
      LOGICAL LHADRO, LBSDCY, LPRBSD, LPRBSI
      CHARACTER*8 BTYPE,GENPAR
      COMMON / FKPAPR / AM  (NALLWP), AMDISC (NALLWP), THALF  (NALLWP),
     &               BIASDC (NALLWP), BIASIN (NALLWP), ICHRGE (NALLWP),
     &               ISCORE     (10), IJDISC (NALLWP), LHADRO (NALLWP),
     &               JSPINP (NALLWP), LBSDCY (NALLWP), LPRBSD, LPRBSI
      COMMON / FKCHPR / BTYPE  (NALLWP), GENPAR (30)
 
*KEEP,PAPROPT.
      DOUBLE PRECISION AM, AMDISC, THALF,BIASDC, BIASIN
 
*KEEP,GFKDIS.
*KEND.
      DOUBLE PRECISION POO,EKE,TXI,TYI,TZI,AMM,WE,ONE,PGEANT,DMOD
      PARAMETER (ONE=1)
      DIMENSION IGTOFL(49),IFLTOG(39),IHVTOG(6),ZSAMP(50)
      DATA IGTOFL / 0, 0, 0, 0, 0, 0,23,13,14,12, 15,16, 8, 1, 2,19, 0,
     +17,21,22, 20, 34, 36, 38, 9,18, 31, 32, 33, 35, 37, 39, 17*0/
 
      DATA IFLTOG /14,15, 3, 2, 4, 4, 1,13,25, 5, 6,10, 8, 9,11,12,18,
     +26,16,21, 19,20, 7, 7*0, 27, 28, 29, 22, 30, 23, 31, 24, 32/
      DATA IHVTOG /13,14,45,46,49,47/
*
      NP = 0
      NPHEAV = 0
*
*    Stopped particles:
*    o Neutral particles are sent to GHSTOP
*    o pi+ and K+/K- are forced to decay
*    o pi-, antiprotons and antineutrons are sent to FLUKA
*      for annihilation (not here but later in this routine)
      IF (IGF.EQ.2.OR.(GEKIN.EQ.0.0.AND.IPART.EQ.13)) THEN
         IF (GEKIN.LT.CUTNEU) THEN
            GEKIN = MAX(GEKIN,1E-14)
* should kinetic energy be deposited?
            ISTOP = 2
            IGF = 0
            GOTO 110
         ENDIF
         CALL GMICAP
         IGF = 0
         GOTO 110
      ELSE IF (GEKIN.EQ.0..AND.
     +        (IPART.EQ.8.OR.IPART.EQ.12.OR.IPART.EQ.11)) THEN
         CALL GDECAY
         NMEC=NMEC+1
         LMEC(NMEC)=5
         ISTOP=1
         GOTO 999
      ENDIF
*
      IF (IFINIT(5) .EQ. 0) CALL FLINIT
      INT=0
      IJ=IGTOFL(IPART)
      IF(IJ.EQ.0) GOTO 110
      NMEC = NMEC + 1
      EKE = GEKIN
      TXI = VECT(4)
      TYI = VECT(5)
      TZI = VECT(6)
      DMOD = ONE/SQRT(TXI**2+TYI**2+TZI**2)
      TXI = TXI*DMOD
      TYI = TYI*DMOD
      TZI = TZI*DMOD
      WE  = 1.
      JMA = LQ(JMATE-NMAT)
      NCOMP = ABS (Q(JMA+11))
      AMM = Q(JMA+6)
      JMIXT = LQ(JMA-5)
 
*    Antiprotons, antineutrons and pi- are sent to
*    eventv for annihilation
      IF (GEKIN.EQ.0..AND.
     +         (IPART.EQ.15.OR.IPART.EQ.9.OR.IPART.EQ.25)) THEN
         IF(NCOMP.LE.1) THEN
            AMSS(1) = Q(JMA+6)
            ZTAR(1) = Q(JMA+7)
            MSSNUM(1) = 0
            RHO(1) = Q(JMA+8)
         ELSE
            ZSAMP(1) = 0.
            DO 10 I=1,NCOMP
               ZSAMP(I+1) = ZSAMP(I) + Q(JMIXT+NCOMP+I)
   10       CONTINUE
            CALL GRNDM(RNDM,1)
            ZCONT=ZSAMP(NCOMP+1)*RNDM(1)
            DO 20 I=1,NCOMP
               IF(ZCONT.LE.ZSAMP(I+1)) GO TO 30
   20       CONTINUE
            I = NCOMP
   30       CONTINUE
            AMSS(1)   = Q(JMIXT+I)
            MSSNUM(1) = 0
            ZTAR(1)   = Q(JMIXT+NCOMP+I)
            RHO(1)    = Q(JMIXT+2*NCOMP+I)*DENS
         END IF
         EKE = 1E-9
         POO=SQRT(EKE*(EKE+2*AM(IJ)))
         CALL EVENTV(IJ,POO,EKE,TXI,TYI,TZI,WE,1)
         GOTO 80
      ELSE IF (GEKIN.LE.CUTHAD .AND. ITRTYP.EQ.4) THEN
         DESTEP = DESTEP + GEKIN
         GEKIN  = 0.
         GETOT  = AMASS
         VECT(7) = 0.
         ISTOP = 1
         GO TO 110
      ENDIF
*
      CALL GRNDM(RNDM,1)
      RNDEVT=RNDM(1)
      IF ( RNDEVT .GE. SINE/FSIG) THEN
 
         IF (GEKIN .GT. 0.02) THEN
            POO=SQRT(EKE*(EKE+2*AM(IJ)))
         ELSE
            GO TO 110
         END IF
         INT=1
         LMEC(NMEC)=13
         IF(NCOMP.LE.1) THEN
            CALL NUCREL(IJ,POO,EKE,TXI,TYI,TZI,AMM,WE)
         ELSE
            CALL GRNDM(RNDM,1)
            RCONT=ELXNOR*RNDM(1)
            DO 40  I=1,NCOMP
               IF(RCONT.LE.CABELX(I)) GO TO 50
   40       CONTINUE
            I=NCOMP
   50       CONTINUE
            CALL NUCREL(IJ,POO,EKE,TXI,TYI,TZI,ONE*Q(JMIXT+I),WE)
         END IF
      ELSE
         LMEC(NMEC)=20
         IF (IHADR.EQ.2) THEN
            ISTOP = 2
            DESTEP = DESTEP + GETOT
            GO TO 110
         ENDIF
         IF (GEKIN .GT. 0.02) THEN
            POO=SQRT(EKE*(EKE+2*AM(IJ)))
         ELSE
            IF ((IJ.EQ.2 .OR. IJ.EQ.9 .OR. IJ.EQ.14 .OR. IJ.EQ.16)
     +            .AND. GEKIN .GT. 0.0) THEN
               POO=SQRT(EKE*(EKE+2*AM(IJ)))
            ELSE
               NMEC=NMEC-1
               GO TO 110
            END IF
         END IF
         INT=2
         IF(NCOMP.LE.1) THEN
            AMSS(1) = Q(JMA+6)
            ZTAR(1) = Q(JMA+7)
            MSSNUM(1) = 0
            RHO(1) = Q(JMA+8)
         ELSE
            CALL GRNDM(RNDM,1)
            RCONT=ANXNOR*RNDM(1)
            DO 60  I=1,NCOMP
               IF(RCONT.LE.CABINX(I)) GO TO 70
   60       CONTINUE
            I=NCOMP
   70       CONTINUE
            AMSS(1)   = Q(JMIXT+I)
            MSSNUM(1) = 0
            ZTAR(1)   = Q(JMIXT+NCOMP+I)
            RHO(1)    = Q(JMIXT+2*NCOMP+I)*DENS
         END IF
         CALL EVENTV(IJ,POO,EKE,TXI,TYI,TZI,WE,1)
      END IF
*
   80 IF(NP.EQ.1.AND.NPHEAV.EQ.0.AND.KPART(1).EQ.IJ) THEN
         VECT(4)=CXR(1)
         VECT(5)=CYR(1)
         VECT(6)=CZR(1)
         VECT(7)=SQRT(TKI(1)*(TKI(1)+2*AMASS))
         GETOT=TKI(1)+AMASS
         GEKIN=TKI(1)
      ELSE
         ISTOP=1
         NSTAK1 = MIN(NP,MXGKIN-NGKINE)
         IF(NP.GT.NSTAK1) THEN
            WRITE(CHMAIL,10000) NP-NSTAK1
            CALL GMAIL(0,0)
         ENDIF
         DO 90  K=1,NSTAK1
            NGKINE = NGKINE + 1
            IF (KPART(K) .EQ. 24 .OR. KPART(K) .EQ. 25) THEN
               KPART(K) = 19
               CALL GRNDM(RNDM,1)
               IF (RNDM(1) .GT. 0.5) KPART(K) = 12
            END IF
            IGEPAR = IFLTOG(KPART(K))
            JPA = LQ(JPART-IGEPAR)
            AGEMAS = Q(JPA+7)
            PGEANT = SQRT(TKI(K)*(TKI(K)+2*AGEMAS))
            GKIN(1,NGKINE)=CXR(K)*PGEANT
            GKIN(2,NGKINE)=CYR(K)*PGEANT
            GKIN(3,NGKINE)=CZR(K)*PGEANT
            GKIN(4,NGKINE)=TKI(K)+AGEMAS
            GKIN(5,NGKINE)=IGEPAR
            TOFD(NGKINE)=0.0
            GPOS(1,NGKINE) = VECT(1)
            GPOS(2,NGKINE) = VECT(2)
            GPOS(3,NGKINE) = VECT(3)
   90    CONTINUE
*
         NSTAK2 = MIN(NPHEAV,MXGKIN-NGKINE)
         IF(NPHEAV.GT.NSTAK2) THEN
            WRITE(CHMAIL,10100) NPHEAV-NSTAK2
            CALL GMAIL(0,0)
         ENDIF
         DO 100 K=1,NSTAK2
            NGKINE = NGKINE + 1
            IGEPAR = IHVTOG(KHEAVY(K))
            JPA = LQ(JPART-IGEPAR)
            AGEMAS = Q(JPA+7)
            PGEANT = SQRT(TKHEAV(K)*(TKHEAV(K)+2*AGEMAS))
            GKIN(1,NGKINE)=CXHEAV(K)*PGEANT
            GKIN(2,NGKINE)=CYHEAV(K)*PGEANT
            GKIN(3,NGKINE)=CZHEAV(K)*PGEANT
            GKIN(4,NGKINE)=TKHEAV(K)+AGEMAS
            GKIN(5,NGKINE)=IGEPAR
            TOFD(NGKINE)=0.0
            GPOS(1,NGKINE) = VECT(1)
            GPOS(2,NGKINE) = VECT(2)
            GPOS(3,NGKINE) = VECT(3)
  100    CONTINUE
*
         KCASE=NAMEC(12)
      END IF
  110 CONTINUE
      ZINTHA = GARNDM(DUMMY)
      SLHADR = SLENG
      STEPHA = 1.0E10
10000 FORMAT(' **** FLUFIN: Stack overflow, ',I6,' particles lost')
10100 FORMAT(' **** FLUFIN: Stack overflow, ',I6,
     +' heavy particles lost')
  999 END
#endif

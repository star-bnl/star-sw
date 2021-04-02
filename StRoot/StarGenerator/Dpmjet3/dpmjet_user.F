*
*  DPMJET3 User code
*      
*      IPAR(1)    = Projectile Mass
*      IPAR(2)    = Projectile Charge
*      IPAR(3)    = Target Mass
*      IPAR(4)    = Target Charge
*      Energy(6)  = 1/2=Projectile/Target Energy and 3/4=crossing angles

      SUBROUTINE DPMJET_INIT(IPAR,ENERGY)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      COMMON /DTPRTA/ IT,ITZ,IP,IPZ,IJPROJ,IBPROJ,IJTARG,IBTARG
      COMMON /DTARGS/ EPN,NPMASS,NPCHAR,NTMASS,NTCHAR,
     &                IDP,IGLAU,CMENER

* from PROJPAR tag      
      IP  = IPAR(1)
      IPZ = IPAR(2)
      IJPROJ = 1
      IBPROJ = 1

* from TARJPAR tag      
      IT     = IPAR(3)
      ITZ    = IPAR(4)
      IJTARG = 1
      IBTARG = 1

* from BEAM tag
      CALL DT_BEAMPR(ENERGY,PPN,1)
      EPN=0
      CMENER=0

      CALL DT_INIT(-2,EPN,NPMASS,NPCHAR,NTMASS,NTCHAR,
     &             IDP,IGLAU)      

      END

* Generate one event of AA interactions
      SUBROUTINE DPMJET_AA_EVENT()
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      COMMON /DTARGS/ EPN,NPMASS,NPCHAR,NTMASS,NTCHAR,
     &                IDP,IGLAU,CMENER
      CALL DT_AAEVT(1,EPN,NPMASS,NPCHAR,NTMASS,NTCHAR,
     &              IDP,IGLAU)
      END

* User code called for each event
      SUBROUTINE DT_USRHIS(MODE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
*
* COMMON /DTEVT1/ :
*                   NHKK         number of entries in common block
*                   NEVHKK       number of the event
*                   ISTHKK(i)    status code for entry i
*                   IDHKK(i)     identifier for the entry
*                                (for particles: identifier according
*                                 to the PDG numbering scheme)
*                   JMOHKK(1,i)  pointer to the entry of the first mother
*                                of entry i
*                   JMOHKK(2,i)  pointer to the entry of the second mother
*                                of entry i
*                   JDAHKK(1,i)  pointer to the entry of the first daughter
*                                of entry i
*                   JDAHKK(2,i)  pointer to the entry of the second daughter
*                                of entry i
*                   PHKK(1..3,i) 3-momentum
*                   PHKK(4,i)    energy
*                   PHKK(5,i)    mass
*
* event history
      PARAMETER (NMXHKK=200000)
      COMMON /DTEVT1/ NHKK,NEVHKK,ISTHKK(NMXHKK),IDHKK(NMXHKK),
     &                JMOHKK(2,NMXHKK),JDAHKK(2,NMXHKK),
     &                PHKK(5,NMXHKK),VHKK(4,NMXHKK),WHKK(4,NMXHKK)
* extended event history
      COMMON /DTEVT2/ IDRES(NMXHKK),IDXRES(NMXHKK),NOBAM(NMXHKK),
     &                IDBAM(NMXHKK),IDCH(NMXHKK),NPOINT(10),
     &                IHIST(2,NMXHKK)

C  general process information from phojet
      COMMON /POPRCS/ IPROCE,IDNODF,IDIFR1,IDIFR2,IDDPOM,IPRON(15,4)
      INTEGER IPROCE,IDNODF,IDIFR1,IDIFR2,IDDPOM,IPRON

      COMMON /POCKIN/ PTWANT,AS,AH,ALNS,ALNH,Z1MAX,Z1DIF,Z2MAX,Z2DIF,
     &                PT,PTfin,ETAC,ETAD,X1,X2,V,U,W,W1,AXX,
     &                QQPD,QQAL,PDF1(-6:6),PDF2(-6:6),WEIGHT,
     &                PHI1(5),PHI2(5),PHO1(5),PHO2(5),
     &                IA,IB,IC,ID,IV1,IV2,MSPR,IREJSC
      INTEGER IA,IB,IC,ID,IV1,IV2,MSPR,IREJSC
      DOUBLE PRECISION PTWANT,AS,AH,ALNS,ALNH,Z1MAX,Z1DIF,Z2MAX,Z2DIF,
     &                 PT,PTfin,ETAC,ETAD,X1,X2,V,U,W,W1,AXX,QQPD,QQAL,
     &                 PDF1,PDF2,WEIGHT,PHI1,PHI2,PHO1,PHO2

C  added for the hard scattering information from phojet
      INTEGER MSCAHD
      PARAMETER ( MSCAHD = 50 )
      INTEGER LSCAHD,LSC1HD,LSIDX,
     &        NINHD,N0INHD,NIVAL,N0IVAL,NOUTHD,NBRAHD,NPROHD
      DOUBLE PRECISION PPH,PTHD,ETAHD,Q2SCA,PDFVA,XHD,VHD,X0HD
      COMMON /POHSLT/ LSCAHD,LSC1HD,LSIDX(MSCAHD),
     &                PPH(8*MSCAHD,2),PTHD(MSCAHD),ETAHD(MSCAHD,2),
     &                Q2SCA(MSCAHD,2),PDFVA(MSCAHD,2),
     &                XHD(MSCAHD,2),VHD(MSCAHD),X0HD(MSCAHD,2),
     &                NINHD(MSCAHD,2),N0INHD(MSCAHD,2),
     &                NIVAL(MSCAHD,2),N0IVAL(MSCAHD,2),
     &                NOUTHD(MSCAHD,2),NBRAHD(MSCAHD,2),NPROHD(MSCAHD)

      COMMON /DIFFTS/ TTtest
      DOUBLE PRECISION TTtest
      DOUBLE PRECISION tHAT

C  added by liang to search IP from phojet( particle combination )
      INTEGER IPSTATUS
      COMMON /PARCMB/ IPSTATUS


* Glauber formalism: collision properties
      COMMON /DTGLCP/ RPROJ,RTARG,BIMPAC,
     &                NWTSAM,NWASAM,NWBSAM,NWTACC,NWAACC,NWBACC

* properties of interacting particles
      COMMON /DTPRTA/ IT,ITZ,IP,IPZ,IJPROJ,IBPROJ,IJTARG,IBTARG

* specify output file unit added by liang
      COMMON /OUTPT/ FOUT
      INTEGER FOUT

* out e- and gamma* information
      COMMON /DTGPRO/ VIRT,PGAMM(4),PLEPT0(4),PLEPT1(4),PNUCL(4),IDIREC
      COMMON /DTLGVX/ PPL0(4),PPL1(4),PPG(4),PPA(4)

* added by liang to check the photon flux 12/28/11
      COMMON /FLCHK/ PFXCHK
      DOUBLE PRECISION PFXCHK

* added by liang to store the output event variables 1/20/12
      COMMON /EVTOUT/ XBJOUT,YYOUT,W2OUT,NUOUT,Q2OUT
      DOUBLE PRECISION XBJOUT,YYOUT,W2OUT,NUOUT,Q2OUT

* added by liang to read beam information to transform back to lab frame
* 1/20/12
      COMMON /BEAMIF/ pBeam, eBeampr
      DOUBLE PRECISION pBeam, eBeampr

* some dummy values for outgoing electron and radiated photon
      INTEGER IDUM

      GOTO (1,2,3) MODE

*------------------------------------------------------------------
*
    1 CONTINUE
*
* initializations
*
*  Called with MODE=1 once at the beginning of the run.
*
      RETURN
*
*------------------------------------------------------------------
*
    2 CONTINUE
*
* scoring of the present event
*
*  Called with MODE=2 every time one event has been finished.
*
*  The final state particles from the actual event (number NEVHKK)
*  can be found in DTEVT1 and identified by their status:
*
*     ISTHKK(i) = 1    final state particle produced in
*                      photon-/hadron-/nucleon-nucleon collisions or
*                      in intranuclear cascade processes
*                -1    nucleons, deuterons, H-3, He-3, He-4 evaporated
*                      from excited nucleus and
*                      photons produced in nuclear deexcitation processes
*                1001  residual nucleus (ground state)
*
*  The types of these particles/nuclei are given in IDHKK as follows
*
*     all final state part. except nuclei :
*       IDHKK(i)=particle identifier according to PDG numbering scheme
*     nuclei (evaporation products, and residual nucleus) :
*       IDHKK(i)=80000, IDRES(i)=mass number, IDXRES(i)=charge number
*
*  The 4-momenta and masses can be found in PHKK (target nucleus rest frame):
*                   PHKK(1..3,i) 3-momentum (p_x,p_y,p_z)
*                   PHKK(4,i)    energy
*                   PHKK(5,i)    mass
*
*
*
*  Pick out the final state particles from DTEVT1 in each event for
*  instance by the following loop (NHKK=number of entries in the present
*  event) and fill your histograms
C     DO 20 I=1,NHKK
C        IF (ABS(ISTHKK(I)).EQ.1) THEN
C        ELSEIF (ABS(ISTHKK(I)).EQ.1001) THEN
C        ENDIF
C  20 CONTINUE

c...deal with some kinematics issue
c  get the hard interaction information      
      NF1=0
      NF2=0
      XPART1=0.0D0
      XPART2=0.0D0      
      NUCL=0
      DO I=1,LSCAHD      
      NF1=NINHD(I,1)
      NF2=NINHD(I,2)
      XPART1=XHD(I,1)
      XPART2=XHD(I,2)
      NUCL=NBRAHD(I,2)
      ENDDO
      IF(IPROCE.EQ.1) THEN
         tHat = VHD(1)
      ELSE IF(IPROCE.EQ.3.OR.IPROCE.EQ.5.OR.IPROCE.EQ.6) THEN
         tHAT = TTtest
      ENDIF
* Lorentz transformation for e- boost from the n rest to real lab frame
      IF(IJPROJ.EQ.7) THEN
         P = pBeam
         E = SQRT(P*P+0.938*0.938)
         GMA = E/0.938
         BGT = P/E
         CALL DT_DALTRA(GMA, 0.0D0,0.0D0,-BGT*GMA,PPL1(1),PPL1(2),
     &   PPL1(3),PPL1(4),PLTOT,PL1,PL2,PL3,PL4)
******calculate the theta of out e-*******************************      
         THETA=ACOS(PL3/PLTOT)
******transform incoming e- into lab frame************************
         eBeamE=SQRT(eBeampr**2+EDUMMY8**2)
         CALL DT_DALTRA(GMA, 0.0D0,0.0D0,-BGT*GMA,0.0D0,0.0D0,eBeampr,
     &   eBeamE,eDummy,eBeamx,eBeamy,eBeamz,eBeam0)      
      ENDIF


c...event file header 
      IF(NEVHKK.EQ.1) THEN
         WRITE(FOUT,*)'DPMJET EVENT FILE'
         WRITE(FOUT,*)'==========================================='

         IF(IJPROJ.EQ.1) THEN
            WRITE(FOUT,*)'ievent, process1, process2, IP, N_coll,',
     &         ' N_par1, N_par2, nTracks, B_impact'
         ELSEIF(IJPROJ.EQ.7) THEN
            WRITE(FOUT,*)'I, ievent, process1, process2, IP, W2, nu,
     &   Q2, x, y, theta_e, photonFlux, targetparton, 
     &   projectileparton,xtargetparton, xprojectileparton, nucleon, 
     &   nrTracks'
         ENDIF

         WRITE(FOUT,*)'==========================================='
         IF(IJPROJ.EQ.1) THEN
            WRITE(FOUT,*)'I  ISTHKK(I)  IDHKK(I) 
     &   JMOHKK(1,I)  JDAHKK(1,I)  JDAHKK(2,I)  PHKK(1,I)  PHKK(2,I)  
     &   PHKK(3,I)  PHKK(4,I)  PHKK(5,I)  VHKK(1,I) VHKK(2,I) VHKK(3,I)'
         ELSEIF(IJPROJ.EQ.7) THEN
            WRITE(FOUT,*)'I  ISTHKK(I)  IDHKK(I)  JMOHKK(1,I) 
     &   JDAHKK(1,I)  JDAHKK(2,I)  PHKK(1,I)  PHKK(2,I)  PHKK(3,I)
     &   PHKK(4,I)  PHKK(5,I)  VHKK(1,I) VHKK(2,I) VHKK(3,I)'
         ENDIF
         WRITE(FOUT,*)'==========================================='
      ENDIF

c...event wise variables output      
      IF(IJPROJ.EQ.1) THEN
         WRITE(FOUT,'(9I11,F17.5)') 0, NEVHKK,IPROCE,MSPR,IPSTATUS,
     &   NWTSAM,NWASAM,NWBSAM,NHKK,BIMPAC
      ELSEIF(IJPROJ.EQ.7) THEN
c...0 initial to be recongonized by eic root maker code
         WRITE(FOUT,'(5I8,7F17.5,2I6,2F17.5,2I6)') 0,NEVHKK,IPROCE,
     &   MSPR,IPSTATUS,W2OUT,NUOUT,Q2OUT,XBJOUT,YYOUT,THETA,PFXCHK,
     &   NF2,NF1,XPART2,XPART1,NUCL,3+NHKK
      ENDIF
      WRITE(FOUT,*)'==========================================='


c...particle wise varaibles output
      DO  I=1,NHKK

         !initialize output index
         INDX=I
c...add beam particle and out e- info for ep mode output structure      
         IF(IJPROJ.EQ.7) THEN
c...modify the mother/daugther relation when more info put in
            DO J=1,2
               IF(JMOHKK(J,I).GT.0) THEN
                  JMOHKK(J,I)=JMOHKK(J,I)+4
               ENDIF
               IF(JDAHKK(J,I).GT.0) THEN
                  JDAHKK(J,I)=JDAHKK(J,I)+4
               ENDIF
            ENDDO
            IF(I.EQ.1) THEN
               WRITE(FOUT,996) 1,21,11,IDUM,IDUM,IDUM,
     &         eBeamx,eBeamy,-eBeamz,eBeam0,0.000511,
     &         0.,0.,0.

               WRITE(FOUT,996) 2,21,2212,IDUM,
     &         IDUM,IDUM,0.,0.,P,E,PPA(4),0.,0.,0.

c...output for scattered e-
c...mother1 set as 3 to be recongonized by eic root maker code
               WRITE(FOUT,996) 3,1,11,3,
     &         IDUM,IDUM,PL1,PL2,-PL3,PL4,0.000511,0.,0.,0.
c...transform gamma to lab frame
               CALL DT_DALTRA(GMA, 0.0D0,0.0D0,-BGT*GMA,PPG(1),PPG(2),
     &         PPG(3),PPG(4),PDUMMY,P1,P2,P3,P4)
c...output for gamma
               WRITE(FOUT,996) I+3,21,IDHKK(I),1,
     &         JDAHKK(1,I),JDAHKK(2,I),P1,P2,-P3,P4,0.,0.,0.,0.
            ENDIF

c...transform final particles from N-rest to Lab frame for ep mode
            IF((ABS(ISTHKK(I)).EQ.1).OR.(ISTHKK(I).EQ.1000).OR.
     &                                  (ISTHKK(I).EQ.1001)) THEN
               CALL DT_DALTRA(GMA, 0.0D0,0.0D0,-BGT*GMA,PHKK(1,I),
     &         PHKK(2,I),PHKK(3,I),PHKK(4,I),PDUMMY,P1,P2,P3,P4)
               PHKK(1,I)=P1
               PHKK(2,I)=P2
               PHKK(3,I)=-P3
               PHKK(4,I)=P4
            ENDIF
            !reassign output index
            INDX=I+4

         ENDIF

         !add extended info, IDRES, IDXRES and so on for AA mode
         IF(IJPROJ.EQ.1) THEN
            WRITE(FOUT,995) INDX,ISTHKK(I),IDHKK(I),
     &     JMOHKK(1,I),JDAHKK(1,I),JDAHKK(2,I),
     &     PHKK(1,I),PHKK(2,I),PHKK(3,I),PHKK(4,I),
     &     PHKK(5,I),VHKK(1,I),VHKK(2,I),VHKK(3,I)
C 995        FORMAT(2I5,6I9,5F17.5,3E15.6)
 995        FORMAT(2I5,4I9,5F17.5,3E15.6)
         ELSEIF(IJPROJ.EQ.7) THEN
            WRITE(FOUT,996) INDX,ISTHKK(I),IDHKK(I),
     &     JMOHKK(1,I),JDAHKK(1,I),JDAHKK(2,I),
     &     PHKK(1,I),PHKK(2,I),PHKK(3,I),PHKK(4,I),
     &     PHKK(5,I),VHKK(1,I),VHKK(2,I),VHKK(3,I)
 996        FORMAT(2I5,4I9,5F17.5,3E15.6)
         ENDIF
         !as VHKK in mm, very small, use scientific precision
         !to be noticed, VHKK is the variable used for coordinates
         !in target rest frame by incorporating bimpact in x direction
         !which means VHKK(1,I)=x(in target rest) + bimpact
      ENDDO
      WRITE(FOUT,*)'=============== Event finished ==============='


*  At any time during the run a list of the actual entries in DTEVT1 and
*  DTEVT2 can be obtained (output unit 6) by the following statement:
C     CALL DT_EVTOUT(4)

      RETURN
*
*------------------------------------------------------------------
*
    3 CONTINUE
*
* output/statistics/histograms etc.
*
*  Called with MODE=3 once after all events have been sampled.
*
      CALL PHO_PHIST(3000,DUM) 
      CALL DT_STATIS(2)
      RETURN

      END

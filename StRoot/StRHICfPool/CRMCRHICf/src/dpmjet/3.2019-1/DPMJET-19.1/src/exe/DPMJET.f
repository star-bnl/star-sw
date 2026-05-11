      PROGRAM DPMJET
 
      IMPLICIT NONE
      DOUBLE PRECISION dum , elab , epn
      INTEGER ICAsca , idp , iemu , ievt , irej , kkmat , NEVent , 
     &        nevts , npchar , npmass , ntchar , ntmass
      SAVE 
 
C block data in DPMJET library (uncomment these declarations if library
C option is used)
C     EXTERNAL DT_BDEVAP,DT_BDNOPT,DT_BDPREE,DT_HADPRP,DT_BLKD46,
C    &         DT_BLKD47,DT_RUNTT,DT_NONAME,DT_ZK,DT_BLKD43
 
C     EXTERNAL PYDATA
 
C event flag
      COMMON /DTEVNO/ NEVent , ICAsca
      INCLUDE 'inc/dtflka'
C-----------------------------------------------------------------------
C initialization
      LPRi = 20
      LOUt = 6
C   the following statement provides a call to DT_USRHIS(MODE=1) for
C   histogram initialization etc.
      CALL DT_DTUINI(nevts,epn,npmass,npchar,ntmass,ntchar,idp,iemu)
      WRITE (6,*) "****************************"
      WRITE (6,*) nevts , epn , npmass , npchar , ntmass , ntchar , 
     &            idp , iemu
C-----------------------------------------------------------------------
C generation of events
 
      DO ievt = 1 , nevts
 
C   some defaults, do not change!
         NEVent = ievt
         kkmat = -1
         elab = epn
C   uncomment if dpmjet3 is linked to particle transport code
C        ICASCA = 1
 
C***********************************************************************
C The following lines show how to select the target nucleus for runs
C with composite targets (and fixed projectile and energy!).
C
C   Sampling of the target nucleus (mass number NTMASS, charge NTCHAR)
C   according to the fractions defined with EMULSION input-cards.
C   The different nuclei are numbered as KKMAT = 1,2,3,...  according to
C   their appearance in the input-file.
         IF ( iemu.GT.0 ) THEN
C   Replace this selection by your own one if needed.
            CALL DT_GETEMU(ntmass,ntchar,kkmat,0)
C   Kkmat has to be negative for composite targets!
            kkmat = -kkmat
         END IF
C***********************************************************************
 
C***********************************************************************
C The following lines show how to define projectile, target and energy
C for this event in runs with Glauber-data file pre-initialized for a
C certain range of projectiles, targets and energies. The definitions
C have to be within the pre-initialized parameter range.
C
C   projectile-id (for hadron projectiles)
C        IDP    = 1
C   projectile mass and charge numbers
C        NPMASS = 12
C        NPCHAR = 6
C   target mass and charge numbers
C        NTMASS = 16
C        NTCHAR = 8
C   lab energy
C        ELAB = 200.0D0
C***********************************************************************
 
C***********************************************************************
C If an energy-range has been defined with the ENERGY input-card the
C laboratory energy ELAB can be set to any value within that range. For
C example:
C        ELO  = 10.0D0
C        EHI  = 1000.0D0
C        ELAB = DT_RNDM(ELAB)*(EHI-ELO)+ELO
C***********************************************************************
 
C   sampling of one event
         CALL DT_KKINC(npmass,npchar,ntmass,ntchar,idp,elab,kkmat,irej)
c          WRITE (6,*) "EVE" , npmass , npchar , ntmass , ntchar , idp , 
c      &               elab , kkmat , irej
 
C   the following statement provides a call to DT_USRHIS(MODE=2) from
C   where the final state particles can be obtained
 
         IF ( irej.EQ.0 ) CALL PHO_PHIST(2000,dum)
 
      END DO
 
C-----------------------------------------------------------------------
C output, statistics etc.
 
C   the following statement provides a call to DT_USRHIS(MODE=3) in
C   order to calculate histograms etc.
      CALL DT_DTUOUT
 
      END PROGRAM
 
C *$ CREATE DT_USRHIS.FOR
C *COPY DT_USRHIS
C *
C *===usrhis=============================================================*
C *
C       SUBROUTINE DT_USRHIS(MODE)
 
C       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C       SAVE
C *
C * COMMON /DTEVT1/ :
C *                   NHKK         number of entries in common block
C *                   NEVHKK       number of the event
C *                   ISTHKK(i)    status code for entry i
C *                   IDHKK(i)     identifier for the entry
C *                                (for particles: identifier according
C *                                 to the PDG numbering scheme)
C *                   JMOHKK(1,i)  pointer to the entry of the first mother
C *                                of entry i
C *                   JMOHKK(2,i)  pointer to the entry of the second mother
C *                                of entry i
C *                   JDAHKK(1,i)  pointer to the entry of the first daughter
C *                                of entry i
C *                   JDAHKK(2,i)  pointer to the entry of the second daughter
C *                                of entry i
C *                   PHKK(1..3,i) 3-momentum
C *                   PHKK(4,i)    energy
C *                   PHKK(5,i)    mass
C *
C * event history
C         include 'dtevt1'
C * extended event history
C         include 'dtevt2'
 
C         GOTO (1,2,3) MODE
 
C *------------------------------------------------------------------
C *
C     1 CONTINUE
C *
C * initializations
C *
C *  Called with MODE=1 once at the beginning of the run.
C *
C         RETURN
C *
C *------------------------------------------------------------------
C *
C     2 CONTINUE
C *
C * scoring of the present event
C *
C *  Called with MODE=2 every time one event has been finished.
C *
C *  The final state particles from the actual event (number NEVHKK)
C *  can be found in DTEVT1 and identified by their status:
C *
C *     ISTHKK(i) = 1    final state particle produced in
C *                      photon-/hadron-/nucleon-nucleon collisions or
C *                      in intranuclear cascade processes
C *                -1    nucleons, deuterons, H-3, He-3, He-4 evaporated
C *                      from excited nucleus and
C *                      photons produced in nuclear deexcitation processes
C *                1001  residual nucleus (ground state)
C *
C *  The types of these particles/nuclei are given in IDHKK as follows
C *
C *     all final state part. except nuclei :
C *       IDHKK(i)=particle identifier according to PDG numbering scheme
C *     nuclei (evaporation products, and residual nucleus) :
C *       IDHKK(i)=80000, IDRES(i)=mass number, IDXRES(i)=charge number
C *
C *  The 4-momenta and masses can be found in PHKK (target nucleus rest frame):
C *                   PHKK(1..3,i) 3-momentum (p_x,p_y,p_z)
C *                   PHKK(4,i)    energy
C *                   PHKK(5,i)    mass
C *
C *
C *
C *  Pick out the final state particles from DTEVT1 in each event for
C *  instance by the following loop (NHKK=number of entries in the present
C *  event) and fill your histograms
C C     DO 20 I=1,NHKK
C C        IF (ABS(ISTHKK(I)).EQ.1) THEN
C C        ELSEIF (ABS(ISTHKK(I)).EQ.1001) THEN
C C        ENDIF
C C  20 CONTINUE
 
C *  At any time during the run a list of the actual entries in DTEVT1 and
C *  DTEVT2 can be obtained (output unit 6) by the following statement:
C C     CALL DT_EVTOUT(4)
 
C       RETURN
C *
C *------------------------------------------------------------------
C *
C     3 CONTINUE
C *
C * output/statistics/histograms etc.
C *
C *  Called with MODE=3 once after all events have been sampled.
C *
C       RETURN
 
C       END

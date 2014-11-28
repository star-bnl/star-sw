
* $Id: calini.F,v 1.2 2012/07/26 00:12:30 perev Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE CALINI
C**************************************************************
C
C           INITIALIZATION of CALOR
C           =======================
C
C  Called by : CALSIG , GCALOR
C
C  Author: Christian Zeitnitz 27.5.92
C
C**************************************************************
C
C GEANT COMMON
#include "geant321/gccuts.inc"
#include "geant321/gctime.inc"
#include "geant321/gcbank.inc"
#include "geant321/gcflag.inc"
#include "geant321/gcunit.inc"
C CALOR COMMONS
#include "gcalor/calgea.inc"
#include "gcalor/ccomon.inc"
#include "gcalor/cjoint.inc"
#include "gcalor/cinout.inc"
#include "gcalor/cxpd.inc"
#include "gcalor/cmagnt.inc"
#include "gcalor/chie.inc"
#include "gcalor/cgeos.inc"
#include "gcalor/ctncol.inc"
#include "geant321/minput.inc"
#include "geant321/cerrcm.inc"
#include "geant321/camass.inc"
C
      DIMENSION IPID(0:11)
      LOGICAL INIT,OPENED,EXISTS
      CHARACTER*256 BERTF
      CHARACTER*20  VERSQQ
      Character*8   DATE
      CHARACTER*20  NAP
#if defined(ATLAS_UNIX)||defined(ATLAS_CRAY)||defined(ATLAS_VAX)
      CHARACTER*100 CHROOT
#endif
      DATA INIT/.TRUE./
C GEANT Particle IDs used to extract masses from GEANT
      DATA IPID /14 , 13 , 8 , 7 , 9 , 5 , 6 , 45 , 46 , 49 , 47 , 1/
C
      IF(.NOT.INIT) RETURN
      INIT = .FALSE.
      VERSQQ = CPP_VERS
      CALL GCDATE(IDAT,ITIM)
      IYEAR = IDAT/10000
      IMONTH= (IDAT-IYEAR*10000)/100
      IDAY  = IDAT-(IDAT/100)*100
      WRITE(DATE,'(I2,''.'',I2,''.'',I2)') IDAY,IMONTH,IYEAR
      PRINT*,'******************************************************'
      PRINT*,'*                                                    *'
      PRINT*,'*    GEANT - CALOR Interface  ',     VERSQQ,     '   *'
      PRINT*,'*    -----------------------------------------       *'
      PRINT*,'*        ',DATE,'  C.Zeitnitz, T.A.Gabriel           *'
      PRINT*,'*                                                    *'
      PRINT*,'*     NMTC is used for hadronic interactions of      *'
      PRINT*,'*        protons,neutrons and charged pions          *'
      PRINT*,'*   up to 3.5 GeV (proton,neutron), 2.5 GeV(pion)    *'
      PRINT*,'*                                                    *'
      PRINT*,'*    A Scaling Model is used for the energy range    *'
      PRINT*,'*                 up to 10 GeV.                      *'
      PRINT*,'*                                                    *'
      PRINT*,'*      MICAP is calculating the interaction of       *'
      PRINT*,'*       Neutrons with an energy below 20 MeV         *'
      PRINT*,'*                                                    *'
      PRINT*,'*   For interactions of hadrons not implemented in   *'
      PRINT*,'*        CALOR or with an energy above 10 GeV        *'
      PRINT*,'*                 FLUKA is called                    *'
      PRINT*,'*                                                    *'
      PRINT*,'*  The transport of electrons, positrons and gammas  *'
      PRINT*,'*                 is done by GEANT                   *'
      PRINT*,'*                                                    *'
      PRINT*,'*     All output is written to file calor.out        *'
      PRINT*,'*                                                    *'
      PRINT*,'******************************************************'
      PRINT '('' *        Neutron cutoff energy='',G10.2,  '
     +        //' '' eV         *'')',CUTNEU*1.E9
      PRINT*,'******************************************************'
C
C fill particle mass array
      DO 10 I=0,11
         CALL GFPART(IPID(I),NAP,ITR,AM,CH,TL,UB,NW)
         XMASS(I)=AM
   10 CONTINUE
      INIT = .FALSE.
      ICPROC = -1
      IN = 5
      EHIN = 3495.0
      EHIPI = 2495.0
      EMAX = 3500.0
      NBERTP = 30
      INQUIRE(UNIT=NBERTP,OPENED=OPENED)
      IF(OPENED) THEN
         REWIND NBERTP
      ELSE
#if defined(ATLAS_UNIX)||defined(ATLAS_CRAY)
         BERTF='chetc.dat'
         CALL GUFILE(BERTF)
         INQUIRE(FILE=BERTF,EXIST=EXISTS)
         IF(.NOT.EXISTS) THEN
            CHROOT = ' '
            CALL GETENV('CERN_ROOT',CHROOT)
            LNROOT = LNBLNK(CHROOT)
            IF(LNROOT.GT.0)BERTF = CHROOT(1:LNROOT)//'/lib/chetc.dat'
         ENDIF
         INQUIRE(FILE=BERTF,EXIST=EXISTS)
         IF(.NOT.EXISTS) THEN
            CHROOT = ' '
            CALL GETENV('STAR',CHROOT)
            LNROOT = LNBLNK(CHROOT)
            IF(LNROOT.GT.0) then
	      BERTF = CHROOT(1:LNROOT)//'/pams/sim/gphysdata/chetc.dat'
            endif
         ENDIF
         INQUIRE(FILE=BERTF,EXIST=EXISTS)
         IF(.NOT.EXISTS) THEN
           PRINT*,'**********************************'
           PRINT*,'*        G C A L O R             *'
           PRINT*,'*        -----------             *'
           PRINT*,'*   File CHETC.DAT not found     *'
           PRINT*,'*         Program STOP           *'
           PRINT*,'**********************************'
           STOP
         ENDIF
         OPEN(UNIT = NBERTP,FILE=BERTF, FORM = 'FORMATTED',STATUS=
     +   'OLD')
#endif
#if defined(ATLAS_IBMVM)
         BERTF = '/CHETC DAT *'
         OPEN(UNIT = NBERTP,FILE=BERTF, FORM = 'FORMATTED',STATUS=
     +   'OLD')
#endif
#if defined(ATLAS_VAX)
         BERTF='chetc.dat'
         INQUIRE(FILE=BERTF,EXIST=EXISTS)
         IF(.NOT.EXISTS) THEN
            ISTAT = LIB$SYS_TRNLOG('CERN_ROOT',NALL,CHROOT,,,%VAL(0))
            IF(ISTAT.EQ.1) BERTF='CERN_ROOT:[LIB]chetc.dat'
         ENDIF
         INQUIRE(FILE=BERTF,EXIST=EXISTS)
         IF(.NOT.EXISTS) THEN
           PRINT*,'**********************************'
           PRINT*,'*        G C A L O R             *'
           PRINT*,'*        -----------             *'
           PRINT*,'*   File CHETC.DAT not found     *'
           PRINT*,'*         Program STOP           *'
           PRINT*,'**********************************'
           STOP
         ENDIF
         OPEN(UNIT = NBERTP,FILE=BERTF, FORM = 'FORMATTED',STATUS=
     +   'OLD',READONLY)
#endif
      ENDIF
C Output unit for Neutron information and error messages
      IOUT = 32
      IERRU = IOUT
#if defined(ATLAS_VAX)
      OPEN(UNIT=IOUT,FILE='CALOR.OUT',FORM='FORMATTED',
     +     STATUS='NEW')
#endif
#if defined(ATLAS_UNIX)||defined(ATLAS_CRAY)
      OPEN(UNIT=IOUT,FILE='calor.out',FORM='FORMATTED',
     +     STATUS='UNKNOWN')
#endif
#if defined(ATLAS_IBMVM)
      OPEN(UNIT=IOUT,FILE='\CALOR OUT *',FORM='FORMATTED',
     +     STATUS='UNKNOWN')
#endif
C PN, 01.06.2003:
      if (Idebug.gt.0) IOUT=LOUT
      IO = IOUT
      WRITE(IOUT,'(/, '
     +    //' 18X,''GEANT-CALOR INTERFACE '',A,'' Output File'',   '
     +    //'/18X,''=============================================''/)')
     +           VERSQQ
C read bert cascade and evaporation dataset
      CALL CRBERT
      CLOSE(UNIT=NBERTP)
      ELOP = AMAX1(1.0,CUTHAD * 1000.0)
      ELON = AMAX1(20.0,CUTNEU * 1000.0)
      NPOWR2 = 11
      NGROUP = 2**NPOWR2
      EMUCUT = AMAX1(1.0,CUTMUO * 1000.0)
      EPICUT = ELOP
      NPIDK =  -1
      CTOFE = 0.0
      CTOFEN = 0.0
      ANDIT = 0.0
      NEXITE = 1
      NBOGUS = NEXITE
      ELAS = 0.0
      BODGE = 0.0
      EMIN(1) = ELOP
      EMIN(2) = ELON
      EMIN(3) = EPICUT
      EMIN(4) = EPICUT
      EMIN(5) = EPICUT
      EMIN(6) = EMUCUT
      EMIN(7) = EMUCUT
C now get some cross sections
      CALL CREADG(GEOSIG)
      CALL GTHSIG(1)
C No decay cross-section needed in GEANT
C      SGPIMX = 0.001259/SQRT ((EMIN(3)/139.9+1.)**2 -1.)
C      SGMUMX = 1.587E-5/SQRT ((EMIN(6)/107.+1.)**2 -1.)
      SGPIMX = 0.0
      SGMUMX = 0.0
      NCOL = 1
C ------------ initialize MICAP ------------------------
      CALL MORINI
C perform garbage collection in constant division
      CALL MZGARB(IXCONS,0)
      RETURN
      END

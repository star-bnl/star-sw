      DOUBLE PRECISION FUNCTION EXMSAZ(AIT,AITZ,FLAG,IZDUM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      LOGICAL FLAG
      EXMSAZ = 0.D0
      END

      SUBROUTINE EVEVAP(WE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      END

      SUBROUTINE EXPLOD ( NPEXPL, AMEXPL, ETOTEX, ETEXPL, PXEXPL,
     &              PYEXPL, PZEXPL )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      END

      SUBROUTINE FLRNOC (ISDRN1,ISDRN2,ISEED1,ISEED2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      END

      SUBROUTINE GLAUBR(PPROJ,UMO,IBPROJ,IT,IP,info,Barr)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      DOUBLE PRECISION info(4), Barr(200)
      END

      DOUBLE PRECISION FUNCTION PFRMAV(I)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      PFRMAV = 1.D0
      RETURN
      END

      INTEGER FUNCTION MCIHAD(IDPDG)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      MCIHAD = IDT_ICIHAD(IDPDG)
      RETURN
      END

      INTEGER FUNCTION MPDGHA(I)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      MPDGHA = IDT_IPDGHA(I)
      RETURN
      END

      DOUBLE PRECISION FUNCTION JLL_SAMDSDT()
      IMPLICIT NONE
      SAVE
      JLL_SAMDSDT = 0.D0
      RETURN
      END

      SUBROUTINE JLL_SET
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      RETURN
      END


*COPY OAUXFI
*
*=== Oauxfi ===========================================================*
*
      SUBROUTINE OAUXFI ( FILE, IONUMB, CHSTTS, IERR )

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
      PARAMETER ( KALGNM = 2 )
      PARAMETER ( KALCH8 = 1 )
      PARAMETER ( I2ALGN = 2 )
      PARAMETER ( ANGLGB = 5.0D-16 )
      PARAMETER ( ANGLSQ = 2.5D-31 )
      PARAMETER ( AXCSSV = 0.2D+16 )
      PARAMETER ( ANDRFL = 1.0D-38 )
      PARAMETER ( AVRFLW = 1.0D+38 )
      PARAMETER ( AINFNT = 1.0D+30 )
      PARAMETER ( AZRZRZ = 1.0D-30 )
      PARAMETER ( EINFNT = +69.07755278982137 D+00 )
      PARAMETER ( EZRZRZ = -69.07755278982137 D+00 )
      PARAMETER ( EXCSSV = +35.23192357547063 D+00 )
      PARAMETER ( ENGLGB = -35.23192357547063 D+00 )
      PARAMETER ( ONEMNS = 0.999999999999999  D+00 )
      PARAMETER ( ONEPLS = 1.000000000000001  D+00 )
      PARAMETER ( CSNNRM = 2.0D-15 )
      PARAMETER ( DMXTRN = 1.0D+08 )
      PARAMETER ( RHFLMN = 1.0D-10 )
      REAL ZERSNG
      PARAMETER ( ZERSNG = 0.E+00 )
      PARAMETER ( ZERZER = 0.D+00 )
      PARAMETER ( ONEONE = 1.D+00 )
      PARAMETER ( TWOTWO = 2.D+00 )
      PARAMETER ( THRTHR = 3.D+00 )
      PARAMETER ( FOUFOU = 4.D+00 )
      PARAMETER ( FIVFIV = 5.D+00 )
      PARAMETER ( SIXSIX = 6.D+00 )
      PARAMETER ( SEVSEV = 7.D+00 )
      PARAMETER ( EIGEIG = 8.D+00 )
      PARAMETER ( ANINEN = 9.D+00 )
      PARAMETER ( TENTEN = 10.D+00 )
      PARAMETER ( ELEVEN = 11.D+00 )
      PARAMETER ( TWELVE = 12.D+00 )
      PARAMETER ( FIFTEN = 15.D+00 )
      PARAMETER ( SIXTEN = 16.D+00 )
      PARAMETER ( HLFHLF = 0.5D+00 )
      PARAMETER ( ONETHI = ONEONE / THRTHR )
      PARAMETER ( ONEFOU = ONEONE / FOUFOU )
      PARAMETER ( ONEFIV = ONEONE / FIVFIV )
      PARAMETER ( ONESIX = ONEONE / SIXSIX )
      PARAMETER ( ONESEV = ONEONE / SEVSEV )
      PARAMETER ( ONEEIG = ONEONE / EIGEIG )
      PARAMETER ( TWOTHI = TWOTWO / THRTHR )
      PARAMETER ( THRFOU = THRTHR / FOUFOU )
      PARAMETER ( THRTWO = THRTHR / TWOTWO )
      PARAMETER ( FOUTHR = FOUFOU / THRTHR )
      PARAMETER ( PIPIPI = 3.141592653589793238462643383279D+00 )
      PARAMETER ( TWOPIP = 6.283185307179586476925286766559D+00 )
      PARAMETER ( PIP5O2 = 7.853981633974483096156608458199D+00 )
      PARAMETER ( PIPISQ = 9.869604401089358618834490999876D+00 )
      PARAMETER ( PIHALF = 1.570796326794896619231321691640D+00 )
      PARAMETER ( R3TOVL = FOUFOU * PIPIPI / THRTHR )
      PARAMETER ( ERFA00 = 0.886226925452758013649083741671D+00 )
      PARAMETER ( SQRTPI = 1.772453850905516027298167483341D+00 )
      PARAMETER ( SQTWPI = 2.506628274631000502415765284811D+00 )
      PARAMETER ( EULERO = 0.577215664901532860606512      D+00 )
      PARAMETER ( EULEXP = 1.781072417990197985236504      D+00 )
      PARAMETER ( EULLOG =-0.5495393129816448223376619     D+00 )
      PARAMETER ( E1M2EU = 0.8569023337737540831433017     D+00 )
      PARAMETER ( ENEPER = 2.718281828459045235360287471353D+00 )
      PARAMETER ( SQRENT = 1.648721270700128146848650787814D+00 )
      PARAMETER ( SQRTWO = 1.414213562373095048801688724210D+00 )
      PARAMETER ( SQRTHR = 1.732050807568877293527446341506D+00 )
      PARAMETER ( SQRFIV = 2.236067977499789696409173668731D+00 )
      PARAMETER ( SQRSIX = 2.449489742783178098197284074706D+00 )
      PARAMETER ( SQRSEV = 2.645751311064590590501615753639D+00 )
      PARAMETER ( SQRT12 = 3.464101615137754587054892683012D+00 )
      PARAMETER ( S2FWHM = 2.354820045030949382023138652919D+00 )
      PARAMETER ( TWOLOG = 0.693147180559945309417232121458D+00 )
      PARAMETER ( TWO2O3 = 1.587401051968199474751705639272D+00 )
      PARAMETER ( TENLOG = 2.302585092994045684017991454684D+00 )
      PARAMETER ( ATNFOU = 1.3258176636680326D+00 )
      PARAMETER ( ATNSIX = 1.4056476493802699D+00 )
      PARAMETER ( CLIGHT = 2.99792458         D+10 )
      PARAMETER ( AVOGAD = 6.0221367          D+23 )
      PARAMETER ( BOLTZM = 1.380658           D-23 )
      PARAMETER ( AMELGR = 9.1093897          D-28 )
      PARAMETER ( PLCKBR = 1.05457266         D-27 )
      PARAMETER ( ELCCGS = 4.8032068          D-10 )
      PARAMETER ( ELCMKS = 1.60217733         D-19 )
      PARAMETER ( AMUGRM = 1.6605402          D-24 )
      PARAMETER ( AMMUMU = 0.113428913        D+00 )
      PARAMETER ( AMPRMU = 1.007276470        D+00 )
      PARAMETER ( AMNEMU = 1.008664904        D+00 )
      PARAMETER ( EPSIL0 = 8.854187817        D-12 )
      PARAMETER ( ALPFSC = 7.2973530791728595 D-03 )
      PARAMETER ( FSCTO2 = 5.3251361962113614 D-05 )
      PARAMETER ( FSCTO3 = 3.8859399018437826 D-07 )
      PARAMETER ( FSCTO4 = 2.8357075508200407 D-09 )
      PARAMETER ( PLABRC = 0.197327053        D+00 )
      PARAMETER ( AMELCT = 0.51099906         D-03 )
      PARAMETER ( AMUGEV = 0.93149432         D+00 )
      PARAMETER ( AMMUON = 0.105658389        D+00 )
      PARAMETER ( AMPRTN = 0.93827231         D+00 )
      PARAMETER ( AMNTRN = 0.93956563         D+00 )
      PARAMETER ( AMDEUT = 1.87561339         D+00 )
      PARAMETER ( AMALPH = 3.72738025692891   D+00 )
      PARAMETER ( COUGFM = ELCCGS * ELCCGS / ELCMKS * 1.D-07 * 1.D+13
     &                   * 1.D-09 )
      PARAMETER ( RCLSEL = 2.8179409183694872 D-13 )
      PARAMETER ( ALAMB0 = TWOTWO * PIPIPI * RCLSEL / ALPFSC )
      PARAMETER ( BLTZMN = 8.617385           D-14 )
      PARAMETER ( A0BOHR = PLABRC / ALPFSC / AMELCT )
      PARAMETER ( GFOHB3 = 1.16639            D-05 )
      PARAMETER ( GFERMI = GFOHB3 * PLABRC * PLABRC * PLABRC )
      PARAMETER ( SIN2TW = 0.2319             D+00 )
      PARAMETER ( PRMGNM = 2.792847386        D+00 )
      PARAMETER ( ANMGNM =-1.91304275         D+00 )
      PARAMETER ( REARTH = 6.378140           D+08 )
      PARAMETER ( AUASTU = 1.4959787066       D+13 )
      PARAMETER ( GEVMEV = 1.0                D+03 )
      PARAMETER ( EV2GEV = 1.0                D-09 )
      PARAMETER ( GEV2EV = 1.0                D+09 )
      PARAMETER ( EMVGEV = 1.0                D-03 )
      PARAMETER ( CMQ2MB = 1.0                D+27 )
      PARAMETER ( FMB2BA = 1.0                D-03 )
      PARAMETER ( BAR2MB = 1.0                D+03 )
      PARAMETER ( FMB2FS = 1.0                D-01 )
      PARAMETER ( FMS2MB = 1.0                D+01 )
      PARAMETER ( BA2CMQ = 1.0                D-24 )
      PARAMETER ( CMQ2BA = 1.0                D+24 )
      PARAMETER ( ALGVMV = 6.90775527898214   D+00 )
      PARAMETER ( RADDEG = 180.D+00 / PIPIPI )
      PARAMETER ( DEGRAD = PIPIPI / 180.D+00 )
      PARAMETER ( GEVOMG = CLIGHT * 1.D+13 / PLABRC )
      PARAMETER ( S0THMS = EIGEIG / THRTHR * PIPIPI * RCLSEL * RCLSEL
     &                   * CMQ2MB )
      PARAMETER ( FERTHO = 14.33       D-09 )
      PARAMETER ( EXPEBN = 2.39        D+00 )
      PARAMETER ( BEXC12 = FERTHO * 72.40715579499394D+00 )
      PARAMETER ( AMUNMU = HLFHLF * AMELCT - BEXC12 / 12.D+00 )
      PARAMETER ( AMUC12 = AMUGEV - AMUNMU )
      PARAMETER ( AMEMEV = GEVMEV * AMELCT )
      PARAMETER ( T12INF = 1.D+30 )
      PARAMETER ( T12ZER = 1.D-15 )
      LOGICAL LFLUKA, LGBIAS, LGBANA, LFLGEO, LOFLTS, LUSRIN,
     &        LUSRGL, LNMGEO, LNMINP, LFRFMT, LDMPCR
      LOGICAL LFDRTR
      COMMON / GLOBAL / LFLUKA, LGBIAS, LGBANA, LFLGEO, LOFLTS, LUSRIN,
     &                  LUSRGL, LNMGEO, LNMINP, LFRFMT, LDMPCR,
     &                  LFDRTR,
     &                  KFLGEO, KFLDNR
      COMMON / GLOBCH / CRVRFL
      CHARACTER*8 CRVRFL
      SAVE / GLOBAL /, / GLOBCH /

      PARAMETER ( MXXRGN =20000 )
      PARAMETER ( MXXMDF =  710 )
      PARAMETER ( MXXMDE =  702 )
      PARAMETER ( MFSTCK =70000 )
      PARAMETER ( MESTCK =  100 )
      PARAMETER ( MOSTCK = 2000 )
      PARAMETER ( MXPRSN =  100 )
      PARAMETER ( MXPDPM =  800 )
      PARAMETER ( MXPSCS =60000 )
      PARAMETER ( MXFRAG =   20 )
      PARAMETER ( MXGLWN =  300 )
      PARAMETER ( MXOUTU =   50 )
      PARAMETER ( MXKNWC =   13 )
      PARAMETER ( MXESHL =   32 )
      PARAMETER ( MXGNPR =   60 )
      PARAMETER ( KXHEAV =   30 )
      PARAMETER ( NALLWP =   64 )
      PARAMETER ( NELEMX =   80 )
      PARAMETER ( MPDPDX =   33 )
      PARAMETER ( MXHTTR =  260 )
      PARAMETER ( MXSEAX =   30 )
      PARAMETER ( MXHTNC = MXSEAX + 1 )
      PARAMETER ( ICOMAX = 2400 )
      PARAMETER ( ICHMAX = ICOMAX + MXXMDF )
      PARAMETER ( NSTBIS =  304 )
      PARAMETER ( NQSTIS =   46 )
      PARAMETER ( NTSTIS = NSTBIS + NQSTIS )
      PARAMETER ( MXPABL =  120 )
      PARAMETER ( IDMAXP =  450 )
      PARAMETER ( IDMXDC = 2000 )
      PARAMETER ( MXMCIN =  410 )
      PARAMETER ( IHYPMX =    4 )
      PARAMETER ( MKBMX1 =   11 )
      PARAMETER ( MKBMX2 =   11 )
      PARAMETER ( MXIRRD = 2500 )
      PARAMETER ( MXTRDC = 1500 )
      PARAMETER ( NKTL   =   17 )
      PARAMETER ( MXNBLN = 55000000 )
      PARAMETER ( NBLNMX = MXNBLN )

      PARAMETER ( LUNIN  =  5 )
      PARAMETER ( LUNOUT = 11 )
      PARAMETER ( LUNERR = 15 )
      PARAMETER ( LUNBER = 14 )
      PARAMETER ( LUNECH =  8 )
      PARAMETER ( LUNFLU = 13 )
      PARAMETER ( LUNGEO = 16 )
      PARAMETER ( LUNPMF = 12 )
      PARAMETER ( LUNRAN =  2 )
      PARAMETER ( LUNXSC =  9 )
      PARAMETER ( LUNDET = 17 )
      PARAMETER ( LUNRAY = 10 )
      PARAMETER ( LUNRDB =  1 )
      PARAMETER ( LUNRD2 = 18 )
      PARAMETER ( LUNDPM = 19 )
      PARAMETER ( LUNPGO =  7 )
      PARAMETER ( LUNPGS =  4 )
      PARAMETER ( LUNSCR =  3 )

*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1997-2013      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     Open AUXiliary FIle:                                             *
*                                                                      *
*     Created  on  30 January 1997  by   Alfredo Ferrari & Paola Sala  *
*                                              INFN - Milan            *
*                                                                      *
*     Last change  on   03-Feb-13   by     Alfredo Ferrari, INFN-Milan *
*                                                                      *
*          file   = file name                                          *
*          ionumb = logical unit number                                *
*          chstts = status word (optional, def. old)                   *
*          ierr   = error flag (output)                                *
*                                                                      *
*----------------------------------------------------------------------*
*
      CHARACTER COMPTR*50 , INPFIL*200, PWDDIR*200, HFLDIR*200,
     &          HOMDIR*200, HOSTNM*200, USRFLK*200, GRPFLK*200
      COMMON / COMPUT / CPUSPE, CPUJOB, KOMPUT, KPWDIR, KFLDIR, KHMDIR,
     &                  MXFTNU
      COMMON / CHCMPT / COMPTR, INPFIL, PWDDIR, HFLDIR, HOMDIR, HOSTNM,
     &                  USRFLK, GRPFLK
      SAVE / COMPUT /
      SAVE / CHCMPT /

*
      CHARACTER FILE*(*), CHSTTS*(*), CARD*132, FSTATS*10, FFORM*12,
     &          FACCSS*10
      LOGICAL LSCRAT, LFABRT
*
      LFABRT = IERR .NE. -1000000
      LSCRAT = .FALSE.
      IERR   = 0
*  +-------------------------------------------------------------------*
*  |  Status New:
      IF ( INDEX ( CHSTTS, 'NEW' ) .GT. 0 .OR.
     &     INDEX ( CHSTTS, 'new' ) .GT. 0 ) THEN
         FSTATS = 'NEW'
*  |
*  +-------------------------------------------------------------------*
*  |  Status Unknown:
      ELSE IF ( INDEX ( CHSTTS, 'UNKNOWN' ) .GT. 0 .OR.
     &          INDEX ( CHSTTS, 'unknown' ) .GT. 0 ) THEN
         FSTATS = 'UNKNOWN'
*  |
*  +-------------------------------------------------------------------*
*  |  Status Scratch:
      ELSE IF ( INDEX ( CHSTTS, 'SCRATCH' ) .GT. 0 .OR.
     &          INDEX ( CHSTTS, 'scratch' ) .GT. 0 ) THEN
         FSTATS = 'SCRATCH'
         LSCRAT = .TRUE.
*  |
*  +-------------------------------------------------------------------*
*  |  Status Old (default):
      ELSE
         FSTATS = 'OLD'
      END IF
*  |
*  +-------------------------------------------------------------------*
*  +-------------------------------------------------------------------*
*  |  Form Unformatted:
      IF ( INDEX ( CHSTTS, 'UNFORMATTED' ) .GT. 0 .OR.
     &     INDEX ( CHSTTS, 'unformatted' ) .GT. 0 ) THEN
         FFORM  = 'UNFORMATTED'
*  |
*  +-------------------------------------------------------------------*
*  |  Form Formatted:
      ELSE
         FFORM  = 'FORMATTED'
      END IF
*  |
*  +-------------------------------------------------------------------*
*  +-------------------------------------------------------------------*
*  |  Access direct:
      IF ( INDEX ( CHSTTS, 'DIRECT' ) .GT. 0 .OR.
     &     INDEX ( CHSTTS, 'direct' ) .GT. 0 ) THEN
         FACCSS = 'DIRECT'
*  |
*  +-------------------------------------------------------------------*
*  |  Access append:
      ELSE IF ( INDEX ( CHSTTS, 'APPEND' ) .GT. 0 .OR.
     &     INDEX ( CHSTTS, 'append' ) .GT. 0 ) THEN
         FACCSS = 'APPEND'
*  |
*  +-------------------------------------------------------------------*
*  |  Access sequential:
      ELSE
         FACCSS = 'SEQUENTIAL'
      END IF
*  |
*  +-------------------------------------------------------------------*
      IF ( .NOT. LSCRAT ) THEN
!          LQ   = MIN ( LNNBLN (FILE), 132 )
         CARD (1:LQ) = FILE (1:LQ)
      END IF
*  First of all: try to open the file in the current directory:
      IF ( LSCRAT ) THEN
         OPEN ( UNIT   = IONUMB,
     &          STATUS = FSTATS, FORM = FFORM , ACCESS = FACCSS,
     &          ERR    = 4000 )
      ELSE
         OPEN ( UNIT   = IONUMB, FILE = CARD (1:LQ),
     &          STATUS = FSTATS, FORM = FFORM , ACCESS = FACCSS,
     &          ERR    = 1000 )
      END IF
      RETURN
 1000 CONTINUE
*  Second attempt: try to open the file in the original work directory:
      OPEN ( UNIT   = IONUMB, FILE = PWDDIR (1:KPWDIR) // CARD (1:LQ),
     &       STATUS = FSTATS, FORM = FFORM , ACCESS = FACCSS,
     &       ERR    = 2000 )
      RETURN
 2000 CONTINUE
*  Third attempt: try to open the file in the FLUKA directory:
      OPEN ( UNIT   = IONUMB, FILE = HFLDIR (1:KFLDIR) // CARD (1:LQ),
     &       STATUS = FSTATS, FORM = FFORM , ACCESS = FACCSS,
     &       ERR    = 3000 )
      RETURN
 3000 CONTINUE
*  Last attempt: try to open the file in the user home directory:
      OPEN ( UNIT   = IONUMB, FILE = HOMDIR (1:KHMDIR) // CARD (1:LQ),
     &       STATUS = FSTATS, FORM = FFORM , ACCESS = FACCSS,
     &       ERR    = 4000 )
      RETURN
 4000 CONTINUE
*  +-------------------------------------------------------------------*
*  |  File opening was supposed to succeed (Lfabrt=.False. means the
*  |  file coud be or could be not exist, both acceptable)
      IF ( LFABRT ) THEN
         WRITE (LUNOUT,5000) CARD(1:LQ), IONUMB
 5000    FORMAT (' *** Impossible to open file ***',/,1X,A,/,
     &           ' *** on unit ',I4,' ***' )
! D        CALL FLABRT ( 'OAUXFI', 'IMPOSSIBLE TO OPEN FILE' )
      END IF
*  |
*  +-------------------------------------------------------------------*
      IERR = 1
      RETURN
*== End of subroutine Oauxfi ==========================================*
      END


      SUBROUTINE PHO_FITOUT(NOUT)
C*********************************************************************
C
C     print out all important data values
C
C     input parameter: NOUT   number of output unit
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE

      INTEGER LI,LO ,LPRI
      COMMON /POINOU/ LI,LO ,LPRI

      DOUBLE PRECISION ALPOM,ALPOMP,GP,B0POM,ALREG,ALREGP,GR,B0REG,
     &                 GPPP,GPPR,B0PPP,B0PPR,VDMFAC,VDMQ2F,B0HAR,AKFAC
      COMMON /POPREG/ ALPOM,ALPOMP,GP(2),B0POM(2),
     &                ALREG,ALREGP,GR(2),B0REG(2),
     &                GPPP,GPPR,B0PPP,B0PPR,
     &                VDMFAC(4),VDMQ2F(4),B0HAR,AKFAC

      DOUBLE PRECISION PHISUP,RMASS,VAR,AMPFAC,ELAFAC,VFAC
      COMMON /PO2CHA/ PHISUP(2),RMASS(2),VAR,AMPFAC(4),ELAFAC(4),VFAC

      INTEGER IFPAP,IFPAB
      DOUBLE PRECISION ECM,PCM,PMASS,PVIRT
      COMMON /POGCMS/ ECM,PCM,PMASS(2),PVIRT(2),IFPAP(2),IFPAB(2)

      CHARACTER*8 PDFNAM
      INTEGER IPARID,IPAVA,ITYPE,IGRP,ISET,IEXT,NPAOLD
      DOUBLE PRECISION PDFLAM,PDFQ2M
      COMMON /POPPDF/ PDFLAM(2),PDFQ2M(2),PDFNAM(2),IPARID(2),
     &                IPAVA(2),ITYPE(2),IGRP(2),ISET(2),IEXT(2),NPAOLD


C  write parameter table in fitpar.dat format
      WRITE(NOUT,'(A)') '****** PARAMETER TABLE ******'
      WRITE(NOUT,'(A)') 'NEXTDATA'
      WRITE(NOUT,'(I8,2X,A8,3I6)')
     &  IFPAP(1),PDFNAM(1),IGRP(1),ISET(1),IEXT(1)
      WRITE(NOUT,'(I8,2X,A8,3I6)')
     &  IFPAP(2),PDFNAM(2),IGRP(2),ISET(2),IEXT(2)
      WRITE(NOUT,'(4X,6F8.3)') ALPOM,ALPOMP,GP,B0POM
      WRITE(NOUT,'(4X,6F8.3)') ALREG,ALREGP,GR,B0REG
      WRITE(NOUT,'(4X,4F8.3)') GPPP,B0PPP,GPPR,B0PPR
      WRITE(NOUT,'(4X,4F8.5)') VDMFAC
      WRITE(NOUT,'(4X,F8.3)') B0HAR
      WRITE(NOUT,'(4X,F8.3)') AKFAC
      WRITE(NOUT,'(4X,2F8.3)') PHISUP
      WRITE(NOUT,'(4X,3F8.3)') RMASS,VAR

      IF(IFPAP(1).NE.IFPAP(2)) THEN
        WRITE(NOUT,'(A)') 'NEXTDATA'
        WRITE(NOUT,'(I8,2X,A8,3I6)')
     &    IFPAP(2),PDFNAM(2),IGRP(2),ISET(2),IEXT(2)
        WRITE(NOUT,'(I8,2X,A8,3I6)')
     &    IFPAP(1),PDFNAM(1),IGRP(1),ISET(1),IEXT(1)
        WRITE(NOUT,'(4X,6F8.3)') ALPOM,ALPOMP,GP(2),GP(1),
     &    B0POM(2),B0POM(1)
        WRITE(NOUT,'(4X,6F8.3)') ALREG,ALREGP,GR(2),GR(1),
     &    B0REG(2),B0REG(1)
        WRITE(NOUT,'(4X,4F8.3)') GPPP,B0PPP,GPPR,B0PPR
        WRITE(NOUT,'(4X,4F8.5)') VDMFAC(3),VDMFAC(4),
     &    VDMFAC(1),VDMFAC(2)
        WRITE(NOUT,'(4X,F8.3)') B0HAR
        WRITE(NOUT,'(4X,F8.3)') AKFAC
        WRITE(NOUT,'(4X,2F8.3)') PHISUP(2),PHISUP(1)
        WRITE(NOUT,'(4X,3F8.3)') RMASS(2),RMASS(1),VAR
      ENDIF

C  write parameter table as steering cards
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',301,ALPOM
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',302,ALPOMP
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',303,GP(1)
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',304,GP(2)
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',305,B0POM(1)
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',306,B0POM(2)
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',307,ALREG
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',308,ALREGP
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',309,GR(1)
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',310,GR(2)
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',311,B0REG(1)
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',312,B0REG(2)
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',313,GPPP
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',314,B0PPP
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',315,VDMFAC(1)
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',316,VDMFAC(2)
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',317,VDMFAC(3)
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',318,VDMFAC(4)
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',319,B0HAR
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',320,AKFAC
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',321,PHISUP(1)
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',322,PHISUP(2)
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',323,RMASS(1)
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',324,RMASS(2)
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',325,VAR
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',327,GPPR
      WRITE(NOUT,'(A,5X,I4,5X,1P,E12.4)') 'SETPARAM',328,B0PPR


      END



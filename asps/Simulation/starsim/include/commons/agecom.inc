* $Id: agecom.inc,v 1.1.1.1 2004/01/12 23:49:49 potekhin Exp $
* $Name:  $
      CHARACTER*20 AG_MODULE,  AG_TITLE,  AG_EXNAME,   AG_PARLIST,
     +             AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM,
     +             AG_CHDIR
      CHARACTER*4  AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,
     +             AG_OPTION,AG_ORT,AG_MARK
      INTEGER      AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,
     +             AG_IMED,AG_IMAT,AG_IFIELD,AG_IDTYPE,AG_NLMAT,AG_ORTI,
     +             AG_IERROR,AG_NWBUF,AG_NPAR,AG_ISTATUS,AG_IROT,AG_JDU,
     +             AG_NBITS,AG_ISET,AG_IDET,AG_ISVOL, AG_ATTRIBUTE(6),
     +             AG_WORK, AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,
     +             AG_LEVEL,AG_NDIV,AG_IAXIS,AG_NDVMAX,AG_NPDV,AG_NCOPY,
     +             AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,AG_ENDSCR,
     +             AG_IRESER,AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,
     +             AG_LDETU,AG_NPDIV,AG_NZ,AG_IGEOM,AG_IDEBU,AG_IGRAP,
     +             AG_IHIST,AG_IMFLD,AG_SERIAL,AG_STANDALONE,AG_ISIMU,
     +             AG_ENDSAVE,IPRIN
      REAL         AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,
     +             AG_STMIN,AG_DENS,AG_RADL,AG_ABSL,AG_THETAX,AG_THETAY,
     +             AG_THETAZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ,AG_PHIX,AG_PHIY,
     +             AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ, AG_PHIZ, AG_TWIST,
     +             AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1,AG_DY2,
     +             AG_THET, AG_THE1, AG_THE2, AG_PHI, AG_PHI1, AG_PHI2,
     +             AG_ALPH, AG_ALP1, AG_ALP2, AG_RMIN, AG_RMAX, AG_RMN,
     +             AG_RMX, AG_ZI, AG_RMN1, AG_RMN2, AG_RMX1, AG_RMX2,
     +             AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,AG_DPHI,
     +             AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,
     +             AG_STEP, AG_C0, AG_PAR, AG_AA,AG_ZZ,AG_WW,AG_TYPE,
     +             AG_STACK,AG_UBUF,AG_XHMAX,AG_YHMAX,AG_ZHMAX,
     +             AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN,AG_DMAXMS,
     +             AG_LX, AG_LY, AG_LZ, AG_HX, AG_HY, AG_HZ,
     +             AG_P1, AG_P2
      PARAMETER   (AG_LSTACK=130, AG_NWUHIT=10, AG_NWUVOL=3,
     +             AG_MAGIC=-696969, AG_LDETU=250)
      COMMON/AGCGLOB/AG_MODULE, AG_CHDIR,   AG_LEVEL,   AG_IDTYPE,
     +              AG_IERROR,  AG_STANDALONE,          IPRIN,
     +              AG_IPRIN,   AG_IGEOM,   AG_IDEBU,   AG_IGRAP,
     +              AG_IHIST,   AG_IMFLD,   AG_ISIMU
C Inherited variables saved during internal calls
      COMMON/AGCPARA/AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,
     +              AG_ISHAPE,  AG_IMED,    AG_IMAT,    AG_IFIELD,
     +              AG_FIELDM,  AG_TMAXFD,  AG_STEMAX,  AG_DEEMAX,
     +              AG_EPSIL,   AG_STMIN,   AG_DENS,    AG_RADL,
     +              AG_ABSL,    AG_DX,      AG_DX1,     AG_DX2,
     +              AG_DY,      AG_DY1,     AG_DY2,
     +              AG_RMN1,    AG_RMN2,    AG_RMX1,    AG_RMX2,
     +              AG_THET,    AG_THE1,    AG_THE2,
     +              AG_PHI,     AG_PHI1,    AG_PHI2,
     +              AG_ALPH,    AG_ALP1,    AG_ALP2,
     +              AG_H1,      AG_BL1,     AG_TL1,
     +              AG_H2,      AG_BL2,     AG_TL2,
     +              AG_RMIN,    AG_RMAX,    AG_DPHI,    AG_NPDIV,
     +              AG_NZ,      AG_DZ,      AG_TWIS,
     +              AG_LX,      AG_LY,      AG_LZ,
     +              AG_HX,      AG_HY,      AG_HZ,
     +              AG_A,       AG_ZA,      AG_W,       AG_NLMAT,
     +              AG_WORK,    AG_SEEN,    AG_LSTY,
     +              AG_LWID,    AG_COLO,    AG_FILL,
     +              AG_SERIAL,  AG_ISVOL,   AG_ISTATUS,
     +              AG_ZI(16),  AG_RMN(16), AG_RMX(16),
     +              AG_VOLUME,  AG_MOTHER,  AG_SHAPE,   AG_CNICK,
     +                                                  AG_ENDSAVE,
     +              AG_RESET1,  AG_THETAX,  AG_THETAY,  AG_THETAZ,
     +              AG_ALFAX,   AG_ALFAY,   AG_ALFAZ,
     +              AG_PHIX,    AG_PHIY,    AG_PHIZ,
     +              AG_X,       AG_Y,       AG_Z,
     +              AG_STEP,    AG_C0,      AG_NDIV,
     +              AG_IAXIS,   AG_NDVMAX,  AG_ORTI,    AG_NCOPY,
     +              AG_RESET2,
     +              AG_KONLY,   AG_ORT,     AG_MARK
C local variables valid inside same block
      COMMON/AGCLOCA/AG_BEGSCR, AG_UBUF(100), AG_PAR(100),
     +              AG_AA(20),  AG_ZZ(20),    AG_WW(20),   AG_NWBUF,
     +              AG_XHMAX,   AG_YHMAX,     AG_ZHMAX,    AG_RHMAX,
     +              AG_FHMAX, AG_FHMIN, AG_NBITS, AG_BIN,  AG_TYPE,
     +              AG_IROT,  AG_NPAR,  AG_ISET,  AG_IDET, AG_JDU,
     +              AG_IRESER,                             AG_ENDSCR,
     +              AG_TITLE,   AG_EXNAME,    AG_PARLIST,  AG_MATERIAL,
     +              AG_MIXTURE, AG_COMPONENT, AG_MEDIUM,   AG_OPTION
      COMMON/AGCSTAC/AG_STACK(AG_LSTACK,15)
      EQUIVALENCE  (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),
     +             (AG_ALFAX,AG_ALPHAX),  (AG_ALFAY,AG_ALPHAY),
     +             (AG_ALFAZ,AG_ALPHAZ),  (AG_TWIST,AG_TWIS),
     +             (AG_P1,AG_HX),         (AG_P2,AG_HY),
     +             (AG_NPDIV,AG_NPDV)
*    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *



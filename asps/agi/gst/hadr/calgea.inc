*
* $Id: calgea.inc,v 1.2 1998/04/16 16:58:40 fisyak Exp $
*
* $Log: calgea.inc,v $
* Revision 1.2  1998/04/16 16:58:40  fisyak
* 2nd pass with gstar
*
*
*
* calgea.inc
*
#if defined(CERNLIB_HADR)
*CMZ :          16/12/97  14.29.01  by  Pavel Nevski
*CMZ :  1.04/02 13/02/95  11.09.49  by  Christian Zeitnitz
*-- Author :   Christian Zeitnitz
C***************************************************************
C
C       CALOR-GEANT Interface common
C
C parameters of incident particle :
C                   IPinc   = particle type a la CALOR
C                   Einc    = kinetic energy
C                   Uinc(3) = direction cosines
C material parameters:
C                   NCEL    = number of elements in mixture (NMTC)
C                             GEANT material no. for MICAP
C                   Amed(I) = mass number
C                   Zmed(I) = charge number
C                   Dmed(I) = Atoms/cm**3 * 1E-24
C                   Hden    = Atoms/cm**3 * 1E-24
C                             of H-Atoms in mixture
C
C particle stack:
C            NPHETC           = number of particles
C            Ekinet(1:NPHETC) = kinetic energy of part.
C            IPCAL(1:NPHETC)  = particle type a la CALOR (extended)
C            UCAL(1:NPHETC,3) = direction cosines
C            CALTIM(1:NPHETC) = age of particle (nsec)
C
C            ATARGT = A no. of target nucleus
C            ZTARGT = Z no. of target nucleus
C
C return of residual nucleus information
C            NRECOL  = no. of heavy recoil products
C            Amed(I) = mass number of residual nucleus
C            Zmed(I) = charge number "          "
C            EXmed   = exitation energy of nucleus
C            ERmed(I)= recoil energy of nucleus
C            IntCal  = type of interaction (GEANT NAMEC index)
C return of cross section of hadronic interaction (CALSIG called)
C            SIG =  x-section
C
C set by CALSIG:
C            ICPROC = -1   undefined
C                   =  0   NMTC called for cross-section
C                   =  1   MICAP called for cross-section
C                   =  2   SKALE(NMTC at 3 GeV) called for cross-section
C                   =  3   FLUKA called for cross-section
C            KCALL : same coding as ICPROC, but is only valid after a
C                    call to GCALOR
C       18/8/92  C.Zeitnitz University of Arizona
C****************************************************************
C
      PARAMETER(EMAXP  = 3.495)
      PARAMETER(EMAXPI = 2.495)
C transition upper limit (GeV) NMTC-FLUKA
      PARAMETER(ESKALE = 10.0)
      PARAMETER(MXCP = 300)
C
      COMMON/ CALGEA / IPINC  , EINC       , UINC(3)   ,NCEL        ,
     +                 HDEN   , AMED(100)  , ZMED(100) ,DMED(100)   ,
     +                 NPHETC ,EKINET(MXCP),IPCAL(MXCP),UCAL(MXCP,3),
     +                 INTCAL , EXMED      , ERMED(100),SIG         ,
     +                 CALTIM(MXCP), ICPROC, NRECOL    ,KCALL       ,
     +                 ATARGT , ZTARGT
C
#endif

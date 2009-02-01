*
* $Id: auxpar.inc,v 1.1.1.2 2009/02/01 17:10:19 fisyak Exp $
*
* $Log: auxpar.inc,v $
* Revision 1.1.1.2  2009/02/01 17:10:19  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:05  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:27  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:26  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:38  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:16  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:37  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_AUXPAR_INC
#define CERNLIB_GEANT321_AUXPAR_INC
*
*
* auxpar.inc
*
C***********************************************************************
C     INCLUDE FILE: AUXPAR COPY                   CREATED 11/12/86 BY PA
C     CHANGES: NONE
C     INCLUDED IN THE FOLLOWING SUBROUTINES OR FUNCTIONS: NOT UPDATED
C
C     DESCRIPTION OF THE COMMON BLOCK(S) AND VARIABLE(S)
C
C***********************************************************************
      PARAMETER (IDMAX6=1133)
      CHARACTER*8 ANA
      COMMON /FKAUXP/PXA(IDMAX6),PYA(IDMAX6),PZA(IDMAX6),HEPA(IDMAX6),
     *AMA(IDMAX6),ICHA(IDMAX6),IBARA(IDMAX6),            NREA(IDMAX6)
      COMMON /FKAUXC/ANA(IDMAX6)
 

#endif

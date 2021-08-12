*
* $Id: auxpar.inc,v 1.1.1.1 2004/01/15 00:12:22 potekhin Exp $
*
* $Log: auxpar.inc,v $
* Revision 1.1.1.1  2004/01/15 00:12:22  potekhin
*
* Revision 1.1.1.1  1997/11/03 15:29:44  atlascvs
* Importing CERNLIB version 08.21.
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

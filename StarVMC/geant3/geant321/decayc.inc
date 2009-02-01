*
* $Id: decayc.inc,v 1.1.1.2 2009/02/01 17:10:20 fisyak Exp $
*
* $Log: decayc.inc,v $
* Revision 1.1.1.2  2009/02/01 17:10:20  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:08  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:28  fisyak
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
* Revision 1.1.1.1  1995/10/24 10:20:38  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_DECAYC_INC
#define CERNLIB_GEANT321_DECAYC_INC
*
*
* decayc.inc
*
C***********************************************************************
C     INCLUDE FILE: DECAYC COPY                   CREATED 12/12/86 BY PA
C     NOTE: SEE ALSO DECAYC2 COPY
C     CHANGES: NONE
C     INCLUDED IN THE FOLLOWING SUBROUTINES OR FUNCTIONS: NOT UPDATED
C
C     DESCRIPTION OF THE COMMON BLOCK(S) AND VARIABLE(S)
C
C***********************************************************************
      PARAMETER (IDMAX9=540)
      CHARACTER*8 ZKNAME
      COMMON/FKDECC/ NZK(IDMAX9,3),WT(IDMAX9)
      COMMON/FKDECH/ ZKNAME(IDMAX9)
 

#endif

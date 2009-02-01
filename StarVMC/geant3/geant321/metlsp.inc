*
* $Id: metlsp.inc,v 1.1.1.2 2009/02/01 17:10:22 fisyak Exp $
*
* $Log: metlsp.inc,v $
* Revision 1.1.1.2  2009/02/01 17:10:22  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:26  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:37:34  fisyak
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
* Revision 1.1.1.1  1995/10/24 10:20:40  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_METLSP_INC
#define CERNLIB_GEANT321_METLSP_INC
*
*
* metlsp.inc
*
C***********************************************************************
C     INCLUDE FILE: METLSP COPY                   CREATED 11/12/86 BY PA
C     CHANGES: NONE
C     INCLUDED IN THE FOLLOWING SUBROUTINES OR FUNCTIONS: NOT UPDATED
C
C     DESCRIPTION OF THE COMMON BLOCK(S) AND VARIABLE(S)
C
C***********************************************************************
      PARAMETER (IDMAX2=1133)
      COMMON/FKMETL/ IS,ITS(IDMAX2),CXS(IDMAX2),CYS(IDMAX2),CZS(IDMAX2),
     *ELS(IDMAX2),PLS(IDMAX2)
 

#endif

*
* $Id: paprop.inc,v 1.1.1.2 2009/02/01 17:10:23 fisyak Exp $
*
* $Log: paprop.inc,v $
* Revision 1.1.1.2  2009/02/01 17:10:23  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:30  fisyak
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
* Revision 1.1.1.1  1999/05/18 15:55:17  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:20:41  cernlib
* Geant
*
*
#ifndef CERNLIB_GEANT321_PAPROP_INC
#define CERNLIB_GEANT321_PAPROP_INC
*
*
* paprop.inc
*
*
*=== paprop ===========================================================*
*
*----------------------------------------------------------------------*
*     include file: paprop copy                   created 26/11/86 by p*
*     changes: on  16 december 1992 by Alfredo Ferrari                 *
*     included in the following subroutines or functions: not updated  *
*                                                                      *
*     description of the common block(s) and variable(s)               *
*                                                                      *
*     /paprop/ contains particle properties                            *
*        btype  = literal name of the particle                         *
*        am     = particle mass in gev                                 *
*        ichrge = electric charge of the particle                      *
*        iscore = explanations for the scored distribution             *
*        genpar = names of the generalized particles                   *
*        ijdisc = list of the particle types to be discarded           *
*        thalf  = half life of the particle in sec                     *
*        biasdc = decay biasing factors                                *
*        biasin = inelastic interaction biasing factors                *
*        lhadro = flag for hadrons                                     *
*        jspinp = particle spin (in units of 1/2)                      *
*        lbsdcy = logical flag for biased decay: if .true. the biasing *
*                 factor is used as an upper limit to the decay length *
*        lprbsd = logical flag for biased decay: if .true. the biasing *
*                 factor is applied only to primaries                  *
*        lprbsi = logical flag for inelastic interaction biasing: if   *
*                 .true. the biasing factor is applied only to prima-  *
*                 ries                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*
      LOGICAL LHADRO, LBSDCY, LPRBSD, LPRBSI
      CHARACTER*8 BTYPE,GENPAR
      COMMON / FKPAPR / AM  (NALLWP), AMDISC (NALLWP), THALF  (NALLWP),
     &               BIASDC (NALLWP), BIASIN (NALLWP), ICHRGE (NALLWP),
     &               ISCORE     (10), IJDISC (NALLWP), LHADRO (NALLWP),
     &               JSPINP (NALLWP), LBSDCY (NALLWP), LPRBSD, LPRBSI
      COMMON / FKCHPR / BTYPE  (NALLWP), GENPAR (30)
 

#endif

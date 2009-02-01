*
* $Id: iounit.inc,v 1.1.1.2 2009/02/01 17:10:22 fisyak Exp $
*
* $Log: iounit.inc,v $
* Revision 1.1.1.2  2009/02/01 17:10:22  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:47:25  fisyak
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
#ifndef CERNLIB_GEANT321_IOUNIT_INC
#define CERNLIB_GEANT321_IOUNIT_INC
*
*
* iounit.inc
*
*                                                                     *
*=== iounit ==========================================================*
*                                                                     *
*---------------------------------------------------------------------*
*                                                                     *
*      Iounit: included in any routine                                *
*                                                                     *
*         lunin  = standard input unit                                *
*         lunout = standard output unit                               *
*         lunerr = standard error unit                                *
*         lunber = input file for bertini nuclear data                *
*         lunech = echo file for pegs dat                             *
*         lunflu = input file for photoelectric edges and X-ray fluo- *
*                  rescence data                                      *
*         lungeo = scratch file for combinatorial geometry            *
*         lunpgs = input file for pegs material data                  *
*         lunran = output file for the final random number seed       *
*         lunxsc = input file for low energy neutron cross sections   *
*                                                                     *
*---------------------------------------------------------------------*
*                                                                     *
      PARAMETER ( LUNIN  = 5  )
      PARAMETER ( LUNOUT = 6  )
      PARAMETER ( LUNERR = 66 )
      PARAMETER ( LUNBER = 22 )
      PARAMETER ( LUNECH = 8  )
      PARAMETER ( LUNFLU = 86 )
      PARAMETER ( LUNGEO = 16 )
      PARAMETER ( LUNPGS = 12 )
      PARAMETER ( LUNRAN = 2  )
      PARAMETER ( LUNXSC = 81 )
 

#endif

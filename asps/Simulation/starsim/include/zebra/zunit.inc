*
* $Id: zunit.inc,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
*
* $Log: zunit.inc,v $
* Revision 1.1.1.1  2004/01/12 23:49:41  potekhin
*
* Revision 1.1  2001/02/27 10:15:59  nevski
*  first working release
*
* Revision 1.1.1.1  1996/03/06 10:46:52  mclareni
* Zebra
*
*
#ifndef CERNLIB_ZEBRA_ZUNIT_INC
#define CERNLIB_ZEBRA_ZUNIT_INC
*
*                Logical Unit Numbers
*
* zunit.inc
*
      COMMON /ZUNIT/ IQREAD,IQPRNT,IQPR2,IQLOG,IQPNCH,IQTTIN,IQTYPE
      COMMON /ZUNITZ/IQDLUN,IQFLUN,IQHLUN,  NQUSED

#endif

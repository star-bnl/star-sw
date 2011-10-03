*
* $Id: q_or.inc,v 1.1.1.1 2004/07/17 20:01:58 perev Exp $
*
* $Log: q_or.inc,v $
* Revision 1.1.1.1  2004/07/17 20:01:58  perev
* STAR version of Geant321 TGeant3 etc
*
* Revision 1.1.1.1  2002/07/24 15:56:28  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:49  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:27  fca
* AliRoot sources
*
* Revision 1.1  1996/04/18 16:15:13  mclareni
* Incorporate changes from J.Zoll for version 3.77
*
*
*
#ifndef CERNLIB_ZEBRA_Q_OR_INC
#define CERNLIB_ZEBRA_Q_OR_INC
*
* q_or.inc
*
#if defined(CERNLIB_APOLLO)
      IOR  (IZV,IZW) =  OR (IZV, IZW)

#elif defined(CERNLIB_CDC)
      IOR  (IZV,IZW) = IZV .OR.  IZW

#elif defined(CERNLIB_MSDOS)&&defined(CERNLIB_QF_F2C)
       IOR(IZV,IZW)  =  OR(IZV,IZW)

#elif defined(CERNLIB_QMIBMSI)
      IOR  (IZV,IZW) =  LOR (IZV, IZW)

#elif defined(CERNLIB_LNX)
       IOR(IZV,IZW)  =  OR(IZV,IZW)

#elif defined(CERNLIB_SUN)
      IOR  (IZV,IZW) =  OR (IZV, IZW)
 
#elif 1

#endif

#endif

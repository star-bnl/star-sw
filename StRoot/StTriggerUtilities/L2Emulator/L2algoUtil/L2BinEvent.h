#ifndef L2BINEVENT_h
#define L2BINEVENT_h
/*********************************************************************
 * $Id: L2BinEvent.h,v 1.1 2007/10/11 00:33:13 balewski Exp $
 * \author Jan Balewski, IUCF, 2006 
 *********************************************************************
 * Descripion:
 * binary dump of TRIG, BTOW & ETOW data
 * very primitive binary dump of STAR events 
 *********************************************************************
 */

class L2BinEvent {
 public:
  struct BinEveContainer {
    unsigned int lenA[4];
    char mHEAD [100];
    char  mTrigData [25000];
    unsigned short mBTOW_BANK [4800];
    unsigned short mETOW_BANK [2000];
    unsigned short mBTOW_in,mETOW_in;
  };
  
  L2BinEvent();
  static int read(struct BinEveContainer*, FILE *fd, int dbg=0);
  static void write(char* headText, int trgLen, void * trgData,
		     int bemcLen, unsigned short *bemcData,
		     int eemcLen, unsigned short *eemcData,
		     FILE *fd);
};

#endif

/*
*********************************************************************
  $Log: L2BinEvent.h,v $
  Revision 1.1  2007/10/11 00:33:13  balewski
  L2algo added

  Revision 1.2  2006/03/11 17:08:32  balewski
  now CVS comments should work

*/


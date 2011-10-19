#ifndef L2PEDALGO12_H
#define L2PEDALGO12_H

/*************************************************************
 * $Id: L2pedAlgo12.h,v 1.3 2011/10/19 16:12:12 jml Exp $
 * \author Jan Balewski, IUCF, 2006 
 *************************************************************
 * Descripion:
 * pedestal algo in L2 , for BTOW & ETOW
 *************************************************************
 */


class L2Histo;
#ifdef  IS_REAL_L2  //in l2-ana  environmen
  #include "L2VirtualAlgo2012.h"
#else
#include "StTriggerUtilities/L2Emulator/L2algoUtil/L2VirtualAlgo2012.h"
#endif

class L2pedAlgo12 : public  L2VirtualAlgo2012 {
  enum{minAdc=-20, maxAdc=4096};

  bool par_pedSubtr;
  bool par_saveBinary;
  int  par_speedFact;
  int  par_dbg;
  int  par_prescAccept;

  // speed-variables
  short s_stepE, s_stepB, s_lastE, s_lastB;

  //.............run-long variables
  int par_maxMatt; // upper limit of ADC for 2D ADC plot for Matt

  L2Histo *btowAdc[BtowGeom::mxRdo]; // my private HBOOK@L2
  L2Histo *etowAdc[EtowGeom::mxRdo]; // my private HBOOK@L2
  int  nInp; // input event counter
  int  run_number;

  /*  fast DB lookup tables */
  unsigned short db_btowPed[BtowGeom::mxRdo];
  unsigned short db_etowPed[EtowGeom::mxRdo];

 public:
  L2pedAlgo12(const char* name, const char *uid, L2EmcDb2012* db, char* outDir, int resOff);
  int  initRunUser(int runNo,  int *rc_ints, float *rc_floats);
  void finishRunUser();// at the end of each run
  void computeUser(int token); // booby trap
  
  bool  doPedestals(int inpEveId, int* L2Result,  // for every event
	      int bemcIn, ushort *bemcData,
	      int eemcIn, ushort *eemcData);

};

#endif 


/**********************************************************************
  $Log: L2pedAlgo12.h,v $
  Revision 1.3  2011/10/19 16:12:12  jml
  more 2012 stuff

  Revision 1.2  2011/10/19 15:39:44  jml
  2012

  Revision 1.1  2011/10/18 15:11:44  jml
  adding 2012 algorithms

  Revision 1.1  2010/04/17 17:14:37  pibero
  *** empty log message ***

  Revision 1.5  2007/11/18 21:58:59  balewski
  L2algos triggerId list fixed

  Revision 1.4  2007/11/08 04:02:33  balewski
  run on l2ana as well

  Revision 1.3  2007/11/02 03:03:50  balewski
  modified L2VirtualAlgo

  Revision 1.2  2007/10/25 02:07:07  balewski
  added L2upsilon & binary event dump

  Revision 1.1  2007/10/11 00:33:25  balewski
  L2algo added

  Revision 1.5  2006/03/28 19:46:51  balewski
  ver16b, in l2new

  Revision 1.4  2006/03/11 17:08:35  balewski
  now CVS comments should work

*/


#ifndef L2PEDALGO_H
#define L2PEDALGO_H

/*********************************************************************
 * $Id: L2pedAlgo.h,v 1.5 2007/11/18 21:58:59 balewski Exp $
 * \author Jan Balewski, IUCF, 2006 
 *********************************************************************
 * Descripion:
 * pedestal algo in L2 , for BTOW & ETOW
 *********************************************************************
 */


class L2Histo;
#ifdef  IS_REAL_L2  //in l2-ana  environmen
  #include "L2VirtualAlgo.h"
#else
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2VirtualAlgo.h"
#endif

class L2pedAlgo : public  L2VirtualAlgo {
  enum{minAdc=-20, maxAdc=4096};
  /* usefull dimensions */
#define MaxBtowRdo (L2EmcDb::BTOW_MAXFEE*L2EmcDb::BTOW_DATSIZE)
#define MaxEtowRdo (L2EmcDb::ETOW_MAXFEE*L2EmcDb::ETOW_DATSIZE)

  bool par_pedSubtr;
  bool par_saveBinary;
  int  par_speedFact;
  int  par_dbg;
  int  par_prescAccept;


  // speed-variables
  short s_stepE, s_stepB, s_lastE, s_lastB;

  //.............run-long variables
  enum { mxHA=64};
  L2Histo *hA[mxHA]; // my private HBOOK@L2 

  L2Histo *btowAdc[MaxBtowRdo]; // my private HBOOK@L2
  L2Histo *etowAdc[MaxEtowRdo]; // my private HBOOK@L2
  int  nInp; // input event counter
  int  run_number;

  /*  fast DB lookup tables */
  unsigned short db_btowPed[MaxBtowRdo];
  unsigned short db_etowPed[MaxEtowRdo];

  // event-like variables
  TrgDataType* myTrigData;

 public:
  L2pedAlgo(const char* name, L2EmcDb* db, char* outDir, int resOff);
  // ~L2pedAlgo(){}; // memory leak
  int  initRun(int runNo,  int *rc_ints, float *rc_floats);
  
  bool  doEvent(int L0trg, int inpEveId, TrgDataType* trgData,  // for every event
	      int bemcIn, ushort *bemcData,
	      int eemcIn, ushort *eemcData);

  void finishRun();// at the end of each run
};

#endif 


/**********************************************************************
  $Log: L2pedAlgo.h,v $
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


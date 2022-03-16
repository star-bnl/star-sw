#ifndef L2etowCalAlgo08_H
#define L2etowCalAlgo08_H
/********************************************************
 * $Id: L2etowCalAlgo08.h,v 1.2 2008/02/01 00:16:40 balewski Exp $
 * \author Jan Balewski,MIT, 2008 
 ********************************************************
 * Descripion:
 * calibrates Endcap towers, result is used by other L2-algo
 ********************************************************
 */


class L2Histo;
class L2EmcGeom;
#include "L2VirtualAlgo2008.h"

class L2etowCalAlgo08 : public  L2VirtualAlgo2008 {
  /* this class fills the folowing bins of counter histo (mhN)
     xx -  add here
  */
 private:

  //..................... params set in initRun
  int   par_dbg; // use 0 for real event processing
  int   par_gainType; enum {kGainZero=0, kGainIdeal=1, kGainOffline=2};
  int   par_nSigPed;    // ADC,  filters towers
  float par_twEneThres; // GeV, filters towers
  float par_hotEtThres; // GeV, only monitoring histos
  
  //.............run-long variables
  L2EmcGeom *mGeom;

  //........ pointer current events
  HitTower1 *mEve_etow_hit;

  // utility methods
  void createHisto();

 public: 
  L2etowCalAlgo08(const char* name, L2EmcDb* db, L2EmcGeom *geo, char* outDir);
  int   initRunUser( int runNo, int *rc_ints, float *rc_floats);
  void  finishRunUser();// at the end of each run
  void  calibrateEtow(int token, int eemcIn, ushort *eemcData);
  void  computeUser(int token); // bubby trap
  void print0();

};

#endif 

/**********************************************************************
  $Log: L2etowCalAlgo08.h,v $
  Revision 1.2  2008/02/01 00:16:40  balewski
  add mxListSize to BTOW/ETOW calibration

  Revision 1.1  2008/01/30 00:47:16  balewski
  Added L2-Etow-calib




*/


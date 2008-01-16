#ifndef L2btowCalAlgo08_H
#define L2btowCalAlgo08_H
/*********************************************************************
 * $Id: L2btowCalAlgo08.h,v 1.2 2008/01/16 23:32:34 balewski Exp $
 * \author Jan Balewski, IUCF, 2006 
 *********************************************************************
 * Descripion:
 * Reco of mono- & di-jets in L2 using BTOW+ETOW
 *********************************************************************
 */


class L2Histo;
class L2EmcGeom;
#include "L2VirtualAlgo2008.h"

class L2btowCalAlgo08 : public  L2VirtualAlgo2008 {
  /* this class fills the folowing bins of counter histo (mhN)
     6 - # of bad token out of range [1...4095]
  */
 private:

  //..................... params set in initRun
  int   par_dbg;
  int   par_gainType; enum {kGainZero=0, kGainIdeal=1, kGainOffline=2};
  int   par_nSigPed;    // ADC,  filters towers
  float par_twEneThres; // GeV, filters towers
  float par_hotEtThres; // GeV, only monitoring
  
  //.............run-long variables
  L2EmcGeom *geom;

  //........ pointer current events
  HitTower1 *mEve_btow_hit;

  // utility methods
  void createHisto();

 public: 
  L2btowCalAlgo08(const char* name, L2EmcDb* db, L2EmcGeom *geo, char* outDir);
  int   initRunUser( int runNo, int *rc_ints, float *rc_floats);
  void  finishRunUser();// at the end of each run
  void  computeBtow(int token, int bemcIn, ushort *bemcData);
  void  computeUser(int token); // bubby trap
  void print0();

};

#endif 

/**********************************************************************
  $Log: L2btowCalAlgo08.h,v $
  Revision 1.2  2008/01/16 23:32:34  balewski
  toward token dependent compute()

  Revision 1.1  2007/12/19 02:30:18  balewski
  new L2-btow-calib-2008

  Revision 1.1  2007/11/19 22:18:25  balewski
  most L2algos provide triggerID's


*/


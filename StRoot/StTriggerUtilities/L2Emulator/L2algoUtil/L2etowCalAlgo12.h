#ifndef L2etowCalAlgo12_H
#define L2etowCalAlgo12_H
/********************************************************
 * $Id: L2etowCalAlgo12.h,v 1.4 2011/10/19 16:12:10 jml Exp $
 * \author Jan Balewski,MIT, 2008 
 ********************************************************
 * Descripion:
 * calibrates Endcap towers, result is used by other L2-algo
 ********************************************************
 */


class L2Histo;
class L2EmcGeom2012;
#include "L2VirtualAlgo2012.h"

class L2etowCalAlgo12 : public  L2VirtualAlgo2012 {
  /* this class fills the folowing bins of counter histo (mhN)
     xx -  add here
  */
 private:

  //..................... params hard coded in initRun
  unsigned short par_adcMask;
  unsigned short par_pedOff;

  //..................... params set in initRun
  int   par_dbg; // use 0 for real event processing
  int   par_gainType; enum {kGainZero=0, kGainIdeal=1, kGainOffline=2};
  int   par_nSigPed;    // ADC,  filters towers
  float par_twEneThres; // GeV, filters towers
  float par_hotEtThres; // GeV, only monitoring histos
  
  //.............run-long variables
  L2EmcGeom2012 *mGeom;

  //........ pointer current events
  HitTower1 *mEve_etow_hit;

  // utility methods
  void createHisto();

 public: 
  L2etowCalAlgo12(const char* name, const char *uid, L2EmcDb2012* db, L2EmcGeom2012 *geo, char* outDir,int resOff);
  int   initRunUser( int runNo, int *rc_ints, float *rc_floats);
  void  finishRunUser();// at the end of each run
  void  calibrateEtow(int token, int eemcIn, ushort *eemcData);
  void clear(int token);
  void  computeUser(int token); // booby trap
  void print0();

};

#endif 

/**********************************************************************
  $Log: L2etowCalAlgo12.h,v $
  Revision 1.4  2011/10/19 16:12:10  jml
  more 2012 stuff

  Revision 1.3  2011/10/19 15:58:06  jml
  more compile offline

  Revision 1.2  2011/10/19 15:39:42  jml
  2012

  Revision 1.1  2011/10/18 15:11:41  jml
  adding 2012 algorithms

  Revision 1.1  2010/04/17 17:27:31  pibero
  *** empty log message ***

  Revision 1.2  2008/02/01 00:16:40  balewski
  add mxListSize to BTOW/ETOW calibration

  Revision 1.1  2008/01/30 00:47:16  balewski
  Added L2-Etow-calib




*/


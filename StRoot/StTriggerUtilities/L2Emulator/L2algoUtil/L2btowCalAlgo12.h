#ifndef L2btowCalAlgo12_H
#define L2btowCalAlgo12_H
/*****************************************************
 * $Id: L2btowCalAlgo12.h,v 1.4 2011/10/19 16:12:10 jml Exp $
 * \author Jan Balewski, MIT, 2008 
 *****************************************************
 * Descripion:
 * calibrates Barrel towers, result is used by other L2-algo
 *****************************************************
 */


class L2Histo;
class L2EmcGeom2012;
#include "L2VirtualAlgo2012.h"

class L2btowCalAlgo12 : public  L2VirtualAlgo2012 {
  /* this class fills the folowing bins of counter histo (mhN)
          5 - # of eve w/ overflow # of towers, in calib() input
  */
 private:

  //..................... params hard coded in initRun
  unsigned short par_adcMask;
  unsigned short par_pedOff;

  //..................... params set in initRun
  int   par_dbg;
  int   par_gainType; enum {kGainZero=0, kGainIdeal=1, kGainOffline=2};
  int   par_nSigPed;    // ADC,  filters towers
  float par_twEneThres; // GeV, filters towers
  float par_hotEtThres; // GeV, only monitoring
  
  //.............run-long variables
  L2EmcGeom2012 *geom;

  //........ pointer current events
  HitTower1 *mEve_btow_hit;

  // utility methods
  void createHisto();

 public: 
  L2btowCalAlgo12(const char* name, const char *uid, L2EmcDb2012* db, L2EmcGeom2012 *geo, char* outDir, int resOff);
  int   initRunUser( int runNo, int *rc_ints, float *rc_floats);
  void  finishRunUser();// at the end of each run
  void  calibrateBtow(int token, int bemcIn, ushort *bemcData);
  void clear(int token);
  void  computeUser(int token); // booby trap
  void print0();

};

#endif 

/**********************************************************************
  $Log: L2btowCalAlgo12.h,v $
  Revision 1.4  2011/10/19 16:12:10  jml
  more 2012 stuff

  Revision 1.3  2011/10/19 15:58:06  jml
  more compile offline

  Revision 1.2  2011/10/19 15:39:42  jml
  2012

  Revision 1.1  2011/10/18 15:11:41  jml
  adding 2012 algorithms

  Revision 1.2  2010/04/17 16:42:12  pibero
  *** empty log message ***

  Revision 1.5  2008/02/01 00:16:40  balewski
  add mxListSize to BTOW/ETOW calibration

  Revision 1.4  2008/01/30 00:47:16  balewski
  Added L2-Etow-calib

  Revision 1.3  2008/01/18 23:29:13  balewski
  now L2result is exported

  Revision 1.2  2008/01/16 23:32:34  balewski
  toward token dependent compute()

  Revision 1.1  2007/12/19 02:30:18  balewski
  new L2-btow-calib-2008

  Revision 1.1  2007/11/19 22:18:25  balewski
  most L2algos provide triggerID's


*/


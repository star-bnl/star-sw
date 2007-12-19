#ifndef L2exampleAlgo08_H
#define L2exampleAlgo08_H
/*********************************************************************
 * $Id: L2exampleAlgo08.h,v 1.1 2007/12/19 02:30:19 balewski Exp $
 * \author Jan Balewski, IUCF, 2006 
 *********************************************************************
 * Descripion:
 *  example algo finding list of 2x2 BTOW clusters based on ET-seed list produced by L2-btow-calib algo
 * features: uses 2D array (ieta vs. iphi )
 * Limitations:
 *   - ignores seeds at the  edges
 *   - double counts for neighbours seeds
 *   - no provision for wrap up in phi
 *********************************************************************
 */


class L2Histo;
class L2EmcGeom;
#include "L2VirtualAlgo2008.h"

class L2exampleAlgo08 : public  L2VirtualAlgo2008 {
 public:
  enum {mxBtow=(BtowGeom::mxEtaBin) * (BtowGeom::mxPhiBin)}; // shortcut
 private:

  //..................... params set in initRun
  int   par_dbg;
  float par_seedEtThres;
  float par_clusterEtThres;

  //.............run-long variables
  L2EmcGeom *geom;
  int mRdo2tower[mxBtow];
  int mTower2rdo[mxBtow];

  //............... event-long variables
  float btow_et[mxBtow]; // full event
  int   btow_tower_seed[mxBtow]; // above seed thresholds, not cleared
  int   btow_tower_seed_size;
  float btow_clusterET[mxBtow]; // above seed thresholds, not cleared
  int   btow_clusterET_size;

  
  // utility methods
  void  createHisto();
  void  clearEvent();
  float sumET(int phi, int eta);
  
 public:
  L2exampleAlgo08(const char* name, L2EmcDb* db, L2EmcGeom *geo, char* outDir);
  int   initRunUser( int runNo, int *rc_ints, float *rc_floats);
  void  finishRunUser();// at the end of each run
  void  computeUser(int flag, int inpL2EveId);
  bool  decisionUser(int flag, int inpL2EveId);

  void print1();
  void print2();
  void print3();

};

#endif 

/**********************************************************************
  $Log: L2exampleAlgo08.h,v $
  Revision 1.1  2007/12/19 02:30:19  balewski
  new L2-btow-calib-2008



*/


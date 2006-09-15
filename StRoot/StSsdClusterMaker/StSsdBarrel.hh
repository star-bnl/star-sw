// $Id: StSsdBarrel.hh,v 1.4 2006/09/15 21:04:50 bouchet Exp $
//
// $Log: StSsdBarrel.hh,v $
// Revision 1.4  2006/09/15 21:04:50  bouchet
// noise of the strips and clusters coded as a float ; read the noise from ssdStripCalib
//
// Revision 1.3  2005/06/13 16:01:00  reinnart
// Jonathan and Joerg changed the update function
//
// Revision 1.2  2005/05/17 14:16:37  lmartin
// CVS tags added
//
#ifndef STSSDBARREL_HH
#define STSSDBARREL_HH
#include <stdlib.h>
#include <math.h>
#include "StSsdLadder.hh"
#include "StSsdWafer.hh"
#include "tables/St_slsCtrl_Table.h"

class St_ssdWafersPosition;
class St_spa_strip;
class St_sdm_calib_db;
class St_scf_cluster;
class St_scm_spt;

class StSsdBarrel
{
 public:
  StSsdBarrel(ssdDimensions_st  *geom_par);
  ~StSsdBarrel();

  void  initLadders(St_ssdWafersPosition *geom_class);
//   int   readDeadStripFromTable(table_head_st *condition_db_h, sdm_condition_db_st *condition_db); 
  int   readStripFromTable(St_spa_strip *spa_strip);
  int   readNoiseFromTable(St_sdm_calib_db *spa_noise, slsCtrl_st *slsCtrl);
  int   readClusterFromTable(St_scf_cluster *scf_cluster);
  int   writeClusterToTable(St_scf_cluster *cluster);
  int   writePointToTable(St_scm_spt *scm_spt);   
  void  doSideClusterisation(int *numberOfCluster,slsCtrl_st *slsCtrl,scf_ctrl_st *scf_ctrl,int parameter);   
  int   doClusterMatching(ssdDimensions_st *geom_par, scm_ctrl_st *scm_ctrl);
  void  convertDigitToAnalog(slsCtrl_st *slsCtrl);
  void  convertUFrameToOther(ssdDimensions_st *geom_par);
  void  sortListStrip();
  void  sortListCluster();

  StSsdLadder** mLadders;
//   StSsdWafer** mWafers;
//   int** mDeadStripP;
//   int** mDeadStripN;
  
 private:
//   StSsdBarrel(const StSsdBarrel & originalBarrel);
//   StSsdBarrel& operator=(const StSsdBarrel  originalBarrel);

  int    mSsdLayer;
  int    mNLadder;
  int    mNWaferPerLadder;
  int    mNStripPerSide;

  int idWaferToWaferNumb(int idWafer);
  int waferNumbToIdWafer(int waferNumb);
};
#endif

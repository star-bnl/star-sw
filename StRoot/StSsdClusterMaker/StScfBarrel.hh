// $Id: StScfBarrel.hh,v 1.6 2006/09/15 21:04:49 bouchet Exp $
//
// $Log: StScfBarrel.hh,v $
// Revision 1.6  2006/09/15 21:04:49  bouchet
// noise of the strips and clusters coded as a float ; read the noise from ssdStripCalib
//
// Revision 1.5  2005/11/22 03:57:05  bouchet
// id_mctrack is using for setIdTruth
//
// Revision 1.4  2005/06/14 12:20:25  bouchet
// cleaner version
//
// Revision 1.3  2005/06/13 16:01:00  reinnart
// Jonathan and Joerg changed the update function
//
// Revision 1.2  2005/05/17 14:16:33  lmartin
// CVS tags added
//
#ifndef STSCFBARREL_HH
#define STSCFBARREL_HH
#include <stdlib.h>
#include <math.h>
#include "StScfWafer.hh"

#include "tables/St_ssdDimensions_Table.h"
#include "tables/St_slsCtrl_Table.h"
#include "tables/St_scf_ctrl_Table.h"
#include "tables/St_ssdStripCalib_Table.h"

class St_spa_strip;
class St_scf_cluster;
class St_sdm_calib_db;
class St_ssdStripCalib;
//class St_scf_ctrl;
//class St_slsCtrl;

class StScfBarrel
{
 public:
  StScfBarrel(ssdDimensions_st  *geom_par);
  ~StScfBarrel();

  void  setSsdParameters(ssdDimensions_st  *geom_par);
  int   readStripFromTable(St_spa_strip *spa_strip);
  int   readNoiseFromTable(St_sdm_calib_db *spa_noise, slsCtrl_st *slsCtrl);
  int   readNoiseFromTable(St_ssdStripCalib *strip_calib, slsCtrl_st *slsCtrl);
  int   writeClusterToTable(St_scf_cluster *cluster,St_spa_strip *spa_strip);
  int   writeClusterToTable(St_scf_cluster *cluster);
  void  doSideClusterisation(int *numberOfCluster,St_slsCtrl *my_slsCtrl,St_scf_ctrl *my_scf_ctrl);
  void  sortListCluster();
  void  sortListStrip();

  StScfWafer** mWafers;
  
 private:
  int    mSsdLayer;
  int    mNLadder;
  int    mNWaferPerLadder;
  int    mNStripPerSide;

  int idWaferToWaferNumb(int idWafer);
  int waferNumbToIdWafer(int waferNumb);
  
};
#endif

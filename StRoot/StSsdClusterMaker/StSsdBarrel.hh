#ifndef STSSDBARREL_HH
#define STSSDBARREL_HH
#include <stdlib.h>
#include <math.h>
#include "StSsdLadder.hh"
#include "StSsdWafer.hh"
#include "tables/St_sls_ctrl_Table.h"

class St_svg_geom;
class St_spa_strip;
class St_sdm_calib_db;
class St_scf_cluster;
class St_scm_spt;

class StSsdBarrel
{
 public:
  StSsdBarrel(sdm_geom_par_st  *geom_par);
  ~StSsdBarrel();

  void  initLadders(St_svg_geom *geom_class);
//   int   readDeadStripFromTable(table_head_st *condition_db_h, sdm_condition_db_st *condition_db); 
  int   readStripFromTable(St_spa_strip *spa_strip);
  int   readNoiseFromTable(St_sdm_calib_db *spa_noise, sls_ctrl_st *sls_ctrl);
  int   readClusterFromTable(St_scf_cluster *scf_cluster);
  int   writeClusterToTable(St_scf_cluster *cluster);
  int   writePointToTable(St_scm_spt *scm_spt);   
  void  doSideClusterisation(int *numberOfCluster, sls_ctrl_st *sls_ctrl, scf_ctrl_st *scf_ctrl);   
  int   doClusterMatching(sdm_geom_par_st *geom_par, scm_ctrl_st *scm_ctrl);
  void  convertDigitToAnalog(sls_ctrl_st *sls_ctrl);
  void  convertUFrameToOther(sdm_geom_par_st *geom_par);
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

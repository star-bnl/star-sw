#ifndef STSCFBARREL_HH
#define STSCFBARREL_HH
#include <stdlib.h>
#include <math.h>
#include "StScfWafer.hh"

#include "tables/St_sdm_geom_par_Table.h"
#include "tables/St_sls_ctrl_Table.h"
#include "tables/St_scf_ctrl_Table.h"

class St_spa_strip;
class St_scf_cluster;
class St_sdm_calib_db;

class StScfBarrel
{
 public:
  StScfBarrel(sdm_geom_par_st  *geom_par);
  ~StScfBarrel();

  void  setSsdParameters(sdm_geom_par_st  *geom_par);
  int   readStripFromTable(St_spa_strip *spa_strip);
  int   readNoiseFromTable(St_sdm_calib_db *spa_noise, sls_ctrl_st *sls_ctrl);
  int   writeClusterToTable(St_scf_cluster *cluster);
  void  doSideClusterisation(int *numberOfCluster, sls_ctrl_st *sls_ctrl, scf_ctrl_st *scf_ctrl);   
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

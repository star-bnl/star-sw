#ifndef STSCFBARREL_HH
#define STSCFBARREL_HH
# include <stdiostream.h>
# include <stdlib.h>
# include <math.h>
# include "scf_am.h"
# include "StScfWafer.hh"

class StScfBarrel
{
 public:
  StScfBarrel(sdm_geom_par_st  *geom_par);
  ~StScfBarrel();

  void  setScfParameters(sdm_geom_par_st  *geom_par);
  int   readStripFromTable(table_head_st *spa_strip_h, spa_strip_st *spa_strip);
  int   readNoiseFromTable(table_head_st *noise_h, sdm_calib_db_st *noise, sls_ctrl_st *sls_ctrl);
  int   writeClusterToTable(table_head_st *cluster_h, scf_cluster_st *cluster);
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

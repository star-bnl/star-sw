#ifndef STSCMBARREL_HH
#define STSCMBARREL_HH
#include "scm_am.h"
#include "StScmWafer.hh"

class StScmBarrel
{
 public:
  StScmBarrel(sdm_geom_par_st  *geom_par);
  ~StScmBarrel();

  void  setSsdParameters(sdm_geom_par_st  *geom_par);
  void  initWafers(table_head_st *geom_h, svg_geom_st *geom);
//   int   readDeadStripFromTable(table_head_st *condition_db_h, sdm_condition_db_st *condition_db); 
  int   readClusterFromTable(table_head_st *cluster_h, scf_cluster_st *cluster);
  void  sortListCluster();
  int   doClusterMatching(sdm_geom_par_st *geom_par, scm_ctrl_st *scm_ctrl);
  void  convertDigitToAnalog(sls_ctrl_st *sls_ctrl);
  void  convertUFrameToOther(sdm_geom_par_st *geom_par);
  int   writePointToTable(table_head_st *spt_h, scm_spt_st *spt);   

  StScmWafer** mWafers;
//   int** mDeadStripP;
//   int** mDeadStripN;
  
 private:
//   StScmBarrel(const StScmBarrel & originalBarrel);
//   StScmBarrel& operator=(const StScmBarrel  originalBarrel);

  int   mSsdLayer;
  int   mNLadder;
  int   mNWaferPerLadder;
  int   mNStripPerSide;

  int   idWaferToWaferNumb(int idWafer);
  int   waferNumbToIdWafer(int waferNumb);
};
#endif

#ifndef STSCMBARREL_HH
#define STSCMBARREL_HH
#include "StScmWafer.hh"
#include "tables/St_sls_ctrl_Table.h"


class St_svg_geom;
class St_scf_cluster;
class St_scm_spt;

class StScmBarrel
{
 public:
  StScmBarrel(sdm_geom_par_st  *geom_par);
  ~StScmBarrel();

  void  setSsdParameters(sdm_geom_par_st  *geom_par);
  void  initWafers(St_svg_geom *geom_class);
//   int   readDeadStripFromTable(table_head_st *condition_db_h, sdm_condition_db_st *condition_db); 
  int   readClusterFromTable(St_scf_cluster *scf_cluster);
  void  sortListCluster();
  int   doClusterMatching(sdm_geom_par_st *geom_par, scm_ctrl_st *scm_ctrl);
  void  convertDigitToAnalog(sls_ctrl_st *sls_ctrl);
  void  convertUFrameToOther(sdm_geom_par_st *geom_par);
  int   writePointToTable(St_scm_spt *scm_spt);   

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

#ifndef STSCEBARREL_HH
#define STSCEBARREL_HH
#include <stdiostream.h>
#include <stdlib.h>
#include <math.h>
#include "sce_am.h"
#include "StSceWafer.hh"
  
class StSceBarrel
{
 public:
  StSceBarrel(sdm_geom_par_st  *geom_par);
  ~StSceBarrel();

  void setSsdParameters(sdm_geom_par_st  *geom_par);
  void initWafers(table_head_st *geom_h, svg_geom_st *geom);
  int  readPointFromTable(table_head_st *g2t_h,g2t_svt_hit_st *g2t);
  void convertGlobalFrameToOther();
  int  readClusterFromTable(table_head_st *cluster_h, scf_cluster_st *cluster);
  int  doEvalCluster(sce_ctrl_st *ctrl);
  int  readRecPointFromTable(table_head_st *rec_spt_h, scm_spt_st *rec_spt);
  int  doEvalSpt(sce_ctrl_st *ctrl);
  int  writeComPointToTable(table_head_st *sce_dspt_h, sce_dspt_st *sce_dspt);

  StSceWafer** mWafers;
  
 private:
  int    mSsdLayer;
  int    mNLadder;
  int    mNWaferPerLadder;
  int    mNStripPerSide;
  float  mDetectorLargeEdge;
  float  mDetectorSmallEdge;
  float  mStripPitch;
  float  mTheta;

  int  idWaferToWaferNumb(int idWafer);
  int  waferNumbToIdWafer(int waferNumb);
};
#endif

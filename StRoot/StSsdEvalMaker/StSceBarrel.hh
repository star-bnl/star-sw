#ifndef STSCEBARREL_HH
#define STSCEBARREL_HH
#include <stdlib.h>
#include <math.h>
#include "StSceWafer.hh"
#include "tables/St_sdm_geom_par_Table.h"
  
class St_svg_geom;
class St_g2t_svt_hit;
class St_scf_cluster;
class St_scm_spt;
class St_sce_dspt;

class StSceBarrel
{
 public:
  StSceBarrel(sdm_geom_par_st  *geom_par);
  ~StSceBarrel();

  void setSsdParameters(sdm_geom_par_st  *geom_par);
  void initWafers(St_svg_geom *svg_geom);
  int  readPointFromTable(St_g2t_svt_hit *g2t_svt_hit);
  void convertGlobalFrameToOther();
  int  readClusterFromTable(St_scf_cluster *scf_cluster);
  int  doEvalCluster(sce_ctrl_st *ctrl);
  int  readRecPointFromTable(St_scm_spt *rec_spt);
  int  doEvalSpt(sce_ctrl_st *ctrl);
  int  writeComPointToTable(St_sce_dspt *sce_dspt);

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

#ifndef STSLSBARREL_HH
#define STSLSBARREL_HH
# include <stdiostream.h>
# include <stdlib.h>
# include <math.h>
#include "sls_am.h"
#include "StSlsWafer.hh"
  
class StSlsBarrel
{
 public:
  StSlsBarrel(sdm_geom_par_st  *geom_par);
  ~StSlsBarrel();

  void setSsdParameters(sdm_geom_par_st  *geom_par);
  void initWafers(table_head_st *geom_h, svg_geom_st *geom);
  int  readPointFromTable(table_head_st *g2t_h,g2t_svt_hit_st *g2t);
  void convertGlobalFrameToOther();
  int  removeInactiveHitInTable(table_head_st *g2t_h,g2t_svt_hit_st *g2t);
  void renumHitAfterRemove();
  void chargeSharingOverStrip(sls_ctrl_st  *ctrl);
  int  writePointToTable(table_head_st *spt_h, sls_spt_st *spt);
  int  writeStripToTable(table_head_st *strip_h, sls_strip_st *strip);


  StSlsWafer** mWafers;
  
 private:

  int    mSsdLayer;
  int    mNLadder;
  int    mNWaferPerLadder;
  int    mNStripPerSide;
  float  mDetectorLargeEdge;
  float  mDetectorSmallEdge;
  float  mStripPitch;
  float  mTheta;

  int idWaferToWaferNumb(int idWafer);
  int waferNumbToIdWafer(int waferNumb);
  
};
#endif

#ifndef STSLSBARREL_HH
#define STSLSBARREL_HH
#include <stdlib.h>
#include <math.h>
#include "StSlsWafer.hh"
#include "tables/St_sdm_geom_par_Table.h"
#include "tables/St_sls_ctrl_Table.h"

class St_svg_geom;
class St_g2t_svt_hit;
class St_sls_strip;  

class StSlsBarrel
{
 public:
  StSlsBarrel(sdm_geom_par_st  *geom_par);
  ~StSlsBarrel();

  void setSsdParameters(sdm_geom_par_st  *geom_par);
  void initWafers(St_svg_geom *geom_class);
  int  readPointFromTable(St_g2t_svt_hit *g2t_svt_hit);
  void convertGlobalFrameToOther();
  int  removeInactiveHitInTable(St_g2t_svt_hit *g2t_svt_hit);
  void renumHitAfterRemove();
  void chargeSharingOverStrip(sls_ctrl_st  *ctrl);
  int  writeStripToTable(St_sls_strip *sls_strip);


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

#ifndef STSSDLADDER_HH
#define STSSDLADDER_HH
#include <stdlib.h>
#include <math.h>
#include "StSsdWafer.hh"
#include "tables/St_sls_ctrl_Table.h"

class St_svg_geom;
class St_spa_strip;
class St_sdm_calib_db;
class St_scf_cluster;
class St_scm_spt;

class StSsdLadder
{
 public:
  StSsdLadder(int rLadderNumb,int rSsdLayer, int rNWaferPerLadder, int rNStripPerSide);
 ~StSsdLadder();

  void  initWafers(St_svg_geom *geom_class);

  StSsdWafer** mWafers;
  
 private:

  int    mLadderNumb;
  int    mSsdLayer;
  int    mNWaferPerLadder;
  int    mNStripPerSide;

  int idWaferToWaferNumb(int idWafer);
  int waferNumbToIdWafer(int waferNumb);
};
#endif

// $Id: StSsdLadder.hh,v 1.3 2006/09/15 21:04:50 bouchet Exp $
//
// $Log: StSsdLadder.hh,v $
// Revision 1.3  2006/09/15 21:04:50  bouchet
// noise of the strips and clusters coded as a float ; read the noise from ssdStripCalib
//
// Revision 1.2  2005/05/17 14:16:39  lmartin
// CVS tags added
//
#ifndef STSSDLADDER_HH
#define STSSDLADDER_HH
#include <stdlib.h>
#include <math.h>
#include "StSsdWafer.hh"
#include "tables/St_slsCtrl_Table.h"

class St_ssdWafersPosition;
class St_spa_strip;
class St_sdm_calib_db;
class St_scf_cluster;
class St_scm_spt;

class StSsdLadder
{
 public:
  StSsdLadder(int rLadderNumb,int rSsdLayer, int rNWaferPerLadder, int rNStripPerSide);
 ~StSsdLadder();

  void  initWafers(St_ssdWafersPosition *geom_class);

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

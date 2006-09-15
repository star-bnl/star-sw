// $Id: StSlsBarrel.hh,v 1.4 2006/09/15 21:09:52 bouchet Exp $
//
// $Log: StSlsBarrel.hh,v $
// Revision 1.4  2006/09/15 21:09:52  bouchet
// read the noise and pedestal from ssdStripCalib
//
// Revision 1.3  2005/05/13 09:28:24  lmartin
// geant information read from g2t_ssd_hit table
//
// Revision 1.2  2005/05/13 08:39:30  lmartin
// CVS tags added
//

#ifndef STSLSBARREL_HH
#define STSLSBARREL_HH
#include <stdlib.h>
#include <math.h>
#include "StSlsWafer.hh"
#include "tables/St_ssdDimensions_Table.h"
#include "tables/St_slsCtrl_Table.h"

class St_ssdWafersPosition;
class St_g2t_svt_hit;
class St_g2t_ssd_hit;
class St_sls_strip;  

class StSlsBarrel
{
 public:
  StSlsBarrel(ssdDimensions_st  *geom_par);
  ~StSlsBarrel();

  void setSsdParameters(ssdDimensions_st  *geom_par);
  void initWafers(St_ssdWafersPosition *geom_class);
  int  readPointFromTable(St_g2t_ssd_hit *g2t_ssd_hit);
  int  readPointFromTable(St_g2t_svt_hit *g2t_svt_hit);
  void convertGlobalFrameToOther();
  int  removeInactiveHitInTable(St_g2t_ssd_hit *g2t_ssd_hit);
  int  removeInactiveHitInTable(St_g2t_svt_hit *g2t_svt_hit);
  void renumHitAfterRemove();
  void chargeSharingOverStrip(slsCtrl_st  *ctrl);
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

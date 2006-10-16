// $Id: StSceBarrel.hh,v 1.5 2006/10/16 19:54:45 fisyak Exp $
//
// $Log: StSceBarrel.hh,v $
// Revision 1.5  2006/10/16 19:54:45  fisyak
// St_DataSet => TDataSet
//
// Revision 1.4  2005/05/13 14:29:28  lmartin
// tg2t_ssd_hit table used, doEvalCluster and doEvalSpt modified
//
// Revision 1.3  2005/05/12 08:22:09  lmartin
// cvs tags added and histograms in the .hist branch
//
#ifndef STSCEBARREL_HH
#define STSCEBARREL_HH
#include <stdlib.h>
#include <math.h>
#include "StSceWafer.hh"
#include "tables/St_ssdDimensions_Table.h"
  
class St_ssdWafersPosition;
class St_g2t_svt_hit;
class St_g2t_ssd_hit;
class St_scf_cluster;
class St_scm_spt;
class St_sce_dspt;

class StSceBarrel
{
 public:
  StSceBarrel(ssdDimensions_st  *geom_par);
  ~StSceBarrel();

  void setSsdParameters(ssdDimensions_st  *geom_par);
  void initWafers(St_ssdWafersPosition *ssdWafersPosition);
  int  readPointFromTable(St_g2t_svt_hit *g2t_svt_hit);
  int  readPointFromTable(St_g2t_ssd_hit *g2t_ssd_hit);
  void convertGlobalFrameToOther();
  int  readClusterFromTable(St_scf_cluster *scf_cluster);
  int  doEvalCluster(St_sce_ctrl *myctrl);
  int  readRecPointFromTable(St_scm_spt *rec_spt);
  int  doEvalSpt(St_sce_ctrl *myctrl);
  int  writeComPointToTable(St_sce_dspt *sce_dspt);

  StSceWafer** mWafers;
  
 private:
  int    mSsdLayer;
  int    mNLadder;
  int    mNWaferPerLadder;
  int    mNStripPerSide;
  Double_t  mDetectorLargeEdge;
  Double_t  mDetectorSmallEdge;
  Double_t  mStripPitch;
  Double_t  mTheta;

  int  idWaferToWaferNumb(int idWafer);
  int  waferNumbToIdWafer(int waferNumb);
};
#endif

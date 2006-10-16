// $Id: StSceWafer.hh,v 1.4 2006/10/16 19:54:45 fisyak Exp $
//
// $Log: StSceWafer.hh,v $
// Revision 1.4  2006/10/16 19:54:45  fisyak
// St_DataSet => TDataSet
//
// Revision 1.3  2005/05/13 14:29:29  lmartin
// tg2t_ssd_hit table used, doEvalCluster and doEvalSpt modified
//
// Revision 1.2  2005/05/12 08:22:11  lmartin
// cvs tags added and histograms in the .hist branch
//

#ifndef STSCEWAFER_HH
#define STSCEWAFER_HH
#include <stdlib.h>
#include <math.h>
#include "StSceListPoint.hh"
#include "StSceListComp.hh"
#include "StSceListCluster.hh"
#include "StScePoint.hh"
#include "StSceComp.hh"
#include "StSceCluster.hh"

#include "tables/St_sce_ctrl_Table.h"

class StSceWafer
{
 public:
                  StSceWafer(int id);
                  ~StSceWafer();

  void            init(int rId, Double_t *rD, Double_t *rT, Double_t *rN, Double_t *rX);
  void            addHit(int rNId , int rMcHit, int rMcTrack, Float_t *rXg , Float_t rDe, Float_t *p);
  int             convertGlobalToLocal();
  int             convertLocalToUFrame(Double_t ActiveLargeEdge, Double_t ActiveSmallEdge, Double_t Theta);
  void            addCluster(StSceCluster *ptr, int iSide);
  void            addRecPoint(StScePoint *ptr);
  void            addComPoint(StSceComp  *ptr);
  int             getId();
  StSceListPoint* getPoint();
  StSceListPoint* getRecPoint();
  StSceListCluster* getClusterP();
  StSceListCluster* getClusterN();
  StSceListComp*  getComPoint();
  int             doEvaluateCluster(St_sce_ctrl *myctrl);
  int             doEvaluateSpt(St_sce_ctrl *myctrl);

 private:
  int              mId;
  Double_t            *mD;
  Double_t            *mT;
  Double_t            *mN;
  Double_t            *mX;
  StSceListPoint   *mPoint;
  StSceListPoint   *mRecPoint;
  StSceListCluster *mClusterP;
  StSceListCluster *mClusterN;
  StSceListComp    *mComPoint;

  Float_t*          findAngle(Float_t *p, Float_t *alpha);
};  
#endif

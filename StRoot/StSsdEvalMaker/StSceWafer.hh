// $Id: StSceWafer.hh,v 1.3 2005/05/13 14:29:29 lmartin Exp $
//
// $Log: StSceWafer.hh,v $
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

  void            init(int rId, float *rD, float *rT, float *rN, float *rX);
  void            addHit(int rNId , int rMcHit, int rMcTrack, float *rXg , float rDe, float *p);
  int             convertGlobalToLocal();
  int             convertLocalToUFrame(float ActiveLargeEdge, float ActiveSmallEdge, float Theta);
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
  float            *mD;
  float            *mT;
  float            *mN;
  float            *mX;
  StSceListPoint   *mPoint;
  StSceListPoint   *mRecPoint;
  StSceListCluster *mClusterP;
  StSceListCluster *mClusterN;
  StSceListComp    *mComPoint;

  float*          findAngle(float *p, float *alpha);
};  
#endif

// $Id: StSsdWafer.hh,v 1.4 2006/09/15 21:04:50 bouchet Exp $
//
// $Log: StSsdWafer.hh,v $
// Revision 1.4  2006/09/15 21:04:50  bouchet
// noise of the strips and clusters coded as a float ; read the noise from ssdStripCalib
//
// Revision 1.3  2006/05/06 00:56:27  fisyak
// Add local coordinate
//
// Revision 1.2  2005/05/17 14:16:41  lmartin
// CVS tags added
//
#ifndef STSSDWAFER_HH
#define STSSDWAFER_HH
#include <stdlib.h>
#include <math.h>
#include "StSsdStrip.hh"
#include "StSsdStripList.hh"
#include "StSsdCluster.hh"
#include "StSsdClusterList.hh"
#include "StSsdPackage.hh"
#include "StSsdPackageList.hh"
#include "StSsdPointMaker/StSsdPoint.hh"
#include "StSsdPointMaker/StSsdPointList.hh"

#include "tables/St_ssdDimensions_Table.h"
#include "tables/St_scf_ctrl_Table.h"

class StSsdWafer
{
 public:
//                  StSsdWafer(int id, int *deadStripP, int *deadStripN);
                    StSsdWafer(int id);
                   ~StSsdWafer();

  void              init(int rId, Double_t *rD, Double_t *rT, Double_t *rN, Double_t *rX);

  StSsdStripList*   getStripP();
  StSsdStripList*   getStripN();
  StSsdClusterList* getClusterP();
  StSsdClusterList* getClusterN();
  StSsdPackageList* getPackage();
  StSsdPointList*   getPoint();

  void              addStrip(StSsdStrip *ptr, int iSide);
  void              addCluster(StSsdCluster *ptr, int iSide);
  void              addPackage(StSsdPackage *ptr);
  void              addPoint(StSsdPoint *ptr);

  void              setSigmaStrip(int iStrip, int iSide, int iSigma, slsCtrl_st *slsCtrl);

  void              sortStrip();
  void              sortCluster();
  void              sortPoint();

  void              doClusterisation(int *numberOfCluster, slsCtrl_st *slsCtrl, scf_ctrl_st *scf_ctrl);

  int               doFindPackage(ssdDimensions_st *geom_par, scm_ctrl_st *scm_ctrl); 
  int               doSolvePerfect(ssdDimensions_st *geom_par, scm_ctrl_st *scm_ctrl);
  void              doStatPerfect(int nPerfectPoint, scm_ctrl_st *scm_ctrl);
  int               doSolvePackage(ssdDimensions_st *geom_par, scm_ctrl_st *scm_ctrl);
  int               convertDigitToAnalog(double pairCreationEnergy);
  int               convertUFrameToLocal(ssdDimensions_st *geom_par);
  int               convertLocalToGlobal();
  int               printborder();

 private:
  StSsdWafer(const StSsdWafer & originalWafer);
  StSsdWafer& operator=(const StSsdWafer originalWafer);

  int                mId;
  Double_t             *mD;
  Double_t             *mT;
  Double_t             *mN;
  Double_t             *mX;
  Double_t              mPerfectMean;
  Double_t              mPerfectSigma;
//   int            *mDeadStripP;
//   int            *mDeadStripN;

  StSsdStripList    *mStripP;
  StSsdStripList    *mStripN;
  StSsdClusterList  *mClusterP;
  StSsdClusterList  *mClusterN;
  StSsdPackageList  *mPackage;
  StSsdPointList    *mPoint;

  int               doFindCluster(slsCtrl_st *slsCtrl, scf_ctrl_st *scf_ctrl, int iSide);
  int               doClusterSplitting(scf_ctrl_st *scf_ctrl, int iSide);

  int               geoMatched(ssdDimensions_st *geom_par, StSsdCluster *ptr1, StSsdCluster *ptr2);
  int               setMatcheds(ssdDimensions_st *geom_par, StSsdPoint *Spt, StSsdCluster *pMatched, StSsdCluster *nMatched);
  Double_t             matchDistr(scm_ctrl_st *scm_ctrl, Double_t x);
};
#endif

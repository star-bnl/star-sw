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
#include "StSsdPoint.hh"
#include "StSsdPointList.hh"

#include "tables/St_sdm_geom_par_Table.h"
#include "tables/St_scf_ctrl_Table.h"

class StSsdWafer
{
 public:
//                  StSsdWafer(int id, int *deadStripP, int *deadStripN);
                    StSsdWafer(int id);
                   ~StSsdWafer();

  void              init(int rId, float *rD, float *rT, float *rN, float *rX);

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

  void              setSigmaStrip(int iStrip, int iSide, int iSigma, sls_ctrl_st *sls_ctrl);

  void              sortStrip();
  void              sortCluster();
  void              sortPoint();

  void              doClusterisation(int *numberOfCluster, sls_ctrl_st *sls_ctrl, scf_ctrl_st *scf_ctrl);

  int               doFindPackage(sdm_geom_par_st *geom_par, scm_ctrl_st *scm_ctrl); 
  int               doSolvePerfect(sdm_geom_par_st *geom_par, scm_ctrl_st *scm_ctrl);
  void              doStatPerfect(int nPerfectPoint, scm_ctrl_st *scm_ctrl);
  int               doSolvePackage(sdm_geom_par_st *geom_par, scm_ctrl_st *scm_ctrl);
  int               convertDigitToAnalog(double PairCreationEnergy);
  int               convertUFrameToLocal(sdm_geom_par_st *geom_par);
  int               convertLocalToGlobal();
  int               printborder();

 private:
  StSsdWafer(const StSsdWafer & originalWafer);
  StSsdWafer& operator=(const StSsdWafer originalWafer);

  int                mId;
  float             *mD;
  float             *mT;
  float             *mN;
  float             *mX;
  float              mPerfectMean;
  float              mPerfectSigma;
//   int            *mDeadStripP;
//   int            *mDeadStripN;

  StSsdStripList    *mStripP;
  StSsdStripList    *mStripN;
  StSsdClusterList  *mClusterP;
  StSsdClusterList  *mClusterN;
  StSsdPackageList  *mPackage;
  StSsdPointList    *mPoint;

  int               doFindCluster(sls_ctrl_st *sls_ctrl, scf_ctrl_st *scf_ctrl, int iSide);
  int               doClusterSplitting(scf_ctrl_st *scf_ctrl, int iSide);

  int               geoMatched(sdm_geom_par_st *geom_par, StSsdCluster *ptr1, StSsdCluster *ptr2);
  int               setMatcheds(sdm_geom_par_st *geom_par, StSsdPoint *Spt, StSsdCluster *pMatched, StSsdCluster *nMatched);
  float             matchDistr(scm_ctrl_st *scm_ctrl, float x);
};
#endif

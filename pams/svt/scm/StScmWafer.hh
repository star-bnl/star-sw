#ifndef STSCMWAFER_HH
#define STSCMWAFER_HH
#include <stdiostream.h>
#include <stdlib.h>
#include <math.h>
#include "StScmListCluster.hh"
#include "StScmListPackage.hh"
#include "StScmListPoint.hh"
#include "StScmCluster.hh"
#include "StScmPackage.hh"
#include "StScmPoint.hh"
class StScmWafer
{
 public:
//                   StScmWafer(int id, int *deadStripP, int *deadStripN);
                  StScmWafer(int id);
                  ~StScmWafer();

  void            init(int rId, float *rD, float *rT, float *rN, float *rX);

  StScmListCluster* getClusterP();
  StScmListCluster* getClusterN();
  StScmListPackage* getPackage();
  StScmListPoint*   getPoint();

  void            addCluster(StScmCluster *ptr, int iSide);
  void            addPackage(StScmPackage *ptr);
  void            addPoint(StScmPoint *ptr);

  void            sortCluster();
  void            sortPoint();

  int             doFindPackage(sdm_geom_par_st *geom_par, scm_ctrl_st *scm_ctrl); 
  int             doSolvePerfect(sdm_geom_par_st *geom_par, scm_ctrl_st *scm_ctrl);
  void            doStatPerfect(int nPerfectPoint, scm_ctrl_st *scm_ctrl);
  int             doSolvePackage(sdm_geom_par_st *geom_par, scm_ctrl_st *scm_ctrl);
  int             convertDigitToAnalog(double PairCreationEnergy);
  int             convertUFrameToLocal(sdm_geom_par_st *geom_par);
  int             convertLocalToGlobal();
  int             printborder();
 
 private:
  StScmWafer(const StScmWafer & originalWafer);
  StScmWafer& operator=(const StScmWafer originalWafer);

  int                      mId;
  float                   *mD;
  float                   *mT;
  float                   *mN;
  float                   *mX;
  float                    mPerfectMean;
  float                    mPerfectSigma;
//   int                     *mDeadStripP;
//   int                     *mDeadStripN;
  StScmListCluster        *mClusterP;
  StScmListCluster        *mClusterN;
  StScmListPackage        *mPackage;
  StScmListPoint          *mPoint;

  int             geoMatched(sdm_geom_par_st *geom_par, StScmCluster *ptr1, StScmCluster *ptr2);
  int             setMatcheds(sdm_geom_par_st *geom_par, StScmPoint *Spt, StScmCluster *pMatched, StScmCluster *nMatched);
  float           matchDistr(scm_ctrl_st *scm_ctrl, float x);
};  
#endif

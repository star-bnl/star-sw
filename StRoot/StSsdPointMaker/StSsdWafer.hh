#ifndef STSSDWAFER_HH
#define STSSDWAFER_HH

class sdm_geom_par_st;
class StSsdStrip;
class StSsdStripList;
class StSsdCluster;
class StSsdClusterList;
class StSsdPackage;
class StSsdPackageList;
class StSsdPoint;
class StSsdPointList;
class StSsdClusterControl;
class StSsdDynamicControl;

class StSsdWafer
{
 public:
//                  StSsdWafer(int id, int *deadStripP, int *deadStripN);
                    StSsdWafer(int id);
                   ~StSsdWafer();
                    StSsdWafer(const StSsdWafer & originalWafer);
                    StSsdWafer& operator=(const StSsdWafer originalWafer);

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

  void              setSigmaStrip(int iStrip, int iSide, int iSigma, StSsdDynamicControl *dynamicControl);

  void              sortStrip();
  void              sortCluster();
  void              sortPoint();

  void              doClusterisation(int *numberOfCluster, StSsdClusterControl *clusterControl);

  int               doFindPackage(sdm_geom_par_st *geom_par, StSsdClusterControl *clusterControl); 
  int               doSolvePerfect(sdm_geom_par_st *geom_par, StSsdClusterControl *clusterControl);
  void              doStatPerfect(int nPerfectPoint, StSsdClusterControl *clusterControl);
  int               doSolvePackage(sdm_geom_par_st *geom_par, StSsdClusterControl *clusterControl);
  int               convertDigitToAnalog(double PairCreationEnergy);
  int               convertUFrameToLocal(sdm_geom_par_st *geom_par);
  int               convertLocalToGlobal();
  int               printborder();

 private:

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

  int               doFindCluster(StSsdClusterControl *clusterControl, int iSide);
  int               doClusterSplitting(StSsdClusterControl *clusterControl, int iSide);

  int               geoMatched(sdm_geom_par_st *geom_par, StSsdCluster *ptr1, StSsdCluster *ptr2);
  int               setMatcheds(sdm_geom_par_st *geom_par, StSsdPoint *Spt, StSsdCluster *pMatched, StSsdCluster *nMatched);
  double            matchDistr(StSsdClusterControl *clusterControl, double x);
};
#endif

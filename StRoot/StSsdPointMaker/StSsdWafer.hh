/*!
 * \class StSsdWafer
 * \author to be filled - doc by L.Martin
 * \date 02/27/04 for the documentation

This class is the description of the SSD wafer objects.
A wafer is made of :

- An unique Id
- Some geometrical information such as the transverse,drift and normal vectors, the position of its center
- Two lists of strips
- Two lists of clusters
- A list of packages (matchable clusters)
- A list of points deduced from the packages

The Clusters are first found on both sides of the wafer. The clusters are then combined into packages. The packages are then solved and the points
 deduced from the packages. The point coordinates can be transformed from the UV frame to the local (wafer) frame and the global frame.
 */
#ifndef STSSDWAFER_HH
#define STSSDWAFER_HH

class ssdDimensions_st;
class ssdConfiguration_st;
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

  void              init(int rId, double *rD, double *rT, double *rN, double *rX);

  StSsdStripList*   getStripP();   //!< Returns the P-side strip list attached to this wafer
  StSsdStripList*   getStripN();   //!< Returns the N-side strip list attached to this wafer
  StSsdClusterList* getClusterP(); //!< Returns the P-side cluster list attached to this wafer
  StSsdClusterList* getClusterN(); //!< Returns the N-side cluster list attached to this wafer
  StSsdPackageList* getPackage();  //!< Returns the package list attached to this wafer
  StSsdPointList*   getPoint();    //!< Returns the point list attached to this wafer

  void              addStrip(StSsdStrip *ptr, int iSide);     //!< Attaches the ptr strip on the iSide of the wafer
  void              addCluster(StSsdCluster *ptr, int iSide); //!< Attaches the ptr cluster on the iSide of the wafer
  void              addPackage(StSsdPackage *ptr);            //!< Attaches the ptr package on that wafer
  void              addPoint(StSsdPoint *ptr);                //!< Attaches the ptr point on that wafer

  void              setSigmaStrip(int iStrip, int iSide, int iSigma, StSsdDynamicControl *dynamicControl);

  void              sortStrip();
  void              sortCluster();
  void              sortPoint();

  void              doClusterisation(int *numberOfCluster, StSsdClusterControl *clusterControl);

  int               doFindPackage(ssdDimensions_st *dimensions, StSsdClusterControl *clusterControl); 
  int               doSolvePerfect(ssdDimensions_st *dimensions, StSsdClusterControl *clusterControl);
  void              doStatPerfect(int nPerfectPoint, StSsdClusterControl *clusterControl);
  int               doSolvePackage(ssdDimensions_st *dimensions, StSsdClusterControl *clusterControl);
  int               convertDigitToAnalog(double PairCreationEnergy);
  int               convertUFrameToLocal(ssdDimensions_st *dimensions);
  int               convertLocalToGlobal();
  int               printborder();
  void              debugStrips();
  void              debugClusters();
  int               getIdWafer();
 private:

  int                mId;           //!< Id of the wafer
  float             *mD;            //!< Vector defining the drift direction
  float             *mT;            //!< Vector defining the transverse direction
  float             *mN;            //!< Vector defining the normal direction
  float             *mX;            //!< Vector defining the center of the wafer
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

  int               doFindCluster(StSsdClusterControl *clusterControl, int iSide);      //!< Does the cluster finding
  int               doClusterSplitting(StSsdClusterControl *clusterControl, int iSide); //!< Tries to split some clusters
  //! Determines if two clusters are geometricaly compatible
  int               geoMatched(ssdDimensions_st *dimensions, StSsdCluster *ptr1, StSsdCluster *ptr2); 
  int               setMatcheds(ssdDimensions_st *dimensions, StSsdPoint *Spt, StSsdCluster *pMatched, StSsdCluster *nMatched);
  double            matchDistr(StSsdClusterControl *clusterControl, double x);
};
inline int StSsdWafer::getIdWafer() { return mId; } 
#endif

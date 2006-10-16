// $Id: StSsdWafer.hh,v 1.1 2006/10/16 16:43:30 bouchet Exp $
//
// $Log: StSsdWafer.hh,v $
// Revision 1.1  2006/10/16 16:43:30  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
// Revision 1.6  2006/09/15 21:03:15  bouchet
// id_mctrack is using for setIdTruth and propagated to the hit
//
// Revision 1.5  2005/03/18 14:59:32  lmartin
// setPedestalSigmaStrip method added, setSigmaStrip removed
//
// Revision 1.4  2005/03/18 14:01:32  lmartin
// Remove first include accidentaly added
//
// Revision 1.3  2005/03/18 13:55:50  lmartin
// missing CVS header added
//

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
#include "tables/St_ssdWafersPosition_Table.h"
#include "tables/St_ssdConfiguration_Table.h"
#include "tables/St_ssdDimensions_Table.h"
#include "tables/St_sdm_condition_db_Table.h"
#include "StSsdStripList.hh"
#include "StSsdStrip.hh"
#include "StSpaNoise.hh"
#include "StSpaListNoise.hh"
#include "StSsdUtil/StSsdClusterList.hh"
#include "StSsdUtil/StSsdCluster.hh"
#include "StSsdUtil/StSsdPackageList.hh"
#include "StSsdUtil/StSsdPackage.hh"
#include "StSsdUtil/StSsdPointList.hh"
#include "StSsdUtil/StSsdPoint.hh"
#include "StSsdUtil/StSsdClusterControl.h"
#include "StSsdDynamicControl.h"

class StSsdWafer
{
 public:
//                  StSsdWafer(Int_t id, Int_t *deadStripP, Int_t *deadStripN);
                    StSsdWafer(Int_t id);
                   ~StSsdWafer();
                    StSsdWafer(const StSsdWafer & originalWafer);
                    StSsdWafer& operator=(const StSsdWafer originalWafer);

  void              init(Int_t rId, Double_t *rD, Double_t *rT, Double_t *rN, Double_t *rX);

  StSsdClusterList* getClusterP() { return mClusterP; } //!< Returns the P-side cluster list attached to this wafer
  StSsdClusterList* getClusterN() { return mClusterN; } //!< Returns the N-side cluster list attached to this wafer
  StSsdPointList*   getDeadHits(Float_t ActiveLargeEdge, Float_t ActiveSmallEdge,Float_t Test);
  StSsdPointList*   getNonActivePointBorder(Float_t ActiveLargeEdge, Float_t ActiveSmallEdge);
  StSsdPointList*   getNonActivePointTriangle(Float_t Test);
  StSsdStripList*   getStripP()   { return mStripP; }   //!< Returns the P-side strip list attached to this wafer
  StSsdStripList*   getStripN()   { return mStripN; }   //!< Returns the N-side strip list attached to this wafer
  StSsdPackageList* getPackage()  {  return mPackage; } //!< Returns the package list attached to this wafer
  StSsdPointList*   getPoint()    {  return mPoint; }   //!< Returns the point list attached to this wafer
  Int_t             getIdWafer(){ return mId; } 
  Int_t             getId() {return getIdWafer();}
  Float_t*          getD(){  return mD; }
  Float_t*          getT(){  return mT; }
  Float_t*          getN(){  return mN; }
  Float_t*          getX(){  return mX; }

  void              addCluster(StSsdCluster *ptr, Int_t iSide); //!< Attaches the ptr cluster on the iSide of the wafer
  void              addHit(Int_t rNId , Int_t rMcHit, Int_t rMcTrack, Float_t *rXg , Float_t rDe, Float_t *p);
  void              addNoiseToStripSignal(StSpaNoise *ptr, Int_t iSide);
  void              addNoiseToStripSignal(long nElectronInAMip,long a128Dynamic);
  void              addPackage(StSsdPackage *ptr);            //!< Attaches the ptr package on that wafer
  void              addPoint(StSsdPoint *ptr);                //!< Attaches the ptr point on that wafer
  void              addStrip(StSsdStrip *ptr, Int_t iSide);     //!< Attaches the ptr strip on the iSide of the wafer
  //! Determines if two clusters are geometricaly compatible
  Int_t             geoMatched(ssdDimensions_st *dimensions, StSsdCluster *ptr1, StSsdCluster *ptr2);
  Double_t          matchDistr(StSsdClusterControl *clusterControl, Double_t x);
  static Double_t   myErf(Double_t x);
  void              setIsActive(Int_t rIsActive, Int_t iSide, Int_t rNStrip);
  Int_t             setMatcheds(ssdDimensions_st *dimensions, StSsdPoint *Point, StSsdCluster *pMatched, StSsdCluster *nMatched);
  void              setPedestalSigmaStrip(Int_t iStrip, Int_t iSide, Int_t iPedestal, Int_t iSigma, StSsdDynamicControl *dynamicControl);

  
  void              sortCluster();
  void              sortNoise();
  void              sortPoint();
  void              sortStrip();  //?

  void              doClusterisation(Int_t *numberOfCluster, StSsdClusterControl *clusterControl);
  Int_t             doClusterSplitting(StSsdClusterControl *clusterControl, Int_t iSide);
  Int_t             doFindCluster(StSsdClusterControl *clusterControl, Int_t iSide);      //!< Does the cluster finding
  Int_t             doFindPackage(ssdDimensions_st *dimensions, StSsdClusterControl *clusterControl); 
  Int_t             doSolvePackage(ssdDimensions_st *dimensions, StSsdClusterControl *clusterControl);
  Int_t             doSolvePerfect(ssdDimensions_st *dimensions, StSsdClusterControl *clusterControl);
  void              doStatPerfect(Int_t nPerfectPoint, StSsdClusterControl *clusterControl);

  void              convertAnalogToDigit(Double_t pairCreationEnergy);
  void              convertAnalogToDigit(Long_t nElectronInAMip,Long_t adcDynamic,Long_t nbitEncoding, Float_t daqCutValue);
  Int_t             convertDigitToAnalog(Double_t pairCreationEnergy);
  Int_t             convertUFrameToLocal(ssdDimensions_st *dimensions);
  Int_t             convertLocalToGlobal();
  Int_t             convertGlobalToLocal();
  Int_t             convertLocalToUFrame(Float_t ActiveLargeEdge, Float_t ActiveSmallEdge, Float_t Theta);
  void              convertHitToStrip(Float_t Pitch, 
				      Int_t nStripPerSide,
				      Int_t nstripInACluster,
				      Double_t parDiffP,
				      Double_t parDiffN,
				      Double_t parIndRightP,
				      Double_t parIndRightN,
				      Double_t parIndLeftP,
				      Double_t parIndLeftN);
  void              convertToStrip(Float_t Pitch,  
				   Int_t nStripPerSide,
				   Double_t pairCreationEnergy,
				   Int_t nstripInACluster,
				   Double_t parDiffP,
				   Double_t parDiffN,
				   Double_t parIndRightP,
				   Double_t parIndRightN,
				   Double_t parIndLeftP,
				   Double_t parIndLeftN);
  void              debugStrips();
  void              debugClusters();
  Float_t*          findAngle(Float_t *p, Float_t *alpha);
  void              pedestalSubstraction();
  Int_t             printborder();
  void              updateStripList();
  void              zeroSubstraction();
  
 private:
  Char_t               first[1];
  Int_t                mId;              //!< Id of the wafer
  Float_t              mD[3];            //!< Vector defining the drift direction
  Float_t              mT[3];            //!< Vector defining the transverse direction
  Float_t              mN[3];            //!< Vector defining the normal direction
  Float_t              mX[3];            //!< Vector defining the center of the wafer
  Float_t              mPerfectMean;
  Float_t              mPerfectSigma;
//   Int_t            *mDeadStripP;
//   Int_t            *mDeadStripN;

  StSsdStripList    *mStripP;
  StSsdStripList    *mStripN;
  StSpaListNoise    *mNoiseP;
  StSpaListNoise    *mNoiseN;
  StSsdClusterList  *mClusterP;
  StSsdClusterList  *mClusterN;
  StSsdPackageList  *mPackage;
  StSsdPointList    *mPoint;
  Char_t             last[1];
};
#endif

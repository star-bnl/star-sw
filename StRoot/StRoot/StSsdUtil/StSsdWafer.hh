// $Id: StSsdWafer.hh,v 1.9 2015/06/28 13:50:48 bouchet Exp $
//
// $Log: StSsdWafer.hh,v $
// Revision 1.9  2015/06/28 13:50:48  bouchet
// re-enable St_sdm_condition_db table (used in StSsdBarrel)
//
// Revision 1.8  2015/06/26 20:13:45  smirnovd
// Removed unused St_sdm_condition_db_Table header
//
// Revision 1.7  2014/10/18 19:31:56  smirnovd
// Revert "1st commit" asked by Jonathan
//
// Revision 1.5  2009/02/23 21:10:40  bouchet
// increase NSaturationSignal to reflect the energy increase of the GEANT hit
//
// Revision 1.4  2007/07/01 15:47:38  bouchet
// add method to remove strips which signal < 3*rms
//
// Revision 1.3  2007/03/27 23:11:49  bouchet
// Add a method to use the gain calibration for the Charge Matching between pulse of p and n sides
//
// Revision 1.2  2007/03/21 17:20:42  fisyak
// use TGeoHMatrix for coordinate transformation
//
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
#include "TGeoMatrix.h"
class St_ssdGainCalib;

class StSsdWafer: public TGeoHMatrix {
 public:
  //                  StSsdWafer(Int_t id, Int_t *deadStripP, Int_t *deadStripN);
  StSsdWafer(Int_t id);
  ~StSsdWafer();
  StSsdWafer(const StSsdWafer & originalWafer);
  StSsdWafer& operator=(const StSsdWafer originalWafer);

  void              init(Int_t rId, Double_t *rD, Double_t *rT, Double_t *rN, Double_t *rX);

  void setID(Int_t i){mId = i;}
  void setDriftDirection(Double_t x1, Double_t x2, Double_t x3)      
  {Double_t *r = GetRotationMatrix();  r[0] = x1; r[3] = x2; r[6] = x3;}
  void setTransverseDirection(Double_t x1, Double_t x2, Double_t x3) 
  {Double_t *r = GetRotationMatrix();  r[1] = x1; r[4] = x2; r[7] = x3;}
  void setNormalDirection(Double_t x1, Double_t x2, Double_t x3)     
  {Double_t *r = GetRotationMatrix();  r[2] = x1; r[5] = x2; r[8] = x3;}
  void setCenterPosition(Double_t x1, Double_t x2, Double_t x3)  {Double_t *t = GetTranslation(); t[0] = x1; t[1] = x2; t[2] = x3;}
  StSsdClusterList* getClusterP() { return mClusterP; } //!< Returns the P-side cluster list attached to this wafer
  StSsdClusterList* getClusterN() { return mClusterN; } //!< Returns the N-side cluster list attached to this wafer
  StSsdPointList*   getDeadHits(Float_t ActiveLargeEdge, Float_t ActiveSmallEdge,Float_t Test);
  StSsdPointList*   getNonActivePointBorder(Float_t ActiveLargeEdge, Float_t ActiveSmallEdge);
  StSsdPointList*   getNonActivePointTriangle(Float_t Test);
  StSsdStripList*   getStripP()   { return mStripP; }   //!< Returns the P-side strip list attached to this wafer
  StSsdStripList*   getStripN()   { return mStripN; }   //!< Returns the N-side strip list attached to this wafer
  StSsdPackageList* getPackage()  { return mPackage; } //!< Returns the package list attached to this wafer
  StSsdPointList*   getPoint()    { return mPoint; }   //!< Returns the point list attached to this wafer
  Int_t             getIdWafer()  { return mId; } 
  Int_t             getId()       {return getIdWafer();}
  Double_t d(Int_t i){Double_t *r = GetRotationMatrix(); return r[3*i];  }
  Double_t t(Int_t i){Double_t *r = GetRotationMatrix(); return r[3*i+1];}
  Double_t n(Int_t i){Double_t *r = GetRotationMatrix(); return r[3*i+2];}
  Double_t x(Int_t i){Double_t *t = GetTranslation();    return t[i];    }

  void              addCluster(StSsdCluster *ptr, Int_t iSide); //!< Attaches the ptr cluster on the iSide of the wafer
  void              addHit(Int_t rNId , Int_t rMcHit, Int_t rMcTrack, Float_t *rXg , Float_t rDe, Float_t *p);
  void              addNoiseToStripSignal(StSpaNoise *ptr, Int_t iSide);
  void              addNoiseToStripSignal(long nElectronInAMip,long adcDynamic);
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

  void              Reset();
  void              sortCluster();
  void              sortNoise();
  void              sortPoint();
  void              sortStrip();  //?

  void              doClusterisation(Int_t *numberOfCluster, StSsdClusterControl *clusterControl);
  Int_t             doClusterSplitting(StSsdClusterControl *clusterControl, Int_t iSide);
  Int_t             doFindCluster(StSsdClusterControl *clusterControl, Int_t iSide);      //!< Does the cluster finding
  Int_t             doFindPackage(ssdDimensions_st *dimensions, StSsdClusterControl *clusterControl); 
  Int_t             doSolvePackage(ssdDimensions_st *dimensions, StSsdClusterControl *clusterControl,Float_t CalibArray);
  Int_t             doSolvePerfect(ssdDimensions_st *dimensions, StSsdClusterControl *clusterControl,Float_t CalibArray);
  void              doStatPerfect(Int_t nPerfectPoint, StSsdClusterControl *clusterControl);
  void              doLorentzShift(ssdDimensions_st *dimensions,Float_t mShift_hole,Float_t mShift_elec);
  Int_t             doLorentzShiftSide(Int_t side,Float_t shift,ssdDimensions_st *dimensions);
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
				      Double_t parIndLeftN,
				      Float_t  mShift_hole,
				      Float_t  mShift_elec
				      );
  void              convertToStrip(Float_t Pitch,  
				   Int_t nStripPerSide,
				   Double_t pairCreationEnergy,
				   Int_t nstripInACluster,
				   Double_t parDiffP,
				   Double_t parDiffN,
				   Double_t parIndRightP,
				   Double_t parIndRightN,
				   Double_t parIndLeftP,
				   Double_t parIndLeftN,
				   Float_t  mShift_hole,
				   Float_t  mShift_elec
				   );
  void              debugStrips();
  void              debugClusters();
  Float_t*          findAngle(Float_t *p, Float_t *alpha);
  void              pedestalSubstraction();
  Int_t             printborder();
  void              updateStripList();
  void              zeroSubstraction();
  void              UndoLorentzShift(StSsdPoint *ptr, Int_t iSide,Float_t mShift_hole,Float_t mShift_elec,Float_t pitch);
  void              doCleanListStrip(StSsdStripList *myListStrip);
  void  SetDebug(Int_t k = 0) {mDebug = k;}
  Int_t Debug() {return mDebug;}
  
 private:
  Char_t               first[1];
  Int_t                mId;              //!< Id of the wafer
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
  Int_t    mDebug;
  Char_t             last[1];
  StSsdStripList    *cleanListStrip;
  StSsdStripList    *myStripList;

};
#endif

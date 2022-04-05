//$Id: StSstWafer.hh,v 1.8 2016/06/08 20:53:24 bouchet Exp $
//
//$Log: StSstWafer.hh,v $
//Revision 1.8  2016/06/08 20:53:24  bouchet
//coverity : PASS_BY_VALUE
//
//Revision 1.7  2016/06/07 21:40:14  bouchet
//setMatcheds() changed to void (since it returns nothing)
//
//Revision 1.6  2016/05/30 21:39:28  bouchet
//coverity : FORWARD_NULL fixed ; cleanup + simplified method
//
//Revision 1.5  2015/07/21 14:54:28  bouchet
//removed unused variables ; Int_t doLorentzShiftSide moved to void()
//
//Revision 1.4  2015/06/27 19:48:51  bouchet
//removed obsolete libraries : ssdConfiguration, ssdDimensions, ssdWafersPosition ; fixed static StSstBarrel name
//
//Revision 1.3  2015/06/27 19:30:12  bouchet
// re-enable St_sdm_condition_db table (used in StSstBarrel) ; last commit did not check compilation
//
//Revision 1.2  2015/06/26 20:13:46  smirnovd
//Removed unused St_sdm_condition_db_Table header
//
//Revision 1.1  2015/06/23 16:26:19  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:32  bouchet
//initial commit ; SST codes
//

//fork from the SSD code, move along - see history therein

#ifndef STSSTWAFER_HH
#define STSSTWAFER_HH
#include "tables/St_sstWafersPosition_Table.h"
#include "tables/St_sstConfiguration_Table.h"
#include "tables/St_sstDimensions_Table.h"
#include "tables/St_sdm_condition_db_Table.h"
#include "StSstStripList.hh"
#include "StSstStrip.hh"
#include "StSpaNoise.hh"
#include "StSpaListNoise.hh"
#include "StSstUtil/StSstClusterList.hh"
#include "StSstUtil/StSstCluster.hh"
#include "StSstUtil/StSstPackageList.hh"
#include "StSstUtil/StSstPackage.hh"
#include "StSstUtil/StSstPointList.hh"
#include "StSstUtil/StSstPoint.hh"
#include "StSstUtil/StSstClusterControl.h"
#include "StSstDynamicControl.h"
#include "TGeoMatrix.h"

class StSstWafer: public TGeoHMatrix {
 public:
  //                  StSstWafer(Int_t id, Int_t *deadStripP, Int_t *deadStripN);
  StSstWafer(Int_t id);
  ~StSstWafer();
  StSstWafer(const StSstWafer & originalWafer);
  StSstWafer& operator=(const StSstWafer & originalWafer);

  void init(Int_t rId, Double_t *rD, Double_t *rN, Double_t *rT, Double_t *rX);

  void setID(Int_t i){mId = i;}
  void setDriftDirection(Double_t x1, Double_t x2, Double_t x3)      
  {Double_t *r = GetRotationMatrix();  r[0] = x1; r[3] = x2; r[6] = x3;}
  void setNormalDirection(Double_t x1, Double_t x2, Double_t x3) 
  {Double_t *r = GetRotationMatrix();  r[1] = x1; r[4] = x2; r[7] = x3;}
  void setTransverseDirection(Double_t x1, Double_t x2, Double_t x3)     
  {Double_t *r = GetRotationMatrix();  r[2] = x1; r[5] = x2; r[8] = x3;}
  void setCenterPosition(Double_t x1, Double_t x2, Double_t x3)  {Double_t *t = GetTranslation(); t[0] = x1; t[1] = x2; t[2] = x3;}
  StSstClusterList* getClusterP() { return mClusterP; } //!< Returns the P-side cluster list attached to this wafer
  StSstClusterList* getClusterN() { return mClusterN; } //!< Returns the N-side cluster list attached to this wafer
  StSstPointList*   getDeadHits(Float_t ActiveLargeEdge, Float_t ActiveSmallEdge,Float_t Test);
  StSstPointList*   getNonActivePointBorder(Float_t ActiveLargeEdge, Float_t ActiveSmallEdge);
  StSstPointList*   getNonActivePointTriangle(Float_t Test);
  StSstStripList*   getStripP()   { return mStripP; }   //!< Returns the P-side strip list attached to this wafer
  StSstStripList*   getStripN()   { return mStripN; }   //!< Returns the N-side strip list attached to this wafer
  StSstPackageList* getPackage()  { return mPackage; } //!< Returns the package list attached to this wafer
  StSstPointList*   getPoint()    { return mPoint; }   //!< Returns the point list attached to this wafer
  Int_t             getIdWafer()  { return mId; } 
  Int_t             getId()       {return getIdWafer();}
  Double_t d(Int_t i){Double_t *r = GetRotationMatrix(); return r[3*i];  }
  Double_t n(Int_t i){Double_t *r = GetRotationMatrix(); return r[3*i+1];}
  Double_t t(Int_t i){Double_t *r = GetRotationMatrix(); return r[3*i+2];}
  Double_t x(Int_t i){Double_t *t = GetTranslation();    return t[i];    }

  void              addCluster(StSstCluster *ptr, Int_t iSide); //!< Attaches the ptr cluster on the iSide of the wafer
  void              addHit(Int_t rNId , Int_t rMcHit, Int_t rMcTrack, Float_t *rXg , Float_t rDe, Float_t *p);
  void              addNoiseToStripSignal(StSpaNoise *ptr, Int_t iSide);
  void              addNoiseToStripSignal(long nElectronInAMip,long adcDynamic);
  void              addPackage(StSstPackage *ptr);            //!< Attaches the ptr package on that wafer
  void              addPoint(StSstPoint *ptr);                //!< Attaches the ptr point on that wafer
  void              addStrip(StSstStrip *ptr, Int_t iSide);     //!< Attaches the ptr strip on the iSide of the wafer
  //! Determines if two clusters are geometricaly compatible
  Int_t             geoMatched(sstDimensions_st *dimensions, StSstCluster *ptr1, StSstCluster *ptr2);
  Double_t          matchDistr(StSstClusterControl *clusterControl, Double_t x);
  static Double_t   myErf(Double_t x);
  void              setIsActive(Int_t rIsActive, Int_t iSide, Int_t rNStrip);
  void              setMatcheds(sstDimensions_st *dimensions, StSstPoint *Point, StSstCluster *pMatched, StSstCluster *nMatched);
  void              setPedestalSigmaStrip(Int_t iStrip, Int_t iSide, Int_t iPedestal, Int_t iSigma, StSstDynamicControl *dynamicControl);

  void              Reset();
  void              sortCluster();
  void              sortNoise();
  void              sortPoint();
  void              sortStrip();  //?

  void              doClusterisation(Int_t *numberOfCluster, StSstClusterControl *clusterControl);
  Int_t             doClusterSplitting(StSstClusterControl *clusterControl, Int_t iSide);
  Int_t             doFindCluster(StSstClusterControl *clusterControl, Int_t iSide);      //!< Does the cluster finding
  Int_t             doFindPackage(sstDimensions_st *dimensions, StSstClusterControl *clusterControl); 
  Int_t             doSolvePackage(sstDimensions_st *dimensions, StSstClusterControl *clusterControl,Float_t CalibArray);
  Int_t             doSolvePerfect(sstDimensions_st *dimensions, StSstClusterControl *clusterControl,Float_t CalibArray);
  void              doStatPerfect(Int_t nPerfectPoint, StSstClusterControl *clusterControl);
  void              doLorentzShift(sstDimensions_st *dimensions,Float_t mShift_hole,Float_t mShift_elec);
  void              doLorentzShiftSide(Float_t shift, Float_t pitch, StSstClusterList *currentList);
  void              convertAnalogToDigit(Double_t pairCreationEnergy);
  void              convertAnalogToDigit(Long_t nElectronInAMip,Long_t adcDynamic,Long_t nbitEncoding, Float_t daqCutValue);
  Int_t             convertDigitToAnalog(Double_t pairCreationEnergy);
  Int_t             convertUFrameToLocal(sstDimensions_st *dimensions);
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
  void              UndoLorentzShift(StSstPoint *ptr, Int_t iSide,Float_t mShift_hole,Float_t mShift_elec,Float_t pitch);
  void              doCleanListStrip(StSstStripList *myListStrip);
  void  SetDebug(Int_t k = 0) {mDebug = k;}
  Int_t Debug() {return mDebug;}
  
 private:
  Char_t               first[1];
  Int_t                mId;              //!< Id of the wafer
  Float_t              mPerfectMean;
  Float_t              mPerfectSigma;
//   Int_t            *mDeadStripP;
//   Int_t            *mDeadStripN;

  StSstStripList    *mStripP;
  StSstStripList    *mStripN;
  StSpaListNoise    *mNoiseP;
  StSpaListNoise    *mNoiseN;
  StSstClusterList  *mClusterP;
  StSstClusterList  *mClusterN;
  StSstPackageList  *mPackage;
  StSstPointList    *mPoint;
  Int_t    mDebug;
  Char_t             last[1];
  StSstStripList    *cleanListStrip;
  StSstStripList    *myStripList;

};
#endif

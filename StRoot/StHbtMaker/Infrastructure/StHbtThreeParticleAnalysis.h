/***************************************************************************
 *
 * $Id: StHbtThreeParticleAnalysis.h,v 1.8 2002/11/01 20:52:39 magestro Exp $
 *
 * Author: Robert Willson, Ohio State, willson@bnl.gov
 ***************************************************************************
 *  
 * Description: part of STAR HBT Framework: StHbtMaker package.
 *      This is the derived class for three particle analysis objects.  
 *      Each of the simultaneous analyses should have one of derived 
 *      analysis classes running these instantiated.  They link
 *      into the manager in an analysis collection.
 *
 ***************************************************************************
 *
 * $Log: StHbtThreeParticleAnalysis.h,v $
 * Revision 1.8  2002/11/01 20:52:39  magestro
 * removed unused class declaration
 *
 * Revision 1.7  2001/06/03 20:56:10  willson
 * Sectoring, Cos(phi) calculation added
 *
 * Revision 1.6  2000/08/11 16:35:41  rcwells
 * Added number of events processed to each HBT analysis
 *
 * Revision 1.5  2000/07/25 03:26:52  willson
 * Error with small event collections fixed.
 *
 * Revision 1.4  2000/06/15 18:54:08  willson
 * Methods to access cuts and correlation functions moved to derived analysis
 * classes.
 *
 * Revision 1.3  2000/05/11 21:18:56  willson
 * Removed StHbtThreeParticleCorrFctn's...put methods in StHbtCorrFctn
 * Some methods in derived analysis classes moved to base analysis class
 *
 * Revision 1.2  2000/04/12 01:54:33  willson
 * Initial Installation - Comments Added
 *
 * 
 ***************************************************************************/


#ifndef StHbtThreeParticleAnalysis_hh
#define StHbtThreeParticleAnalysis_hh

#include "StHbtMaker/Base/StHbtBaseAnalysis.h"        // base analysis class
#include "StHbtMaker/Base/StHbtTripletCut.h"              // base class
#include "StHbtMaker/Base/StHbtEventCut.h"
#include "StHbtMaker/Base/StHbtParticleCut.h"
#include "StHbtMaker/Base/StHbtCorrFctn.hh" 
#include "StHbtMaker/Infrastructure/StHbtCorrFctnCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtPicoEventCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtSectoredPicoEventCollection.hh"

class StHbtThreeParticleAnalysis : public StHbtBaseAnalysis {

public:

  StHbtThreeParticleAnalysis();
  virtual ~StHbtThreeParticleAnalysis();

  int CreateRealTriplets(StHbtSectoredPicoEvent*, int Index1);
  int CreateRealTriplets(StHbtSectoredPicoEvent*, int Index1, int Index2);
  int CreateRealTriplets(StHbtSectoredPicoEvent*, int Index1, int Index2, int Index3);
  int CalculateCosPhi(StHbtSectoredPicoEvent*, int Index1);
  int CalculateCosPhi(StHbtSectoredPicoEvent*, int Index1, int Index2);
  int CalculateCosPhi(StHbtSectoredPicoEvent*, int Index1, int Index2, int Index3);
  int CreateMixedTriplets(StHbtParticleCollection*, StHbtParticleCollection*, StHbtParticleCollection*);
  void SortHbtParticleCollection(StHbtParticleCut*, StHbtEvent*, StHbtParticleCollection**);
  int  Index(int, int, int);

  // Gets and Sets
  
  StHbt1DHisto*          Q2CF();
  StHbt1DHisto*          Q3CF();
  StHbt1DHisto*          CosPhi();
  StHbt1DHisto*          CosPhiN();
  bool                   CalcCosPhi();
  bool                   IsSectoring();

  virtual StHbtTripletCut*    TripletCut();
  virtual StHbtEventCut*      EventCut();
  virtual StHbtParticleCut*   FirstParticleCut();
  virtual StHbtParticleCut*   SecondParticleCut();
  virtual StHbtParticleCut*   ThirdParticleCut();
  
  StHbtCorrFctnCollection* CorrFctnCollection();
  virtual StHbtCorrFctn* CorrFctn(int n);     // Access to CFs within the collection
  void AddCorrFctn(StHbtCorrFctn*);
  void AddEventProcessed();

  void SetTripletCut(StHbtTripletCut*);
  void SetEventCut(StHbtEventCut*);
  void SetFirstParticleCut(StHbtParticleCut*);
  void SetSecondParticleCut(StHbtParticleCut*);
  void SetThirdParticleCut(StHbtParticleCut*);
  void SetQ2CF(StHbt1DHisto*);
  void SetQ3CF(StHbt1DHisto*);
  void SetCosPhi(StHbt1DHisto*);
  void SetCosPhiN(StHbt1DHisto*);
  void SetCalcCosPhi(const char*);
  void SetSectoring(bool);
  void SetNormFactor(const double);

  int                    NumBinsX();
  int                    NumBinsY();
  int                    NumBinsZ();
  float                  PXmax();
  float                  PXmin();
  float                  PYmax();
  float                  PYmin();
  float                  PZmax();
  float                  PZmin();
  float                  DeltaP();

  void SetPXmax(float);
  void SetPXmin(float);
  void SetPYmax(float);
  void SetPYmin(float);
  void SetPZmax(float);
  void SetPZmin(float);
  void SetDeltaP(float);

  unsigned int NumEventsToMix();
  void SetNumEventsToMix(const unsigned int&);
  StHbtPicoEventCollection* MixingBuffer();
  bool MixingBufferFull();
  StHbtSectoredPicoEventCollection* SectoredMixingBuffer();
  bool SectoredMixingBufferFull();

  bool AnalyzeIdenticalParticles();
  virtual StHbtString Report();       //! returns reports of all cuts applied and correlation functions being done

  virtual void ProcessEvent(const StHbtEvent*);
  void EventBegin(const StHbtEvent*); //startup for EbyE
  void EventEnd(const StHbtEvent*);   // cleanup for EbyE
  int GetNeventsProcessed();

  virtual void Finish();


protected:
  unsigned int mNumEventsToMix;
  unsigned int mNeventsProcessed;
  StHbtTripletCut*          mTripletCut;
  StHbtCorrFctnCollection*  mCorrFctnCollection;
  StHbtEventCut*            mEventCut;
  StHbtParticleCut*         mFirstParticleCut;
  StHbtParticleCut*         mSecondParticleCut;
  StHbtParticleCut*         mThirdParticleCut;
  StHbtPicoEventCollection*  mMixingBuffer;
  StHbtSectoredPicoEventCollection*  mSectoredMixingBuffer;

  float                              mPXmax;             // Sectoring Information
  float                              mPXmin;
  float                              mPYmax;
  float                              mPYmin;
  float                              mPZmax;
  float                              mPZmin;
  float                              mDeltaP;
  int                                mNumBinsX;
  int                                mNumBinsY;
  int                                mNumBinsZ;

  double                 mNormFactor;         // 3P Norm Factor

  StHbt1DHisto*          mQ2CF;               //  Two particle correlation function used to calculate Cos(phi)
  StHbt1DHisto*          mQ3CF;               //  Three particle correlation function used to calculate Cos(phi)
  StHbt1DHisto*          mCosPhi;             //  Cos(phi) histogram
  StHbt1DHisto*          mCosPhiN;            //  Number of entries for each bin, used to calculate Cos(phi) histogram
  StHbt1DHisto*          mCosPhiE;            //  Error in CosPhi histo
  bool                   mCalcCosPhi;         //  If this is 0, normal correlation function processing occurs, else, calculate Cos(phi)
  bool                   mIsSectoring;        //  If this is 0, normal correlation function calculation, if 1, use sectoring. 
  char                   mSaveFile[100];      //  Place to save cosphi histo
  
#ifdef __ROOT__
  ClassDef(StHbtThreeParticleAnalysis, 0)
#endif

};

// Get's
inline StHbtTripletCut*          StHbtThreeParticleAnalysis::TripletCut() {return mTripletCut;}
inline StHbtEventCut*            StHbtThreeParticleAnalysis::EventCut() {return mEventCut;}
inline StHbtParticleCut*         StHbtThreeParticleAnalysis::FirstParticleCut() {return mFirstParticleCut;}
inline StHbtParticleCut*         StHbtThreeParticleAnalysis::SecondParticleCut() {return mSecondParticleCut;}
inline StHbtParticleCut*         StHbtThreeParticleAnalysis::ThirdParticleCut() {return mThirdParticleCut;}
inline StHbtCorrFctnCollection*  StHbtThreeParticleAnalysis::CorrFctnCollection() {return mCorrFctnCollection;}
inline unsigned int              StHbtThreeParticleAnalysis::NumEventsToMix(){return mNumEventsToMix;}

inline StHbt1DHisto*          StHbtThreeParticleAnalysis::Q2CF() {return mQ2CF;}
inline StHbt1DHisto*          StHbtThreeParticleAnalysis::Q3CF() {return mQ3CF;}
inline StHbt1DHisto*          StHbtThreeParticleAnalysis::CosPhi() {return mCosPhi;}
inline StHbt1DHisto*          StHbtThreeParticleAnalysis::CosPhiN() {return mCosPhiN;}
inline bool                   StHbtThreeParticleAnalysis::CalcCosPhi() {return mCalcCosPhi;}
inline bool                   StHbtThreeParticleAnalysis::IsSectoring() {return mIsSectoring;}

inline StHbtPicoEventCollection*  StHbtThreeParticleAnalysis::MixingBuffer() {return mMixingBuffer;}
inline StHbtSectoredPicoEventCollection*  StHbtThreeParticleAnalysis::SectoredMixingBuffer() {return mSectoredMixingBuffer;}

inline float                  StHbtThreeParticleAnalysis::PXmax() {return mPXmax;}
inline float                  StHbtThreeParticleAnalysis::PXmin() {return mPXmin;}
inline float                  StHbtThreeParticleAnalysis::PYmax() {return mPYmax;}
inline float                  StHbtThreeParticleAnalysis::PYmin() {return mPYmin;}
inline float                  StHbtThreeParticleAnalysis::PZmax() {return mPZmax;}
inline float                  StHbtThreeParticleAnalysis::PZmin() {return mPZmin;}
inline float                  StHbtThreeParticleAnalysis::DeltaP() {return mDeltaP;}
inline int                    StHbtThreeParticleAnalysis::NumBinsX() {return mNumBinsX;}
inline int                    StHbtThreeParticleAnalysis::NumBinsY() {return mNumBinsY;}
inline int                    StHbtThreeParticleAnalysis::NumBinsZ() {return mNumBinsZ;}

// Set's
inline bool StHbtThreeParticleAnalysis::AnalyzeIdenticalParticles(){return ((mFirstParticleCut==mSecondParticleCut) && (mSecondParticleCut==mThirdParticleCut));}
inline void StHbtThreeParticleAnalysis::SetTripletCut(StHbtTripletCut* x) { mTripletCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtThreeParticleAnalysis::AddCorrFctn(StHbtCorrFctn* cf) {mCorrFctnCollection->push_back(cf); cf->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtThreeParticleAnalysis::SetEventCut(StHbtEventCut* x) {mEventCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtThreeParticleAnalysis::SetFirstParticleCut(StHbtParticleCut* x) {mFirstParticleCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtThreeParticleAnalysis::SetSecondParticleCut(StHbtParticleCut* x) {mSecondParticleCut = x; x->SetAnalysis((StHbtBaseAnalysis*)this);}
inline void StHbtThreeParticleAnalysis::SetThirdParticleCut(StHbtParticleCut* x) {mThirdParticleCut = x; /*x->SetAnalysis((StHbtBaseAnalysis)this);*/}

inline void StHbtThreeParticleAnalysis::SetQ2CF(StHbt1DHisto* x) {mQ2CF = x;}
inline void StHbtThreeParticleAnalysis::SetQ3CF(StHbt1DHisto* x) {mQ3CF = x;}
inline void StHbtThreeParticleAnalysis::SetCosPhi(StHbt1DHisto* x) {mCosPhi = x;}
inline void StHbtThreeParticleAnalysis::SetCosPhiN(StHbt1DHisto* x) {mCosPhiN = x;}
inline void StHbtThreeParticleAnalysis::SetNormFactor(const double x) {mNormFactor = x;} 

inline void StHbtThreeParticleAnalysis::SetCalcCosPhi(const char* x) 
{
  mCalcCosPhi = true;
  // Copy save filename
  sprintf(mSaveFile, "%s", x);
  // Create Error Histogram to be used in CosPhi calculation
  mCosPhiE = new StHbt1DHisto("CosPhiE", "Error Histo", mCosPhi->GetNbinsX(), mCosPhi->GetXaxis()->GetBinUpEdge(0), mCosPhi->GetXaxis()->GetBinUpEdge(mCosPhi->GetNbinsX()));
}

inline void StHbtThreeParticleAnalysis::SetNumEventsToMix(const unsigned int& nmix){ mNumEventsToMix = nmix;}
inline bool StHbtThreeParticleAnalysis::MixingBufferFull(){return (mMixingBuffer->size() >= mNumEventsToMix);}
inline bool StHbtThreeParticleAnalysis::SectoredMixingBufferFull(){return (mSectoredMixingBuffer->size() >= NumEventsToMix());}
inline int StHbtThreeParticleAnalysis::GetNeventsProcessed() {return mNeventsProcessed;}

inline void StHbtThreeParticleAnalysis::SetSectoring(bool x) {mIsSectoring = x;}
inline void StHbtThreeParticleAnalysis::SetPXmax(float x) {
  mPXmax = x;
  if (mPXmax<mPXmin) mNumBinsX=0;
  else mNumBinsX = (int)ceil((mPXmax-mPXmin)/mDeltaP);
}
inline void StHbtThreeParticleAnalysis::SetPXmin(float x) {
  mPXmin = x;
  if (mPXmax<mPXmin) mNumBinsX=0;
  else mNumBinsX = (int)ceil((mPXmax-mPXmin)/mDeltaP);
}
inline void StHbtThreeParticleAnalysis::SetPYmax(float x) {
  mPYmax = x;
  if (mPYmax<mPYmin) mNumBinsY=0;
  else mNumBinsY = (int)ceil((mPYmax-mPYmin)/mDeltaP);
}
inline void StHbtThreeParticleAnalysis::SetPYmin(float x) {
  mPYmin = x;
  if (mPYmax<mPYmin) mNumBinsY=0;
  else mNumBinsY = (int)ceil((mPYmax-mPYmin)/mDeltaP);
}
inline void StHbtThreeParticleAnalysis::SetPZmax(float x) {
  mPZmax = x;
  if (mPZmax<mPZmin) mNumBinsZ=0;
  else mNumBinsZ = (int)ceil((mPZmax-mPZmin)/mDeltaP);
}
inline void StHbtThreeParticleAnalysis::SetPZmin(float x) {
  mPZmin = x;
  if (mPZmax<mPZmin) mNumBinsZ=0;
  else mNumBinsZ = (int)ceil((mPZmax-mPZmin)/mDeltaP);
}
inline void StHbtThreeParticleAnalysis::SetDeltaP(float x) {
  if (x<=0) {
    mDeltaP = 1.0;
    cout << "****ERROR****  DeltaP must be greater than zero...setting DeltaP to 1.0" << endl;
  }
  else {
    mDeltaP = x;
    mNumBinsX = (int)ceil((mPXmax-mPXmin)/mDeltaP);
    mNumBinsY = (int)ceil((mPYmax-mPYmin)/mDeltaP);
    mNumBinsZ = (int)ceil((mPZmax-mPZmin)/mDeltaP);
  }
}

#endif

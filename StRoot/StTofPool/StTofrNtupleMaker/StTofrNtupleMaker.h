/*******************************************************************
 *
 * $Id: StTofrNtupleMaker.h,v 1.5 2014/08/06 11:43:48 jeromel Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: example maker to get the matched TOFr cells and fill
 *              into TOFr tree.
 *
 *****************************************************************
 *
 * $Log: StTofrNtupleMaker.h,v $
 * Revision 1.5  2014/08/06 11:43:48  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.4  2008/05/06 18:42:09  dongx
 * Updated for Run8 analysis
 *
 * Revision 1.2  2004/04/12 16:17:03  dongx
 * add AdcLoRes in the ntuple
 *
 * Revision 1.1  2004/03/11 22:39:54  dongx
 * first release
 *
 *
 *******************************************************************/
#ifndef StTofrNtupleMaker_hh     
#define StTofrNtupleMaker_hh
#include "StMaker.h"

#define __SIGMA_SCALE__ 1000.
#include <string>
#ifndef ST_NO_NAMESPACES
using std::string;
#endif
#include "StTofrCellData.h"
#include <StPhysicalHelixD.hh>

class StEvent;
class StTofCollection;
class StTofDataCollection;
class StSPtrVecTofData;
class StTofrGeometry;
//class StTofrDaqMap;
class TFile;
class TTree;
class TNtuple;

class StTofrNtupleMaker : public StMaker {
 public:
  StTofrNtupleMaker(const Char_t *name, const Char_t *outname);
  ~StTofrNtupleMaker();

  Int_t  Init();
  Int_t  InitRun(int runnumber);
  Int_t  Make();
  Int_t  FinishRun(int runnumber);
  Int_t  Finish();

  void SetNtupleFileName(Char_t*);
  void setOuterTrackGeometry();
  void setStandardTrackGeometry();
  void setValidAdcRange(Int_t, Int_t);
  void setValidTdcRange(Int_t, Int_t);
  void setInitGeomFromOther(const Bool_t);

  void bookNtuples();

  bool strobeEvent(StSPtrVecTofData&); // check pVPD data for strobe event

  bool validAdc(float const);
  bool validTdc(float const);

private:
  static const Int_t mNTOFR = 120;   //72;
  static const Int_t mNPVPD = 3; 

  Bool_t mYear2; //! STAR year2: TOFp+pVPD
  Bool_t mYear3; //! STAR year3: TOFp+pVPD+TOFr
  Bool_t mYear4;

  StPhysicalHelixD* mBeamHelix;
  float mBeamX, mBeamY;

  StTofrGeometry *mTofrGeom;  //! tofr geometry
  Bool_t mInitGeomFromOther;  //! geometry initial from other makers
  //  StTofrDaqMap * mDaqMap;     //! tofr daq map
  Bool_t mOuterTrackGeometry; //! select outer track geometry (true)

  Int_t mStrobeTdcMin[mNPVPD]; //! lower strobe event range
  Int_t mStrobeTdcMax[mNPVPD]; //! upper strobe event range

  // various cut-offs and ranges
  float	mMinValidTdc; //! lower cut on  TDC value
  float	mMaxValidTdc; //! upper cut on TDC value
  float	mMinValidAdc; //! lower cut on ADC value
  float	mMaxValidAdc; //! upper cut on ADC value
  Float_t mPvpdAdc[mNPVPD];
  Float_t mPvpdTdc[mNPVPD];
  Float_t mPvpdAdcLoRes[mNPVPD];

  // ntuple related data members
  Int_t mAcceptedEvents; //! number of accepted events
  Int_t mPvpdEntries; //! number of pVPD ntuple entries
  Int_t mTofrEvents; //! number of Tofr events
  Int_t mTofrEntries; //! number of Tofr ntuple entries

  string mTupleFileName; //!
  TFile *mTupleFile; //!

  StTofrCellData  mCellData;

  Bool_t  doPrintMemoryInfo; //! control debug memory data
  Bool_t  doPrintCpuInfo; //! control debug timing data

  TTree *mPvpdTuple; //! pVPD start ntuple
  TTree *mCellTuple; //! Tofr calibration ntuple
  TTree *mMatchTuple; //! Tofr match ntuple
  TTree *mNoMatchTuple; //! Tofr match ntuple
  TNtuple *mHitPosTuple; //! Hit position data

  virtual const char *GetCVS() const 
    {static const char cvs[]="Tag $Name:  $ $Id: StTofrNtupleMaker.h,v 1.5 2014/08/06 11:43:48 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

  ClassDef(StTofrNtupleMaker,2)
};

inline void StTofrNtupleMaker::setOuterTrackGeometry(){mOuterTrackGeometry=true;}
inline void StTofrNtupleMaker::setStandardTrackGeometry(){mOuterTrackGeometry=false;}
inline void StTofrNtupleMaker::SetNtupleFileName(Char_t* filename){mTupleFileName=filename;}

inline void StTofrNtupleMaker::setValidAdcRange(Int_t min, Int_t max){
  mMinValidAdc=min;
  mMaxValidAdc=max;
}

inline void StTofrNtupleMaker::setValidTdcRange(Int_t min, Int_t max){
  mMinValidTdc=min;
  mMaxValidTdc=max;
}

inline bool StTofrNtupleMaker::validAdc(const float adc){return((adc>=mMinValidAdc) && (adc<=mMaxValidAdc));}
inline bool StTofrNtupleMaker::validTdc(const float tdc){return((tdc>=mMinValidTdc) && (tdc<=mMaxValidTdc));}

inline void StTofrNtupleMaker::setInitGeomFromOther(const Bool_t init) { mInitGeomFromOther = init; }

#endif

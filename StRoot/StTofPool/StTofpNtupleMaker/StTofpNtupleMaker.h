//! TOF Ntuple Maker
/*! \class StTofpNtupleMaker
 *  \brief Ntuple and TTree Maker for the TOFp detector
 *  \author Frank Geurts
 *  \date August 2003
 *
 *  The TOFp NtupleMaker creates and fills the pVPD ntuple and 
 *  TOFp match TTree. It digs the data out of StEvent's tofCollection.
 *  Both StTofMaker and StTofpMatchMaker need to run before this Maker
 *  in order to fill tofData() and tofSlats(), respectively.
 *
 *  The pVPD ntuple and TOFp match TTree are used for calibration purposes.
 *  This maker is not supposed to run in regular BFC production chains.
 *
 * $Id: StTofpNtupleMaker.h,v 1.3 2014/08/06 11:43:48 jeromel Exp $
 */    
/*  -------------------------------------------------------------------------
 * $Log: StTofpNtupleMaker.h,v $
 * Revision 1.3  2014/08/06 11:43:48  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.2  2004/04/01 19:19:00  dongx
 * update for year4 run
 *
 * Revision 1.1  2003/08/07 23:55:47  geurts
 * first release
 *
 *
 * -------------------------------------------------------------------------
 */
#ifndef StTofNtupleMaker_hh     
#define StTofNtupleMaker_hh
#include "StMaker.h"
#include <string>
#ifndef ST_NO_NAMESPACES
using std::string;
#endif
#include "StTofpSlatData.h"
#include "StTofpMatchData.h"

class StTofGeometry;
class StTofCollection;
class TFile;
class TNtuple;
class TTree;

class StTofpNtupleMaker : public StMaker {
  static const Int_t NTOFP = 41;
  static const Int_t NPVPD = 6;
public:
  StTofpNtupleMaker(const Char_t *name="tofpNtuple");
  ~StTofpNtupleMaker();

  Int_t  Init();
  Int_t  Make();
  Int_t  Finish();

  void SetNtupleFileName(Char_t*);
  void setOuterTrackGeometry();
  void setStandardTrackGeometry();
  void setValidAdcRange(Int_t, Int_t);
  void setValidTdcRange(Int_t, Int_t);

  Bool_t  doPrintMemoryInfo; //! control debug memory data
  Bool_t  doPrintCpuInfo; //! control debug timing data

protected:
  Int_t getTofData(StTofCollection*);
  Float_t mTofpAdc[NTOFP];
  Float_t mTofpTdc[NTOFP];
  Float_t mPvpdAdc[NPVPD];
  Float_t mPvpdAdcLoRes[NPVPD];
  Float_t mPvpdTdc[NPVPD];

private:
  void bookNtuples();
  Bool_t mYear2; //! STAR year2: TOFp+pVPD
  Bool_t mYear3; //! STAR year3: TOFp+pVPD+TOFr
  Bool_t mYear4; //! STAR year4: TOFp+pVPD+TOFr'
  Bool_t mOuterTrackGeometry; //! select outer track geometry (true)

  Bool_t validAdc(float const);
  Bool_t validTdc(float const);

  // various cut-offs and ranges
  Float_t mMinValidTdc; //!
  Float_t mMaxValidTdc; //!
  Float_t mMinValidAdc; //!
  Float_t mMaxValidAdc; //!

  // ntuple related data members
  Int_t mAcceptedEvents; //! number of accepted events
  Int_t mPvpdEntries; //! number of pVPD ntuple entries
  Int_t mTofpEvents; //! number of TOFp events
  Int_t mTofpEntries; //! number of TOFp ntuple entries
  StTofGeometry *mTofGeom; //!
  string mTupleFileName; //!
  TFile *mTupleFile; //!
  StTofpSlatData  mSlatData;
  StTofpMatchData mMatchData;
  TNtuple *mPvpdTuple; //! pVPD start ntuple
  TTree *mSlatTuple; //! TOFp calibration ntuple
  TTree *mMatchTuple; //! TOFp match ntuple
  TTree *mNoMatchTuple; //! TOFp match ntuple
  TNtuple *mHitPosTuple; //! Hit position data

  virtual const char *GetCVS() const 
    {static const char cvs[]="Tag $Name:  $ $Id: StTofpNtupleMaker.h,v 1.3 2014/08/06 11:43:48 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

  ClassDef(StTofpNtupleMaker,1)
};

inline void StTofpNtupleMaker::setValidAdcRange(Int_t min, Int_t max){mMinValidAdc=min;mMaxValidAdc=max;}
inline void StTofpNtupleMaker::setValidTdcRange(Int_t min, Int_t max){mMinValidTdc=min;mMaxValidTdc=max;}
inline void StTofpNtupleMaker::setOuterTrackGeometry(){mOuterTrackGeometry=true;}
inline void StTofpNtupleMaker::setStandardTrackGeometry(){mOuterTrackGeometry=false;}
inline void StTofpNtupleMaker::SetNtupleFileName(Char_t* filename){mTupleFileName=filename;}

inline Bool_t StTofpNtupleMaker::validAdc(const float adc){return((adc>=mMinValidAdc) && (adc<=mMaxValidAdc));}
inline Bool_t StTofpNtupleMaker::validTdc(const float tdc){return((tdc>=mMinValidTdc) && (tdc<=mMaxValidTdc));}

#endif

/*******************************************************************
 *
 * $Id: StTofrMatchMaker.h,v 1.2 2004/03/09 17:44:56 dongx Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: Tofr Match Maker to do the matching between the 
 *              fired celles and TPC tracks ( similar to  Frank's
 *              TofpMatchMaker )
 *
 *****************************************************************
 *
 * $Log: StTofrMatchMaker.h,v $
 * Revision 1.2  2004/03/09 17:44:56  dongx
 * first release
 *
 *
 *******************************************************************/
#ifndef STTOFRMATCHMAKER_HH     
#define STTOFRMATCHMAKER_HH
#include "StMaker.h"
#include "StThreeVectorD.hh"

#include <string>
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::string;
using std::vector;
#endif

class StEvent;
class StTrack;
class StHelix;
class StThreeVectorD;
class StTrackGeometry;
class StTofrGeometry;
class StTofCollection;
class StTofCellCollection;
class StTofDataCollection;
class StSPtrVecTofData;
class StTofrDaqMap;
class TH1D;
class TH2D;

class StTofrMatchMaker : public StMaker {
public:
    StTofrMatchMaker(const Char_t *name="tofrMatch");
    ~StTofrMatchMaker();
    
    //    void Clear(Option_t *option="");
    Int_t  Init();
    Int_t  InitRun(Int_t);
    Int_t  FinishRun(Int_t);
    Int_t  Make();
    Int_t  Finish();
    
    void setCreateHistoFlag(Bool_t histos=kTRUE);
    void setOuterTrackGeometry();
    void setStandardTrackGeometry();
    void setValidAdcRange(Int_t, Int_t);
    void setValidTdcRange(Int_t, Int_t);
    void setMinHitsPerTrack(Int_t);
    void setMinFitPointsPerTrack(Int_t);
    void setMaxDCA(Float_t);
    void setHistoFileName(Char_t*);
    void setNtupleFileName(Char_t*);
    
private:
    StTrackGeometry* trackGeometry(StTrack*);//!
    Float_t slatPropagationTime(StThreeVectorD*); // calculate hit position correction
    Float_t startTime(const Float_t); // calculated pvpd startTime
    Int_t getTofData(StTofCollection*); // check, remap and fill local arrays with tof and pvpd data
    Int_t storeMatchData(StTofCellCollection*, StTofCollection*); 

    Bool_t strobeEvent(StSPtrVecTofData&);// check pVPD data for strobe event
    void bookHistograms();
    void writeHistogramsToFile();
    
    Bool_t validAdc(Float_t const);
    Bool_t validTdc(Float_t const);
    Bool_t validEvent(StEvent *);
    Bool_t validTrack(StTrack*);
    Bool_t validTofTrack(StTrack*);

public:
    Bool_t  doPrintMemoryInfo;     //! 
    Bool_t  doPrintCpuInfo;        //!

private:
    static const Int_t mDAQOVERFLOW = 255;
    static const Int_t mNTOFP = 41;
    static const Int_t mNPVPD = 6;
    static const Int_t mNTOFR = 120;
    static const Float_t mWidthPad = 3.45;
    
    Float_t	mTofrAdc[mNTOFR];
    Float_t	mTofrTdc[mNTOFR];
    Float_t	mPvpdAdc[mNPVPD];
    Float_t	mPvpdAdcLoRes[mNPVPD];
    Float_t	mPvpdTdc[mNPVPD];
    
    Int_t mStrobeTdcMin[mNPVPD]; //! lower strobe event range
    Int_t mStrobeTdcMax[mNPVPD]; //! upper strobe event range
    Double_t mPedTOFr[mNTOFR]; //! pedestals for tofr
    
    StEvent *mEvent;
    StTofrGeometry *mTofrGeom; //! pointer to the TOFr geometry utility class
    StTofrDaqMap *mDaqMap; //! pointer to the TOFr daq map
    
    Bool_t mHisto; //! create, fill and write out histograms
    
    Bool_t mYear2; //! STAR year2: TOFp+pVPD
    Bool_t mYear3; //! STAR year3: TOFp+pVPD+TOFr
    Bool_t mYear4; //! STAR year4: TOFp+pVPD+TOFr'
    Bool_t mOuterTrackGeometry; //! use outer track geometry (true) for extrapolation
    string mHistoFileName; //! name of histogram file, if empty no write-out
    
    // event counters
    Int_t  mEventCounter;          //! #processed events
    Int_t  mAcceptedEventCounter;  //! #events w/ valid prim.vertex
    Int_t  mTofEventCounter;       //! #events w/ Tof raw data
    Int_t  mTofStrobeEventCounter; //! #(strobe events)
    Int_t  mAcceptAndStrobe;       //! #(strobe events) w/ prim.vertex
    Int_t  mAcceptAndBeam;         //! #(beam events) w/ prim.vertex
    
    // various cut-offs and ranges
    Float_t	mMinValidTdc; //! lower cut on  TDC value
    Float_t	mMaxValidTdc; //! upper cut on TDC value
    Float_t	mMinValidAdc; //! lower cut on ADC value
    Float_t	mMaxValidAdc; //! upper cut on ADC value
    unsigned int mMinHitsPerTrack; //! lower cut on #hits per track
    unsigned int mMinFitPointsPerTrack; //! lower cut on #fitpoints per track
    Float_t mMaxDCA; //! upper cut (centimeters) on final (global) DCA
    
    // misc.
    Float_t mValidNeighbours; //!
    Float_t mSumValidNeighbours; //!
    
    // TOFr histograms
    TH2D* mADCTDCCorelation;
    
    TH1D* mEventCounterHisto;
    TH1D* mCellsMultInEvent;
    TH1D* mHitsMultInEvent;
    TH1D* mHitsMultPerTrack;
    TH1D* mDaqOccupancy;
    TH1D* mDaqOccupancyValid;
    TH1D* mDaqOccupancyProj;
    TH2D* mHitsPosition;
    
    TH1D* mCellsPerEventMatch1;
    TH1D* mHitsPerEventMatch1;
    TH1D* mCellsPerTrackMatch1;
    TH1D* mTracksPerCellMatch1;
    TH1D* mDaqOccupancyMatch1;
    TH2D* mDeltaHitMatch1;
    
    TH1D* mCellsPerEventMatch2;
    TH1D* mHitsPerEventMatch2;
    TH1D* mCellsPerTrackMatch2;
    TH1D* mTracksPerCellMatch2;
    TH1D* mDaqOccupancyMatch2;
    TH2D* mDeltaHitMatch2;
    
    TH1D* mCellsPerEventMatch3;
    TH1D* mHitsPerEventMatch3;
    TH1D* mCellsPerTrackMatch3;
    TH1D* mTracksPerCellMatch3;
    TH1D* mDaqOccupancyMatch3;
    TH2D* mDeltaHitMatch3;
    
    
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    typedef vector<Int_t> idVector;
#else
    typedef vector<Int_t,allocator<Int_t>> idVector;
#endif
    typedef idVector::iterator idVectorIter;   
    
    struct StructCellHit{
      Int_t channel;
      Int_t tray;
      Int_t module;
      Int_t cell;
      StThreeVectorD hitPosition;
      idVector trackIdVec;
      Int_t matchFlag;
      Float_t zhit;
      Float_t yhit;
    };
    
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    typedef vector<StructCellHit> tofCellHitVector;
#else
    typedef vector<StructCellHit,allocator<StructCellHit>> tofCellHitVector;
#endif
    typedef vector<StructCellHit>::iterator tofCellHitVectorIter;
    
    
    virtual const char *GetCVS() const 
      {static const char cvs[]="Tag $Name:  $ $Id: StTofrMatchMaker.h,v 1.2 2004/03/09 17:44:56 dongx Exp $ built "__DATE__" "__TIME__ ; return cvs;}
    
    ClassDef(StTofrMatchMaker,1)
};
      
inline void StTofrMatchMaker::setValidAdcRange(Int_t min, Int_t max){
  mMinValidAdc=min;
  mMaxValidAdc=max;
}

inline void StTofrMatchMaker::setValidTdcRange(Int_t min, Int_t max){
  mMinValidTdc=min;
  mMaxValidTdc=max;
}

inline void StTofrMatchMaker::setOuterTrackGeometry(){mOuterTrackGeometry=true;}

inline void StTofrMatchMaker::setStandardTrackGeometry(){mOuterTrackGeometry=false;}

inline void StTofrMatchMaker::setMinHitsPerTrack(Int_t nhits){mMinHitsPerTrack=nhits;}

inline void StTofrMatchMaker::setMinFitPointsPerTrack(Int_t nfitpnts){mMinFitPointsPerTrack=nfitpnts;}

inline void StTofrMatchMaker::setMaxDCA(Float_t maxdca){mMaxDCA=maxdca;}

inline void StTofrMatchMaker::setHistoFileName(Char_t* filename){mHistoFileName=filename;}

inline void StTofrMatchMaker::setCreateHistoFlag(Bool_t histos){mHisto = histos;}

inline Bool_t StTofrMatchMaker::validAdc(const Float_t adc){return((adc>=mMinValidAdc) && (adc<=mMaxValidAdc));}

inline Bool_t StTofrMatchMaker::validTdc(const Float_t tdc){return((tdc>=mMinValidTdc) && (tdc<=mMaxValidTdc));}

#endif

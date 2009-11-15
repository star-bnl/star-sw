/*******************************************************************
 *
 * $Id: StTofrMatchMaker.h,v 1.14 2008/07/23 19:22:03 dongx Exp $
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
 * Revision 1.14  2008/07/23 19:22:03  dongx
 * New track quality cuts for Run8
 *
 * Revision 1.13  2008/05/06 18:41:40  dongx
 * - Fixed bug in ouput histogram filename switch
 * - Added switch for tpc track tree output
 *
 * Revision 1.12  2008/03/27 18:12:12  dongx
 * Update the HPTDC bin width to full precision
 *
 * Revision 1.11  2008/03/27 00:16:03  dongx
 *  update for Run8 finished.
 *
 * Revision 1.10  2007/11/29 22:43:12  dongx
 * changed vpd trayId definition to 121 (East) and 122 (West)
 *
 * Revision 1.9  2007/11/22 00:22:37  dongx
 * update for run8 - first version
 *
 * Revision 1.8  2007/02/28 23:32:00  dongx
 * completion for Run V matching
 *   - trailing tdc and leading tdc stored as adc and tdc in StTofCell
 *   - multi-hit association cases use hit position priority
 *
 * Revision 1.7  2005/07/06 23:10:24  fisyak
 * Use template StThreeVectorD
 *
 * Revision 1.6  2005/04/12 17:31:56  dongx
 * update for year 5 data - not completed, leave as empty at present
 *
 * Revision 1.5  2004/05/03 23:08:50  dongx
 * change according to the update of StTofrGeometry, save CPU time by 100 times
 *
 * Revision 1.4  2004/03/16 22:30:51  dongx
 * fix the warning message when compiling
 *
 * Revision 1.3  2004/03/11 22:30:34  dongx
 * -move m_Mode control to Init()
 * -clear up
 *
 * Revision 1.2  2004/03/09 17:44:56  dongx
 * first release
 *
 *
 *******************************************************************/
#ifndef STTOFRMATCHMAKER_HH     
#define STTOFRMATCHMAKER_HH
#include "StMaker.h"
#include "StThreeVectorD.hh"

#define VHRBIN2PS 24.4140625  // Very High resolution mode, pico-second per bin
                              // 1000*25/1024 (ps/chn)
#define HRBIN2PS 97.65625     // High resolution mode, pico-second per bin
                              // 97.65625= 1000*100/1024  (ps/chn)

#include <string>
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::string;
using std::vector;
#endif

class StEvent;
class StTrack;
class StGlobalTrack;
class StHelix;
#include "StThreeVectorD.hh"
class StTrackGeometry;
class StDcaGeometry;
#include "StTofUtil/StSortTofRawData.h"
class StTofINLCorr;
class StTofrGeometry;
class StTofCollection;
class StTofCellCollection;
class StTofRawDataCollection;
class StTofDataCollection;
class StSPtrVecTofData;
class StTofrDaqMap;
class TH1D;
class TH2D;
class TTree;

#if !defined(ST_NO_TEMPLATE_DEF_ARGS) || defined(__CINT__)
typedef vector<Int_t>  IntVec;
typedef vector<Double_t>  DoubleVec;
#else
typedef vector<Int_t, allocator<Int_t>>  IntVec;
typedef vector<Double_t, allocator<Double_t>>  DoubleVec;
#endif

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
    void setCreateTreeFlag(Bool_t tree=kTRUE);
    void setOuterTrackGeometry();
    void setStandardTrackGeometry();
    void setValidAdcRange(Int_t, Int_t);
    void setValidTdcRange(Int_t, Int_t);
    void setMinHitsPerTrack(Int_t);
    void setMinFitPointsPerTrack(Int_t);
    void setMinFitPointsOverMax(Float_t);
    void setMaxDCA(Float_t);
    void setHistoFileName(Char_t*);
    void setNtupleFileName(Char_t*);
    void setSaveGeometry(Bool_t geomSave=kFALSE);

    Int_t processEventYear2to4();
    Int_t processEventYear5();
    Int_t processEventYear8();

private:
    StTrackGeometry* trackGeometry(StTrack*);//!
    Int_t getTofData(StTofCollection*); // check, remap and fill local arrays with tof and pvpd data
    Int_t storeMatchData(StTofCellCollection*, StTofCollection*); 

    Bool_t strobeEvent(StSPtrVecTofData&);// check pVPD data for strobe event
    void bookHistograms();
    void writeHistogramsToFile();
    
    Bool_t validAdc(Float_t const);
    Bool_t validTdc(Float_t const);
    Bool_t validEvent(StEvent *);
    Bool_t validTrack(StTrack*);
    Bool_t validTrackRun8(StGlobalTrack*);
    Bool_t validTofTrack(StTrack*);

    // year5 moved to calibration maker
    //    float GetINLcorr(int edgeid,int tempchan,int bin);
    //

    //y8++ INL Corr moved here
    float getINLCorr(int boardId, int tdcChanId);

public:
    Bool_t  doPrintMemoryInfo;     //! 
    Bool_t  doPrintCpuInfo;        //!

private:
    static const Int_t mDAQOVERFLOW = 255;
    static const Int_t mNTOFP = 41;
    static const Int_t mNPVPD = 6;
    static const Int_t mNTOFR = 120;
    static const Int_t mNTOFR5 = 192; 
    //    static const Float_t mWidthPad = 3.45;

    static const Int_t mNTOF = 192;    // 192 for tof in Run 8++
    static const Int_t mNModule = 32;  // 32 for tofr5++ 
    static const Int_t mNCell = 6;
    static const Int_t mNVPD = 19;    //

    static const Int_t mEastVpdTrayId = 122;
    static const Int_t mWestVpdTrayId = 121;

    static const Int_t mNValidTrays_Run3 = 1;
    static const Int_t mNValidTrays_Run4 = 1;
    static const Int_t mNValidTrays_Run5 = 1;
    static const Int_t mNValidTrays_Run6 = 0;
    static const Int_t mNValidTrays_Run7 = 0;
    static const Int_t mNValidTrays_Run8 = 5;
    
    Float_t     mWidthPad;
    Float_t	mTofrAdc[mNTOFR];
    Float_t	mTofrTdc[mNTOFR];
    Float_t	mPvpdAdc[mNPVPD];
    Float_t	mPvpdAdcLoRes[mNPVPD];
    Float_t	mPvpdTdc[mNPVPD];
    //year 5
    static const Int_t mTdigBoard = 10;
    static const Int_t mTdcOnBoard = 4;
    static const Int_t mTdcChannel = 1024;
    Float_t	mPvpdToT[mNPVPD];
    IntVec      mTofr5LdChan;
    IntVec      mTofr5LdTdc;
    IntVec      mTofr5TrChan;
    IntVec      mTofr5TrTdc;
    Float_t     mTofr5Tdc[mNTOFR5];
    Float_t     mTofr5ToT[mNTOFR5]; // ToT as adc

    // moved to calibration maker
    //    Float_t     mINLtable[mTdigBoard][mTdcOnBoard][mTdcChannel];
    //
    
    Int_t mStrobeTdcMin[mNPVPD]; //! lower strobe event range
    Int_t mStrobeTdcMax[mNPVPD]; //! upper strobe event range
    Double_t mPedTOFr[mNTOFR]; //! pedestals for tofr
    
    StEvent *mEvent;
    StTofrGeometry *mTofrGeom; //! pointer to the TOFr geometry utility class
    StTofrDaqMap *mDaqMap; //! pointer to the TOFr daq map
    StSortTofRawData *mSortTofRawData; // sorted TOFr5 raw data
    // tofr8++
    StTofINLCorr *mTofINLCorr;   // INL Correction
    
    Bool_t mHisto; //! create, fill and write out histograms
    Bool_t mSaveTree; //! create, fill and write out trees for tpc tracks
    
    Bool_t mYear2; //! STAR year2: TOFp+pVPD
    Bool_t mYear3; //! STAR year3: TOFp+pVPD+TOFr
    Bool_t mYear4; //! STAR year4: TOFp+pVPD+TOFr'
    Bool_t mYear5; //! STAR year5: pVPD+TOFr5
    Bool_t mYear8; //! STAR year8; upVPD+TOFr8

    Bool_t mOuterTrackGeometry; //! use outer track geometry (true) for extrapolation
    Bool_t mGeometrySave;

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
    Float_t mMinFitPointsOverMax; //! lower cut on #fitpoints / #maxpoints
    Float_t mMaxDCA; //! upper cut (centimeters) on final (global) DCA
    
    //

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
    
    TH1D* mDaqOccupancyValidAll;
    TH1D* mDaqOccupancyProjAll;
    TH1D* mDaqOccupancyVpd;
    TH1D* mDaqOccupancyValidVpd;
    
    TH2D* mHitCorr[mNValidTrays_Run8];
    TH2D* mHitCorrModule[mNValidTrays_Run8];
    TH2D* mHitCorrAll;


    TH2D* mTrackPtEta;
    TH2D* mTrackPtPhi;
    TH1D* mTrackNFitPts;
    TH2D* mTrackdEdxvsp;
    TH2D* mNSigmaPivsPt;

    TTree* mTrackTree;
    
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
    
    struct TRACKTREE{
      Float_t pt;
      Float_t eta;
      Float_t phi;
      Int_t   nfitpts;
      Float_t dEdx;
      Int_t   ndEdxpts;
      Int_t   charge;
      Int_t   projTrayId;
      Int_t   projCellChan;
      Float_t projY;
      Float_t projZ;
    };
    TRACKTREE trackTree;
    
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    typedef vector<StructCellHit> tofCellHitVector;
#else
    typedef vector<StructCellHit,allocator<StructCellHit>> tofCellHitVector;
#endif
    typedef vector<StructCellHit>::iterator tofCellHitVectorIter;
    
    
    virtual const char *GetCVS() const 
      {static const char cvs[]="Tag $Name:  $ $Id: StTofrMatchMaker.h,v 1.14 2008/07/23 19:22:03 dongx Exp $ built "__DATE__" "__TIME__ ; return cvs;}
    
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

inline void StTofrMatchMaker::setMinFitPointsOverMax(Float_t ratio) {mMinFitPointsOverMax=ratio;}

inline void StTofrMatchMaker::setMaxDCA(Float_t maxdca){mMaxDCA=maxdca;}

inline void StTofrMatchMaker::setHistoFileName(Char_t* filename){mHistoFileName=filename;}

inline void StTofrMatchMaker::setCreateHistoFlag(Bool_t histos){mHisto = histos;}

inline void StTofrMatchMaker::setCreateTreeFlag(Bool_t tree){mSaveTree = tree;}

inline void StTofrMatchMaker::setSaveGeometry(Bool_t geomSave){mGeometrySave = geomSave; }

inline Bool_t StTofrMatchMaker::validAdc(const Float_t adc){return((adc>=mMinValidAdc) && (adc<=mMaxValidAdc));}

inline Bool_t StTofrMatchMaker::validTdc(const Float_t tdc){return((tdc>=mMinValidTdc) && (tdc<=mMaxValidTdc));}

#endif

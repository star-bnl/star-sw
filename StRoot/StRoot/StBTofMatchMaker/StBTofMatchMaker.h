//! Barrel TOF Match Maker
/*!  \class StBTofMatchMaker
 *   \brief Match Maker for the BTOF detector
 *   \author Xin Dong, Frank Geurts
 *   \date June 2009
 *
 * The Barrel TOF MatchMaker matches STAR tracks to the BTOF cells.
 * 
 * $Id: StBTofMatchMaker.h,v 1.13 2017/10/20 17:50:33 smirnovd Exp $
 */
/*****************************************************************
 *
 * $Log: StBTofMatchMaker.h,v $
 * Revision 1.13  2017/10/20 17:50:33  smirnovd
 * Squashed commit of the following:
 *
 *     StBTof: Remove outdated ClassImp macro
 *
 *     Prefer explicit namespace for std:: names in header files
 *
 *     Removed unnecessary specification of default std::allocator
 *
 * Frank signed-off
 *
 * Revision 1.12  2014/08/06 11:42:53  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.11  2012/05/07 14:11:16  fisyak
 * Keep btofGeometry in const area for future use
 *
 * Revision 1.10  2011/07/27 16:13:58  geurts
 * Alignment calibration modifications [Patrick Huck]:
 *  -  modified to open the local Z window cut to determine the z offset
 *  -  variables mZLocalCut, mCalculateAlign and mAlignFileName added
 *  -  functions setCalculateAlign and setAlignFileName added
 *
 * Revision 1.9  2010/08/09 19:18:45  geurts
 * Include local theta calculation in CellHit structure. Pass LocalTheta info on to TOF PID traits. [Masa]
 *
 * Revision 1.8  2010/07/14 20:35:21  geurts
 * introduce switch to enable ideal MC geometry, without alignment updates. Default: disabled
 *
 * Revision 1.7  2010/03/19 22:25:39  dongx
 * - Added getBTofGeom() function for outside use
 * - Remove AddConst(btofGeometry) to avoid crash due to duplication
 * - TOT selection window opened to 40 ns
 * - Added CPU timer printouts for processStEvent() funciton
 *
 * Revision 1.6  2009/09/15 00:30:45  dongx
 * 1) Added the functionality to perform the matching with MuDst directly.
 * 2) Several updates on the track cuts used for matching
 *    - flag<1000 was added
 *    - nHits>15 cut was removed
 * 3) Created a new StBTofPidTraits for any primary track
 * 4) Local Z window cut set to symmetric (fabs(localz)<3.05)
 * 5) Some small changes in the LOGGER output.
 *
 * Revision 1.5  2009/08/26 20:33:56  dongx
 * Geometry init moved to Init() function, also allow reading in from others
 *
 * Revision 1.4  2009/08/25 15:46:44  fine
 * fix the compilation issues under SL5_64_bits  gcc 4.3.2
 *
 * Revision 1.3  2009/07/24 18:52:53  dongx
 * - Local Z window restricted in the projection
 * - ToT selection is used firstly when more than one hits associated with a track
 * - matchFlag updated
 *    0:   no matching
 *    1:   1-1 matching
 *    2:   1-2 matching, pick up the one with higher ToT value (<25ns)
 *    3:   1-2 matching, pick up the one with closest projection posision along y
 *
 * Revision 1.2  2009/06/23 21:15:09  geurts
 * first set of doxygen tags
 *
 * Revision 1.1  2009/06/23 13:15:03  geurts
 * *** empty log message ***
 *
 *
 *******************************************************************/
#ifndef STBTOFMATCHMAKER_HH     
#define STBTOFMATCHMAKER_HH
#include "StMaker.h"
#include "StThreeVectorD.hh"
#include <string>
#include <vector>

class StEvent;
class StTrack;
class StGlobalTrack;
class StHelix;
#include "StThreeVectorF.hh"
class StTrackGeometry;
class StDcaGeometry;
class StBTofGeometry;
class StBTofCollection;
class StBTofRawHitCollection;
class StBTofHitCollection;
class StSPtrVecBTofRawHit;
class StSPtrVecBTofHit;
class TH1D;
class TH2D;
class TTree;

class StMuDst;
class StMuEvent;
class StMuTrack;

typedef std::vector<Int_t>  IntVec;
typedef std::vector<UInt_t>  UIntVec;
typedef std::vector<Double_t>  DoubleVec;

class StBTofMatchMaker : public StMaker {
public:
    /// Default constructor
    StBTofMatchMaker(const Char_t *name="btofMatch");
    ~StBTofMatchMaker();
    
    // void Clear(Option_t *option="");
    /// process start-up options
    Int_t  Init();
    /// initialize  DaqMap, Geometry, and INL
    Int_t  InitRun(Int_t);
    Int_t  FinishRun(Int_t); 
    /// Main match algorithm
    Int_t  Make();
    /// Print run summary, and write QA histograms
    Int_t  Finish();
    
    /// enable QA histogram filling
    void setCreateHistoFlag(Bool_t histos=kTRUE); 
    /// enable track-tree filling
    void setCreateTreeFlag(Bool_t tree=kTRUE);
    /// selection of inner or outer geometry. By default - outerGeometry
    void setOuterTrackGeometry();
    void setStandardTrackGeometry();
    /// set minimum hits per track
    void setMinHitsPerTrack(Int_t);
    /// set minimum fit points per track
    void setMinFitPointsPerTrack(Int_t);
    /// set minimum fit-points/max-points ratio
    void setMinFitPointsOverMax(Float_t);
    /// set maximum distance of closest approach
    void setMaxDCA(Float_t);
    /// set histogram output file name
    void setHistoFileName(const Char_t*);
    /// set ntuple output file name
    void setNtupleFileName(const Char_t*);
    /// save geometry if it will be used by following makers in the chain
    void setSaveGeometry(const Bool_t geomSave=kFALSE);
    /// switch between standard and ideal MC geometry
    void setIdealGeometry(const Bool_t useIdealGeometry=kTRUE);
    /// switch for alignment calculation
    void setCalculateAlign(const Bool_t calcAlign=kTRUE);
    /// input file for alignment parameters
    void setAlignFileName(const Char_t* infile="");

    /// switch to read in StEvent/MuDst
    void setMuDstIn(Bool_t muDstIn=kTRUE);
    /// method to retrieve the BTofGeom
    StBTofGeometry* getBTofGeom();

private:
    StTrackGeometry* trackGeometry(StTrack*);//!

    /// book histograms
    void bookHistograms();
    /// write histograms
    void writeHistogramsToFile();

    ///
    void processStEvent();
    ///
    void processMuDst();
        
    /// track selection
    Bool_t validTrack(StTrack*);

    /// track selection
    Bool_t validTrack(StMuTrack*);
                
public:
    Bool_t  doPrintMemoryInfo;     //! 
    Bool_t  doPrintCpuInfo;        //!

private:
    static const Int_t mDAQOVERFLOW = 255;

    /// number of trays (12)
    static const Int_t mNTray = 120;
    /// number of cells per tray (192)
    static const Int_t mNTOF = 192;
    /// number of modules per tray (32)
    static const Int_t mNModule = 32;
    /// number of cells per module (6)
    static const Int_t mNCell = 6;
    /// number of tubes per upVPD (19)
    static const Int_t mNVPD = 19;

    /// fixed tray ID for upVPD-east
    static const Int_t mEastVpdTrayId = 122;
    /// fixed tray ID for upVPD-west
    static const Int_t mWestVpdTrayId = 121;

    ///
    Float_t     mWidthPad;   //! cell pad width
    Float_t     mZLocalCut;  //! zlocal edge, 3.05 by default, open up to 5cm for alignment calculation

    StEvent *mEvent;
    StBTofGeometry *mBTofGeom;         //! pointer to the TOF geometry utility class
    
    Bool_t mHisto;    //! create, fill and write out histograms
    Bool_t mSaveTree; //! create, fill and write out trees for tpc tracks
    Bool_t mUseIdealGeometry; //! ignore alignment corrections and use the ideal MC geometry
    Bool_t mCalculateAlign;   //! if used for alignment calculation
    
    Bool_t mOuterTrackGeometry; //! use outer track geometry (true) for extrapolation
    Bool_t mGeometrySave;    //! flag to save the geometry for others
    Bool_t mInitFromOther;   //! flag indicating geometry initialized from others
    
    string mHistoFileName; //! name of histogram file, if empty no write-out
    string mAlignFileName; //! name of input align file
    
    StMuDst*          mMuDst;
    Bool_t            mMuDstIn;          //! switch - default is to read in StEvent
                
    /// event counters
    Int_t  mEventCounter;          //! #processed events
    Int_t  mAcceptedEventCounter;  //! #events w/ valid prim.vertex
    Int_t  mTofEventCounter;       //! #events w/ Tof raw data
    Int_t  mAcceptAndBeam;         //! #(beam events) w/ prim.vertex
    
    /// various cut-offs and ranges
    unsigned int mMinHitsPerTrack; //! lower cut on #hits per track
    unsigned int mMinFitPointsPerTrack; //! lower cut on #fitpoints per track
    Float_t mMinFitPointsOverMax; //! lower cut on #fitpoints / #maxpoints
    Float_t mMaxDCA; //! upper cut (centimeters) on final (global) DCA
    
    //

    /// TOF histograms for matching QA
    TH2D* mADCTDCCorelation;
    
    TH1D* mEventCounterHisto;
    TH1D* mCellsMultInEvent;
    TH1D* mHitsMultInEvent;
    TH1D* mHitsPrimaryInEvent;   // ! primary tracks
    TH1D* mHitsMultPerTrack;
    TH1D* mDaqOccupancy[mNTray];
    TH1D* mDaqOccupancyProj[mNTray];
    TH2D* mHitsPosition;
        
    TH2D* mHitCorr[mNTray];
    TH2D* mHitCorrModule[mNTray];

    TH2D* mDeltaHitFinal[mNTray];

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
    
    TH1D* mCellsPrimaryPerEventMatch3;
            
    typedef std::vector<Int_t> idVector;
    typedef idVector::iterator idVectorIter;   

    struct StructCellHit{
      Int_t tray;
      Int_t module;
      Int_t cell;
      StThreeVectorF hitPosition;
      idVector trackIdVec;
      Int_t matchFlag;
      Float_t zhit;
      Float_t yhit;
      Double_t tot;
      Int_t index2BTofHit;
      Double_t theta;
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
    
    typedef std::vector<StructCellHit> tofCellHitVector;
    typedef std::vector<StructCellHit>::iterator tofCellHitVectorIter;
    
    
    virtual const char *GetCVS() const 
      {static const char cvs[]="Tag $Name:  $ $Id: StBTofMatchMaker.h,v 1.13 2017/10/20 17:50:33 smirnovd Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
    
    ClassDef(StBTofMatchMaker,1)
};
      

inline void StBTofMatchMaker::setOuterTrackGeometry(){mOuterTrackGeometry=true;}

inline void StBTofMatchMaker::setStandardTrackGeometry(){mOuterTrackGeometry=false;}

inline void StBTofMatchMaker::setMinHitsPerTrack(Int_t nhits){mMinHitsPerTrack=nhits;}

inline void StBTofMatchMaker::setMinFitPointsPerTrack(Int_t nfitpnts){mMinFitPointsPerTrack=nfitpnts;}

inline void StBTofMatchMaker::setMinFitPointsOverMax(Float_t ratio) {mMinFitPointsOverMax=ratio;}

inline void StBTofMatchMaker::setMaxDCA(Float_t maxdca){mMaxDCA=maxdca;}

inline void StBTofMatchMaker::setHistoFileName(const Char_t* filename){mHistoFileName=filename;}

inline void StBTofMatchMaker::setCreateHistoFlag(const Bool_t histos){mHisto = histos;}

inline void StBTofMatchMaker::setCreateTreeFlag(const Bool_t tree){mSaveTree = tree;}

inline void StBTofMatchMaker::setSaveGeometry(const Bool_t geomSave){mGeometrySave = geomSave; }

inline void StBTofMatchMaker::setIdealGeometry(const Bool_t useIdealGeometry){mUseIdealGeometry = useIdealGeometry;}

inline void StBTofMatchMaker::setCalculateAlign(const Bool_t calcAlign){mCalculateAlign = calcAlign;}

inline void StBTofMatchMaker::setAlignFileName(const Char_t* infile) {mAlignFileName = infile;}

inline void StBTofMatchMaker::setMuDstIn(const Bool_t val) { mMuDstIn = val; }

inline StBTofGeometry* StBTofMatchMaker::getBTofGeom() { return mBTofGeom; }
#endif

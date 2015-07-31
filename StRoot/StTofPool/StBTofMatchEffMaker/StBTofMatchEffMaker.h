/*******************************************************************
 *
 * $Id: StBTofMatchEffMaker.h,v 1.3 2015/07/28 22:50:03 smirnovd Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: BTof Match Maker to do the matching between the 
 *              fired celles and TPC tracks
 *
 *****************************************************************
 *
 * $Log: StBTofMatchEffMaker.h,v $
 * Revision 1.3  2015/07/28 22:50:03  smirnovd
 * C++11 requires a space between literal and identifier
 *
 * Revision 1.2  2010/01/28 18:17:53  perev
 * WarningOff
 *
 * Revision 1.1  2009/02/26 21:23:17  dongx
 * first release - example to calculate the TPC->TOF matching efficiency
 *
 *
 *******************************************************************/
#ifndef STBTOFMATCHEFFMAKER_HH     
#define STBTOFMATCHEFFMAKER_HH
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
class StPrimaryTrack;
class StHelix;
#include "StThreeVectorF.hh"
class StTrackGeometry;
class StBTofPidTraits;
class TH1D;
class TH2D;
class TTree;

class StBTofMatchEffMaker : public StMaker {
public:
    /// Default constructor
    StBTofMatchEffMaker(const Char_t *name="btofMatch");
    ~StBTofMatchEffMaker();
    
    //    void Clear(Option_t *option="");
    Int_t  Init();
    /// initial functions - DaqMap, Geometry Alignment, INL are extracted from db
    Int_t  Make();
    Int_t  Finish();
    
    void setCreateHistoFlag(Bool_t histos=kTRUE);
    void setMinFitPointsPerTrack(Int_t);
    void setMinFitPointsOverMax(Float_t);
    void setMaxDCA(Float_t);
    void setHistoFileName(const Char_t*);

private:
    /// book and write histograms
    void bookHistograms();
    void writeHistogramsToFile();

    /// event selection    
    Bool_t validEvent(StEvent *);
    /// track selection
    Bool_t validTrack(StTrack*);

public:
    Bool_t  doPrintMemoryInfo;     //! 
    Bool_t  doPrintCpuInfo;        //!

private:
    StEvent *mEvent;
    
    Bool_t mHisto;    //! create, fill and write out histograms
    string mHistoFileName; //! name of histogram file, if empty no write-out
    
    /// event counters
    Int_t  mEventCounter;          //! #processed events
    Int_t  mAcceptedEventCounter;  //! #events w/ valid prim.vertex
    Int_t  mTofEventCounter;       //! #events w/ Tof raw data
    Int_t  mAcceptAndBeam;         //! #(beam events) w/ prim.vertex
    
    /// various cut-offs and ranges
    unsigned int mMinFitPointsPerTrack; //! lower cut on #fitpoints per track
    Float_t mMinFitPointsOverMax; //! lower cut on #fitpoints / #maxpoints
    Float_t mMaxDCA; //! upper cut (centimeters) on final (global) DCA
    
    //

    /// TOF histograms for matching QA
    TH1D* mEventCounterHisto;

    TH2D* mPionDen;
    TH2D* mKaonDen;
    TH2D* mProtonDen;
    TH2D* mAntiPDen;

    TH2D* mPionNum;
    TH2D* mKaonNum;
    TH2D* mProtonNum;
    TH2D* mAntiPNum;
    
    virtual const char *GetCVS() const 
      {static const char cvs[]="Tag $Name:  $ $Id: StBTofMatchEffMaker.h,v 1.3 2015/07/28 22:50:03 smirnovd Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
    
    ClassDef(StBTofMatchEffMaker,1)
};
      

inline void StBTofMatchEffMaker::setMinFitPointsPerTrack(Int_t nfitpnts){mMinFitPointsPerTrack=nfitpnts;}

inline void StBTofMatchEffMaker::setMinFitPointsOverMax(Float_t ratio) {mMinFitPointsOverMax=ratio;}

inline void StBTofMatchEffMaker::setMaxDCA(Float_t maxdca){mMaxDCA=maxdca;}

inline void StBTofMatchEffMaker::setHistoFileName(const Char_t* filename){mHistoFileName=filename;}

inline void StBTofMatchEffMaker::setCreateHistoFlag(Bool_t histos){mHisto = histos;}

#endif

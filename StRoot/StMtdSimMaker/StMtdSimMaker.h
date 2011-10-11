/***************************************************************************
 *
 * $Id: StMtdSimMaker.h,v 1.1 2011/10/11 16:28:11 perev Exp $
 *
 * Author:  Frank Geurts
 ***************************************************************************
 *
 * Description: StMtdSimMaker virtual base class for Barrel TOF Simulations
 *
 ***************************************************************************
 *
 * $Log: StMtdSimMaker.h,v $
 * Revision 1.1  2011/10/11 16:28:11  perev
 * *** empty log message ***
 *
 *
 **************************************************************************/
#ifndef STBMTDSIMMAKER_HH
#define STBMTDSIMMAKER_HH
#include "StMaker.h"

#include "St_DataSet.h"
class TH1F;
class TH2F;
class TNtuple;
class TNtuple;
class TProfile;

class StEvent;
class StMtdCollection;
struct g2t_mtd_hit_st;

// g2t tables
#include "tables/St_g2t_mtd_hit_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_tpc_hit_Table.h"

#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcMtdHitCollection.hh"
#include "StMcEvent/StMcMtdHit.hh"
#include "StThreeVectorF.hh"
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

class StMtdSimMaker : public StMaker{
 protected:


  StMcMtdHitCollection *mMcMtdHitCollection; //! barrel tof hit

  St_DataSet        *mGeantData;        //! geant table
  StEvent           *mEvent;            //!
  StMcEvent         *mMcEvent;
  StMtdCollection   *mMtdCollection;   

  //define some constants
  enum {
    //    mNBackleg = 32,
    // mNModule   = 5,
    mNBackleg = 99,
    mNModule   = 99,
    mNCell = 12,     //! 12 cells per box
//    mAMP = 50000,     
//    mADCBINWIDTH = 25,
//    mTDCBINWIDTH = 50
  };

//  const static float mVHRBIN2PS = 24.4;  //! Very High resolution mode, ps/bin
//  const static float mHRBIN2PS = 97.7;     //! High resolution mode, ps/bin
  const static float mMtdPadWidth = 3.8 + 0.6; //! Pad Width: 38mm padwidth + 6mm innerspacing


//  Bool_t mCellXtalk;     //! switch for cell xtalk
//  Bool_t mSlow;
  Bool_t mBookHisto;
  Bool_t mWriteStEvent;  //! switch to enable Maker to write out simulated hits to StEvent

  Int_t     mMtdHitFlag[mNBackleg][mNModule*mNCell];   //! hit flag for tof geant hits

  struct TrackHit{
    Int_t          backleg;
    Int_t          module;
    Int_t          cell;
    Int_t          trkId;
    Double_t       dE;
    Double_t       dQ;
    Double_t       dQdt[600];//this 600 (nTimebins) comes from the /TofUtil/StTofParam file
    Double_t       tof;
    Double_t       s_track;
    Double_t          t0;              //! t0 (in ps) as the start of this tof hit -- was ns - changed for consistency
    StThreeVectorF position;
  };


  typedef vector<TrackHit, allocator<TrackHit> > TrackVec;

  string mHistFile;//for QA histograms
  TH1F* mBetaHist;    //! speed of particles hitting tof
  TH1F* mPathLHist;    //! speed of particles hitting tof
  TH1F* mTofHist;    //! total time of flight of partilce
  TH1F* mRecMass;    //! reconstructed mass of particle

  TH2F* mCellGeant;    //! cellId of geant hit
  TH1F* mVpdGeant;     //! Vpd tubeId of geant hit
  TH2F* mNCellGeant;   //! # of cells of geant hit
  TH2F* mNVpdGeant;    //! # of vpd tubes of geant hit
  TH1F* mDeGeant;      //! deposited-energy in geant hit
  TH1F* mTofGeant;     //! tof in geant hit

  TH2F* mCellSeen;     //! cellId after DetectorResponse
  TH1F* mVpdSeen;      //! Vpd tubeId after DetectorResponse
  TH2F* mNCellSeen;    //! # of cells after DetectorResponse
  TH2F* mNVpdSeen;     //! # of vpd tubes after DetectorResponse
  TH1F* mDeSeen;       //! deposited-energy after DetectorResponse
  TH1F* mT0Seen;      //! 
  TH1F* mTofSeen;      //! smeared-tof after DetectorResponse
  TH1F* mTofResSeen;   //! time resolution after Detector Response
  TH1F* mVpdResSeen;   //! vpd time resolution after DetectorResponse

  TH2F* mCellReco;     //! cellId after recon
  TH1F* mVpdReco;      //! Vpd tubeId after recon
  TH2F* mNCellReco;    //! # of cells after recon
  TH2F* mNVpdReco;     //! # of vpd tubes after recon
  TH1F* mTDCReco;      //! TDC recon
  TH1F* mADCReco;      //! ADC recon -- empty
  TH1F* mT0Reco;   //! 
  TH1F* mTofResReco;   //! time resolution after recon
  TH1F* mVpdResReco;   //! vpd time resolution after recon
  TH2F* mTACorr;       //! T-A Slewing Correlation
  TH1F* mModHist;       //! T-A Slewing Correlation

  /// TOFp histograms
    TH1F* mdE;           //!
    TH1F* mdS;           //!
    TH1F* mNumberOfPhotoelectrons;  //!
    TH1F* mT;            //!
    TH1F* mTime;         //!
    TH1F* mTime1;        //!
    TH1F* mPMlength;     //!
    TH1F* mAdc;          //!
    TH1F* mTdc;          //!

    TVolume *starHall;

//fg    Int_t CellResponse(g2t_mtd_hit_st* mtd_hit,
//fg				TrackVec& trackVec);   //! Slow simulation step one
//fg    Int_t CellTimePassTh(TrackVec& trackVec);        //! Slow simulation step two
//fg
    Int_t FastCellResponse(g2t_mtd_hit_st* mtd_hit);
//fg
    vector<Int_t>    CalcCellId(Int_t volume_id, Float_t ylocal);
//fg    Int_t CellXtalk(Int_t icell, Float_t ylocal, Float_t& wt, Int_t& icellx);
    Int_t      storeMcMtdHit(StMcMtdHit *mcCellHit);
//fg
//fg    Int_t        fillRaw(void);
//fg    Int_t        electronicNoise(void);
//fg    Float_t       slatResponseExp(Float_t&);
//fg    Double_t GammaRandom();
//fg
//fg
    Int_t        fillEvent();
    Int_t        bookHistograms();
    Int_t        writeHistograms();
    Int_t        ResetFlags();


 public:
    StMtdSimMaker(const char *name="TofSim");
    virtual ~StMtdSimMaker();

    void           Reset();
    virtual Int_t  Init();
    Int_t          InitRun(Int_t);
    Int_t          FinishRun(Int_t);
    virtual Int_t  Make();
    virtual Int_t  Finish();

//fg    StMtdCollection*  GetMtdCollection()  const { return mMtdCollection; }
//fg    StMcMtdHitCollection* GetMcMtdHitCollection() const { return mMcMtdHitCollection; }
//fg
//fg    void   setHistFileName(string s);
//fg    void   setBookHist(Bool_t val) { mBookHisto = val; }
//fg    void   writeStEvent(Bool_t val = kTRUE) {mWriteStEvent = val;}

    virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StMtdSimMaker.h,v 1.1 2011/10/11 16:28:11 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

    ClassDef(StMtdSimMaker,1)
};
#endif

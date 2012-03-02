/***************************************************************************
 *
 * $Id: StMtdSimMaker.h,v 1.2 2012/03/02 02:18:34 perev Exp $
 *
 * Author:  Frank Geurts
 ***************************************************************************
 *
 * Description: StMtdSimMaker virtual base class for Barrel TOF Simulations
 *
 ***************************************************************************
 *
 * $Log: StMtdSimMaker.h,v $
 * Revision 1.2  2012/03/02 02:18:34  perev
 * New provisional version rewritten by VP
 *
 *
 **************************************************************************/
#ifndef STBMTDSIMMAKER_HH
#define STBMTDSIMMAKER_HH
#include "StMaker.h"

class StEvent;
struct g2t_mtd_hit_st;
class StMtdCollection;
class TH2F;

// g2t tables
#include "tables/St_g2t_mtd_hit_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_tpc_hit_Table.h"

#include <vector>

class StMtdSimMaker : public StMaker
{
 protected:
  St_DataSet        *mGeantData;        //! geant table
  StEvent           *mEvent;            //!
  StMtdCollection   *mMtdCollection;   
  int                mNMtdHits;
  g2t_mtd_hit_st    *mMtdHitsFromGeant;

  //define some constants
  enum {
   kNBackleg = 99,
   kNModule  = 99,
   kNCell    = 12,     //! 12 cells per box
   kAMP      = 50000,     
   kADCBINWIDTH = 25,
   kTDCBINWIDTH = 50
 };

//const static float kVHRBIN2PS = 24.4;  	//! Very High resolution mode, ps/bin
//const static float kHRBIN2PS = 97.7;     	//! High resolution mode, ps/bin
  const static float kMtdPadWidth = 3.8 + 0.6; 	//! Pad Width: 38mm padwidth + 6mm innerspacing


//  Bool_t mCellXtalk;     //! switch for cell xtalk
//  Bool_t mSlow;
  Bool_t mBookHisto;
  Bool_t mWriteStEvent;  //! switch to enable Maker to write out simulated hits to StEvent



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

    Int_t        bookHistograms();
    Int_t        writeHistograms();


 public:
    StMtdSimMaker(const char *name="TofSim");
    virtual ~StMtdSimMaker();

    void           Reset();
    virtual Int_t  Init();
    Int_t          InitRun(Int_t);
    Int_t          FinishRun(Int_t);
    virtual Int_t  Make();
    virtual Int_t  Finish();
            Int_t  FastCellResponse();
              int  CalcCellId(Int_t volume_id, Float_t ylocal,
                              int &ibackleg,int &imodule,int &icell);
			       
//fg    StMtdCollection*  GetMtdCollection()  const { return mMtdCollection; }
//fg
//fg    void   setHistFileName(string s);
//fg    void   setBookHist(Bool_t val) { mBookHisto = val; }
//fg    void   writeStEvent(Bool_t val = kTRUE) {mWriteStEvent = val;}

    virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StMtdSimMaker.h,v 1.2 2012/03/02 02:18:34 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

    ClassDef(StMtdSimMaker,1)
};
#endif

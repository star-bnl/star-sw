/*
  StSkimPionMaker.h
  
*/

#ifndef STAR_StSkimPionMaker
#define STAR_StSkimPionMaker

#ifndef StMaker_H
#include "StMaker.h"
#include "TH1.h"
#include "TObjArray.h"
#endif

#include "StThreeVectorF.hh"

//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEmcUtil.h"
#include "StEmcRawMaker/defines.h"
#include "StEmcRawMaker/StBemcRaw.h"
#include "StEmcRawMaker/StBemcTables.h"

#include "TSkimPionEvent.h"

class TNtuple;
class TH2F;
class StChain;
class StPrimaryTrack;
class StPrimaryVertex;
class StEmcCollection;
class StEmcGeom;
class StEmcPoint;

#define badrunidmax 200
#define ntower 4800

class StSkimPionMaker : public StMaker 
{
private:
protected:
    Bool_t       mDoTracks; //!
    StChain*     mChain; //!      
    const char*  mFileName; //!
    
    StMuDstMaker* mMuDstMaker;  ///< Pointer to the StMuDstMaker which provides the events to analyse.
    StMuDst* mMuDst; ///< Pointer to the StMuDst class, which holds all the events information. Will be updated in the event loop.
    
    StBemcTables* mTables; // used to access EMC status and pedestal info
    
    float mPi; //!
    float mPi0Mass; //!
    
    bool debug; //!
    
    bool mb; //!
    bool httpl2; //!
    bool httpl2_test; //!
    Int_t mBBCTrig; //!
    Int_t triggerTower; //!
    Float_t mBBCVertexZ; //!

    Int_t badrunid[badrunidmax]; //!
    Int_t tower[ntower]; //!
    Float_t gain[ntower]; //!
    Float_t c_factors[4801]; //!
    
    Int_t runN; //!
    Int_t eventN; //!
    Int_t fillN; //!
    Int_t indicator; //!
    
    Int_t ievtot; //!
    Int_t ievaccep; //!
    Int_t inochainpt; //!
    Int_t inoevpt; //!
    Int_t ibadrun; //!
    Int_t iemc; //!
    Int_t inoprimvert; //!
    Int_t itrig; //!
    Int_t ievmix; //!
    Int_t iWrittenEvents; //!
    Int_t ii; //!
    Double_t mField; //!
    Float_t mHiTowerAdc6Bit; //! used for MC trigger selection and offline trig definition
    // for HT1 triggers, this should be > 10
    
    TObjArray   *photonlist;
    
    //Temp
    Int_t startphoton1;   //!
    Int_t startphoton2;   //!
    Int_t fnummixed;  //! number of mixed events 
    
    TObjArray  *mixedphoton1list; //! 
    TObjArray  *mixedphoton2list; //! 
    
    TFile*       mFile; //!
    
    TTree*            pi0Tree; //!
    TSkimPionEvent*   pi0Event; //!
        
    Float_t      getHiTowerEt(StEmcCollection*);
    Int_t        doTrackPtHist(Float_t energy, Float_t threshold, TObjArray *photonlist);
    Int_t        associateTracksWithEmcPoints(StMaker* anyMaker);
    
    void         getTowerHitInfo();
    StThreeVectorF getPoint(StEmcPoint *p, Int_t&, Float_t&, Float_t&, Int_t&, Int_t&, Float_t&, Float_t&, Float_t&,Float_t&, Float_t&, Float_t&); //added info on SMD and Tower energy, size
    void         printPointInfo(StEmcPoint *p);
    void         getPhotonSpectra(TObjArray *photonlist, Int_t, Float_t, Int_t);
    Float_t	 getNeutralEnergySum(TObjArray *photonlist);
    Bool_t       getMass(StThreeVectorF, StThreeVectorF, Float_t, Float_t, Double_t&, StThreeVectorF&, Float_t&, Float_t&, Float_t&, Float_t&);
    
    void         getInvMass(Int_t, TObjArray *photonlist1, TObjArray *photonlist2, Float_t, StThreeVectorF, int[3], int[3]);
    
public: 
    
    StSkimPionMaker(const char *name="pi0AnaMaker", Bool_t mDoTracks=kTRUE, const char *mFileName="bla.root");
    virtual       ~StSkimPionMaker();  
    
    virtual Int_t Init();
    virtual Int_t InitRun(int);
    virtual Int_t Make();
    virtual Int_t Finish();
    Bool_t  readPointList();
    
    ClassDef(StSkimPionMaker, 1)   //StAF chain virtual base class for Makers
	};
#endif

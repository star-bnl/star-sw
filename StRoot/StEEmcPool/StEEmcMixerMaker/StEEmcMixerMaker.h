/*!\class StEEmcMixerMaker
\author Wei-Ming Zhang, KSU 3/16/2005 
Modified from StEmcMixerMaker of Alexandre A. P. Suaide

  This is the basic embedding scheme for EEMC. This maker gets the output
of two StEvent objects (The second is converted from a MuDst) and merges 
the EEMC hits of the second event into the first one. Before doing that, 
it erases all the clusters and points if they exisit, so cluster finder and 
point maker should be run again. 
  The maker has full conectivity to the EEMC database through StEEmcDb, 
so it will embed hits only if the corresponding channel is active in the real 
data. It also has the feature of embed the global and primary tracks 
of the second StEvent into the first but THIS IS NOT A TPC EMBED. This feature
can be turned off if desired.
*/

#ifndef STAR_StEEmcMixerMaker
#define STAR_StEEmcMixerMaker

#include "StMaker.h"
#include <TH1.h>

// NEEMCDETS=NDETECTORS =4. NDETECTORS already defined in StEEmcSimulatorMaker
#define NEEMCDETS 4

#define DETOFFSET  4   // offset to the order in EMC (BEMC+EEMC)  

#define TOWCHANNELS 720    
#define PRECHANNELS 2160  
#define MAXCHANNELS 3600    

class StEvent;
class StEmcRawHit;
class StMuEmcUtil;
class StEEmcDb;
                                                      
class StEEmcMixerMaker : public StMaker 
{
  private:
    StEvent       *mEvent1;
    StEvent       *mEvent2;

    StMuEmcUtil   *mMuUtil;
    StEEmcDb      *mEEDb;

    Int_t         mStatus[NEEMCDETS][MAXCHANNELS];
    Float_t       mPed[NEEMCDETS][MAXCHANNELS];
    Float_t       mSigPed[NEEMCDETS][MAXCHANNELS];
    Float_t       mGain[NEEMCDETS][MAXCHANNELS];
    
    Bool_t        mMinusPed[NEEMCDETS];
    Bool_t        mAdcToE;
    Bool_t        mClear;
    Bool_t        mAddHits;
    Bool_t        mFakeTrackEmbed;
    Bool_t        mDoPrint;
    Bool_t        mEmbedAll;

    TH1F          *m_hit_1;
    TH1F          *m_hit_2;
    TH1F          *m_edep_1;
    TH1F          *m_edep_2;
    
    virtual Int_t addHits(); 
    virtual Int_t addTracks(); 
    void          clearPoints();
    void          clearClusters();
    void          getDB();
    void          adcToEnergy();
    Bool_t        getEvents();
    Bool_t        checkHit(Int_t, StEmcRawHit*);
    Float_t       getPed(Int_t, StEmcRawHit*);
    Float_t       getSigPed(Int_t, StEmcRawHit*);
    Float_t       getGain(Int_t, StEmcRawHit*);

  protected:

  Float_t mScale;
  Float_t mSigmaPed[NEEMCDETS]; // cut nsigma > ped for 4 sub-detectors 

  public: 

                  StEEmcMixerMaker(const char *name="eemcEmbed");
    virtual       ~StEEmcMixerMaker();
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();
  
    void          printHits(StEvent*); ///< Prints all EEMC hits in the StEvent object
    
    void          setClear(Bool_t a)        { mClear = a; } ///< Set kTRUE to clear EMC clusters and points
    void          setAddFlag(Bool_t a)      { mAddHits = a; } ///< Set kTRUE to embed hits
    void          setFakeTrack(Bool_t a)    { mFakeTrackEmbed = a; } ///< Set kTRUE to merge tracks
    void          setEmbedAll(Bool_t a)     { mEmbedAll = a; } ///< Set kTRUE to embedd all hits even if the first emcCollection has no hits
    void          setPrint(Bool_t a)        { mDoPrint = a; } ///< Set kTRUE to print debug information
    void          setAdcToE(Bool_t a)       { mAdcToE = a; } ///< Set kTRUE to covert ADC to energy

  void scale(Float_t s){ mScale=s; }
  void threshold(Float_t c, Int_t det){ mSigmaPed[det]=c; }

// Set mMinusPed 
    void          setMinusPed(Int_t i, Bool_t a)   { mMinusPed[i] = a; } 
    void          setAllMinusPed(Bool_t a)   { 
                  for(Int_t i=0; i<NEEMCDETS; i++) mMinusPed[i] = a; } 
    
    virtual const char *GetCVS() const {static const char cvs[]="Tag $Name:  $ $Id: StEEmcMixerMaker.h,v 1.2 2009/02/04 20:33:18 ogrebeny Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StEEmcMixerMaker,0) 
};
#endif

///////////////////////////////////////////////////////////////////////////
//
// $Id: StEEmcMixerMaker.h,v 1.2 2009/02/04 20:33:18 ogrebeny Exp $
// $Log: StEEmcMixerMaker.h,v $
// Revision 1.2  2009/02/04 20:33:18  ogrebeny
// Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
//
// Revision 1.1.1.1  2005/05/31 18:53:25  wzhang
// First version
//
//
///////////////////////////////////////////////////////////////////////////


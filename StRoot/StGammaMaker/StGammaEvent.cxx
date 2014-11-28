#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StGammaPythiaEvent.h"

#include "StGammaEvent.h"

ClassImp(StGammaEvent);

//////////////////////////////////////////////////
//                 Constructor                  //
//////////////////////////////////////////////////
StGammaEvent::StGammaEvent()
      :   mFlags(0x0000)
        , mRunNumber(0)             /// Run number
        , mEventNumber(0)           /// Event number  
        , mVertex(0,0,0)            /// Event vertex (TPC)
        , mMagneticField(1956)      /// Magnetic field (kG)
        , mPythia(0)

        , mSpinDbValid(false)
        , mSpin4(kUnknownSpin4)
        , mBunchCrossing7bit(255)
        , mBunchCrossing48bit(255)
        , mBunchCrossingStar(255)
        , mPolarizationType(kUnknownPol)
        
        , mDsmVertex(255)
        , mTracks(0)     //-> array of all tracks
        , mTowers(0)     //-> array of all towers
        , mPreshower1(0) //-> array of all preshower1
        , mPreshower2(0) //-> array of all preshower2
        , mPostshower(0) //-> array of all postshower
        , mStrips(0)     //-> array of all strips
        , mCandidates(0) //-> array of all candidates
{
    InitArrays(); // initialize tclones arrays
    Clear();
}

//////////////////////////////////////////////////
//                  Destructor                  //
//////////////////////////////////////////////////
StGammaEvent::~StGammaEvent()
{   
  delete mTracks;     mTracks     = 0;
  delete mTowers;     mTowers     = 0;     //-> array of all towers
  delete mPreshower1; mPreshower1 = 0; //-> array of all preshower1
  delete mPreshower2; mPreshower2 = 0;//-> array of all preshower2
  delete mPostshower; mPostshower = 0;//-> array of all postshower
  delete mStrips;     mStrips     = 0; //-> array of all strips
  delete mCandidates; mCandidates = 0; //-> array of all candidates
}

//////////////////////////////////////////////////
//               Clear the event                //
//////////////////////////////////////////////////
void StGammaEvent::Clear(Option_t *opts)
{
    mRunNumber = 0;
    mEventNumber = 0;
    mTriggerIds.clear();
    mSimuTriggerIds.clear();
    mVertex = TVector3(0.,0.,0.);
    
    mTracks->Clear();
    mTowers->Clear();
    mPreshower1->Clear();
    mPreshower2->Clear();
    mPostshower->Clear();
    mStrips->Clear();
    mCandidates->Clear();
    
    mFlags = 0x0000;
    
    mSpinDbValid        = false;
    mSpin4              = kUnknownSpin4;
    mBunchCrossing7bit  = 255;
    mBunchCrossing48bit = 255;
    mBunchCrossingStar  = 255;
    mPolarizationType   = kUnknownPol;
    mDsmVertex          = 255;
    
    if(mPythia) mPythia->Clear(opts);
  
}

//////////////////////////////////////////////////
//     Initialize the internal TClonesArrays    //
//////////////////////////////////////////////////
Int_t StGammaEvent::InitArrays()
{
    if (!mTracks)    mTracks     = new TClonesArray("StGammaTrack", 1000);
    if (!mTowers)    mTowers      = new TClonesArray("StGammaTower", 4800 + 720);
    if (!mPreshower1)mPreshower1 = new TClonesArray("StGammaTower", 4800 + 720);
    if (!mPreshower2)mPreshower2 = new TClonesArray("StGammaTower", 720);
    if (!mPostshower)mPostshower = new TClonesArray("StGammaTower", 720);
    if (!mStrips)    mStrips     = new TClonesArray("StGammaStrip", 10000);
    if (!mCandidates)mCandidates = new TClonesArray("StGammaCandidate", 10);
    return 1;
}

//////////////////////////////////////////////////
//            Add detector objects              //
//        and candidates to the event           //
//////////////////////////////////////////////////
StGammaTrack *StGammaEvent::newTrack(StMuTrack *mutr)
{

    TClonesArray &tracks = *mTracks;
    StGammaTrack *track = 0;
    
    if(mutr) track = new( tracks[tracks.GetEntriesFast()] ) StGammaTrack(mutr);
    else     track = new( tracks[tracks.GetEntriesFast()] ) StGammaTrack;
    
    return track;
  
}

StGammaTower *StGammaEvent::newTower()
{
    TClonesArray &towers = *mTowers;
    StGammaTower *tower = new( towers[towers.GetEntriesFast()] ) StGammaTower;
    return tower;
}

StGammaTower *StGammaEvent::newPreshower1()
{
    TClonesArray &preshower1 = *mPreshower1;
    StGammaTower *tower = new( preshower1[preshower1.GetEntriesFast()] ) StGammaTower;
    return tower;
}

StGammaTower *StGammaEvent::newPreshower2()
{
    TClonesArray &preshower2 = *mPreshower2;
    StGammaTower *tower = new( preshower2[preshower2.GetEntriesFast()] ) StGammaTower;
    return tower;
}

StGammaTower *StGammaEvent::newPostshower()
{
    TClonesArray &postshower = *mPostshower;
    StGammaTower *tower = new( postshower[postshower.GetEntriesFast()] ) StGammaTower;
    return tower;
}

StGammaStrip *StGammaEvent::newStrip()
{
    TClonesArray &strips = *mStrips;
    StGammaStrip *strip = new( strips[strips.GetEntriesFast()] ) StGammaStrip;
    return strip;
}

StGammaCandidate *StGammaEvent::newCandidate()
{
    TClonesArray &candidates = *mCandidates;
    StGammaCandidate *candidate = new( candidates[candidates.GetEntriesFast()] ) StGammaCandidate;
    return candidate;
}

//////////////////////////////////////////////////
//     Sum the total track pT and tower eT      //
//         within the given eta ring            //
//////////////////////////////////////////////////
Float_t StGammaEvent::sumPt(Float_t eta_min, Float_t eta_max) const
{
    return sumTrackPt(eta_min,eta_max) + sumTowerPt(eta_min,eta_max);
}

//////////////////////////////////////////////////
//           Sum the total track pT             //
//         within the given eta ring            //
//////////////////////////////////////////////////
Float_t StGammaEvent::sumTrackPt(Float_t eta_min, Float_t eta_max) const
{  

    Float_t sum = 0.0;
    for(Int_t i = 0; i < numberOfTracks(); i++)
    {
        StGammaTrack *t = track(i);
        if(t->eta() < eta_min || t->eta() > eta_max) continue;
        sum += t->pt();
    }
    
    return sum;
  
}

//////////////////////////////////////////////////
//           Sum the total tower eT             //
//         within the given eta ring            //
//////////////////////////////////////////////////
Float_t StGammaEvent::sumTowerPt(Float_t eta_min, Float_t eta_max) const
{  

    Float_t sum = 0.0;
    for (Int_t i = 0; i < numberOfTowers(); i++)
    {
        StGammaTower *t = tower(i);
        if(t->fail) continue;      
        if(t->eta < eta_min || t->eta > eta_max) continue;
        sum += t->pt();
    }
    
    return sum;
  
}

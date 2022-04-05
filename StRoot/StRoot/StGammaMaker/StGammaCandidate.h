////////////////////////////////////////////////////////////
//                                                        //
//    StGammaCandidate                                    //
//                                                        //
//    Jason Webb (Valpo)                                  //
//                                                        //
//    Cosmetic update by Michael Betancourt               //
//                                                        //
//    Detector objects may be added to the candidate      //
//    as follows,                                         //
//                                                        //
//      StGammaTower tower = candidate.AddTower();        //
//      tower.id     = <tower id>;                        //
//      tower.ET     = <tower ET>;                        //
//      tower.eta    = <tower eta>;                       //
//      tower.phi    = <tower phi>;                       //
//      tower.status = <tower status>;                    //
//      tower.fail   = <tower fail>;                      //
//                                                        //
////////////////////////////////////////////////////////////

#ifndef STAR_StGammaCandidate
#define STAR_StGammaCandidate

#include "TObject.h"
#include "TVector3.h"
#include "TRefArray.h"

#include <vector>

#include "StGammaTower.h"
#include "StGammaStrip.h"
#include "StGammaTrack.h"
#include "StGammaFitterResult.h"

class StEEmcCluster;
class StEEmcSmdCluster;
class StEmcCluster;

class StGammaCandidate: public TObject
{

    public:

        enum CalorimeterId { kEEmc = 0, kBEmc, kUnknown = 255}; 
        enum thresholdCut { kMagnitude, kTransverse };
      
        StGammaCandidate();
        ~StGammaCandidate();
        
        virtual const char* GetCVS() const
        {static const char cvs[] = "Tag $Name:  $ $Id: StGammaCandidate.h,v 1.16 2014/08/06 11:43:17 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs; }

        ////////////////////////////////////////////
        //               Accessors                //
        ////////////////////////////////////////////
        
        Float_t sumPt( Float_t radius, Float_t threshold = 0.0, thresholdCut cut = kTransverse );
        Float_t sumTrackPt( Float_t radius, Float_t threshold = 0.0, thresholdCut cut = kTransverse );
        Float_t sumTowerPt( Float_t radius, Float_t threshold = 0.0, thresholdCut cut = kTransverse );
        Float_t sumPreshower1( Float_t radius, Float_t threshold = 0.0 );
        Float_t sumPreshower2( Float_t radius, Float_t threshold = 0.0 );
        Float_t sumPostshower( Float_t radius, Float_t threshold = 0.0 );
        
        Int_t numberOfTracks( Float_t radius, Float_t threshold = 0.0, thresholdCut cut = kTransverse );
        Int_t numberOfTowers( Float_t radius, Float_t threshold = 0.0, thresholdCut cut = kTransverse );
        Int_t numberOfPreshower1( Float_t radius, Float_t threshold = 0.0 );
        Int_t numberOfPreshower2( Float_t radius, Float_t threshold = 0.0 );
        Int_t numberOfPostshower( Float_t radius, Float_t threshold = 0.0 );
        
        Int_t detectorId() { return mDetector; }
        Int_t id() { return mGammaId; }
        
        Float_t  energy() const { return mEnergy; }
        Float_t  seedEnergy() const { return mSeedEnergy; }
        Float_t  pre1Energy(Float_t threshold = 0.0);
        Float_t  pre2Energy(Float_t threshold = 0.0);
        Float_t  postEnergy(Float_t threshold = 0.0);
        Float_t  smduEnergy(Float_t threshold = 0.0);
        Float_t  smdvEnergy(Float_t threshold = 0.0);
        Float_t  smdEtaEnergy(Float_t threshold = 0.0) { return smduEnergy(threshold); }
        Float_t  smdPhiEnergy(Float_t threshold = 0.0) { return smdvEnergy(threshold); }

        TVector3 position() const { return mPosition; }
        TVector3 momentum() const { return mMomentum; }
        
        TVector3 momentum1x1();
        TVector3 momentum1x1c();
        TVector3 momentum2x1();
    
        Int_t numberOfTracks(){ return mTracks.GetLast()+1; }
        Int_t numberOfTowers(){ return mTowers.GetLast()+1; }
        Int_t numberOfPreshower1(){ return mPreshower1.GetLast()+1; }
        Int_t numberOfPreshower2(){ return mPreshower2.GetLast()+1; }
        Int_t numberOfPostshower(){ return mPostshower.GetLast()+1; }
        Int_t numberOfSmdu(){ return mSmdu.GetLast()+1; }
        Int_t numberOfSmdv(){ return mSmdv.GetLast()+1; }
        Int_t numberOfSmdEta(){ return mSmdu.GetLast()+1; }
        Int_t numberOfSmdPhi(){ return mSmdv.GetLast()+1; }
        
        StGammaTrack *track( Int_t i ){ return (StGammaTrack*)mTracks.At(i); }
        StGammaTower *tower( Int_t i ){ return (StGammaTower*)mTowers.At(i); }
        StGammaTower *preshower1( Int_t i ){ return (StGammaTower*)mPreshower1.At(i); }
        StGammaTower *preshower2( Int_t i ){ return (StGammaTower*)mPreshower2.At(i); }
        StGammaTower *postshower( Int_t i ){ return (StGammaTower*)mPostshower.At(i); }
        StGammaStrip *smdu( Int_t i ){ return (StGammaStrip *)mSmdu.At(i); }
        StGammaStrip *smdv( Int_t i ){ return (StGammaStrip *)mSmdv.At(i); }
        StGammaStrip *smdEta( Int_t i ){ return (StGammaStrip *)mSmdu.At(i); }
        StGammaStrip *smdPhi( Int_t i ){ return (StGammaStrip *)mSmdv.At(i); }
        
        StGammaTrack *mytrack( Int_t i ){ return (StGammaTrack*)mMyTracks.At(i); }
        StGammaTower *mytower( Int_t i ){ return (StGammaTower*)mMyTowers.At(i); }
        StGammaTower *mypreshower1( Int_t i ){ return (StGammaTower*)mMyPreshower1.At(i); }
        StGammaTower *mypreshower2( Int_t i ){ return (StGammaTower*)mMyPreshower2.At(i); }
        StGammaTower *mypostshower( Int_t i ){ return (StGammaTower*)mMyPostshower.At(i); }
        
        Int_t numberOfMyTracks(){ return mMyTracks.GetLast()+1; }
        Int_t numberOfMyTowers(){ return mMyTowers.GetLast()+1; }
        Int_t numberOfMyPreshower1(){ return mMyPreshower1.GetLast()+1; }
        Int_t numberOfMyPreshower2(){ return mMyPreshower2.GetLast()+1; }
        Int_t numberOfMyPostshower(){ return mMyPostshower.GetLast()+1; }
        
        StGammaFitterResult& smdFit(Int_t plane=0) { return mSmdFit[plane]; }
        
        ////////////////////////////////////////////
        //                Mutators                //
        ////////////////////////////////////////////

        void recluster(TVector3 vertex, Float_t threshold, thresholdCut cut = kTransverse);
        
        void SetId(Int_t id) { if( id == kEEmc || id == kBEmc ) mGammaId = id; else mGammaId = kUnknown; }
        void SetTowerId( Int_t id ) { mTowerId = id; }
        void SetTowerClusterId( Int_t id ) { mTowerClusterId = id; }
        void SetSmduClusterId( Int_t id ) { mSmduClusterId = id; }
        void SetSmdvClusterId( Int_t id ) { mSmdvClusterId = id; }
        
        void SetSmdEtaClusterId( Int_t id ) { mSmduClusterId = id; }
        void SetSmdPhiClusterId( Int_t id ) { mSmdvClusterId = id; }
        
        void SetDetectorId( Int_t id ) { mDetector = id; }
        
        void SetMomentum( TVector3 p ){ mMomentum=p; }
        void SetPosition( TVector3 p ){ mPosition=p; }
        void SetEnergy( Float_t e ){ mEnergy=e; }
        void SetSeedEnergy( Float_t e ){ mSeedEnergy=e; }
        void SetPre1Energy( Float_t e ){ mPre1Energy=e; }
        void SetBPrsEnergy( Float_t e ){ mPre1Energy=e; } 
        void SetPre2Energy( Float_t e ){ mPre2Energy=e; }
        void SetPostEnergy( Float_t e ){ mPostEnergy=e; }
        void SetSmduEnergy( Float_t e ){ mSmduEnergy=e; }
        void SetSmdvEnergy( Float_t e ){ mSmdvEnergy=e; }
        void SetSmdEtaEnergy( Float_t e ){ mSmduEnergy=e; }
        void SetSmdPhiEnergy( Float_t e ){ mSmdvEnergy=e; }
        
        // Add event objects
        void addTrack ( StGammaTrack *track ){ mTracks.Add( track ); }
        void addTower ( StGammaTower *tower ){ mTowers.Add( tower ); }
        void addPreshower1( StGammaTower *pre1 ){ mPreshower1.Add( pre1 ); }
        void addPreshower2( StGammaTower *pre2 ){ mPreshower2.Add( pre2 ); }
        void addPostshower( StGammaTower *post ){ mPostshower.Add( post ); }
        void addSmdu( StGammaStrip *strip ){ mSmdu.Add(strip); }
        void addSmdv( StGammaStrip *strip ){ mSmdv.Add(strip); }
        void addSmdEta( StGammaStrip *strip ){ mSmdu.Add(strip); }
        void addSmdPhi( StGammaStrip *strip ){ mSmdv.Add(strip); }
        
        // Add cluster objects
        void addMyTrack ( StGammaTrack *track ){ mMyTracks.Add( track ); }
        void addMyTower ( StGammaTower *tower ){ mMyTowers.Add( tower ); }
        void addMyPreshower1( StGammaTower *pre1 ){ mMyPreshower1.Add( pre1 ); }
        void addMyPreshower2( StGammaTower *pre2 ){ mMyPreshower2.Add( pre2 ); }
        void addMyPostshower( StGammaTower *post ){ mMyPostshower.Add( post ); }
        
        void SetSmdFit(const StGammaFitterResult& fit, Int_t plane=0) { mSmdFit[plane] = fit; }

    protected:

        Int_t mGammaId;         /// Unique ID assigned to gamma candidate in event
        Int_t mTowerId;         /// Seed tower ID (Eemc=index, Bemc=softid)
        Int_t mTowerClusterId;  /// Unique ID of tower cluster
        Int_t mSmduClusterId;   /// Unique ID of ESMD U (BSMD Eta) cluster
        Int_t mSmdvClusterId;   /// Unique ID of ESMD V (BSMD Phi) cluster
        
        UChar_t mDetector;      /// 0=EEMC 1=BEMC

        TVector3 mMomentum;     /// Momentum of the gamma candidate
        TVector3 mPosition;     /// Position of the gamma at z (r) = SMD
        
        Float_t mEnergy;        /// Energy of the gamma candidate
        
        Float_t mSeedEnergy;    /// Energy of the seed tower
        Float_t mPre1Energy;    /// Energy deposited in epre1 (or bprs)
        Float_t mPre2Energy;    /// Energy deposited in epre2 (possibly tof?)
        Float_t mPostEnergy;    /// Energy deposited in epost
        Float_t mSmduEnergy;    /// Energy deposited in esmdu (or bsmd eta)
        Float_t mSmdvEnergy;    /// Energy deposited in emsdv (or bsmd phi)
        
        StGammaFitterResult mSmdFit[2];


    private:

        TRefArray mTracks;      // all tracks w/in radius
        TRefArray mTowers;      // all towers w/in radius
        TRefArray mPreshower1;  // ditto
        TRefArray mPreshower2;  // ditto
        TRefArray mPostshower;  // ditto
        TRefArray mSmdu;        // all smdu (eta) strips w/in some window
        TRefArray mSmdv;        // all smdv (phi) strips w/in some window
        
        TRefArray mMyTracks;    // tracks which project to the gamma candidate
        TRefArray mMyTowers;    // towers which belong to the gamma candidate
        TRefArray mMyPreshower1; 
        TRefArray mMyPreshower2;
        TRefArray mMyPostshower;


  ClassDef(StGammaCandidate, 1);

};

typedef std::vector<StGammaCandidate> StGammaCandidateVec_t;

#endif

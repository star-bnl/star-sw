#ifndef __StGammaCandidate_h__
#define __StGammaCandidate_h__

#include "TObject.h"
#include "TVector3.h"
#include "TRefArray.h"

#include <vector>

//#include "StGammaIsolation.h"
//#include "StGammaDistribution.h"
//#include "StGammaFit.h"

#include "StGammaTower.h"
#include "StGammaStrip.h"
#include "StGammaTrack.h"

class StEEmcCluster;
class StEEmcSmdCluster;
class StEmcCluster;



class StGammaCandidate : public TObject
{
 public:
  
  StGammaCandidate();
  ~StGammaCandidate();


  //
  // Functions to access raw data and return (simple) isolation sums
  //
  Float_t sumPt( Float_t radius );
  Float_t sumTrackPt( Float_t radius );
  Float_t sumTowerPt( Float_t radius );
  Float_t sumPreshower1( Float_t radius );
  Float_t sumPreshower2( Float_t radius );
  Float_t sumPostshower( Float_t radius );

  Int_t numberOfTracks( Float_t radius, Float_t minpt = 0. );
  Int_t numberOfTowers( Float_t radius, Float_t minpt = 0. );
  Int_t numberOfPreshower1( Float_t radius, Float_t threshold = 0. );
  Int_t numberOfPreshower2( Float_t radius, Float_t threshold = 0. );
  Int_t numberOfPostshower( Float_t radius, Float_t threshold = 0. );
  
  
  //                                   Association of candidate with clusters
  //                                     Author EEMC:
  //                                     Author BEMC:
 protected:

  Int_t mGammaId;         /// unique ID assigned to gamma candidate in event
  Int_t mTowerId;         /// seed tower ID (eemc=index, bemc=softid)
  Int_t mTowerClusterId;  /// unique ID of tower cluster
  Int_t mSmduClusterId;   /// unique ID of esmdu (bsmd eta) cluster
  Int_t mSmdvClusterId;   /// unique ID of esmdv (bsmd phi) cluster

  UChar_t mDetector;      /// 0=EEMC 1=BEMC

 public:
  void SetId(Int_t id){ mGammaId=id; }
  void SetTowerId( Int_t id ){ mTowerId=id; }
  void SetTowerClusterId( Int_t id ){ mTowerClusterId=id; }
  void SetSmduClusterId( Int_t id ){ mSmduClusterId=id; }
  void SetSmdvClusterId( Int_t id ){ mSmdvClusterId=id; }

  void SetSmdEtaClusterId( Int_t id ){ mSmduClusterId=id; }
  void SetSmdPhiClusterId( Int_t id ){ mSmdvClusterId=id; }


  void SetDetectorId( Int_t id ){ mDetector=id; }
  enum CalorimeterId { kEEmc=0, kBEmc, kUnknown }; 

  Int_t detectorId(){ return mDetector; }
  Int_t id(){ return mGammaId; }




  //                                   Kinematics of the candidate
  //                                     Author EEMC:
  //                                     Author BEMC:
 protected:
  TVector3 mMomentum;     /// Momentum of the gamma candidate
  TVector3 mPosition;     /// Position of the gamma at z (r) = SMD

  Float_t mEnergy;        /// Energy of the gamma candidate

  Float_t mSeedEnergy;    /// Energy of the seed tower
  Float_t mPre1Energy;    /// Energy deposited in epre1 (or bprs)
  Float_t mPre2Energy;    /// Energy deposited in epre2 (possibly tof?)
  Float_t mPostEnergy;    /// Energy deposited in epost
  Float_t mSmduEnergy;    /// Energy deposited in esmdu (or bsmd eta)
  Float_t mSmdvEnergy;    /// Energy deposited in emsdv (or bsmd phi)

 public:
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

  TVector3 momentum(){ return mMomentum; }
  TVector3 position(){ return mPosition; }



  
  // Tower and track information w/in given radius
 
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

  
 public:

  void addTrack ( StGammaTrack *track ){ mTracks.Add( track ); }
  void addTower ( StGammaTower *tower ){ mTowers.Add( tower ); }
  void addPreshower1( StGammaTower *pre1 ){ mPreshower1.Add( pre1 ); }
  void addPreshower2( StGammaTower *pre2 ){ mPreshower2.Add( pre2 ); }
  void addPostshower( StGammaTower *post ){ mPostshower.Add( post ); }
  void addSmdu( StGammaStrip *strip ){ mSmdu.Add(strip); }
  void addSmdv( StGammaStrip *strip ){ mSmdv.Add(strip); }
  void addSmdEta( StGammaStrip *strip ){ mSmdu.Add(strip); }
  void addSmdPhi( StGammaStrip *strip ){ mSmdv.Add(strip); }

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
  
  void addMyTrack ( StGammaTrack *track ){ mMyTracks.Add( track ); }
  void addMyTower ( StGammaTower *tower ){ mMyTowers.Add( tower ); }
  void addMyPreshower1( StGammaTower *pre1 ){ mMyPreshower1.Add( pre1 ); }
  void addMyPreshower2( StGammaTower *pre2 ){ mMyPreshower2.Add( pre2 ); }
  void addMyPostshower( StGammaTower *post ){ mMyPostshower.Add( post ); }
  
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


  // to add a new tower, do the following:

  // StGammaTower tower = candidate.AddTower();
  // tower.id     = <tower id>;
  // tower.ET     = <tower ET>;
  // tower.eta    = <tower eta>;
  // tower.phi    = <tower phi>;
  // tower.status = <tower status>;
  // tower.fail   = <tower fail>;

  ClassDef(StGammaCandidate,1);

};

typedef std::vector<StGammaCandidate> StGammaCandidateVec_t;

#endif

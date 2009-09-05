// -*- mode:c++ -*-
//////////////////////////////////////////////////////////////////////
//
// $Id: StJet.h,v 1.3 2009/09/05 22:15:16 pibero Exp $
// $Log: StJet.h,v $
// Revision 1.3  2009/09/05 22:15:16  pibero
// Add tracks and towers references to jet
//
// Revision 1.2  2009/09/05 18:18:05  pibero
// Add utility functions for trigger patches
//
// Revision 1.1  2008/06/01 03:41:45  tai
// moved StJet, StJets, and TrackToJetIndex to StSpinPool/StJets
//
// Revision 1.4  2008/03/27 02:25:03  tai
// moved the definitions of the construcors from .h to .cxx
// set jetEt and 3 other variables in a constructor
//
// Revision 1.3  2007/06/05 21:56:11  kocolosk
// added data members for zVertex and geometric trigger associations, plus methods for detEta (barrel only)
//
// Revision 1.2  2005/01/27 18:39:03  mmiller
// Added some extra accessors to StJet object to keep track of Et from TPC, BTOW, ETOW, etc.
//
// Revision 1.1  2004/07/08 15:41:03  mmiller
// First release of StJetMaker.  Mike's commit of full clean of StJetFinder, StJetMaker, and StSpinMaker.  builds and runs in dev.
//
// Revision 1.7  2003/09/23 19:37:02  thenry
// Fixed et and ez (again) correctly this time.
//
// Revision 1.6  2003/09/22 22:09:28  thenry
// Fixed formula for Et and Ez.
//
// Revision 1.5  2003/09/02 17:59:01  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.4  2003/07/17 23:47:11  akio
// bug fix. Thanks Dylan
//
// Revision 1.3  2002/12/04 20:28:07  thenry
// StppuDstMaker was modified to allow multiple jet analysis modules to be
// run simultaneosly with various parameters while the Maker loads the events
// and analyses them.  Four different jet analyzers exist:
//
// Konstanin's Analyzers:
//     Kt type: StppKonstKtJetAnalyzer
//     Cone type: StppKonstConeJetAnalyzer
//
// Mike's Analyzers:
//     Kt type: StppMikeKtJetAnalyzer
//     Cone type: StppMikeConeJetAnalyzer
//
// These modules all require the StJetFinder modules.
//
// Revision 1.2  2002/06/24 13:22:59  akio
// numerous bug fix & updates
//
// Revision 1.1  2002/02/11 20:30:48  akio
// Many updates, including very first version of jet finder.
//
//
// Revision 1.0  2002/01/25 Akio Ogawa
// First Version of StJet 
//
//////////////////////////////////////////////////////////////////////
//
// StJet
//
// Event class for a Jet
//
//////////////////////////////////////////////////////////////////////
#ifndef StJet_h
#define StJet_h

class TrackToJetIndex;
class TowerToJetIndex;

#include <cmath>
#include <vector>

#include "TLorentzVector.h"
#include "TRefArray.h"

/*!
  \class StJet
  \author Ako Ogowa (BNL)
  StJet is a simple class for persistent storage of the properties of a single jet.
  The 4-momentum is handled via the inheritance from TLorentzVector, and jet-specific
  properties are added as public members.
*/
class StJet : public TLorentzVector {

 public:
  StJet();
  StJet(double lE, double lpx, double lpy, double lpz, Int_t size, int c);
    
  ///The number of 4-vectors contributing to this jet
  Int_t nCell;
    
  ///The summed coulomb charge of the tracks in this jet
  int charge;

  ///The number of tracks in this jet
  int nTracks;

  ///The number of Barrel towers in this jet
  int nBtowers;

  ///The number of Endcap towers in this jet
  int nEtowers;

  ///The summed Et from tracks
  float tpcEtSum;

  ///The summed Et from Barrel towers
  float btowEtSum;

  ///The summed Et from Endcap towers
  float etowEtSum;

  ///Et (stored for convenience when drawing TTree)
  float jetEt;

  ///Pt (stored for convenience when drawing TTree)
  float jetPt;

  ///Eta (stored for convenience when drawing TTree)
  float jetEta;

  ///Phi (stored for convenience when drawing TTree)
  float jetPhi;
    
  ///position of vertex used to reconstruct jet
  float zVertex;
    
  Float_t      et() const {return E()*sqrt(1.0-tanh(Eta())*tanh(Eta()));}
  Float_t      ez() const {return E()*fabs(tanh(Eta()));}

  float neutralFraction() const { return (btowEtSum+etowEtSum)/(btowEtSum+etowEtSum+tpcEtSum); }
  float chargedFraction() const { return 1-neutralFraction(); }
  float rt() const { return neutralFraction(); }
  TrackToJetIndex* leadingChargedParticle() const;
    
  //default radius is in between BSMD radii
  Float_t      detEta(float vz, float radius = 231.72) const;
  Float_t      detEta() const;
    
  //methods to record jet pointing at trigger tower/patch
  void addGeomTrigger(int trigId);
  bool geomTrigger(int trigId) const;
  std::vector<int>& geomTriggers() { return mGeomTriggers; }

  // Distance in eta-phi plane between this jet and the argument
  double deltaPhi(const StJet* jet) const { return Vect().DeltaPhi(jet->Vect()); }
  double deltaR(const StJet* jet) const { return Vect().DeltaR(jet->Vect()); }

  // Utility functions to get jet patch eta and phi from jet patch id and vice-versa
  static bool getJetPatchEtaPhi(int id, float& eta, float& phi);
  static bool getJetPatchId(float eta, float phi, int& id);

  // Tracks and towers associated with this jet
  int numberOfTracks() const { return mTracks.GetEntriesFast(); }
  int numberOfTowers() const { return mTowers.GetEntriesFast(); }

  TrackToJetIndex* track(int i) const { return (TrackToJetIndex*)mTracks.At(i); }
  TowerToJetIndex* tower(int i) const { return (TowerToJetIndex*)mTowers.At(i); }

  TRefArray& tracks() { return mTracks; }
  TRefArray& towers() { return mTowers; }

  void addTrack(const TrackToJetIndex* track) { mTracks.Add((TObject*)track); }
  void addTower(const TowerToJetIndex* tower) { mTowers.Add((TObject*)tower); }
    
 private:
  std::vector<int> mGeomTriggers;

  TRefArray mTracks;
  TRefArray mTowers;

  ClassDef(StJet,8);
};

#endif

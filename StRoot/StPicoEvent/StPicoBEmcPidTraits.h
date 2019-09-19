/**
 * \class StPicoBEmcPidTraits
 * \brief
 *
 * Keep information about Barrel ElectroMagnetic Calorimeter (BEMC)
 * matched tracks.
 */

#ifndef StPicoBEmcPidTraits_h
#define StPicoBEmcPidTraits_h

#include <limits>

// ROOT headers
#include "TObject.h"

//_________________
class StPicoBEmcPidTraits: public TObject {

 public:
  /// Default constructor
  StPicoBEmcPidTraits();
  /// Constructor that fills the parameters accordingly to the input
  StPicoBEmcPidTraits(Int_t index, Int_t id, Int_t adc0, const Float_t* e,
		      const Float_t* dist, const Int_t* nhit, const Int_t* ntow);
  /// Copy constructor
  StPicoBEmcPidTraits(const StPicoBEmcPidTraits &traits);
  /// Destructor
  virtual ~StPicoBEmcPidTraits();
  /// Print BEMC PID traits information
  virtual void Print(const Char_t* option = "") const;

  //
  // Getters
  //
  
  /// Return track index
  Int_t   trackIndex() const     { return (Int_t)mTrackIndex; }
  /// Associated BEMC cluster id (STAR standard clustering algorithm)
  Int_t   bemcId() const         { return (Int_t)mBemcId; }
  /// Associated bemc cluster highest tower adc (STAR standard clustering algorithm)
  Int_t   bemcAdc0() const       { return (Int_t)mBemcAdc0; }
  /// Associated bemc cluster highest tower energy (STAR standard clustering algorithm)
  Float_t bemcE0() const         { return (Float_t)mBemcE0 / 1000.; }
  /// Associated bemc cluster energy (STAR standard clustering algorithm)
  Float_t bemcE() const          { return (Float_t)mBemcE / 1000.; }
  /// Associated bemc cluster Z-distance (cm) (STAR standard clustering algorithm)
  Float_t bemcZDist() const      { return (Float_t)mBemcZDist / 100.; }
  /// Associated bemc cluster phi-distance (cm) (STAR standard clustering algorithm)
  Float_t bemcPhiDist() const    { return (Float_t)mBemcPhiDist / 10000.; }
  /// Associated bemc cluster number of fired SMD-eta wires (STAR standard clustering algorithm)
  Int_t   bemcSmdNEta() const    { return (Int_t)mBemcSmdNEta;}
  /// Associated bemc cluster number of fired SMD-phi wires (STAR standard clustering algorithm)
  Int_t   bemcSmdNPhi() const    { return (Int_t)mBemcSmdNPhi; }

  /// Track matched tower id (using StEmcPosition::projTrack())
  Int_t   btowId() const         { return (Int_t)mBtowId; }
  /// Track second closest tower local id
  Int_t   btowId2() const
  { return ( ( (Int_t)mBtowId23 / 10 ) == 9 ? -1 : (Int_t)mBtowId23 / 10 ); }
  /// Track third closest tower local id
  Int_t   btowId3() const
  { return ( ( (Int_t)mBtowId23 % 10 ) == 9 ? -1 : (Int_t)mBtowId23 % 10 ); }
  /// Matched tower energy
  Float_t btowE() const          { return (Float_t)mBtowE / 1000.; }
  /// Energy of second closest tower
  Float_t btowE2() const         { return (Float_t)mBtowE2 / 1000.; }
  /// Energy of third closest tower
  Float_t btowE3() const         { return (Float_t)mBtowE3 / 1000.; }
  /// Eta distance to matched tower (cm)
  Float_t btowEtaDist() const    { return (Float_t)mBtowEtaDist / 10000.; }
  /// Phi distance to matched tower (cm)
  Float_t btowPhiDist() const    { return (Float_t)mBtowPhiDist / 10000.; }

  //
  // Setters
  //
  
  /// Set track index of the assiciated track
  void setTrackIndex(Int_t idx)  { mTrackIndex = (idx > std::numeric_limits<short>::max()) ? -1 : (Short_t)idx; }
  /// Set BEMC ID
  void setBEmcId(Int_t id)       { mBemcId = (id > std::numeric_limits<short>::max()) ? -1 : (Short_t)id; }
  /// Set ADC
  void setAdc0(Int_t adc0)
  { mBemcAdc0 = (adc0 > std::numeric_limits<unsigned short>::max()) ?
      std::numeric_limits<unsigned short>::max() : (UShort_t)adc0; }
  /// Set energy
  void setEnergy(Float_t energy[5]);
  /// Set distances
  void setDistances(Float_t dist[4]);
  /// Set number of hits
  void setNHits(Int_t nhit[2]);
  /// Set IDs of the towers
  void setNTOW(Int_t ntow[3]);

 private:

  /// Index to the associated track in the event
  Short_t  mTrackIndex;       
  
  /// Next variables are extracted from the standard BEMC cluster algorithm
  /// Index in bemcPoint array
  Short_t  mBemcId;
  /// adc0 is the higest adc in the cluster
  Short_t  mBemcAdc0;
  /// E0*1000 highest tower in the cluster
  Short_t  mBemcE0;
  /// EMC point E*1000
  Short_t  mBemcE;
  /// z*100
  Short_t  mBemcZDist;
  /// phi*10000
  Short_t  mBemcPhiDist;
  /// # of hits in eta
  UChar_t  mBemcSmdNEta;
  /// # of hits in phi
  UChar_t  mBemcSmdNPhi;

  /// Next variables are purely from single tower or nearby towers
  /// Projected tower Id 1-4800
  Short_t  mBtowId;
  /// The 2nd and 3rd closest tower local id  ( 2nd X 10 + 3rd), each id 0-8
  Char_t   mBtowId23;
  /// E1*1000 of matched (closest) tower E
  Short_t  mBtowE;
  /// E2*1000 of the 2nd closest tower E
  Short_t  mBtowE2;
  /// E3*1000 of the 3rd closest tower E
  Short_t  mBtowE3;
  /// eta*10000 distance between track and matched tower center
  Short_t  mBtowEtaDist;
  /// phi*10000 distance between track and matched tower center
  Short_t  mBtowPhiDist;

  ClassDef(StPicoBEmcPidTraits, 1);
};

#endif

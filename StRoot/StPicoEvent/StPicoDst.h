#ifndef StPicoDst_h
#define StPicoDst_h

/// ROOT headers
#include "TClonesArray.h"

/// PicoDst headers
#include "StPicoArrays.h"

/// Forward declarations
class StPicoEvent;
class StPicoTrack;
class StPicoEmcTrigger;
class StPicoMtdTrigger;
class StPicoBTowHit;
class StPicoBTofHit;
class StPicoMtdHit;
class StPicoEpdHit;
class StPicoBbcHit;
class StPicoFmsHit;
class StPicoBEmcPidTraits;
class StPicoBTofPidTraits;
class StPicoMtdPidTraits;
class StPicoTrackCovMatrix;

//_________________
class StPicoDst {

 public:

  /// Default constructor
  StPicoDst() { /* emtpy */}
  /// Destructor
  ~StPicoDst() { /* empty*/ }

  /// Set the pointers to the TClonesArrays
  static void set(TClonesArray**);
  /// Reset the pointers to the TClonesArrays to 0
  static void unset();
  /// Return pointer to the n-th TClonesArray
  static TClonesArray* picoArray(Int_t type) { return picoArrays[type]; }

  /// Return pointer to current StPicoEvent (class holding the event wise information)
  static StPicoEvent* event() { return (StPicoEvent*)picoArrays[StPicoArrays::Event]->UncheckedAt(0); }
  /// Return pointer to i-th track
  static StPicoTrack* track(Int_t i) { return (StPicoTrack*)picoArrays[StPicoArrays::Track]->UncheckedAt(i); }
  /// Return pointer to i-th trigger data
  static StPicoEmcTrigger* emcTrigger(Int_t i) { return (StPicoEmcTrigger*)picoArrays[StPicoArrays::EmcTrigger]->UncheckedAt(i); }
  /// Return pointer to i-th MTD trigger data
  static StPicoMtdTrigger* mtdTrigger(Int_t i) { return (StPicoMtdTrigger*)picoArrays[StPicoArrays::MtdTrigger]->UncheckedAt(i); }
  /// Return pointer to i-th btow hit
  static StPicoBTowHit* btowHit(Int_t i) { return (StPicoBTowHit*)picoArrays[StPicoArrays::BTowHit]->UncheckedAt(i); }
  /// Return pointer to i-th btof hit
  static StPicoBTofHit* btofHit(Int_t i) { return (StPicoBTofHit*)picoArrays[StPicoArrays::BTofHit]->UncheckedAt(i); }
  /// Return pointer to i-th mtd hit
  static StPicoMtdHit*  mtdHit(Int_t i) { return (StPicoMtdHit*)picoArrays[StPicoArrays::MtdHit]->UncheckedAt(i); }
  /// Return pointer to i-th bbc hit
  static StPicoBbcHit* bbcHit(Int_t i) { return (StPicoBbcHit*)picoArrays[StPicoArrays::BbcHit]->UncheckedAt(i); }
  /// Return pointer to i-th epd hit
  static StPicoEpdHit* epdHit(Int_t i) { return (StPicoEpdHit*)picoArrays[StPicoArrays::EpdHit]->UncheckedAt(i); }
  /// Return pointer to i-th fms hit
  static StPicoFmsHit*  fmsHit(Int_t i) { return (StPicoFmsHit*)picoArrays[StPicoArrays::FmsHit]->UncheckedAt(i); }
  /// Return pointer to i-th emc pidTraits
  static StPicoBEmcPidTraits* bemcPidTraits(Int_t i) { return (StPicoBEmcPidTraits*)picoArrays[StPicoArrays::BEmcPidTraits]->UncheckedAt(i); }
  /// Return pointer to i-th btof pidTraits
  static StPicoBTofPidTraits* btofPidTraits(Int_t i) { return (StPicoBTofPidTraits*)picoArrays[StPicoArrays::BTofPidTraits]->UncheckedAt(i); }
  /// Return pointer to i-th mtd pidTraits
  static StPicoMtdPidTraits* mtdPidTraits(Int_t i) { return (StPicoMtdPidTraits*)picoArrays[StPicoArrays::MtdPidTraits]->UncheckedAt(i); }
  /// Return pointer to i-th track covariance matrix
  static StPicoTrackCovMatrix* trackCovMatrix(Int_t i) { return (StPicoTrackCovMatrix*)picoArrays[StPicoArrays::TrackCovMatrix]->UncheckedAt(i); }

  /// Return number of entries in the pico arrays
  static UInt_t numberOfTracks() { return picoArrays[StPicoArrays::Track]->GetEntries(); }
  static UInt_t numberOfEmcTriggers() { return picoArrays[StPicoArrays::EmcTrigger]->GetEntries(); }
  static UInt_t numberOfMtdTriggers() { return picoArrays[StPicoArrays::MtdTrigger]->GetEntries(); }
  static UInt_t numberOfBTowHits() { return picoArrays[StPicoArrays::BTowHit]->GetEntries(); }
  static UInt_t numberOfBTofHits() { return picoArrays[StPicoArrays::BTofHit]->GetEntries(); }
  static UInt_t numberOfMtdHits() { return picoArrays[StPicoArrays::MtdHit]->GetEntries(); }
  static UInt_t numberOfBbcHits() { return picoArrays[StPicoArrays::BbcHit]->GetEntries(); }
  static UInt_t numberOfEpdHits() { return picoArrays[StPicoArrays::EpdHit]->GetEntries(); }
  static UInt_t numberOfFmsHits() { return picoArrays[StPicoArrays::FmsHit]->GetEntries(); }
  static UInt_t numberOfBEmcPidTraits() { return picoArrays[StPicoArrays::BEmcPidTraits] ->GetEntries(); }
  static UInt_t numberOfBTofPidTraits() { return picoArrays[StPicoArrays::BTofPidTraits]->GetEntries(); }
  static UInt_t numberOfMtdPidTraits() { return picoArrays[StPicoArrays::MtdPidTraits]->GetEntries(); }
  static UInt_t numberOfTrackCovMatrices() { return picoArrays[StPicoArrays::TrackCovMatrix]->GetEntries(); }

  /// Print information
  void print() const;
  static void printTracks();
  static void printTriggers();
  static void printBTowHits();
  static void printBTofHits();
  static void printMtdHits();
  static void printFmsHits();
  static void printBEmcPidTraits();
  static void printBTofPidTraits();
  static void printMtdPidTraits();
  static void printTrackCovMatrices();

 private:

  /// Array of TClonesArrays
  static TClonesArray** picoArrays;
};

#endif

#ifndef StPicoDst_h
#define StPicoDst_h


class StPicoDstMaker;
class StPicoEvent;
class StPicoTrack;
class StPicoEmcTrigger;
class StPicoMtdTrigger;
class StPicoBTowHit;
class StPicoBTofHit;
class StPicoMtdHit;
class StPicoBEmcPidTraits;
class StPicoBTofPidTraits;
class StPicoMtdPidTraits;

#include "TClonesArray.h"
#include "StPicoDstMaker/StPicoArrays.h"

class StPicoDst
{
public:
  StPicoDst() {}
  ~StPicoDst() {}

  /// set the pointers to the TClonesArrays
  static void set(TClonesArray**);
  /// resets the pointers to the TClonesArrays to 0
  static void unset();

protected:
  /// array of TClonesArrays
  static TClonesArray** picoArrays;

public:
  /// returns pointer to the n-th TClonesArray
  static TClonesArray* picoArray(int type) { return picoArrays[type]; }

  /// returns pointer to current StPicoEvent (class holding the event wise information)
  static StPicoEvent* event() { return (StPicoEvent*)picoArrays[StPicoArrays::Event]->UncheckedAt(0); }
  /// return pointer to i-th track
  static StPicoTrack* track(int i) { return (StPicoTrack*)picoArrays[StPicoArrays::Track]->UncheckedAt(i); }
  /// return pointer to i-th trigger data
  static StPicoEmcTrigger* emcTrigger(int i) { return (StPicoEmcTrigger*)picoArrays[StPicoArrays::EmcTrigger]->UncheckedAt(i); }
  static StPicoMtdTrigger* mtdTrigger(int i) { return (StPicoMtdTrigger*)picoArrays[StPicoArrays::MtdTrigger]->UncheckedAt(i); }

  /// return pointer to i-th btow hit
  static StPicoBTowHit* btowHit(int i) { return (StPicoBTowHit*)picoArrays[StPicoArrays::BTowHit]->UncheckedAt(i); }
  /// return pointer to i-th btof hit
  static StPicoBTofHit* btofHit(int i) { return (StPicoBTofHit*)picoArrays[StPicoArrays::BTofHit]->UncheckedAt(i); }
  /// return pointer to i-th mtd hit
  static StPicoMtdHit*  mtdHit(int i) { return (StPicoMtdHit*)picoArrays[StPicoArrays::MtdHit]->UncheckedAt(i); }

  /// return pointer to i-th emc pidTraits
  static StPicoBEmcPidTraits* bemcPidTraits(int i) { return (StPicoBEmcPidTraits*)picoArrays[StPicoArrays::BEmcPidTraits]->UncheckedAt(i); }
  /// return pointer to i-th btof pidTraits
  static StPicoBTofPidTraits* btofPidTraits(int i) { return (StPicoBTofPidTraits*)picoArrays[StPicoArrays::BTofPidTraits]->UncheckedAt(i); }
  /// return pointer to i-th mtd pidTraits
  static StPicoMtdPidTraits* mtdPidTraits(int i) { return (StPicoMtdPidTraits*)picoArrays[StPicoArrays::MtdPidTraits]->UncheckedAt(i); }

  static unsigned int numberOfTracks() { return picoArrays[StPicoArrays::Track]->GetEntries(); }
  static unsigned int numberOfEmcTriggers() { return picoArrays[StPicoArrays::EmcTrigger]->GetEntries(); }
  static unsigned int numberOfMtdTriggers() { return picoArrays[StPicoArrays::MtdTrigger]->GetEntries(); }
  static unsigned int numberOfBTOWHits() { return picoArrays[StPicoArrays::BTowHit]->GetEntries(); }
  static unsigned int numberOfBTofHits() { return picoArrays[StPicoArrays::BTofHit]->GetEntries(); }
  static unsigned int numberOfMtdHits() { return picoArrays[StPicoArrays::MtdHit]->GetEntries(); }
  static unsigned int numberOfBEmcPidTraits() { return picoArrays[StPicoArrays::BEmcPidTraits] ->GetEntries(); }
  static unsigned int numberOfBTofPidTraits() { return picoArrays[StPicoArrays::BTofPidTraits]->GetEntries(); }
  static unsigned int numberOfMtdPidTraits() { return picoArrays[StPicoArrays::MtdPidTraits] ->GetEntries(); }

  void print() const; ///< Print basic event info
  static void printTracks();
  static void printTriggers();
  static void printBTOWHits();
  static void printBTofHits();
  static void printMtdHits();
  static void printBEmcPidTraits();
  static void printBTofPidTraits();
  static void printMtdPidTraits();
};

#endif

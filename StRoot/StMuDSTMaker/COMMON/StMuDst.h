/***************************************************************************
 *
 * $Id: StMuDst.h,v 1.2 2002/03/08 20:04:31 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#ifndef StMuDst_h
#define StMuDst_h

#include "TObject.h"
#include "TClonesArray.h"
#include "StMuDstMaker.h"

class StMuDstMaker;
class StMuEvent;
class StMuTrack;
class StRichSpectra;
class StDetectorState;
class StL3AlgorithmInfo;

class StStrangeEvMuDst;
class StV0MuDst;
class StXiMuDst;
class StKinkMuDst;
class StV0Mc;
class StXiMc;
class StKinkMc;

class StEvent;
class StGlobalTrack;
class StPrimaryTrack;
class StStrangeEvMuDst;


class StV0MuDst;
class StXiMuDst;
class StKinkMuDst;

#include "TObject.h"
#include "StMuDstMaker.h"
#include "StMuArrays.h"




#define ARRAY(NAME)  static TClonesArray* (NAME)##s() { return tca_##NAME##s;}
#define OBJECT(TYPE,FUNC) static TYPE FUNC##(unsigned int i=0) { if (FUNC##s() && (i<(unsigned int)FUNC##s()->GetEntries()) ) return (##TYPE##)FUNC##s()->UncheckedAt(i); return 0;}

#define DO(TYPE,NAME) ARRAY(NAME)    OBJECT(TYPE,NAME)

class StMuDst : public TObject {
public:
  StMuDst();
  void set(StMuDstMaker* maker);
  void unset();
  void fixTrackIndices();
  StEvent* createStEvent();
  StPrimaryTrack* createStPrimaryTrack(StMuTrack*);
  StGlobalTrack* createStGlobalTrack(StMuTrack*);

 private:
  static TClonesArray* arrays[__NARRAYS__];
  static TClonesArray* strangeArrays[__NSTRANGEARRAYS__];

public:
  static TClonesArray* StMuDst::array(int type) { return arrays[type]; }
  static TClonesArray* StMuDst::strangeArray(int type) { return strangeArrays[type]; }

  static TClonesArray* primaryTracks() { return arrays[muPrimary]; }
  static TClonesArray* globalTracks() { return arrays[muGlobal]; }
  static TClonesArray* otherTracks() { return arrays[muOther]; }
  static TClonesArray* l3Tracks() { return arrays[muL3]; }
  static TClonesArray* richSpectra() { return arrays[muRich]; }
  static TClonesArray* detectorStates() { return arrays[muState]; }
  static TClonesArray* l3AlgoAccept() { return arrays[muAccept]; }
  static TClonesArray* l3AlgoReject() { return arrays[muReject]; }

  static StMuEvent* event() { return (StMuEvent*)arrays[muEvent]->UncheckedAt(0); }
  static StMuTrack* primaryTracks(int i) { return (StMuTrack*)arrays[muPrimary]->UncheckedAt(i); }
  static StMuTrack* globalTracks(int i) { return (StMuTrack*)arrays[muGlobal]->UncheckedAt(i); }
  static StMuTrack* otherTracks(int i) { return (StMuTrack*)arrays[muOther]->UncheckedAt(i); }
  static StMuTrack* l3Tracks(int i) { return (StMuTrack*)arrays[muL3]->UncheckedAt(i); }
  static StRichSpectra* richSpectra(int i) { return (StRichSpectra*)arrays[muRich]->UncheckedAt(i); }
  static StDetectorState* detectorStates(int i) { return (StDetectorState*)arrays[muState]->UncheckedAt(i); }
  static StL3AlgorithmInfo* l3AlgoAccept(int i) { return (StL3AlgorithmInfo*)arrays[muAccept]->UncheckedAt(i); }
  static StL3AlgorithmInfo* l3AlgoReject(int i) { return (StL3AlgorithmInfo*)arrays[muReject]->UncheckedAt(i); }

  static TClonesArray* v0s() { return strangeArrays[smuV0]; }
  static StV0MuDst* v0s(int i) { return (StV0MuDst*)strangeArrays[smuV0]->UncheckedAt(i); }

    ClassDef(StMuDst,1)
};

#endif

/***************************************************************************
 *
 * $Log: StMuDst.h,v $
 * Revision 1.2  2002/03/08 20:04:31  laue
 * change from two trees to 1 tree per file
 *
 * Revision 1.1  2002/03/08 17:04:17  laue
 * initial revision
 *
 *
 **************************************************************************/

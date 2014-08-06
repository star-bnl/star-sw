/***************************************************************************
 *
 * $Id: StMuDst.h,v 1.6 2002/05/20 17:23:31 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#ifndef StMuDst_h
#define StMuDst_h

#include "TObject.h"
#include "TClonesArray.h"

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
class TCut;

class StEvent;
class StTrack;
class StTrackGeometry;

class StPhysicalHelixD;

#include "TObject.h"
#include "StMuArrays.h"




#define ARRAY(NAME)  static TClonesArray* (NAME)##s() { return tca_##NAME##s;}
#define OBJECT(TYPE,FUNC) static TYPE FUNC##(unsigned int i=0) { if (FUNC##s() && (i<(unsigned int)FUNC##s()->GetEntries()) ) return (##TYPE##)FUNC##s()->UncheckedAt(i); return 0;}

#define DO(TYPE,NAME) ARRAY(NAME)    OBJECT(TYPE,NAME)

class StMuDst : public TObject {
public:
  StMuDst();
  void set(StMuDstMaker* maker);
  void set(TClonesArray**, TClonesArray**);
  void unset();
  void fixTrackIndices();
  StEvent* createStEvent();
  StTrackGeometry* trackGeometry(int q, StPhysicalHelixD* h);
  StTrack* createStTrack(StMuTrack*);

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

  static StStrangeEvMuDst* strangeEvent() { return (StStrangeEvMuDst*)strangeArrays[smuEv]->UncheckedAt(0); }
  static TClonesArray* v0s() { return strangeArrays[smuV0]; }
  static StV0MuDst* v0s(int i) { return (StV0MuDst*)strangeArrays[smuV0]->UncheckedAt(i); }
  static TClonesArray* xis() { return strangeArrays[smuXi]; }
  static StXiMuDst* xis(int i) { return (StXiMuDst*)strangeArrays[smuXi]->UncheckedAt(i); }
  static TClonesArray* kinks() { return strangeArrays[smuKink]; }
  static StKinkMuDst* kinks(int i) { return (StKinkMuDst*)strangeArrays[smuKink]->UncheckedAt(i); }
  static TClonesArray* strangeCuts() { return strangeArrays[smuCut]; }
  static TCut* strangeCuts(int i) { return (TCut*)strangeArrays[smuCut]->UncheckedAt(i); }

    ClassDef(StMuDst,1)
};

#endif

/***************************************************************************
 *
 * $Log: StMuDst.h,v $
 * Revision 1.6  2002/05/20 17:23:31  laue
 * StStrangeCuts added
 *
 * Revision 1.5  2002/04/01 22:42:30  laue
 * improved chain filter options
 *
 * Revision 1.4  2002/03/20 16:04:11  laue
 * minor changes, mostly added access functions
 *
 * Revision 1.3  2002/03/14 04:12:44  laue
 * bug fix: StMuL3EventSummary.cxx
 * update: StMuDst.h StMuDst.cxx
 *
 * Revision 1.2  2002/03/08 20:04:31  laue
 * change from two trees to 1 tree per file
 *
 * Revision 1.1  2002/03/08 17:04:17  laue
 * initial revision
 *
 *
 **************************************************************************/

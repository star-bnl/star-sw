/***************************************************************************
 *
 * $Id: StarMuDst.h,v 1.1 2002/03/05 15:41:08 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#ifndef StarMuDst_h
#define StarMuDst_h

#include "TObject.h"
#include "TClonesArray.h"
#include "StarMuDstMaker.h"

class StarMuDstMaker;
class StarMuEvent;
class StarMuTrack;
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

class StStrangeEvMuDst;

#include "TObject.h"
#include "StarMuDstMaker.h"



#define ARRAY(NAME)  static TClonesArray* (NAME)##s() { return tca_##NAME##s;}
#define OBJECT(TYPE,FUNC) static TYPE FUNC##(unsigned int i=0) { if (FUNC##s() && (i<(unsigned int)FUNC##s()->GetEntries()) ) return (##TYPE##)FUNC##s()->UncheckedAt(i); return 0;}

#define DO(TYPE,NAME) ARRAY(NAME)    OBJECT(TYPE,NAME)

class StarMuDst : public TObject {
public:
  StarMuDst();
  void set(StarMuDstMaker* maker);
  void unset();

 private:
  static TClonesArray* arrays[__NARRAYS__];
  static TClonesArray* strangeArrays[__NSTRANGEARRAYS__];

public:
  static TClonesArray* StarMuDst::array(int type) { return arrays[type]; }
  static TClonesArray* StarMuDst::strangeArray(int type) { return strangeArrays[type]; }

  static TClonesArray* primaryTracks() { return arrays[muPrimary]; }
  static TClonesArray* globalTracks() { return arrays[muGlobal]; }
  static TClonesArray* otherTracks() { return arrays[muOther]; }
  static TClonesArray* l3Tracks() { return arrays[muL3]; }
  static TClonesArray* richSpectra() { return arrays[muRich]; }
  static TClonesArray* detectorStates() { return arrays[muState]; }
  static TClonesArray* l3AlgoAccept() { return arrays[muAccept]; }
  static TClonesArray* l3AlgoReject() { return arrays[muReject]; }

  static StarMuEvent* event() { return (StarMuEvent*)arrays[muEvent]->UncheckedAt(0); }
  static StarMuTrack* primaryTracks(int i) { return (StarMuTrack*)arrays[muPrimary]->UncheckedAt(i); }
  static StarMuTrack* globalTracks(int i) { return (StarMuTrack*)arrays[muGlobal]->UncheckedAt(i); }
  static StarMuTrack* otherTracks(int i) { return (StarMuTrack*)arrays[muOther]->UncheckedAt(i); }
  static StarMuTrack* l3Tracks(int i) { return (StarMuTrack*)arrays[muL3]->UncheckedAt(i); }
  static StRichSpectra* richSpectra(int i) { return (StRichSpectra*)arrays[muRich]->UncheckedAt(i); }
  static StDetectorState* detectorStates(int i) { return (StDetectorState*)arrays[muState]->UncheckedAt(i); }
  static StL3AlgorithmInfo* l3AlgoAccept(int i) { return (StL3AlgorithmInfo*)arrays[muAccept]->UncheckedAt(i); }
  static StL3AlgorithmInfo* l3AlgoReject(int i) { return (StL3AlgorithmInfo*)arrays[muReject]->UncheckedAt(i); }


    ClassDef(StarMuDst,1)
};

#endif

/***************************************************************************
 *
 * $Log: StarMuDst.h,v $
 * Revision 1.1  2002/03/05 15:41:08  jeromel
 * First version of Frank's Commone MicroDST.
 *
 *
 **************************************************************************/

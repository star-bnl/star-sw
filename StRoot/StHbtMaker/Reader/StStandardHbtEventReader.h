/***************************************************************************
 *
 * $Id: StStandardHbtEventReader.h,v 1.21 2001/12/05 14:42:18 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *       This is the HbtEventReader class to be used when running
 *  root4star with StEventReaderMaker.
 *  It inherits from StHbtReaderMaker
 *
 *  Since this StHbtEventReader class gets its input from StEvent in root4star,
 *  it needs to know what chain has the StEventReaderMaker on it.  So you have
 *  to initialize (thru SetTheChain()).
 *  Other StHbtEventReader classes (that might read ASCII files for example)
 *  would need other information, like the filename I guess, and so would
 *  have other private data members that they access.
 *
 ***************************************************************************
 *
 * $Log: StStandardHbtEventReader.h,v $
 * Revision 1.21  2001/12/05 14:42:18  laue
 * updated for trigger(action)word and l3TriggerAlgorithm
 *
 * Revision 1.18  2001/06/04 19:09:54  rcwells
 * Adding B-field, run number, and improved reaction plane functionality
 *
 * Revision 1.17  2001/02/08 22:38:26  laue
 * Reader can now switch between different track types: primary is default
 *
 * Revision 1.16  2000/08/31 22:32:37  laue
 * Readers updated for new StHbtEvent version 3.
 *
 * Revision 1.15  2000/07/16 21:14:45  laue
 * StStandardHbtEventReader modified to read primary tracks only
 *
 * Some unnecessary includes removed.
 * Changes from StV0MiniDst to StStrangeMuDst
 *
 * Revision 1.14  2000/05/25 21:04:30  laue
 * StStandarsHbtEventReader updated for the new StStrangMuDstMaker
 *
 * Revision 1.13  2000/02/18 22:01:56  laue
 * Implementation of a collections of StHbtEventWriters.
 * We now can write multiple microDsts at a time.
 *
 * All readers can have front-loaded cuts now. For that reason some
 * functionality was moved from the specific readers to the base class
 *
 * Revision 1.12  2000/01/25 17:35:27  laue
 * I. In order to run the stand alone version of the StHbtMaker the following
 * changes have been done:
 * a) all ClassDefs and ClassImps have been put into #ifdef __ROOT__ statements
 * b) unnecessary includes of StMaker.h have been removed
 * c) the subdirectory StHbtMaker/doc/Make has been created including everything
 * needed for the stand alone version
 *
 * II. To reduce the amount of compiler warning
 * a) some variables have been type casted
 * b) some destructors have been declared as virtual
 *
 * Revision 1.11  1999/12/03 22:24:37  lisa
 * (1) make Cuts and CorrFctns point back to parent Analysis (as well as other way). (2) Accommodate new PidTraits mechanism
 *
 * Revision 1.10  1999/11/24 22:01:41  laue
 * reader adopted to the new StEvent 2.x
 *
 * Revision 1.9  1999/10/15 01:57:37  lisa
 * Important enhancement of StHbtMaker - implement Franks CutMonitors
 * ----------------------------------------------------------
 * This means 3 new files in Infrastructure area (CutMonitor),
 * several specific CutMonitor classes in the Cut area
 * and a new base class in the Base area (StHbtCutMonitor).
 * This means also changing all Cut Base class header files from .hh to .h
 * so we have access to CutMonitor methods from Cint command line.
 * This last means
 * 1) files which include these header files are slightly modified
 * 2) a side benefit: the TrackCuts and V0Cuts no longer need
 * a SetMass() implementation in each Cut class, which was stupid.
 * Also:
 * -----
 * Include Franks StHbtAssociationReader
 * ** None of these changes should affect any user **
 *
 * Revision 1.8  1999/09/24 01:23:14  fisyak
 * Reduced Include Path
 *
 * Revision 1.7  1999/09/17 22:38:03  lisa
 * first full integration of V0s into StHbt framework
 *
 * Revision 1.6  1999/09/16 18:48:01  lisa
 * replace placeholder HbtV0Track stuff with Helens StHbtV0 classes
 *
 * Revision 1.5  1999/09/09 02:59:55  lisa
 * fix Randys factor of 2 in CoulombCorrection AND add SetCut methods to StStandardHbtEventReader which were forgotten last commit
 *
 * Revision 1.4  1999/09/08 04:15:53  lisa
 * persistent microDST implementation tweaked to please fickle solaris details
 *
 * Revision 1.3  1999/09/03 22:39:17  lisa
 * Readers now MUST have Report() methods and MAY have WriteHbtEvent() methods
 *
 * Revision 1.2  1999/07/06 22:33:24  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 * With the .DEV->DEV revolution of June 1999, must change
 * the way that this thing gets StEvent object
 * no more going through the chain.  Now, we have to have
 * a pointer to the object (of type StEventMaker, which is
 * derived from StMaker) which has a method to get the StEvent.
 **************************************************************************/

#ifndef StStandardHbtEventReader_hh
#define StStandardHbtEventReader_hh

#include "StHbtMaker/Base/StHbtEventReader.hh"
#include "StHbtMaker/Reader/StHbtTagReader.h"


#include "StMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"

#include "StEvent/StEnumerations.h"
#include "StStrangeMuDstMaker/StStrangeMuDstMaker.h"

class StPionPlus;
class StKaonPlus;
class StProton;
class StTpcDedxPidAlgorithm;
class StParticleDefinition;
class StFlowMaker;
class StFlowEvent;
class StFlowAnalysisMaker;
class StFlowSelection;

class StStandardHbtEventReader : public StMaker, public StHbtEventReader{

private:

  StMaker* mTheEventMaker;      //! this is the chain where the StEventReaderMaker is
  StStrangeMuDstMaker* mTheV0Maker; //! this is the chain where the StStrangeMuDstMaker is

  StHbtTagReader* mTheTagReader;  //! this tag reader opens a tags.root file 

  StTrackType mTrackType;
  bool mReadTracks;
  bool mReadV0s;
  bool mReadXis;
  bool mReadKinks;

  StFlowMaker* mFlowMaker;             //!
  StFlowAnalysisMaker* mFlowAnalysisMaker; //!

 protected:

 public:
  StStandardHbtEventReader();
  ~StStandardHbtEventReader();

  StHbtEvent* ReturnHbtEvent();
  StHbtString Report();

  void SetTheEventMaker(StMaker*);
  StMaker* TheEventMaker();
  void SetTheV0Maker(StStrangeMuDstMaker*);
  StStrangeMuDstMaker* TheV0Maker();
  void SetTheTagReader(StHbtTagReader*);
  StHbtTagReader* TheTagReader();

  StTrackType TrackType(); 
  bool ReadTracks();
  bool ReadV0s();
  bool ReadXis();
  bool ReadKinks();
  void SetTrackType(StTrackType);
  void SetReadTracks(bool);
  void SetReadV0s(bool);
  void SetReadXis(bool);
  void SetReadKinks(bool);
  void SetFlowMaker(StFlowMaker* flowMaker);
  void SetFlowAnalysisMaker(StFlowAnalysisMaker* flowAnal);

#ifdef __ROOT__
  ClassDef(StStandardHbtEventReader, 1)
#endif
};

inline void StStandardHbtEventReader::SetTheEventMaker(StMaker* maker){mTheEventMaker=maker;}
inline StMaker* StStandardHbtEventReader::TheEventMaker(){return mTheEventMaker;}
inline void StStandardHbtEventReader::SetTheV0Maker(StStrangeMuDstMaker* maker){mTheV0Maker=maker;}
inline StStrangeMuDstMaker* StStandardHbtEventReader::TheV0Maker(){return mTheV0Maker;}
inline void StStandardHbtEventReader::SetTheTagReader(StHbtTagReader* maker){mTheTagReader=maker;}
inline StHbtTagReader* StStandardHbtEventReader::TheTagReader(){return mTheTagReader;}
inline StTrackType StStandardHbtEventReader::TrackType() { return mTrackType;}
inline bool StStandardHbtEventReader::ReadTracks() { return mReadTracks;}
inline bool StStandardHbtEventReader::ReadV0s() { return mReadV0s;}
inline bool StStandardHbtEventReader::ReadXis() { return mReadXis;}
inline bool StStandardHbtEventReader::ReadKinks() { return mReadKinks;}
inline void StStandardHbtEventReader::SetTrackType(StTrackType t) { mTrackType=t;}
inline void StStandardHbtEventReader::SetReadTracks(bool b) { mReadTracks=b;}
inline void StStandardHbtEventReader::SetReadV0s(bool b) { mReadV0s=b;}
inline void StStandardHbtEventReader::SetReadXis(bool b) { mReadXis=b;}
inline void StStandardHbtEventReader::SetReadKinks(bool b) { mReadKinks=b;}
inline void StStandardHbtEventReader::SetFlowMaker(StFlowMaker* flowMaker){mFlowMaker = flowMaker;}
inline void StStandardHbtEventReader::SetFlowAnalysisMaker(StFlowAnalysisMaker* flowAnal) {
  mFlowAnalysisMaker = flowAnal;
}

#endif


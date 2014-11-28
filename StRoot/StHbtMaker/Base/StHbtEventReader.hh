/***************************************************************************
 *
 * $Id: StHbtEventReader.hh,v 1.13 2003/09/02 17:58:20 perev Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   base class for a StHbtEventReader             
 *   All HbtEventReaders should inherit from this.
 *   Objects of these classes are required
 *   to obtain the data and convert it somehow to the STAR StEvent object
 * 
 * A major change is that on 3sep99, the StHbtReader classes _MUST_ implement
 *   a Report() method, like the Cuts and CorrFctns.
 * Also, a StHbtReader MAY implement a WriteHbtEvent(StHbtEvent*) method.
 *
 ***************************************************************************
 *
 * $Log: StHbtEventReader.hh,v $
 * Revision 1.13  2003/09/02 17:58:20  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.12  2001/09/05 20:41:00  laue
 * Updates of the hbtMuDstTree microDSTs
 *
 * Revision 1.11  2001/06/21 19:06:49  laue
 * Some minor structural changes (forward declarations, etc)
 *
 * Revision 1.10  2001/05/25 23:23:58  lisa
 * Added in StHbtKink stuff
 *
 * Revision 1.9  2001/05/10 21:00:24  laue
 * new member 'mDebug' in order to reduce output
 *
 * Revision 1.8  2000/02/18 21:25:00  laue
 * Implementation of a collections of StHbtEventWriters.
 * We now can write multiple microDsts at a time.
 *
 * Revision 1.7  2000/01/07 23:21:17  laue
 * 0.) all 'ClassDef(...)' put inside #ifdef __ROOT__  #endif
 * 1.) unnecessary includes of 'StMaker.h' deleted
 *
 * Revision 1.6  1999/09/08 04:15:52  lisa
 * persistent microDST implementation tweaked to please fickle solaris details
 *
 * Revision 1.5  1999/09/05 02:58:11  lisa
 * add ASCII microDST reader/writer AND franksParticle cuts
 *
 * Revision 1.4  1999/09/04 04:41:01  lisa
 * StHbtEvent IO   --and--  StHbtEventWriter (microDST) method added to framework
 *
 * Revision 1.3  1999/09/03 22:39:14  lisa
 * Readers now MUST have Report() methods and MAY have WriteHbtEvent() methods
 *
 * Revision 1.2  1999/06/29 17:50:26  fisyak
 * formal changes to account new StEvent, does not complie yet
 *
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtEventReader_hh
#define StHbtEventReader_hh
#include "Stiostream.h"
class StHbtEvent;
class StHbtEventCut;
class StHbtTrackCut;
class StHbtV0Cut;
class StHbtXiCut;
class StHbtKinkCut;

#include "StMaker.h"
#include "StHbtMaker/Infrastructure/StHbtString.hh"

class StHbtEventReader {

protected:
  StHbtEventCut* mEventCut;     //!
  StHbtTrackCut* mTrackCut; //!
  StHbtV0Cut* mV0Cut; //!
  StHbtXiCut* mXiCut; //!
  StHbtKinkCut* mKinkCut; //!
  int mReaderStatus;     // 0="good"
  int mDebug;
public:
  // even tho it's only a base class and never constructed, if you don't have an implementation,
  // you get "StHbtEventReader type_info node" upon dynamical loading
  StHbtEventReader() : mEventCut(0), mTrackCut(0), mV0Cut(0), mXiCut(0), mKinkCut(0), mDebug(1) { /* no-op */ }
  virtual ~StHbtEventReader(){/* no-op */}

  virtual StHbtEvent* ReturnHbtEvent() =0;

  virtual StHbtString Report();    // user-written method to return string describing reader
                                      // Including whatever "early" cuts are being done

  // this next method does NOT need to be implemented, in which case the 
  // "default" method below is executed
  virtual int WriteHbtEvent(StHbtEvent*){cout << "No WriteHbtEvent implemented\n"; return (0);}

  // these next two are optional but would make sense for, e.g., opening and closing a file
  virtual int Init(const char* ReadWrite, StHbtString& Message){cout << "do-nothing StHbtEventReader::Init()\n"; return(0);}
  virtual void Finish(){/*no-op*/};

  int Status(){return mReaderStatus;} // StHbtManager looks at this for guidance if it gets null pointer from ReturnHbtEvent

  virtual void SetEventCut(StHbtEventCut* ecut);
  virtual void SetTrackCut(StHbtTrackCut* pcut);
  virtual void SetV0Cut(StHbtV0Cut* pcut);
  virtual void SetXiCut(StHbtXiCut* pcut);
  virtual void SetKinkCut(StHbtKinkCut* pcut);
  virtual StHbtEventCut* EventCut();
  virtual StHbtTrackCut* TrackCut();
  virtual StHbtV0Cut*    V0Cut();
  virtual StHbtXiCut*    XiCut();
  virtual StHbtKinkCut*    KinkCut();

  /* control of debug informations print out, my rule is: */
  /* 0: no output at all                                  */
  /* 1: once (e.g. in constructor, finsh                  */
  /* 2: once per event                                    */
  /* 3: once per track                                    */
  /* 4: once per pair                                     */
  int Debug(){return mDebug;} 
  void SetDebug(int d){mDebug=d;}

#ifdef __ROOT__
  ClassDef(StHbtEventReader,0)
#endif
};


#endif


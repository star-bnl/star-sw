/***************************************************************************
 *
 * $Id: StStandardHbtEventReader.h,v 1.6 1999/09/16 18:48:01 lisa Exp $
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
//#include "StChain/StChain.h"
#include "StChain/StMaker.h"
#include "StV0MiniDstMaker/StV0MiniDstMaker.h"
#include "StHbtMaker/Base/StHbtEventCut.hh"
#include "StHbtMaker/Base/StHbtParticleCut.hh"

class TOrdCollection;
class StStandardHbtEventReader : public StHbtEventReader{

private:
  StMaker* mTheEventMaker;      //! this is the chain where the StEventReaderMaker is
  StV0MiniDstMaker* mTheV0Maker; //! this is the chain where the StV0MiniDstMaker is
  StHbtEventCut* mEventCut;     //!
  StHbtParticleCut* mParticleCut; //!
  long              mV0;        //! Number of v0s looked at to date

 protected:
 TOrdCollection *mCollection; //!

public:
  StStandardHbtEventReader();
  ~StStandardHbtEventReader();

  virtual StHbtEvent* ReturnHbtEvent();
  virtual StHbtString Report();

  void SetTheEventMaker(StMaker*);
  StMaker* TheEventMaker();
  void SetTheV0Maker(StV0MiniDstMaker*);
  StV0MiniDstMaker* TheV0Maker();

  void SetEventCut(StHbtEventCut*);          // use these methods to do
  void SetParticleCut(StHbtParticleCut*);    // "front-loaded" cuts

  ClassDef(StStandardHbtEventReader, 1)

};

inline void StStandardHbtEventReader::SetTheEventMaker(StMaker* maker){mTheEventMaker=maker;}
inline StMaker* StStandardHbtEventReader::TheEventMaker(){return mTheEventMaker;}
inline void StStandardHbtEventReader::SetTheV0Maker(StV0MiniDstMaker* maker){mTheV0Maker=maker;}
inline StV0MiniDstMaker* StStandardHbtEventReader::TheV0Maker(){return mTheV0Maker;}
inline void StStandardHbtEventReader::SetEventCut(StHbtEventCut* ecut){mEventCut=ecut;}
inline void StStandardHbtEventReader::SetParticleCut(StHbtParticleCut* pcut){mParticleCut=pcut;}

#endif


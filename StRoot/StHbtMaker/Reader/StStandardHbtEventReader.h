/***************************************************************************
 *
 * $Id: StStandardHbtEventReader.h,v 1.3 1999/09/03 22:39:17 lisa Exp $
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

class StStandardHbtEventReader : public StHbtEventReader{

private:
  StMaker* mTheEventMaker;      //! this is the chain where the StEventReaderMaker is

public:
  StStandardHbtEventReader();
  //  ~StStandardHbtEventReader();

  virtual StHbtEvent* ReturnHbtEvent();
  virtual StHbtString Report();

  void SetTheEventMaker(StMaker*);
  StMaker* TheEventMaker();

  ClassDef(StStandardHbtEventReader, 1)

};

inline void StStandardHbtEventReader::SetTheEventMaker(StMaker* maker){mTheEventMaker=maker;}
inline StMaker* StStandardHbtEventReader::TheEventMaker(){return mTheEventMaker;}

#endif

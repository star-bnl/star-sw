/***************************************************************************
 *
 * $Id: StStandardHbtEventReader.h,v 1.1.1.1 1999/06/29 16:02:57 lisa Exp $
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
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StStandardHbtEventReader_hh
#define StStandardHbtEventReader_hh

#include "StHbtMaker/Base/StHbtEventReader.hh"
//#include "StChain/StChain.h"
#include "StChain/StMaker.h"

class StStandardHbtEventReader : public StHbtEventReader{

private:
  StMaker* mTheChain;      //! this is the chain where the StEventReaderMaker is

public:
  StStandardHbtEventReader();
  //  ~StStandardHbtEventReader();

  virtual StHbtEvent* ReturnHbtEvent();

  void SetTheChain(StMaker*);
  StMaker* TheChain();

  ClassDef(StStandardHbtEventReader, 1)

};

inline void StStandardHbtEventReader::SetTheChain(StMaker* chain){mTheChain=chain;}
inline StMaker* StStandardHbtEventReader::TheChain(){return mTheChain;}

#endif

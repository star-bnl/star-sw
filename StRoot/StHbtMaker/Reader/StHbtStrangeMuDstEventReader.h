/***************************************************************************
 *
 * $Id: StHbtStrangeMuDstEventReader.h,v 1.1 2000/12/13 20:44:59 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 * This is the HbtEventReader class to be used when reading the
 * StStrangeMuDsts produced by the STAR Strangeness group
 *
 ***************************************************************************
 *
 * $Log: StHbtStrangeMuDstEventReader.h,v $
 * Revision 1.1  2000/12/13 20:44:59  laue
 * New reader to read directly from the StStrangeMuDstMaker's V0 files
 *
 *
 **************************************************************************/

#ifndef StHbtStrangeMuDstEventReader_hh
#define StHbtStrangeMuDstEventReader_hh

#include "StHbtMaker/Base/StHbtEventReader.hh"

#include "StMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"

class StStrangeMuDstMaker;

class StHbtStrangeMuDstEventReader : public StMaker, public StHbtEventReader{

  // private:
  StStrangeMuDstMaker* mStrangeMuDstMaker; //! this is the chain where the StStrangeMuDstMaker is
 protected:

public:
  StHbtStrangeMuDstEventReader();
  StHbtStrangeMuDstEventReader(StStrangeMuDstMaker*);
  ~StHbtStrangeMuDstEventReader();

  StHbtEvent* ReturnHbtEvent();
  StHbtString Report();

  //  void SetStStrangeMuDstMaker(StStrangeMuDstMaker*);
  void SetStrangeMuDstMaker(StStrangeMuDstMaker* maker) { mStrangeMuDstMaker=maker;};
  StStrangeMuDstMaker* StrangeMuDstMaker() {return mStrangeMuDstMaker;};

#ifdef __ROOT__
  ClassDef(StHbtStrangeMuDstEventReader, 1)
#endif
};


#endif


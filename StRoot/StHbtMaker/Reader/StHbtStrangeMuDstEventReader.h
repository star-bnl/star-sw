/***************************************************************************
 *
 * $Id: StHbtStrangeMuDstEventReader.h,v 1.2 2001/06/21 19:18:42 laue Exp $
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
 * Revision 1.2  2001/06/21 19:18:42  laue
 * Modified Files: (to match the changed base classes)
 * 	StHbtAsciiReader.cxx StHbtAsciiReader.h
 * 	StHbtAssociationReader.cxx StHbtAssociationReader.h
 *  	StHbtBinaryReader.cxx StHbtBinaryReader.h
 *  	StHbtGstarTxtReader.cxx StHbtGstarTxtReader.h
 *  	StHbtStrangeMuDstEventReader.cxx
 *  	StHbtStrangeMuDstEventReader.h StStandardHbtEventReader.cxx
 * Added Files: new reader
 *  	StHbtTTreeReader.cxx StHbtTTreeReader.h
 *
 * Revision 1.1  2000/12/13 20:44:59  laue
 * New reader to read directly from the StStrangeMuDstMaker's V0 files
 *
 *
 **************************************************************************/

#ifndef StHbtStrangeMuDstEventReader_hh
#define StHbtStrangeMuDstEventReader_hh



class StStrangeMuDstMaker;
class StEvent;

#include "StMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StHbtMaker/Base/StHbtEventReader.hh"

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


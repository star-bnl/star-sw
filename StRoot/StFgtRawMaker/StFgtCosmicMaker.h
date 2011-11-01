// \class StFgtRawMaker
// \author Anselm Vossen (avossen@indiana.edu)
// 
//  $Id: StFgtCosmicMaker.h,v 1.15 2011/11/01 18:45:32 sgliske Exp $
//  $Log: StFgtCosmicMaker.h,v $
//  Revision 1.15  2011/11/01 18:45:32  sgliske
//  Updated to correspond with StEvent containers, take 2.
//  Note: new FGT containers (and StEvent access) no longer
//  motivate the use of a common base class
//
//  Revision 1.14  2011/10/26 20:57:48  avossen
//  hopefully made cosmic and raw maker compatible with bfc (again), added clear in make. Unnecessary if member fkt clear() is called after every event
//
//  Revision 1.13  2011/10/18 03:16:08  avossen
//  make compatible with chain like event saving, first step
//
//  Revision 1.12  2011/10/04 18:38:59  sgliske
//  made cut on short events optional
//
//  Revision 1.11  2011/09/30 17:24:39  sgliske
//  LOG_* bug solved, so can now return kStEof
//
//  Revision 1.10  2011/09/21 19:30:51  sgliske
//  Need RTS/src in path to DAQ_* directores
//  so other non-RAW-makers can find the headers
//
//  Revision 1.9  2011/09/21 17:49:32  sgliske
//  alternate base class with more
//   functionality and not an StMaker
//
//  Revision 1.7  2011/09/20 15:53:09  sgliske
//  Update so that everything compiles nicely
//  and so that one can execute the macro/simpleTestStandTest.C file
//
//
//
//

#ifndef STAR_StFgtCosmicMaker_HH
#define STAR_StFgtCosmicMaker_HH

#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/St_base/Stypes.h"
#include "RTS/src/DAQ_READER/daqReader.h"
#include "StMaker.h"

class StFgtCollection;

class StFgtCosmicMaker : public StMaker
{

 public: 
  StFgtCosmicMaker( const Char_t* name = "FgtCosmicMaker", const Char_t *daqFileName = "" );
  virtual ~StFgtCosmicMaker();

  void setFilename( std::string filename );

  virtual Int_t Init();
  virtual Int_t Make();
  virtual void Clear( Option_t *opts = "" );
  virtual Int_t prepareEnvironment();

  void cutShortEvents( Bool_t doIt = 1 );

 protected:
  Bool_t mCutShortEvents;
  StFgtCollection *mFgtCollectionPtr;

 private:
  std::string mDaqFileName;
  daqReader *mRdr;

  ClassDef(StFgtCosmicMaker,1);

};

// inline functions

inline void StFgtCosmicMaker::setFilename( std::string filename ){ mDaqFileName = filename; };
inline void StFgtCosmicMaker::cutShortEvents( Bool_t doIt ){ mCutShortEvents = doIt; };

#endif

// \class StFgtRawMaker
// \author Anselm Vossen (avossen@indiana.edu)
// 
//  $Id: StFgtCosmicMaker.h,v 1.10 2011/09/21 19:30:51 sgliske Exp $
//  $Log: StFgtCosmicMaker.h,v $
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
//subclass StFgtRawMaker
//replace prepare environment etc
//provide getStFgtEvent method with the data

#ifndef STAR_StFgtCosmicMaker_HH
#define STAR_StFgtCosmicMaker_HH

#include "StFgtRawMaker.h"
#include "StRoot/St_base/StMessMgr.h"
#include "StRoot/St_base/Stypes.h"

#include "RTS/src/DAQ_READER/daqReader.h"

class StFgtCosmicMaker : public StFgtRawBase, public StMaker
{

 public: 
  StFgtCosmicMaker( const Char_t* name = "FgtCosmicMaker", const Char_t *daqFileName = "" );
  virtual ~StFgtCosmicMaker();

  void setFilename( std::string filename );

  virtual Int_t Init();
  virtual Int_t Make();
  virtual void Clear( Option_t *opts = "" );

 protected:

 private:
  std::string mDaqFileName;
  daqReader *mRdr;

  ClassDef(StFgtCosmicMaker,1);

};

// inline functions

inline void StFgtCosmicMaker::setFilename( std::string filename ){ mDaqFileName = filename; };

#endif

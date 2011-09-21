// \class StFgtRawMaker
// \author Anselm Vossen (avossen@indiana.edu)
// 
//  $Id: StFgtCosmicMaker.h,v 1.8 2011/09/21 00:39:56 avossen Exp $
//  $Log: StFgtCosmicMaker.h,v $
//  Revision 1.8  2011/09/21 00:39:56  avossen
//  added simple Fgt maker base class
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
#include "StFgtBaseMaker.h"
#include <DAQ_READER/daqReader.h>

class StFgtCosmicMaker : public StFgtBaseMaker
{

 public: 
  StFgtCosmicMaker( const Char_t* name = "FgtCosmicMaker" );
  StFgtCosmicMaker( const Char_t* name, const Char_t* daqFileName, Int_t numDiscs);
  StFgtEvent& currentFgtEvent();
  Int_t setFilename(string filename);

  virtual ~StFgtCosmicMaker(){};

 protected:
  //virtual void constructDiscs();
  virtual void PrepareEnvironment();
  //advance to the next event
  virtual Int_t Make();


 private:
  void clearHits();
  daqReader *mRdr;

  ClassDef(StFgtCosmicMaker,1);

};
#endif

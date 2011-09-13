// \class StFgtRawMaker
// \author Anselm Vossen (avossen@indiana.edu)
// 
//  $Id
//  $Log
//
//
//
//subclass StFgtRawMaker
//replace prepare environment etc
//provide getStFgtEvent method with the data
#ifndef STAR_StFgtCosmicMaker_HH
#define STAR_StFgtCosmicMaker_HH
#include "StFgtRawMaker.h"
#include <DAQ_READER/daqReader.h>

class StFgtCosmicMaker : public StFgtRawMaker
{

 public: 
  StFgtCosmicMaker();
  StFgtCosmicMaker(char* daqFileName, int numDiscs);
  StFgtEvent& currentFgtEvent();
  int setFilename(string filename);

  virtual ~StFgtCosmicMaker(){};

 protected:
  virtual void constructDiscs();
  virtual void PrepareEnvironment();
  //advance to the next event
  virtual Int_t Make();


 private:
  void clearHits();
  StFgtEvent* mFgtEvent;
  daqReader *mRdr ;
  ClassDef(StFgtCosmicMaker,1);

};
#endif

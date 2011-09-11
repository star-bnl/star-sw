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

class StFgtCosmicMaker : public StFgtRawMaker
{

 public: 
  StFgtEvent& fgtEvent();

 protected:
  virtual void constructDiscs();
  virtual Int_t Make();
 private:
  ClassDef(StFgtCosmicMaker,1)
}

#endif

// \class StFgtBaseMaker
//\author Anselm Vossen (avossen@indiana.edu)
// 
// 

#include "StFgtBaseMaker.h"

StFgtBaseMaker::StFgtBaseMaker(const Char_t* makerName, const Char_t* detName):StMaker(detName)
{
  if(makerName &&makerName[0]) SetName(makerName);
};


ClassImp(StFgtBaseMaker);

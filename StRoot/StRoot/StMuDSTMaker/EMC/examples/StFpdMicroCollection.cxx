//###########################################################
// EMC Micro Event
// Author: Alexandre A. P. Suaide
// initial version 08/2001
//
// See README for details
//########################################################### 
#include "StFpdMicroCollection.h"

ClassImp(StFpdMicroCollection)

StFpdMicroCollection::StFpdMicroCollection()
{
  mToken = 0; 
  mAdcNorth = 0; 
  mAdcSouth = 0; 
  mAdcTop = 0; 
  mAdcBottom = 0; 
  mAdcPS1 = 0; 
  mAdcPS2 = 0; 
  mAdcSmdX = 0; 
  mAdcSmdY = 0; 
    
}
StFpdMicroCollection::~StFpdMicroCollection()
{
}

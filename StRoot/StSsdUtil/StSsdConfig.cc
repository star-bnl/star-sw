/***************************************************************************
 *
 * $Id: StSsdConfig.cc,v 1.1 2004/03/12 04:24:19 jeromel Exp $
 *
 * author: christelle Roy
 ***************************************************************************
 *
 * Description: SSD Config
 *
 **************************************************************************/

#include "StSsdConfig.hh"
#include "StMessMgr.h"

ClassImp(StSsdConfig)

StSsdConfig::StSsdConfig()
{}

StSsdConfig::~StSsdConfig()
{}

StSsdConfig::StSsdConfig(const StSsdConfig& geom)
{}

StSsdConfig& StSsdConfig::operator = (const StSsdConfig& geom)
{
  return *this;
}

void StSsdConfig::setConfiguration()
{
  if  ((mTotalNumberOfHybrids == 32) && (mNumberOfBarrels == 1))  // LADDER 0
    mConfig = TString("LAD0");
  else if (mTotalNumberOfHybrids == 320) // HALF SSD
    mConfig = TString("HALF");
  else if (mTotalNumberOfHybrids == 640) // FULL SSD
    mConfig = TString("SSD");
  else
    mConfig = TString("NULL");
}

void StSsdConfig::setConfiguration(const char* config)
{
  // set the Collection configuration

  mConfig = TString(config);

//   if ( !strncmp(config, "LAD0", strlen("LAD0")) ) {
//     setNumberOfBarrels(1);
//     setNumberOfLadders(1,1);
//     setNumberOfWafers(1,16);
//     setNumberOfHybrids(2);
//     setTotalNumberOfHybrids(32);
//   }

//   else if ( !strncmp(config, "HALF", strlen("HALF")) ) {
//     setNumberOfBarrels(1);
//     setNumberOfLadders(1,10);
//     setNumberOfWafers(1,160);
//     setNumberOfHybrids(2);
//     setTotalNumberOfHybrids(320);
//   }

//   else if ( !strncmp(config, "SSD", strlen("SSD")) ) {
//     setNumberOfBarrels(1);
//     setNumberOfLadders(1,20);
//     setNumberOfWafers(1,320);
//     setNumberOfHybrids(2);
//     setTotalNumberOfHybrids(640);
//   }

//   else
//     gMessMgr->Message("Configuration of SSD not defined! It must be LAD0 or HALF or SSD ","E");
  
  setNumberOfBarrels(1);
  setNumberOfLadders(1,20);
  setNumberOfWafers(1,16);
  setNumberOfHybrids(2);
  setTotalNumberOfHybrids(640);
}

const char* StSsdConfig::getConfiguration()
{
  return mConfig.Data();
}

int StSsdConfig::getHybridIndex(int barrelID, int ladderID, int waferID, int hybridID)
{
  // returns an internal index for the specified hybrid. 
  // This index should be used to store/retrieve a specific hybrid in/from the collection.
  // Or one can use the getObject method which parameters are the barrel, ladder, wafer and hybrid numbers.

  int index;
  int mNumberOfBarrels;                        // Number of Barrels
  int mNumberOfLadders[MAX_NUMBER_OF_BARRELS]; // Number of Ladders of each Barrel
  int mNumberOfWafers[MAX_NUMBER_OF_BARRELS];  // Number of Wafers of each Ladder (Barrel dependent)
  int mNumberOfHybrids;                        // Number of Hybrids of each Wafer ( = 2)

  mNumberOfBarrels = getNumberOfBarrels();

  for (int i = 0;i < mNumberOfBarrels;i++) {
    mNumberOfLadders[i] = getNumberOfLadders(i+1);
    mNumberOfWafers[i] = getNumberOfWafers(i+1);
  }
  mNumberOfHybrids = getNumberOfHybrids();


  switch  (barrelID) {
    
  case 1:
    index = (ladderID-1)*mNumberOfWafers[barrelID-1]*mNumberOfHybrids + (waferID-1)*mNumberOfHybrids + (hybridID-1);
    break;
        
  default:
    gMessMgr->Error() << "There is NO barrel number " << barrelID << " !!!";
    gMessMgr->Print();
    index = -1;
    break;
  }

    
  if      ((barrelID == 1) && (ladderID == 1)) index = 0;
  else if ((barrelID == 1) && (ladderID == 2)) index = 1;
  else if ((barrelID == 1) && (ladderID == 3)) index = 2;
  else if ((barrelID == 1) && (ladderID == 4)) index = 3;
  else if ((barrelID == 1) && (ladderID == 5)) index = 4;
  else if ((barrelID == 1) && (ladderID == 6)) index = 5;
  else if ((barrelID == 1) && (ladderID == 7)) index = 6;
  else if ((barrelID == 1) && (ladderID == 8)) index = 7;
  else if ((barrelID == 1) && (ladderID == 9)) index = 8;
  else if ((barrelID == 1) && (ladderID == 10)) index = 9;
  else if ((barrelID == 1) && (ladderID == 11)) index = 10;
  else if ((barrelID == 1) && (ladderID == 12)) index = 11;
  else if ((barrelID == 1) && (ladderID == 13)) index = 12;
  else if ((barrelID == 1) && (ladderID == 14)) index = 13;
  else if ((barrelID == 1) && (ladderID == 15)) index = 14;
  else if ((barrelID == 1) && (ladderID == 16)) index = 15;
  else if ((barrelID == 1) && (ladderID == 17)) index = 16;
  else if ((barrelID == 1) && (ladderID == 18)) index = 17;
  else if ((barrelID == 1) && (ladderID == 19)) index = 18;
  else if ((barrelID == 1) && (ladderID == 20)) index = 19;
  
  else index = -1;
  return index;
}

int StSsdConfig::getProperHybridIndex(int barrelID, int ladderID, int waferID, int hybridID)
{
  
  //Returns the index that the barrel,ladder,wafer and hybrid should be if
  // things werent swapped around
  int index = getHybridIndex( barrelID, ladderID, waferID,  hybridID);  
  
  return index;
}

int StSsdConfig::getBarrel(int index){
  
  int MaxIndex=0; 
  for( int i=0; i< mNumberOfBarrels;i++) {
    
    MaxIndex +=  getNumberOfLadders(i+1)*getNumberOfWafers(i+1)*
      getNumberOfHybrids();
    if( MaxIndex > index) return i+1;
  }
  
  return -1;
}

int StSsdConfig::getLayer(int index)
{
  int Layer;
  int Barrel = getBarrel(index);
  //  int Ladder = getLadder(index);

  switch (Barrel) {

  case 1:
    Layer = 1;
    
    break;

  default:
    gMessMgr->Error() << "There is NO barrel number !!!";
    gMessMgr->Print();
    break;
  }

  return Layer;
}


int StSsdConfig::getLadder(int index){

  int i, indexsav, CurrentIndex=0;

  int Barrel = getBarrel(index);

  // Cope with switch readout in real data

  indexsav = index;
  
  for( i=0; i< Barrel-1; i++){
    CurrentIndex += getNumberOfLadders(i+1)*getNumberOfWafers(i+1)*
      getNumberOfHybrids();
  }

  for( i=0; i< getNumberOfLadders(Barrel); i++){
    CurrentIndex += getNumberOfWafers(Barrel)*getNumberOfHybrids();
    if( CurrentIndex > indexsav) return i+1;
  }
  
  return -1;
}

int StSsdConfig::getWafer(int index){

  int i, indexsav=0,CurrentIndex=0;

  int Barrel = getBarrel(index);
  int Ladder = getLadder(index);

  
  indexsav = index;

  for( i=0; i< Barrel-1; i++){
    CurrentIndex += getNumberOfLadders(i+1)*getNumberOfWafers(i+1)*
      getNumberOfHybrids();
  }

  for( i=0; i< Ladder-1; i++){
    CurrentIndex += getNumberOfWafers(Barrel)*getNumberOfHybrids();
  }

  for(i=0; i<getNumberOfWafers(Barrel); i++){
    CurrentIndex += getNumberOfHybrids();
    if( CurrentIndex > indexsav) return i+1;
  }
  
  return -1;
}
    
int StSsdConfig::getHybrid(int index){

  int i, indexsav, CurrentIndex=0;

  int Barrel = getBarrel(index);
  int Ladder = getLadder(index);
  int Wafer = getWafer(index);

  
  indexsav = index;

  for( i=0; i< Barrel-1; i++){
    CurrentIndex += getNumberOfLadders(i+1)*getNumberOfWafers(i+1)*
      getNumberOfHybrids();
  }

  for( i=0; i< Ladder-1; i++){
    CurrentIndex += getNumberOfWafers(Barrel)*getNumberOfHybrids();
  }

  for(i=0; i<Wafer-1; i++){
    CurrentIndex += getNumberOfHybrids();
  }
   
  if( indexsav == CurrentIndex++) return 1;
  else if ( indexsav == CurrentIndex) return 2;
 
  return -1;
}

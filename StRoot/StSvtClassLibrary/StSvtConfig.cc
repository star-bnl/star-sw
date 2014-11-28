/***************************************************************************
 *
 * $Id: StSvtConfig.cc,v 1.12 2008/06/12 14:22:11 fisyak Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Config
 *
 ***************************************************************************
 *
 * $Log: StSvtConfig.cc,v $
 * Revision 1.12  2008/06/12 14:22:11  fisyak
 * Add default no. of anodes and timeBins
 *
 * Revision 1.11  2008/05/21 19:09:30  fine
 * fix the STAR messager interface # 1190
 *
 * Revision 1.10  2002/05/06 00:36:12  munhoz
 * correct hybrid swapping
 *
 * Revision 1.9  2002/02/22 20:12:10  caines
 * Add getLayer() call
 *
 * Revision 1.8  2001/10/04 02:56:26  caines
 * Fix some of the hybrid swapping indexing
 *
 * Revision 1.7  2001/10/02 22:57:49  caines
 * Wafer 3 is also swapped on getHybrid()B2L8
 *
 * Revision 1.6  2001/09/06 22:11:30  caines
 * Still fixing swapped hybrids
 *
 * Revision 1.5  2001/09/06 15:46:13  caines
 * Swapped to many hybrids on Barrel2 ladder 8 - fixed
 *
 * Revision 1.4  2001/08/24 21:02:58  caines
 * Do index swapping for year2001 data
 *
 * Revision 1.3  2001/08/16 21:02:03  munhoz
 * changing StObjArray to StStrArray. StSvtConfig reestructured. New classes for geometry DB
 *
 * Revision 1.2  2001/02/18 00:10:28  caines
 * Improve and use StSvtConifg
 *
 * Revision 1.1  2000/11/30 20:38:50  caines
 * Svt configuration files
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// This class represents the SVT Config object.                   //
//                                                                        //
////////////////////////////////////////////////////////////////////////////


#include "StSvtConfig.hh"
#include "StMessMgr.h"

ClassImp(StSvtConfig)

StSvtConfig::StSvtConfig()
{}

StSvtConfig::~StSvtConfig()
{}

StSvtConfig::StSvtConfig(const StSvtConfig& geom)
{}

StSvtConfig& StSvtConfig::operator = (const StSvtConfig& geom)
{
  return *this;
}

void StSvtConfig::setConfiguration()
{
  if (mTotalNumberOfHybrids == 18 )  // SYSTEM TEST  
    mConfig = TString("SYST");
  else if ((mTotalNumberOfHybrids == 14) && (mNumberOfBarrels == 3))  // YEAR 1 LADDER  
    mConfig = TString("Y1L");
  else if (mTotalNumberOfHybrids == 432 )  // FULL SVT  
    mConfig = TString("FULL");
  else if ((mTotalNumberOfHybrids == 14) && (mNumberOfBarrels == 1))  // YEAR 1 LADDER  
    mConfig = TString("LADDER");
  else if (mTotalNumberOfHybrids == 36 )  // FULL SVT  
    mConfig = TString("BARREL");
  else if (mTotalNumberOfHybrids == 3 )  // FULL SVT  
    mConfig = TString("SVT");
  else
    mConfig = TString("NULL");
}

void StSvtConfig::setConfiguration(const char* config)
{
  // set the Collection configuration

  mConfig = TString(config);
  setNumberOfAnodes(240);
  setNumberOfTimeBins(128);
  if ( !strncmp(config, "ASCII", strlen("ASCII")) ) {
    setNumberOfBarrels(1);
    setNumberOfLadders(1,1);
    setNumberOfWafers(1,1);
    setNumberOfHybrids(2);
    setTotalNumberOfHybrids(2);    
  }
  
  else if ( !strncmp(config, "SYST", strlen("SYST")) ) {
    setNumberOfBarrels(3);
    setNumberOfLadders(1,8);
    setNumberOfLadders(2,12);
    setNumberOfLadders(3,16);
    setNumberOfWafers(1,4);
    setNumberOfWafers(2,6);
    setNumberOfWafers(3,7);
    setNumberOfHybrids(2);
    setTotalNumberOfHybrids(18);
  }

  else if ( !strncmp(config, "Y1L", strlen("Y1L")) ) {
    setNumberOfBarrels(3);
    setNumberOfLadders(1,0);
    setNumberOfLadders(2,0);
    setNumberOfLadders(3,2);
    setNumberOfWafers(1,0);
    setNumberOfWafers(2,0);
    setNumberOfWafers(3,7);
    setNumberOfHybrids(2);
    setTotalNumberOfHybrids(14);
  }

  else if ( !strncmp(config, "LADDER", strlen("LADDER")) ) {
    setNumberOfBarrels(1);
    setNumberOfLadders(1,1);
    setNumberOfWafers(1,7);
    setNumberOfHybrids(2);
    setTotalNumberOfHybrids(14);
  }

  else if ( !strncmp(config, "BARREL", strlen("BARREL")) ) {
    setNumberOfBarrels(3);
    setNumberOfLadders(1,8);
    setNumberOfLadders(2,12);
    setNumberOfLadders(3,16);
    setNumberOfWafers(1,1);
    setNumberOfWafers(2,1);
    setNumberOfWafers(3,1);
    setNumberOfHybrids(1);
    setTotalNumberOfHybrids(36);
  }

  else if ( !strncmp(config, "SVT", strlen("SVT")) ) {
    setNumberOfBarrels(3);
    setNumberOfLadders(1,1);
    setNumberOfLadders(2,1);
    setNumberOfLadders(3,1);
    setNumberOfWafers(1,1);
    setNumberOfWafers(2,1);
    setNumberOfWafers(3,1);
    setNumberOfHybrids(1);
    setTotalNumberOfHybrids(3);
  }

  else if ( !strncmp(config, "FULL", strlen("FULL")) ) {
    setNumberOfBarrels(3);
    setNumberOfLadders(1,8);
    setNumberOfLadders(2,12);
    setNumberOfLadders(3,16);
    setNumberOfWafers(1,4);
    setNumberOfWafers(2,6);
    setNumberOfWafers(3,7);
    setNumberOfHybrids(2);
    setTotalNumberOfHybrids(432);
  }
  else {
    LOG_ERROR << "Configuration of SVT not defined! It must be SYST, Y1L or FULL"<< endm;
 }
}

const char* StSvtConfig::getConfiguration()
{
  return mConfig.Data();
}

int StSvtConfig::getHybridIndex(int barrelID, int ladderID, int waferID, int hybridID)
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
    
  case 2:
    index = mNumberOfLadders[barrelID-2]*mNumberOfWafers[barrelID-2]*mNumberOfHybrids +
      (ladderID-1)*mNumberOfWafers[barrelID-1]*mNumberOfHybrids + (waferID-1)*mNumberOfHybrids + (hybridID-1);
    break;
    
  case 3:
    index = mNumberOfLadders[barrelID-2]*mNumberOfWafers[barrelID-2]*mNumberOfHybrids + 
      mNumberOfLadders[barrelID-3]*mNumberOfWafers[barrelID-3]*mNumberOfHybrids +
      (ladderID-1)*mNumberOfWafers[barrelID-1]*mNumberOfHybrids + (waferID-1)*mNumberOfHybrids + (hybridID-1);
    break;
    
  default:
    LOG_ERROR << "There is NO barrel number " << barrelID << " !!!" << endm;
    // gMessMgr->Print();
    index = -1;
    break;
  }

  if ( !strncmp(mConfig, "SYST", strlen("SYST")) ) {
    if      ((barrelID == 3) && (ladderID == 1) && (waferID == 7) && (hybridID == 1)) index = 0;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 7) && (hybridID == 2)) index = 1;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 6) && (hybridID == 1)) index = 2;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 6) && (hybridID == 2)) index = 3;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 5) && (hybridID == 1)) index = 4;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 5) && (hybridID == 2)) index = 5;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 4) && (hybridID == 1)) index = 6;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 4) && (hybridID == 2)) index = 7;    
    else if ((barrelID == 1) && (ladderID == 1) && (waferID == 4) && (hybridID == 1)) index = 4;
    else if ((barrelID == 1) && (ladderID == 1) && (waferID == 4) && (hybridID == 2)) index = 5;
    else if ((barrelID == 1) && (ladderID == 1) && (waferID == 3) && (hybridID == 1)) index = 6;
    else if ((barrelID == 1) && (ladderID == 1) && (waferID == 3) && (hybridID == 2)) index = 7;
    else if ((barrelID == 3) && (ladderID == 2) && (waferID == 7) && (hybridID == 1)) index = 8;
    else if ((barrelID == 3) && (ladderID == 2) && (waferID == 7) && (hybridID == 2)) index = 9;
    else if ((barrelID == 3) && (ladderID == 2) && (waferID == 6) && (hybridID == 1)) index = 10;
    else if ((barrelID == 3) && (ladderID == 2) && (waferID == 6) && (hybridID == 2)) index = 11;
    else if ((barrelID == 3) && (ladderID == 2) && (waferID == 5) && (hybridID == 1)) index = 16;
    else if ((barrelID == 3) && (ladderID == 2) && (waferID == 5) && (hybridID == 2)) index = 17;
    
    else index = -1;
  }
  else if ( !strncmp(mConfig, "Y1L", strlen("Y1L")) ) {
    if      ((barrelID == 3) && (ladderID == 2) && (waferID == 1) && (hybridID == 1)) index = 0;
    else if ((barrelID == 3) && (ladderID == 2) && (waferID == 1) && (hybridID == 2)) index = 1;
    else if ((barrelID == 3) && (ladderID == 2) && (waferID == 2) && (hybridID == 1)) index = 2;
    else if ((barrelID == 3) && (ladderID == 2) && (waferID == 2) && (hybridID == 2)) index = 3;
    else if ((barrelID == 3) && (ladderID == 2) && (waferID == 3) && (hybridID == 1)) index = 4;
    else if ((barrelID == 3) && (ladderID == 2) && (waferID == 3) && (hybridID == 2)) index = 5;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 4) && (hybridID == 1)) index = 6;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 4) && (hybridID == 2)) index = 7;    
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 5) && (hybridID == 1)) index = 8;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 5) && (hybridID == 2)) index = 9;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 6) && (hybridID == 1)) index = 10;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 6) && (hybridID == 2)) index = 11;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 7) && (hybridID == 1)) index = 12;
    else if ((barrelID == 3) && (ladderID == 1) && (waferID == 7) && (hybridID == 2)) index = 13;

    else index = -1;
  }

  else if ( !strncmp(mConfig, "LADDER", strlen("LADDER")) ) {
    if      ((waferID == 1) && (hybridID == 1)) index = 0;
    else if ((waferID == 1) && (hybridID == 2)) index = 1;
    else if ((waferID == 2) && (hybridID == 1)) index = 2;
    else if ((waferID == 2) && (hybridID == 2)) index = 3;
    else if ((waferID == 3) && (hybridID == 1)) index = 4;
    else if ((waferID == 3) && (hybridID == 2)) index = 5;
    else if ((waferID == 4) && (hybridID == 1)) index = 6;
    else if ((waferID == 4) && (hybridID == 2)) index = 7;    
    else if ((waferID == 5) && (hybridID == 1)) index = 8;
    else if ((waferID == 5) && (hybridID == 2)) index = 9;
    else if ((waferID == 6) && (hybridID == 1)) index = 10;
    else if ((waferID == 6) && (hybridID == 2)) index = 11;
    else if ((waferID == 7) && (hybridID == 1)) index = 12;
    else if ((waferID == 7) && (hybridID == 2)) index = 13;

    else index = -1;
  }

  else if ( !strncmp(mConfig, "BARREL", strlen("BARREL")) ) {
    if      ((barrelID == 1) && (ladderID == 1)) index = 0;
    else if ((barrelID == 1) && (ladderID == 2)) index = 1;
    else if ((barrelID == 1) && (ladderID == 3)) index = 2;
    else if ((barrelID == 1) && (ladderID == 4)) index = 3;
    else if ((barrelID == 1) && (ladderID == 5)) index = 4;
    else if ((barrelID == 1) && (ladderID == 6)) index = 5;
    else if ((barrelID == 1) && (ladderID == 7)) index = 6;
    else if ((barrelID == 1) && (ladderID == 8)) index = 7;
    else if ((barrelID == 2) && (ladderID == 1)) index = 8;
    else if ((barrelID == 2) && (ladderID == 2)) index = 9;
    else if ((barrelID == 2) && (ladderID == 3)) index = 10;
    else if ((barrelID == 2) && (ladderID == 4)) index = 11;
    else if ((barrelID == 2) && (ladderID == 5)) index = 12;
    else if ((barrelID == 2) && (ladderID == 6)) index = 13;
    else if ((barrelID == 2) && (ladderID == 7)) index = 14;
    else if ((barrelID == 2) && (ladderID == 8)) index = 15;
    else if ((barrelID == 2) && (ladderID == 9)) index = 16;
    else if ((barrelID == 2) && (ladderID == 10)) index = 17;
    else if ((barrelID == 2) && (ladderID == 11)) index = 18;
    else if ((barrelID == 2) && (ladderID == 12)) index = 19;
    else if ((barrelID == 3) && (ladderID == 1)) index = 20;
    else if ((barrelID == 3) && (ladderID == 2)) index = 21;
    else if ((barrelID == 3) && (ladderID == 3)) index = 22;
    else if ((barrelID == 3) && (ladderID == 4)) index = 23;
    else if ((barrelID == 3) && (ladderID == 5)) index = 24;
    else if ((barrelID == 3) && (ladderID == 6)) index = 25;
    else if ((barrelID == 3) && (ladderID == 7)) index = 26;
    else if ((barrelID == 3) && (ladderID == 8)) index = 27;
    else if ((barrelID == 3) && (ladderID == 9)) index = 28;
    else if ((barrelID == 3) && (ladderID == 10)) index = 29;
    else if ((barrelID == 3) && (ladderID == 11)) index = 30;
    else if ((barrelID == 3) && (ladderID == 12)) index = 31;
    else if ((barrelID == 3) && (ladderID == 13)) index = 32;
    else if ((barrelID == 3) && (ladderID == 14)) index = 33;
    else if ((barrelID == 3) && (ladderID == 15)) index = 34;
    else if ((barrelID == 3) && (ladderID == 16)) index = 35;

    else index = -1;
  }

  else if ( !strncmp(mConfig, "SVT", strlen("SVT")) ) {
    if      ((barrelID == 1)) index = 0;
    else if ((barrelID == 2)) index = 1;
    else if ((barrelID == 3)) index = 2;

    else index = -1;
  }

  return index;
}

int StSvtConfig::getProperHybridIndex(int barrelID, int ladderID, int waferID, int hybridID)
{
  
  //Returns the index that the barrel,ladder,wafer and hybrid should be if
  // things werent swapped around
  int index = getHybridIndex( barrelID, ladderID, waferID,  hybridID);  
  
  return index;
}

int StSvtConfig::getBarrel(int index){
  
  int MaxIndex=0; 
  for( int i=0; i< mNumberOfBarrels;i++) {
    
    MaxIndex +=  getNumberOfLadders(i+1)*getNumberOfWafers(i+1)*
      getNumberOfHybrids();
    if( MaxIndex > index) return i+1;
  }
  
  return -1;
}

int StSvtConfig::getLayer(int index)
{
  int Layer;
  int Barrel = getBarrel(index);
  int Ladder = getLadder(index);
  switch (Barrel) {

  case 1:
    if (Ladder%2)
      Layer = 2;
    else
      Layer = 1;

    break;

  case 2:    
    if (Ladder%2)
      Layer = 4; //
    else          // switched due to different geometry (07/12/2001) 
      Layer = 3; //

    break;

  case 3:
    if (Ladder%2)
      Layer = 6;
    else
      Layer = 5;

    break;
  }

  return Layer;
}


int StSvtConfig::getLadder(int index){

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

int StSvtConfig::getWafer(int index){

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
    
int StSvtConfig::getHybrid(int index){

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

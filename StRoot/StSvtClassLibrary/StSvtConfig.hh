/***************************************************************************
 *
 * $Id: StSvtConfig.hh,v 1.4 2002/02/22 20:12:15 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Geometry object. It makes the link with the Data Base
 *
 ***************************************************************************
 *
 * $Log: StSvtConfig.hh,v $
 * Revision 1.4  2002/02/22 20:12:15  caines
 * Add getLayer() call
 *
 * Revision 1.3  2001/10/04 02:56:26  caines
 * Fix some of the hybrid swapping indexing
 *
 * Revision 1.2  2001/08/24 21:03:03  caines
 * Do index swapping for year2001 data
 *
 * Revision 1.1  2000/11/30 20:38:50  caines
 * Svt configuration files
 *
 **************************************************************************/

#ifndef STSVTCONFIG_HH
#define STSVTCONFIG_HH

#define MAX_NUMBER_OF_BARRELS 3

#include "StObject.h"
#include "TString.h"

class StSvtConfig: public StObject
{
public:
  StSvtConfig();
  virtual ~StSvtConfig();

  StSvtConfig(const StSvtConfig&);
  StSvtConfig& operator = (const StSvtConfig&);

  int getNumberOfBarrels() {return mNumberOfBarrels;}
  int getNumberOfLadders(int barrel) {return mNumberOfLadders[barrel-1];}
  int getNumberOfWafers(int barrel)  {return mNumberOfWafers[barrel-1];}
  int getNumberOfHybrids() {return mNumberOfHybrids;}
  int getTotalNumberOfHybrids() {return mTotalNumberOfHybrids;}

  int getNumberOfAnodes() {return mNumberOfAnodes;}
  int getNumberOfTimeBins() {return mNumberOfTimeBins;}

  void setNumberOfBarrels(int barrels) {mNumberOfBarrels = barrels;}
  void setNumberOfLadders(int barrel, int ladders) {mNumberOfLadders[barrel-1] = ladders;}
  void setNumberOfWafers(int barrel, int wafers)  {mNumberOfWafers[barrel-1] = wafers;}
  void setNumberOfHybrids(int hybrids) {mNumberOfHybrids = hybrids;}
  void setTotalNumberOfHybrids(int hybrids) {mTotalNumberOfHybrids = hybrids;}

  void setNumberOfAnodes(int anodes) {mNumberOfAnodes = anodes;}
  void setNumberOfTimeBins(int timeBins) {mNumberOfTimeBins = timeBins;}

  void setConfiguration(); // set the SVT configuration
  void setConfiguration(const char* config); // set the SVT configuration

  int getHybridIndex(int barrelID, int ladderID, int waferID, int hybridID);
  int getProperHybridIndex(int barrelID, int ladderID, int waferID, int hybridID);
  int getBarrel(int index);
  int getLayer(int index);
  int getLadder(int index);
  int getWafer(int index);
  int getHybrid(int index);
  
  const char* getConfiguration(); // Returns the SVT configuration

protected:
  int mNumberOfBarrels;                        // Number of Barrels
  int mNumberOfLadders[MAX_NUMBER_OF_BARRELS]; // Number of Ladders of each Barrel
  int mNumberOfWafers[MAX_NUMBER_OF_BARRELS];  // Number of Wafers of each Ladder (Barrel dependent)
  int mNumberOfHybrids;        // Number of Hybrids of each Wafer 
  
  int mTotalNumberOfHybrids;   // Total Number of Hybrids (entire SVT)
  
  int mNumberOfAnodes;   // Number of Anodes in one hybrid
  int mNumberOfTimeBins; // Number of Time Bins in one hybrid

  TString mConfig;       // SVT Configuration 

  ClassDef(StSvtConfig,1)
};

#endif

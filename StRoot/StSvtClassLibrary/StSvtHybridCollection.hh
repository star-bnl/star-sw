/***************************************************************************
 *
 * $Id: StSvtHybridCollection.hh,v 1.1.1.1 2000/03/10 14:26:21 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Array BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridCollection.hh,v $
 * Revision 1.1.1.1  2000/03/10 14:26:21  munhoz
 * SVT Class Library
 *
 **************************************************************************/

#ifndef STSVTHYBRIDCOLLECTION_HH
#define STSVTHYBRIDCOLLECTION_HH

#define MAX_NUMBER_OF_BARRELS  3

#include "StArray.h"

class StHybridObject;

class StSvtHybridCollection: public StObjArray
{
public:
  StSvtHybridCollection(char* config = 0);
  virtual ~StSvtHybridCollection();

  int getNumberOfBarrels() {return mNumberOfBarrels;}
  int getNumberOfLadders(int barrel) {return mNumberOfLadders[barrel-1];}
  int getNumberOfWafers(int barrel)  {return mNumberOfWafers[barrel-1];}
  int getNumberOfHybrids() {return mNumberOfHybrids;}
  int getTotalNumberOfHybrids() {return mTotalNumberOfHybrids;}
  int getHybridIndex(int barrel, int ladder, int wafer, int hybrid);

  void setConfiguration(char* config); // Set the SVT configuration
  char* getConfiguration() {return fConfig;} // Returns the SVT configuration

  StHybridObject* getObject(int barrel, int ladder, int wafer, int hybrid); // Returns a object of the collection

protected:

  int mNumberOfBarrels;                        // Number of Barrels
  int mNumberOfLadders[MAX_NUMBER_OF_BARRELS]; // Number of Ladders of each Barrel
  int mNumberOfWafers[MAX_NUMBER_OF_BARRELS];  // Number of Wafers of each Ladder (Barrel dependent)
  int mNumberOfHybrids;                        // Number of Hybrids of each Wafer ( = 2)
  
  int mTotalNumberOfHybrids;                   // Total Number of Hybrids (entire SVT)

  char* fConfig;    //! SVT Configuration 

  ClassDef(StSvtHybridCollection,1)
};

#endif

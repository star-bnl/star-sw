/*!
 * \class  StSsdHybridCollection
 * \author cr
 *
 * SSD Hybrid Array BASE class
 */

#ifndef STSSDHYBRIDCOLLECTION_HH
#define STSSDHYBRIDCOLLECTION_HH

#define MAX_NUMBER_OF_BARRELS  1              /*! \def MAX_NUMBER_OF_BARRELS */

#include "StArray.h"

class StSsdHybridObject;
class StSsdConfig;
class TString;

class StSsdHybridCollection: public StStrArray
{
public:
  StSsdHybridCollection();
  StSsdHybridCollection(StSsdConfig* config);
  StSsdHybridCollection(const char* config);
  virtual ~StSsdHybridCollection();

  int getNumberOfBarrels();
  int getNumberOfLadders(int barrel);
  int getNumberOfWafers(int barrel);
  int getNumberOfHybrids();
  int getTotalNumberOfHybrids();
  int getHybridIndex(int barrel, int ladder, int wafer, int hybrid);
  int getProperHybridIndex(int barrel, int ladder, int wafer, int hybrid);
  void setConfiguration(const char* config);       /// Set the SSD configuration
  void setConfiguration(StSsdConfig* config);      /// Set the SSD configuration
  StSsdConfig* getSsdConfig() {return mSsdConfig;} /// Returns the SSD configuration object
  const char* getConfiguration();                  /// Returns the SSD configuration

//   StSsdHybridObject* At(int index);  // needed for backward compatibility
//   void AddAt(StSsdHybridObject* object, int index);  // needed for backward compatibility

  /// Returns a object of the collection
  StSsdHybridObject* getObject(int barrel, int ladder, int wafer, int hybrid); 

protected:

  StSsdConfig* mSsdConfig;    //! SSD Configuration 
  TString mConfig;            //  SSD Configuration 

  ClassDef(StSsdHybridCollection,1)
};

#endif

/***************************************************************************
 *
 * $Id: StSsdHybridCollection.hh,v 1.1 2004/03/12 04:24:20 jeromel Exp $
 *
 * $Log: StSsdHybridCollection.hh,v $
 * Revision 1.1  2004/03/12 04:24:20  jeromel
 * First version of SSD Util
 *
 *
 ***************************************************************************/

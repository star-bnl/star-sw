/*!
 * \class   StSsdWaferCollection
 * \author cr
 * 
 * SSD Wafer Array BASE class
 */

#ifndef STSSDWAFERCOLLECTION_HH
#define STSSDWAFERCOLLECTION_HH

#define MAX_NUMBER_OF_BARRELS  1                    /*! \def MAX_NUMBER_OF_BARRELS */

#include "StArray.h"

class StSsdHybridObject;
class StSsdConfig;
class TString;

class StSsdWaferCollection: public StStrArray
{
public:
  StSsdWaferCollection();
  StSsdWaferCollection(StSsdConfig* config);
  StSsdWaferCollection(const char* config);
  virtual ~StSsdWaferCollection();

  int getNumberOfBarrels();
  int getNumberOfLadders(int barrel);
  int getNumberOfWafers(int barrel);
  int getTotalNumberOfWafers();
  int getWaferIndex(int barrel, int ladder, int wafer);

  void setConfiguration(const char* config);       // Set the SSD configuration
  void setConfiguration(StSsdConfig* config);      // Set the SSD configuration
  StSsdConfig* getSsdConfig() {return mSsdConfig;} // Returns the SSD configuration object
  const char* getConfiguration();                  // Returns the SSD configuration

  //StSvtHybridObject* At(int index);  // needed for backward compatibility
  //void AddAt(StSvtHybridObject* object, int index);  // needed for backward compatibility

  StSsdHybridObject* getObject(int barrel, int ladder, int wafer); // Returns a object of the collection

protected:

  StSsdConfig* mSsdConfig;       //! SSD Configuration 
  TString mConfig;               // SSD Configuration 

  ClassDef(StSsdWaferCollection,1)
};

#endif

/***************************************************************************
 *
 * $Id: StSsdWaferCollection.hh,v 1.1 2004/03/12 04:24:20 jeromel Exp $
 *
 * $Log: StSsdWaferCollection.hh,v $
 * Revision 1.1  2004/03/12 04:24:20  jeromel
 * First version of SSD Util
 *
 *
 **************************************************************************/

/*!
 * \class   StSsdWaferCollection
 * \author christelle roy
 * 
 * SSD Wafer Array BASE class
 */

#ifndef STSSDWAFERCOLLECTION_HH
#define STSSDWAFERCOLLECTION_HH

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

  int getNumberOfLadders();
  int getNumberOfWafers();
  int getTotalNumberOfWafers();

  void setConfiguration(const char* config);       // Set the SSD configuration
  void setConfiguration(StSsdConfig* config);      // Set the SSD configuration
  StSsdConfig* getSsdConfig() {return mSsdConfig;} // Returns the SSD configuration object
  const char* getConfiguration();                  // Returns the SSD configuration


protected:

  StSsdConfig* mSsdConfig;       //! SSD Configuration 
  TString mConfig;               // SSD Configuration 

  ClassDef(StSsdWaferCollection,1)
};

#endif


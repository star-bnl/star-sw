/*!
 * \class  StSsdHybridCollection
 * \author christelle roy
 *
 * SSD Hybrid Array BASE class
 */

#ifndef STSSDHYBRIDCOLLECTION_HH
#define STSSDHYBRIDCOLLECTION_HH

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

  int getNumberOfLadders();
  int getNumberOfWafers();
  int getNumberOfHybrids();
  int getTotalNumberOfHybrids();

  void setConfiguration(const char* config);       /// Set the SSD configuration
  void setConfiguration(StSsdConfig* config);      /// Set the SSD configuration
  StSsdConfig* getSsdConfig() {return mSsdConfig;} /// Returns the SSD configuration object
  const char* getConfiguration();                  /// Returns the SSD configuration

  StSsdHybridObject* getObject(int barrel, int ladder, int wafer, int hybrid); 

protected:

  StSsdConfig* mSsdConfig;    //! SSD Configuration 
  TString mConfig;            //  SSD Configuration 

  ClassDef(StSsdHybridCollection,1)
};

#endif


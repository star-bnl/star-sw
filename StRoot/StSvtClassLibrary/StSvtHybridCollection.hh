/***************************************************************************
 *
 * $Id: StSvtHybridCollection.hh,v 1.3 2000/11/30 20:39:12 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Array BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridCollection.hh,v $
 * Revision 1.3  2000/11/30 20:39:12  caines
 * Changed to allow us of database
 *
 **************************************************************************/

#ifndef STSVTHYBRIDCOLLECTION_HH
#define STSVTHYBRIDCOLLECTION_HH

#define MAX_NUMBER_OF_BARRELS  3

#include "StArray.h"

class StSvtHybridObject;
class StSvtConfig;
class TString;

class StSvtHybridCollection: public StObjArray
{
public:
  StSvtHybridCollection();
  StSvtHybridCollection(StSvtConfig* config);
  StSvtHybridCollection(const char* config);
  virtual ~StSvtHybridCollection();

  int getNumberOfBarrels();
  int getNumberOfLadders(int barrel);
  int getNumberOfWafers(int barrel);
  int getNumberOfHybrids();
  int getTotalNumberOfHybrids();
  int getHybridIndex(int barrel, int ladder, int wafer, int hybrid);

  void setConfiguration(const char* config); // Set the SVT configuration
  void setConfiguration(StSvtConfig* config); // Set the SVT configuration
  StSvtConfig* getSvtConfig() {return mSvtConfig;} // Returns the SVT configuration object
  const char* getConfiguration(); // Returns the SVT configuration

  //StSvtHybridObject* At(int index);  // needed for backward compatibility
  //void AddAt(StSvtHybridObject* object, int index);  // needed for backward compatibility

  StSvtHybridObject* getObject(int barrel, int ladder, int wafer, int hybrid); // Returns a object of the collection

protected:

  StSvtConfig* mSvtConfig;    //! SVT Configuration 
  TString mConfig;               // SVT Configuration 

  ClassDef(StSvtHybridCollection,1)
};

#endif

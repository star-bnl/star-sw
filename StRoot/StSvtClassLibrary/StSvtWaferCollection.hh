/***************************************************************************
 *
 * $Id: StSvtWaferCollection.hh,v 1.2 2007/03/21 17:22:21 fisyak Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Wafer Array BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtWaferCollection.hh,v $
 * Revision 1.2  2007/03/21 17:22:21  fisyak
 * Ivan Kotov's drift velocities, use TGeoHMatrix for coordinate transformation
 *
 * Revision 1.1  2001/08/16 21:02:04  munhoz
 * changing StObjArray to StStrArray. StSvtConfig reestructured. New classes for geometry DB
 *
 *
 **************************************************************************/

#ifndef STSVTWAFERCOLLECTION_HH
#define STSVTWAFERCOLLECTION_HH

#define MAX_NUMBER_OF_BARRELS  3

#include "StArray.h"

class StSvtHybridObject;
class StSvtConfig;
class TString;

class StSvtWaferCollection: public StRefArray
{
public:
  StSvtWaferCollection();
  StSvtWaferCollection(StSvtConfig* config);
  StSvtWaferCollection(const char* config);
  virtual ~StSvtWaferCollection();

  int getNumberOfBarrels();
  int getNumberOfLadders(int barrel);
  int getNumberOfWafers(int barrel);
  int getTotalNumberOfWafers();
  int getWaferIndex(int barrel, int ladder, int wafer);

  void setConfiguration(const char* config); // Set the SVT configuration
  void setConfiguration(StSvtConfig* config); // Set the SVT configuration
  StSvtConfig* getSvtConfig() {return mSvtConfig;} // Returns the SVT configuration object
  const char* getConfiguration(); // Returns the SVT configuration

  //StSvtHybridObject* At(int index);  // needed for backward compatibility
  //void AddAt(StSvtHybridObject* object, int index);  // needed for backward compatibility

  StSvtHybridObject* getObject(int barrel, int ladder, int wafer); // Returns a object of the collection

protected:

  StSvtConfig* mSvtConfig;    //! SVT Configuration 
  TString mConfig;               // SVT Configuration 

  ClassDef(StSvtWaferCollection,1)
};

#endif

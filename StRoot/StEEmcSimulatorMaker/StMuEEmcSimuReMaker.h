#ifndef __StMuEEmcSimuReMaker__
#define __StMuEEmcSimuReMaker__

#include "StMaker.h"
#include <TString.h>

#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"



class StEEmcDbMaker;
class StMuEmcCollection;

class StMuEEmcSimuReMaker : public StMaker {

 public:

  StMuEEmcSimuReMaker( const Char_t *myName = "muEEmcSimu" );
  ~StMuEEmcSimuReMaker() { /* nada */ };

  Int_t Init();
  Int_t Make();  

  void MakeTowers( StMuEmcCollection *e );
  //  void MakePreshower( StMuEmcCollection *e );
  void MakeSmd( StMuEmcCollection *e );

  void setDbName( const Char_t *name ) { mDbName = name; }

 private:

  TString mDbName;
  StEEmcDbMaker *mEEmcDb;

  Float_t mTowerGains[kEEmcNumEtas]; // array of tower gains
  Float_t mSampFrac;                 // Sampling fraction

  Int_t mMaxAdc;                     // Maximum ADC 
  Int_t mMaxET;                      // Maximum E_T

 protected:

  ClassDef(StMuEEmcSimuReMaker,1);

};

#endif

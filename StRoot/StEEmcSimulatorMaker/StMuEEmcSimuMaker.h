#ifndef __StMuEEmcSimuMaker__
#define __StMuEEmcSimuMaker__

#include "StMaker.h"
#include <TString.h>

#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"



class StEEmcDbMaker;
class StMuEmcCollection;

class StMuEEmcSimuMaker : public StMaker {

 public:

  StMuEEmcSimuMaker( const Char_t *myName = "muEEmcSimu" );
  ~StMuEEmcSimuMaker() { /* nada */ };

  Int_t Init();
  Int_t Make();  

  void MakeTowers( StMuEmcCollection *e );
  //  void MakePreshower( StMuEmcCollection *e );
  void MakeSmd( StMuEmcCollection *e );

  void setDbName( const Char_t *name ) { mDbName = name; }

 private:

  TString mDbName;
  StEEmcDbMaker *mEEmcDb;

  Float_t mTowerGains[kEEmcNumEtas]; // MC tower gains
  Float_t mSampFrac;                 // Sampling fraction

 protected:

  ClassDef(StMuEEmcSimuMaker,1);

};

#endif

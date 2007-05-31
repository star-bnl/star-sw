#ifndef __StGammaRawMaker_h__
#define __StGammaRawMaker_h__

#include "StMaker.h"

#include "StGammaTrack.h"
#include "StGammaTower.h"
#include "StGammaStrip.h"

#include "StEmcUtil/database/StBemcTables.h"

#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"

class EEmcGeomSimple;
class StBemcTables;
class StMuTrack;

class StGammaRawMaker : public StMaker {

 public:
  StGammaRawMaker( const Char_t *name="grawmaker" );
  ~StGammaRawMaker(){ /* nada */ };

  const StGammaTrackVec_t &tracks(){ return mTracks; }
  const StGammaTowerVec_t &towers(){ return mTowers; }
  const StGammaTowerVec_t &preshower1(){ return mPreshower1; }
  const StGammaTowerVec_t &preshower2(){ return mPreshower2; }
  const StGammaTowerVec_t &postshower(){ return mPostshower; }
  const StGammaStripVec_t &strips(){ return mStrips; }

  /// Sets minimum ET for a tower to be stored
  void SetTowerCutoff( Float_t t );
  /// Sets minimum ET for a track to be stored
  void SetTrackCutoff( Float_t t );

  /// given a tower id and a layer (as defined in StGammaTower.h), returns
  /// a pointer to the corresponding StGammaTower stored in StGammaEvent
  StGammaTower *tower( Int_t id, Int_t layer );

  /// given the sector (module), plane (as defined in StGammaStrip.h)
  /// and index of the strip, returns a pointer to the corresponding
  /// StGammaStrip stored in the StGammaEvent.
  StGammaStrip *strip( Int_t sector, Int_t plane, Int_t index );

 protected:

  Float_t mTowerCutoff;// ET
  Float_t mTrackCutoff;// ET
  
  StGammaTrackVec_t mTracks; // stores all tracks which pass QA
  StGammaTowerVec_t mTowers; // stores all towers which pass QA
  StGammaStripVec_t mStrips; // stores all strips which pass QA

  StGammaTowerVec_t mPreshower1;
  StGammaTowerVec_t mPreshower2;
  StGammaTowerVec_t mPostshower;

  void GetEndcap();
  void GetBarrel();
  void GetTracks();

  Bool_t Accept( StGammaTrack &track );
  Bool_t Accept( StGammaTower &tower );
  Bool_t Accept( StGammaStrip &strip );
  Bool_t Accept( StMuTrack *track );

  EEmcGeomSimple *mEEmcGeometry;

  StBemcTables *tables;
  Bool_t mCorrupt;

  
  // store pointers to towers and strips for easier matching to clusters

  StGammaTower *mEEtowers[ kEEmcNumSectors * kEEmcNumSubSectors * kEEmcNumEtas ][ 4 ];
  StGammaStrip *mEEstrips[ kEEmcNumSectors ][ kEEmcNumSmdUVs     ][ kEEmcNumStrips ];

  StGammaTower* mBarrelEmcTower[4801];
  StGammaTower* mBarrelEmcPreshower[4801];

  map<int, StGammaStrip*> mBarrelSmdEtaStrip;
  map<int, StGammaStrip*> mBarrelSmdPhiStrip;
  
 public:

  Int_t Init();
  Int_t Make();
  void  Clear(Option_t *opts="");
  
  ClassDef(StGammaRawMaker,1);

};

#endif

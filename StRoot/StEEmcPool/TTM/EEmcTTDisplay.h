// Hey Emacs this is -*-c++-*-
#ifndef STAR_EEmcTTDisplay
#define STAR_EEmcTTDisplay
// $Id: EEmcTTDisplay.h,v 1.3 2004/01/26 21:51:53 zolnie Exp $

/*!
 *                                                                     
 * \class  EEmcTTDisplay
 * \author Piotr A. Zolnierczuk
 * \date   2003/12/08
 *
 * \brief  EEMC tower display
 *
 */                                                                      

#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

class TGeoVolume;
class TGeoMatrix;
class TList;
//
class StMuTrack;
class EEmcTower;

class EEmcTTDisplay : public EEmcGeomSimple {
public: 
  EEmcTTDisplay(const char *name="eemc");
  ~EEmcTTDisplay();
  
  TGeoVolume* operator() () { return mEEmc; };
  TGeoVolume* getVolume()   { return mEEmc; };

  Bool_t       towerHit(const char *tile);
  Bool_t       towerHit(int sec, int subsec, int eta) {   return towerHit(volumeName(sec,subsec,eta)); }  
  Bool_t       towerHit(const EEmcTower& tower)       ;

  Bool_t       trackHit(Double_t x, Double_t y, Double_t z, Double_t px, Double_t py, Double_t pz, Double_t qB);
  Bool_t       trackHit(const StMuTrack& track);

  void         DrawHits();
  void         Clear(const Option_t *option);
  void         Out(ostream &out, const StMuTrack &track, const EEmcTower &tower);

protected:
  void  initGeometry(const char *topName);
  char *volumeName(int sec, int sub=-1, int eta=-1);

  TGeoVolume* mEEmc;  
  TList *mTowerHits;
  TList *mTrackHits;
public:
  ClassDef(EEmcTTDisplay, 1)   // 
};


#endif


// $Log: EEmcTTDisplay.h,v $
// Revision 1.3  2004/01/26 21:51:53  zolnie
// shorter names
//
// Revision 1.2  2004/01/26 21:08:31  zolnie
// working track/tower display (before big farewell cleanup)
//
// Revision 1.1  2004/01/19 22:07:50  zolnie
// toward track/tower display
//


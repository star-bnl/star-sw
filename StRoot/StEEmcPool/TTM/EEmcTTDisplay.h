// Hey Emacs this is -*-c++-*-
#ifndef STAR_EEmcTTDisplay
#define STAR_EEmcTTDisplay
// $Id: EEmcTTDisplay.h,v 1.4 2004/01/26 22:54:15 zolnie Exp $

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
  /// the constructor
  EEmcTTDisplay(const char *name="eemc");
  /// the destructor
  ~EEmcTTDisplay();
  
  /// returns top TGeoVolume 
  TGeoVolume* operator() () { return mEEmc; }; 
  /// returns top TGeoVolume 
  TGeoVolume* getVolume()   { return mEEmc; }; 

  /// adds tower hit
  Bool_t       towerHit(const char *tile);

  /// adds tower hit
  Bool_t       towerHit(int sec, int subsec, int eta) {   return towerHit(volumeName(sec,subsec,eta)); }  

  /// adds tower hit
  Bool_t       towerHit(const EEmcTower& tower)       ;

  /// adds track hit
  Bool_t       trackHit(Double_t x, Double_t y, Double_t z, Double_t px, Double_t py, Double_t pz, Double_t qB);
  /// adds track hit
  Bool_t       trackHit(const StMuTrack& track);

  /// draws hits (towers/tracks)
  void         DrawHits();

  /// clears hits (towers/tracks)
  void         Clear(const Option_t *option);
  /// prints StMuTrack and EEmcTower to ostream
  void         Out(ostream &out, const StMuTrack &track, const EEmcTower &tower);

protected:
  void  initGeometry(const char *topName);
  char *volumeName(int sec, int sub=-1, int eta=-1);

  TGeoVolume* mEEmc; /**<- top TGeoVolume */
  TList *mTowerHits; /**<- number of tracks */
  TList *mTrackHits; /**<- number of tracks */
public:
  ClassDef(EEmcTTDisplay, 1)   // 
};


#endif


// $Log: EEmcTTDisplay.h,v $
// Revision 1.4  2004/01/26 22:54:15  zolnie
// after name cleanup
//
// Revision 1.3  2004/01/26 21:51:53  zolnie
// shorter names
//
// Revision 1.2  2004/01/26 21:08:31  zolnie
// working track/tower display (before big farewell cleanup)
//
// Revision 1.1  2004/01/19 22:07:50  zolnie
// toward track/tower display
//


// Hey Emacs this is -*-c++-*-
#ifndef STAR_EETowDisplay
#define STAR_EETowDisplay
// $Id: EEmcTTDisplay.h,v 1.1 2004/01/19 22:07:50 zolnie Exp $

/*!
 *                                                                     
 * \class  EETowDisplay
 * \author Piotr A. Zolnierczuk
 * \date   2003/12/08
 *
 * \brief  EEMC tower display
 *
 */                                                                      


//#include "TGeoVolume.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
class TGeoVolume;
class TGeoMatrix;


class EETowDisplay : public EEmcGeomSimple {
public: 
  EETowDisplay(const char *name="eemc");
  ~EETowDisplay() {};
  
  TGeoVolume* operator() () { return mEEmc; };
  TGeoVolume* getVolume()   { return mEEmc; };

  void        towerHit(int eta);
  
protected:
  TGeoVolume* mEEmc;
public:
  ClassDef(EETowDisplay, 1)   // 
};


#endif


// $Log: EEmcTTDisplay.h,v $
// Revision 1.1  2004/01/19 22:07:50  zolnie
// toward track/tower display
//


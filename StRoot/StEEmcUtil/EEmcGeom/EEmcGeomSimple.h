// Hey Emacs this is really -*-c++-*- ! 
// \class  EEmcGeomSimple
// \author Piotr A. Zolnierczuk             
// \date   Jan 14, 2003
#ifndef EEmcGeomSimple_h
#define EEmcGeomSimple_h
/*********************************************************************
 * $Id: EEmcGeomSimple.h,v 1.2 2003/01/16 23:04:05 zolnie Exp $
 *********************************************************************
 * Description:
 * STAR Endcap Electromagnetic Calorimeter Simple Geometry Class
 *********************************************************************
 * $Log: EEmcGeomSimple.h,v $
 * Revision 1.2  2003/01/16 23:04:05  zolnie
 * added more functionality
 *
 * Revision 1.1  2003/01/16 19:33:51  zolnie
 * added a simple Geom class to conver a track hit -> tower hit
 *
 *********************************************************************/
#include "TObject.h"

#include "EEmcDefs.h"

class StThreeVectorD;
class StEmcRawHit;
class StTrack;

const int kEEmcGeomZRangeErr   = -1;
const int kEEmcGeomEtaRangeErr = -2;

class  EEmcGeomSimple : public TObject { 
public:
  EEmcGeomSimple();
  virtual ~EEmcGeomSimple();
  
  int   getHit(const StThreeVectorD& r,        StEmcRawHit& hit); // given r point return EmcRawHit 
  int   getHit(const StTrack& track, double z, StEmcRawHit& hit); // a wrapper for the above when 
                                                                  //         a track & z is given

  float getEtaBin(int eta) {  // return the "mean" value of an eta bin
    if(eta<1 && mNumEta<eta) return (-1.0);
    return 0.5 * ( mEtaBin[eta] + mEtaBin[eta+1] );
  }
  float getEtaBinWidth(int eta) { // return the "half-width" of an eta bin
    if(eta<1 && mNumEta<eta) return (-1.0);
    return 0.5 * ( mEtaBin[eta] - mEtaBin[eta+1] );
  }

protected:  
  Float_t  mZ1   ;   // z1  
  Float_t  mZ2   ;   // z2
  Float_t *mEtaBin;  // eta bins [0..mNumEta]
  Int_t    mNumEta;  // number of eta bins 
  Int_t    mNumSec;  // number of sectors    (in phi)
  Int_t    mNumSSec; // number of subsectors (in phi)
  Float_t  mPhi0;    // phi0 of the 0th sector 
  Int_t    mClock;   // +1 == clockwise  -1 == counter-clockwise


private:
  void InitDefaults();
  
  ClassDef(EEmcGeomSimple,1)   // STAR Endcap Electromagnetic Calorimeter Simple Geometry Class
   
};

#endif


// Hey Emacs this is really -*-c++-*- ! 
// \class  EEmcGeomSimple
// \author Piotr A. Zolnierczuk             
// \date   Jan 14, 2003
#ifndef EEmcGeomSimple_h
#define EEmcGeomSimple_h
/*********************************************************************
 * $Id: EEmcGeomSimple.h,v 1.1 2003/01/16 19:33:51 zolnie Exp $
 *********************************************************************
 * Description:
 * STAR Endcap Electromagnetic Calorimeter Simple Geometry Class
 *********************************************************************
 * $Log: EEmcGeomSimple.h,v $
 * Revision 1.1  2003/01/16 19:33:51  zolnie
 * added a simple Geom class to conver a track hit -> tower hit
 *
 *********************************************************************/
#include "TObject.h"

#include "EEmcDefs.h"

class StThreeVectorD;
class StEmcRawHit;


class  EEmcGeomSimple : public TObject { 
public:
 
  EEmcGeomSimple();
  virtual ~EEmcGeomSimple();
  
  int   getHit(const StThreeVectorD& r,  StEmcRawHit &t);
  float getEtaBin(int eta) { 
    if(eta<1 && mNumEta<eta) return (-1.0);
    return 0.5 * ( mEtaBin[eta] + mEtaBin[eta+1] );
  }
  float getEtaBinWidth(int eta) { 
    if(eta<1 && mNumEta<eta) return (-1.0);
    return 0.5 * ( mEtaBin[eta] - mEtaBin[eta+1] );
  }

protected:  
  Float_t  mZ1   ;
  Float_t  mZ2   ;
  Float_t *mEtaBin;
  Int_t    mNumEta;
  Int_t    mNumSec;
  Int_t    mNumSSec;
  Float_t  mPhi0;
  Int_t    mClock;


private:
  void InitDefaults();
  
  ClassDef(EEmcGeomSimple,1)   // STAR Endcap Electromagnetic Calorimeter Simple Geometry Class
   
};



#endif


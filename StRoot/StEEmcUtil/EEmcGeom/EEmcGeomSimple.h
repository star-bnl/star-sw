// Hey Emacs this is really -*-c++-*- ! 
// \class  EEmcGeomSimple
// \author Piotr A. Zolnierczuk             
// \date   Jan 14, 2003
#ifndef EEmcGeomSimple_h
#define EEmcGeomSimple_h
/*********************************************************************
 * $Id: EEmcGeomSimple.h,v 1.4 2003/01/19 03:47:11 zolnie Exp $
 *********************************************************************
 * Description:
 * STAR Endcap Electromagnetic Calorimeter Simple Geometry Class
 *********************************************************************
 * $Log: EEmcGeomSimple.h,v $
 * Revision 1.4  2003/01/19 03:47:11  zolnie
 * still further improvements
 *
 * Revision 1.3  2003/01/18 02:35:54  zolnie
 * further modifications
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


class  EEmcGeomSimple : public TObject { 
public:
  enum Chiral_t { CounterClockwise=-1, Clockwise=1};

  EEmcGeomSimple();
  virtual ~EEmcGeomSimple();


  
  Int_t getHit(const StThreeVectorD& r,          StEmcRawHit& hit);//given r point return EmcRawHit
  Int_t getHit(const StTrack& track, Double_t z, StEmcRawHit& hit);//a wrapper for the above when
                                                                   //         a track & z is given

  inline Float_t getZ1() const { return mZ1; };
  inline Float_t getZ2() const { return mZ2; };


  // return the "lower"  edge of the eta bin
  inline Float_t getEtaBin(Int_t eta) const {  
    if(eta<0  || mNumEta<mNumEta) return (-1.0);
    return mEtaBin[eta];
  }

  // return the "mean" value of an eta bin
  inline Float_t getEtaBinMean(Int_t eta) const {
    if(eta<0  || mNumEta<=mNumEta) return (-1.0);
    return 0.5 * ( mEtaBin[eta] + mEtaBin[eta+1] );
  }

  // return the "half-width" of an eta bin
  inline Float_t getEtaBinWidth(Int_t eta) const { 
    if(eta<0  || mNumEta<=mNumEta) return (-1.0);
    return 0.5 * ( mEtaBin[eta] - mEtaBin[eta+1] );
  }

  inline Int_t   getNumberOfEtas()       const { return mNumEta;  }
  inline Int_t   getNumberOfSectors()    const { return mNumSec;  }
  inline Int_t   getNumberOfSubSectors() const { return mNumSSec; } 
  inline Float_t getPhi0() const { return mPhi0; };
  inline Bool_t  isClockwise() const        { return ( mClock == Clockwise        ); };
  inline Bool_t  isCounterClockwise() const { return ( mClock == CounterClockwise ); }

protected:  
  Float_t  mZ1   ;   // z1  
  Float_t  mZ2   ;   // z2
  Float_t *mEtaBin;  // eta bins [0..mNumEta]
  Int_t    mNumEta;  // number of eta bins 
  Int_t    mNumSec;  // number of sectors    (in phi)
  Int_t    mNumSSec; // number of subsectors (in phi)
  Float_t  mPhi0;    // phi0 of the 0th sector 
  Chiral_t mClock;   // +1 == clockwise  -1 == counter-clockwise

  void    useDefaultGeometry();

private:

  
  ClassDef(EEmcGeomSimple,1)   // STAR Endcap Electromagnetic Calorimeter Simple Geometry Class
   
};

#endif


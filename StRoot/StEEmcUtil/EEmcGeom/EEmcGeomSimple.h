// Hey Emacs this is really -*-c++-*- ! 
// \class  EEmcGeomSimple
// \author Piotr A. Zolnierczuk             
// \date   Jan 14, 2003
#ifndef EEmcGeomSimple_h
#define EEmcGeomSimple_h
/*********************************************************************
 * $Id: EEmcGeomSimple.h,v 1.8 2003/03/06 18:54:21 zolnie Exp $
 *********************************************************************
 * Description:
 * STAR Endcap Electromagnetic Calorimeter Simple Geometry Class
 *********************************************************************
 * $Log: EEmcGeomSimple.h,v $
 * Revision 1.8  2003/03/06 18:54:21  zolnie
 * improvements for track/tower matching
 *
 * Revision 1.7  2003/02/20 21:47:25  zolnie
 * *** empty log message ***
 *
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
#include "StThreeVectorD.hh"


class StEmcRawHit;
class StTrack;


class  EEmcGeomSimple : public TObject { 
public:
  enum Chiral_t { CounterClockwise=-1, Clockwise=1};

  EEmcGeomSimple();
  virtual ~EEmcGeomSimple();

  // 
  inline StThreeVectorD getTrackPoint (const StTrack&     track, Double_t z ) const ;

  inline StThreeVectorD getTowerCenter(const UInt_t sec, const UInt_t sub, const UInt_t etabin) const;
  inline StThreeVectorD getTowerCenter(const StEmcRawHit& hit               ) const; 
  
  
  //given point return tower as EmcRawHit
  Int_t   getHit   (const StThreeVectorD& point,       StEmcRawHit& hit)     const;
  // get an r^2 (in x-y plane) distance between a point and the tower center
  Float_t getR2Dist(const StThreeVectorD& point, const StEmcRawHit& hit)     const;
  // checks if point matches tower hit
  Bool_t  pointMatch(const StThreeVectorD& point, const StEmcRawHit& hit,
		     Float_t deta=0.0, Float_t dphi=0.0, Float_t dz=0.0)   
    const;

  //wrappers for the above a track & z is given
  // (implicitely assumed that the center of the world is at z==0) 
  inline Int_t getHit(const StTrack& track, Double_t z, StEmcRawHit& hit)           const {
    return getHit(getTrackPoint(track,z),hit);
  }
  inline Float_t getR2Dist(const StTrack& track, Double_t z,const StEmcRawHit& hit) const { 
    return getR2Dist(getTrackPoint(track,z),hit);
  }
  inline Bool_t trackMatch(const StTrack& track, Double_t z, 
			   const StEmcRawHit& hit, 
			   Float_t deta=0.0,Float_t dphi=0.0,Float_t dz=0.0) 
    const {
    return(pointMatch(getTrackPoint(track,z),hit,deta,dphi,dz));
  }

  
  inline Float_t getZ1()   const { return mZ1;  };
  inline Float_t getZ2()   const { return mZ2;  };
  inline Float_t getZSMD() const { return mZSMD;};

  
  // return the "mean" value of an eta bin
  inline Float_t getEtaMean(UInt_t eta) const {
    if(mNumEta<=eta) return (-1.0);
    return 0.5 * ( mEtaBin[eta] + mEtaBin[eta+1] );
  }

  // return the "half-width" of an eta bin
  inline Float_t getEtaHalfWidth(UInt_t eta) const { 
    if(mNumEta<=eta) return (-1.0);
    return 0.5 * fabs( mEtaBin[eta] - mEtaBin[eta+1] );
  }

  // return the mean value of phi
  inline Float_t getPhiMean(UInt_t sec, UInt_t ssec) const {
    double dPhi=2.0*M_PI/mNumSec;
    return mClock*(Float_t(sec)+(ssec+0.5)/mNumSSec)*dPhi+mPhi0;
  }
  // return the phi half-width of a subsector
  inline Float_t getPhiHalfWidth(UInt_t sec=0, UInt_t ssec=0) const {
    double dPhi=2.0*M_PI/mNumSec;
    return 0.5/mNumSSec*dPhi;
  }

  // return the mean value of phi
  inline Float_t getZMean() const {
    return 0.5*(mZ1+mZ2);
  }
  // return the phi half-width of a subsector
  inline Float_t getZHalfWidth() const {
     return 0.5*fabs(mZ1-mZ2);
  }



  inline Int_t   getNumberOfEtas()       const { return mNumEta;  }
  inline Int_t   getNumberOfSectors()    const { return mNumSec;  }
  inline Int_t   getNumberOfSubSectors() const { return mNumSSec; } 
  inline Float_t getPhi0()               const { return mPhi0; };
  inline Bool_t  isClockwise()           const { return ( mClock == Clockwise        ); };
  inline Bool_t  isCounterClockwise()    const { return ( mClock == CounterClockwise ); }

protected:  
  Float_t  mZ1   ;   // z1  
  Float_t  mZ2   ;   // z2
  Float_t  mZSMD ;
  Float_t *mEtaBin;  // eta bins [0..mNumEta]
  UInt_t   mNumEta;  // number of eta bins 
  UInt_t   mNumSec;  // number of sectors    (in phi)
  UInt_t   mNumSSec; // number of subsectors (in phi)
  Float_t  mPhi0;    // phi0 of the 0th sector 
  Chiral_t mClock;   // +1 == clockwise  -1 == counter-clockwise

  void    useDefaultGeometry();

private:
  ClassDef(EEmcGeomSimple,1)  // STAR Endcap Electromagnetic Calorimeter Simple Geometry Class
   
};

#endif


// Hey Emacs this is really -*-c++-*- ! 
// \class  EEmcGeomSimple
// \author Piotr A. Zolnierczuk             
// \date   Jan 14, 2003
#ifndef EEmcGeomSimple_h
#define EEmcGeomSimple_h
/*********************************************************************
 * $Id: EEmcGeomSimple.h,v 1.16 2004/05/20 21:12:08 zolnie Exp $
 *********************************************************************
 * Description:
 * STAR Endcap Electromagnetic Calorimeter Simple Geometry Class
 *********************************************************************
 * $Log: EEmcGeomSimple.h,v $
 * Revision 1.16  2004/05/20 21:12:08  zolnie
 * added a static instance of EEmcGeomSimple
 *
 * Revision 1.15  2004/01/26 21:12:19  zolnie
 * added one one more quick member getPhiMean with one argument
 * returns phi for a sector rather than a subsector
 *
 * Revision 1.14  2004/01/19 20:19:44  zolnie
 * added getEtaMin and getEtaMax inline members
 *
 * Revision 1.13  2003/05/23 22:13:04  zolnie
 * SUN does not like inlines (why??)
 *
 * Revision 1.12  2003/04/25 15:53:54  zolnie
 * always initalize
 *
 * Revision 1.11  2003/04/23 18:11:31  balewski
 * 'continous' eta & phi bins added
 *
 * Revision 1.10  2003/03/22 23:59:00  zolnie
 * standalone modifications
 *
 * Revision 1.9  2003/03/22 22:44:57  zolnie
 * make it standalone library
 *
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
#include "TVector3.h"


class  EEmcGeomSimple : public TObject { 
public:
  enum Chiral_t { CounterClockwise=-1, Clockwise=1, Undefined=0};

  EEmcGeomSimple();
  virtual ~EEmcGeomSimple();

  TVector3 getTowerCenter(const UInt_t  sec, const UInt_t sub, const UInt_t etabin) const;
  TVector3 getDirection  (const Float_t xetaBin, const Float_t xphiBin) const;
  
  inline Float_t getZ1()     const { return mZ1;  };
  inline Float_t getZ2()     const { return mZ2;  };
  inline Float_t getZSMD()   const { return mZSMD;};
  inline Float_t getEtaMin() const { return mEtaBin[0];       };
  inline Float_t getEtaMax() const { return mEtaBin[mNumEta]; };
  
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

  // return the mean value of phi (subsector)
  inline Float_t getPhiMean(UInt_t sec) const {
    double dPhi=2.0*M_PI/mNumSec;
    return mClock*(sec+0.5)*dPhi+mPhi0;
  }
  // return the mean value of phi (subsector)
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

  static EEmcGeomSimple& Instance() { return sInstance; } 

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
  static EEmcGeomSimple sInstance; //! 

  ClassDef(EEmcGeomSimple,2)  // STAR Endcap Electromagnetic Calorimeter Simple Geometry Class
   
};

#endif


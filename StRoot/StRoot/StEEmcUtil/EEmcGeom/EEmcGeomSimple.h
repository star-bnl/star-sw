// Hey Emacs this is really -*-c++-*- ! 
// \class  EEmcGeomSimple
// \author Piotr A. Zolnierczuk             
// \date   Jan 14, 2003
#ifndef EEmcGeomSimple_h
#define EEmcGeomSimple_h
/*********************************************************************
 * $Id: EEmcGeomSimple.h,v 1.27 2010/08/26 22:48:47 ogrebeny Exp $
 *********************************************************************
 * Description:
 * STAR Endcap Electromagnetic Calorimeter Simple Geometry Class
 *********************************************************************
 */


#include "TObject.h"
#include "TVector3.h"
#include "TMath.h"
#include <math.h>
#include <StMessMgr.h>

class  EEmcGeomSimple : public TObject { 
public:

  /// chirality defined
  enum Chiral_t { CounterClockwise=-1, Clockwise=1, Undefined=0};

  /// default constructor
  EEmcGeomSimple();
  /// the destructor
  virtual ~EEmcGeomSimple();

  /// gets EEMC tower center given sector,subsector and eta indices (0-offset) 
  /// \param sec     sector index    [0,mNumSec )
  /// \param sub     subsector index [0,mNumSSec)
  /// \param etabin  tile/eta index  [0,mNumEta )
  /// \return tower center as TVector3
  TVector3 getTowerCenter(const UInt_t  sec, const UInt_t sub, const UInt_t etabin) const;

  /// gets 'direction' vector from (0,0,0) toward a point on EEMC.
  /// detaBin is defined from [-0.5,11.5).  [-0.5,0.5) returns a point
  /// within etabin 1, [0.5,1.5) within etabin 2, etc...  dphiBin is
  /// defined similarly.
  /// \param detaBin [-0.5,11.5)
  /// \param dphiBin [-0.5,59.5)
  /// \return direction as TVector3
  TVector3 getDirection  (const Float_t detaBin, const Float_t dphiBin) const;

  /// gets tower ID given 'direction' vector r (only eta and phi are relevant, z is ignored)
  /// \param  r   - direction vecrot
  /// \param  sec     sector index    [0,mNumSec )
  /// \param  sub     subsector index [0,mNumSSec)
  /// \param  etabin  tile/eta index  [0,mNumEta )
  /// \param  dphi    fractional distance from the tower center in units of phiHW=getPhiHalfWidth(sec,sub)
  /// \param  deta    fractional distance from the tower center in units of etaHW=getEtaHalfWidth(etabin)
  /// \return true    if r points toward a tower and false if does not
  bool     getTower(const TVector3& r, int &sec, int &sub, int &etabin, Float_t &dphi, Float_t &deta) const;

  /// gets tower ID given 'direction' vector r (only eta and phi are relevant, z is ignored)
  /// \param  r   - direction vecrot
  /// \param  sec     sector index    [0,mNumSec )
  /// \param  sub     subsector index [0,mNumSSec)
  /// \param  etabin  tile/eta index  [0,mNumEta )
  /// \return true    if r points toward a tower and false if does not
  bool     getTower(const TVector3& r, int &sec, int &sub, int &etabin) const { 
    Float_t dphi,deta;
    return getTower(r,sec,sub,etabin,dphi,deta);
  };

  const Float_t *getEtaBinRangeArray() const { return mEtaBin;}
  
  /// gets lower Z edge of EEMC (preshower)
  inline Float_t getZ1()     const { return mZ1;  };
  /// gets upper Z edge of EEMC (postshower)
  inline Float_t getZ2()     const { return mZ2;  };
  /// gets z-depth of the SMD layer in EEMC 
  inline Float_t getZSMD()   const { return mZSMD;};
  /// gets lower eta limit 
  inline Float_t getEtaMin() const { return mEtaBin[0];       };
  /// gets upper eta bound
  inline Float_t getEtaMax() const { return mEtaBin[mNumEta]; };
  
  /// returns the "mean" value of a tile (eta bin)
  /// \param eta tile index (eta bin) [0,mNumEta)
  inline Float_t getEtaMean(UInt_t eta) const {
    if (mNumEta<=eta) {
	LOG_ERROR << "getEtaHalfWidth: invalid eta index " << eta << endm;
	return 0;
    } else {
        return 0.5 * ( mEtaBin[eta] + mEtaBin[eta+1] );
    }
  }

  /// returns the "half-width" of a tile (eta bin)
  /// \param eta tile index (eta bin) [0,mNumEta)
  inline Float_t getEtaHalfWidth(UInt_t eta) const { 
    if(mNumEta<=eta) {
	LOG_ERROR << "getEtaHalfWidth: invalid eta index" << eta << endm;
	return 0;
    } else {
	return 0.5 * fabs( mEtaBin[eta] - mEtaBin[eta+1] );
    }
  }

  /// returns the center value of phi for a given sector
  /// \param sec sector index [0,mNumSec)
  inline Float_t getPhiMean(UInt_t sec) const {
    //const  double dPhi=2.0*M_PI/mNumSec;
    const  double dPhi= TMath::TwoPi()/mNumSec;
    if(mNumSec<=sec) {
	LOG_ERROR << "getPhiMean: invalid sector index" << sec << endm;
	return 0;
    } else {
        return AdjustAngle(mClock*(sec+0.5L)*dPhi+mPhi0);
    }
  }
  
  /// returns the center value of phi for a subsector
  /// \param sec  sector index    [0,mNumSec )
  /// \param ssec subsector index [0,mNumSSec)
  inline Float_t getPhiMean(UInt_t sec, UInt_t ssec) const {
    //const double dPhi=2.0*M_PI/mNumSec;
    const double dPhi=TMath::TwoPi()/mNumSec;
    if(mNumSec <=sec ) {
	LOG_ERROR << "getPhiMean: invalid sector index " << sec << endm;
	return 0;
    } else if(mNumSSec<=ssec) {
	LOG_ERROR << "getPhiMean: invalid subsector index " << ssec << endm;
	return 0;
    } else {
	return AdjustAngle(mClock*(Double_t(sec)+(ssec+0.5L)/mNumSSec)*dPhi+mPhi0);
    }
  }
  
  /// returns the half-width (in phi) of a subsector
  /// \param sec  sector index    [0,mNumSec )
  /// \param ssec subsector index [0,mNumSSec)
  inline Float_t getPhiHalfWidth(UInt_t sec=0, UInt_t ssec=0) const {
    //const double dPhi=2.0*M_PI/mNumSec;
    const double dPhi=TMath::TwoPi()/mNumSec;
    if(mNumSec <=sec ) {
	LOG_ERROR << "getPhiMean: invalid sector index " << sec << endm;
	return 0;
    } else if(mNumSSec<=ssec) {
	LOG_ERROR << "getPhiMean: invalid subsector index " << ssec << endm;
	return 0;
    } else {
        return (Float_t)(0.5L/mNumSSec*dPhi);
    }
  }

  /// returns the center of EEMC in z direction 
  inline Float_t getZMean() const {
    return (Float_t)(0.5L*(mZ1+mZ2));
  }
  
  /// returns the half-width of EEMC (in z-direction)
  inline Float_t getZHalfWidth() const {
    return (Float_t)(0.5L*fabs(mZ1-mZ2));
  }


  /// gets number of tiles (eta bins)
  inline Int_t   getNumberOfEtas()       const { return mNumEta;  }
  /// gets number of sectors 
  inline Int_t   getNumberOfSectors()    const { return mNumSec;  }
  /// gets number of subsectors
  inline Int_t   getNumberOfSubSectors() const { return mNumSSec; } 
  /// gets azimuthal angle of the  edge of first sector (index 0)
  /// the edge is 'upper' for counter-clockwise, and 'lower' for clockwise indexing
  inline Float_t getPhi0()               const { return mPhi0; };
  /// is endcap labeling clockwise?
  inline Bool_t  isClockwise()           const { return ( mClock == Clockwise        ); };
  /// is endcap labeling clockwise?
  inline Bool_t  isCounterClockwise()    const { return ( mClock == CounterClockwise ); }

  /// returns a reference to a static instance of EEmcGeomSimple
  static EEmcGeomSimple& Instance() { return sInstance; } 

  

protected:  
  Float_t  mZ1   ;   // z preshower)
  Float_t  mZ2   ;   // z postshower)
  Float_t  mZSMD ;   // z smd
  Float_t *mEtaBin;  // eta bins [0,mNumEta)
  UInt_t   mNumEta;  // number of eta bins 
  UInt_t   mNumSec;  // number of sectors    (in phi)
  UInt_t   mNumSSec; // number of subsectors (in phi)
  Double_t mPhi0;    // phi0 of the 0th sector 
  Chiral_t mClock;   // +1 == clockwise  -1 == counter-clockwise

  void    useDefaultGeometry();
  
  // adjust angle so it falls into [-pi,pi] interval
  static inline double AdjustAngle(double alpha) { 
    while(alpha<-TMath::Pi() ) alpha += TMath::TwoPi();
    while(alpha> TMath::Pi() ) alpha -= TMath::TwoPi();
    return alpha;
  }


private:
  static EEmcGeomSimple sInstance; //! 


  ClassDef(EEmcGeomSimple,2)  // STAR Endcap Electromagnetic Calorimeter Simple Geometry Class
   
};

#endif


/*********************************************************************
 * $Log: EEmcGeomSimple.h,v $
 * Revision 1.27  2010/08/26 22:48:47  ogrebeny
 * Improved constness
 *
 * Revision 1.26  2009/02/11 20:04:24  ogrebeny
 * 1. Fix the sectors initialization.
 * 2. Remove exceptions from the geom code.
 *
 * Revision 1.25  2007/07/12 19:30:14  fisyak
 * Add includes for ROOT 5.16
 *
 * Revision 1.24  2005/07/15 20:53:09  balewski
 * more get methods
 *
 * Revision 1.23  2005/02/07 19:44:37  jwebb
 * *** empty log message ***
 *
 * Revision 1.22  2005/02/07 19:26:09  jwebb
 * Clarified documentation for EEmcGeomSimple::getDirection(...).
 *
 * Revision 1.21  2005/02/05 00:57:38  perev
 * TMath is namespace now
 *
 * Revision 1.20  2004/06/03 20:59:54  zolnie
 * - phi angle now adjusted to [-pi,pi] interval in accordace to TVecror3 convention
 * - replaced Jan's interesting implementation of direction2tower method with
 * a resurrected getTower (formerly getHit) method see EEmcGeomSimple.h
 *
 * Revision 1.19  2004/06/01 21:20:49  balewski
 * direction2tower ()
 *
 * Revision 1.18  2004/05/25 15:32:37  zolnie
 * phi angles adjusted to [0,2pi] interval
 *
 * Revision 1.17  2004/05/24 18:33:40  zolnie
 * comment cleanup, added a small exception class
 * more argument checking, exception thrown when argument invalid
 *
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
 ********************************************************************
*/


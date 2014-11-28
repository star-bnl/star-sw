// $Id: EEmcGeomSimple.cxx,v 1.26 2010/08/26 22:48:47 ogrebeny Exp $
/// \author Piotr A. Zolnierczuk, Indiana University Cyclotron Facility
/// \date   Jan 14, 2003
/// doxygen info here
/** 
 * \class  EEmcGeomSimple
 * \brief  EEMC simple geometry 
 *
 */

#include <cmath>
#include <iostream>

#include "TMath.h"
#include "TVector3.h"

#if 0
#include "SystemOfUnits.h"
#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"

#include "StEmcRawHit.h"
#include "StTrackGeometry.h"
#include "StTrack.h"
#endif

#include "EEmcGeomDefs.h"
#include "EEmcGeomSimple.h"

// ######################################################################
//         *** WARNING NOT TESTED FOR mClock==1 (clock-wise) ***
// see EEmcGeomSimple.h for function documentation
// ######################################################################
ClassImp(EEmcGeomSimple)


// single instance of EEmcGeomSimple
EEmcGeomSimple EEmcGeomSimple::sInstance;

//

//
EEmcGeomSimple::EEmcGeomSimple() 
    : TObject()
{
  // always initialize
  mEtaBin  = NULL;
  mNumSec  = 0; 
  mNumSSec = 0; 
  mNumEta  = 0; 

  mZ1     =  0.0; 
  mZ2     =  0.0; 
  mZSMD   =  0.0; 
  mPhi0   =  0.0; 
  mClock  =  Undefined; 

  useDefaultGeometry();
}

// 
EEmcGeomSimple::~EEmcGeomSimple() 
{
  if(mEtaBin) delete [] mEtaBin;
}

//
// default geometry
// counter-clockwise (actual) Endcap (mClock==-1)
//     3'clock [2] :  center at   0  deg
//     6'clock [5] :  center at  -90 deg
//     9'clock [8] :  center at -180 deg
//    12'clock [11]:  center at -270 deg
void
EEmcGeomSimple::useDefaultGeometry() 
{
  // default EtaBins 2.0 -> 1.086
  // the first 13 entries mark the bounds of the 12 eta Bins.  14th value is not used
  static const Float_t defaultEtaBin[] = {
    2.0    , 
    1.9008 , 1.8065 , 1.7168 , 1.6317 , 1.5507 , 1.4738 ,
    1.4007 , 1.3312 , 1.2651 , 1.2023 , 1.1427 , 1.086  ,
    0.0
  };

  mNumSec  = kEEmcNumSectors;
  mNumSSec = kEEmcNumSubSectors;
  mNumEta  = kEEmcNumEtas;

  // fill in eta boundaries
  if(mEtaBin) delete [] mEtaBin;
  mEtaBin = new Float_t[mNumEta+1];
  for(UInt_t i=0;i<=mNumEta && defaultEtaBin[i]>0.0 ;i++) mEtaBin[i] = defaultEtaBin[i];

  mZ1     =  kEEmcZPRE1; // preshower
  mZ2     =  kEEmcZPOST; // postshower
  mZSMD   =  kEEmcZSMD;  // 
  mPhi0   =  75.0*M_PI/180.0;  // first sector (1 o'clock) phi value [index 0]
  mClock  =  CounterClockwise; // indexing goes counter-clockwise 
}


TVector3 
EEmcGeomSimple::getTowerCenter(const UInt_t sec, const UInt_t sub, const UInt_t etabin) const 
{
  Double_t phi =  0.0;
  Double_t eta = -1.0;
  Double_t z   =  0.0;
  Double_t rho =  0.0;

  phi  = getPhiMean(sec,sub);
  eta  = getEtaMean(etabin);
  if(eta<0.0) {
    LOG_ERROR << "invalid eta " << eta << endm;
    return TVector3(0, 0, 0);
  } else {
    z     = getZMean();
    rho   = z*tan(2.0*atan(exp(-1.0*eta)));  
    // create vector pointing toward the center of the tower
    return TVector3(rho*cos(phi),rho*sin(phi),z);
  }
}

TVector3 
EEmcGeomSimple::getDirection(const Float_t xetaBin, const Float_t xphiBin) const
{
  int ietaBin=(int)(xetaBin+0.5);
  int iphiBin=(int)(xphiBin+0.5);

  int isec=iphiBin/kEEmcNumSubSectors;
  int isub=iphiBin%kEEmcNumSubSectors;

  // note the higher etaBin the smaller eta,
  //      the larger sec/sub the smaller phi-angle
  Double_t  phi   = getPhiMean(isec,isub) - (xphiBin-iphiBin)*2*getPhiHalfWidth(isec,isub) ;
  Double_t  eta   = getEtaMean(ietaBin)   - (xetaBin-ietaBin)*2*getEtaHalfWidth(ietaBin);
  if(eta<0.0) {
    LOG_ERROR << "invalid eta " << eta << endm;
    return TVector3(0, 0, 0);
  } else {
    Double_t  z     = getZMean();
    Double_t  rho   = z/sinh(eta);  

    // create vector pointing toward's the point in the tower
    return TVector3(rho*cos(phi),rho*sin(phi),z);
  }
}





// =========================================================================
// gets a hit vector r checks if inside the EEmc
// and returns sector (0..mNumSec-1), subsector (0..mNumSSec-1) 
// and eta(0..mNumEta)
// =========================================================================
bool
EEmcGeomSimple::getTower(const TVector3& r, 
			 int     &sec , int     &sub, int    &eta,
			 Float_t &dphi, Float_t &deta) const
{
  const double dPhiSec  = 2.0*M_PI/mNumSec; // phi width of a sector
  const double dPhiSub  = dPhiSec/mNumSSec; // phi width of a subsector

  // some shorcuts
  // double rZ  = r.Z();
  double  rEta  = r.Eta();
  double  rPhi  = r.Phi();
  double  rPhi0 = r.Phi() - mPhi0;

  sec=sub=eta=-1; // set invalid values

  // check if inside EEMC
  //if(rZ  <mZ1            || mZ2<rZ          ) return false; // do not check the z-depth
  // FIXME assumes that mEtaBin[i] decreas monotonically with increasing i
  if(rEta<mEtaBin[mNumEta] || mEtaBin[0]<rEta ) return false; 

  // ------------------------------------------------------------------------  
  // get the eta index
  // FIXME assumes that mEtaBin[i] decreas monotonically with increasing i
  // TODO use bisection for (slightly) faster search
  for(eta=mNumEta;eta>=0;eta--) if(rEta<mEtaBin[eta]) break;
#if 0 /* use bisection */
  int ek=0;
  int el=mNumEta;
  eta=(ek+el)/2;
  while(ek!=eta) {  
    if( mEtaBin[eta]<rEta) 
      el=eta;
    else
      ek=eta;
    eta=(ek+el)/2;
  } 
#endif
  
  // ------------------------------------------------------------------------
  // get the sector index
  int   k = isClockwise() ? (int)floor(rPhi0/dPhiSec) : (int) ceil(rPhi0/dPhiSec);
  sec = mClock*k;
  while(sec<0) sec+=mNumSec;           // adjust the numbers to [0,mNumSec)
  sec %= mNumSec; // 

  // ------------------------------------------------------------------------
  // get the subsector index
  int   m = isClockwise() ? (int)floor(rPhi0/dPhiSub) : (int) ceil(rPhi0/dPhiSub);
  sub = mClock*m;
  while(sub<0) sub+=mNumSSec*mNumSec;  // adjust the numbers to [0,mNumSec)
  sub %= mNumSSec;// 

 
  // -------------------------------------------------
  // these are (very) fast inline's
  float xxx=getPhiMean(sec,sub) - rPhi;
  if(xxx>TMath::Pi()) xxx=TMath::TwoPi()-xxx;
  else if(xxx<-TMath::Pi()) xxx=TMath::TwoPi()+xxx;
  dphi =xxx/ getPhiHalfWidth(sec,sub);
  deta =(getEtaMean(eta)     - rEta ) / getEtaHalfWidth(eta);

  return true;
}


#if 0

// converts direction vector 'r' to sec/sub/eta bin. All counted from zero.
void 
EEmcGeomSimple::direction2tower( TVector3 r,
	     int &iSec, int &iSub, int &iEta, float &rPhi  , float &rEta )
{
  // printf("in GetTowNo() \n");
  
  //printf("intersection at x/y/z=%f/%f/%f\n",r.x(),r.y(),r.z());

  float eta=r.Eta();
  float phiDeg=180.*r.Phi()/3.14159;
  float phi=phiDeg -75;
  if(phi>0) phi-=360;
  phi=-phi;

  // printf("phiDeg=%f -->  phi=%f eta=%f\n",phiDeg,phi,eta);
  int ix=((int)phi)/6;
  iSec=ix/5;
  iSub=ix%5;
  rPhi   =phi-iSec*30-iSub*5 -2.5;

  Float_t *dEB= mEtaBin;
  iEta=-1;
  rEta=-999;
  for(int i=0;i<13;i++){
    // printf(" %d %f %f %d \n",i,eta,defaultEtaBin[i],iEta);
    if(eta<dEB[i]) continue;
    iEta=i-1;
    if(i>0 && i<=12) rEta= -(dEB[i]+dEB[i-1]-2*eta)/2./(dEB[i]-dEB[i-1]);
    break;
  }
  // printf("  ix=%d sec=%d sub=%c  eta=%d\n",ix,iSec+1,iSub+'A',iEta+1);

}


// compute the distance of a point from the center of a tower pointed by hit
Float_t 
EEmcGeomSimple::getR2Dist(const StThreeVectorD& point,const StEmcRawHit& hit)
  const 
{
  StThreeVectorD r = getTowerCenter(hit) - point;
  return r.mag2();
}

Bool_t  
EEmcGeomSimple::pointMatch(const StThreeVectorD& pt, const StEmcRawHit& hit,
			   Float_t deta, Float_t dphi, Float_t dz) const
{
  StThreeVectorD tc = getTowerCenter(hit);

  // check z
  if( fabs(tc.z()  -pt.z()  ) > (1.0+dz)*getZHalfWidth() )     return kFALSE; 
  // check phi
  if( fabs(tc.phi()-pt.phi()) > (1.0+dphi)*getPhiHalfWidth() ) return kFALSE;
  // finally check eta
  if( fabs(tc.pseudoRapidity()-pt.pseudoRapidity()) > 
      (1.0+deta)*getEtaHalfWidth(hit.eta()) ) return kFALSE;
  return kTRUE;
}




inline StThreeVectorD 
EEmcGeomSimple::getTowerCenter(const UInt_t sec, const UInt_t sub, const UInt_t etabin) const 
{
  Double_t  phi   = getPhiMean(sec,sub);
  Double_t  eta   = getEtaMean(etabin);
  if(eta<0.0) return StThreeVectorD();
  Double_t  z     = getZMean();
  Double_t  rho   = z*tan(2.0*atan(exp(-1.0*eta)));  

  // create vector pointing toward the center of the tower
  return StThreeVectorD(rho*cos(phi),rho*sin(phi),z);
}

inline StThreeVectorD 
EEmcGeomSimple::getTowerCenter(const StEmcRawHit &hit) const
{
  return getTowerCenter(hit.module(),hit.sub(),hit.eta());
}


inline StThreeVectorD 
EEmcGeomSimple::getTrackPoint(const StTrack& track, Double_t z) const 
{
  StPhysicalHelixD   helix = track.geometry()->helix();
  if(helix.dipAngle()<1e-13) return StThreeVectorD();
  double s  = ( z - helix.origin().z() ) / sin( helix.dipAngle())  ;
  return StThreeVectorD(helix.at(s));
} 

#endif


// $Log: EEmcGeomSimple.cxx,v $
// Revision 1.26  2010/08/26 22:48:47  ogrebeny
// Improved constness
//
// Revision 1.25  2009/02/11 20:37:36  ogrebeny
// *** empty log message ***
//
// Revision 1.24  2009/02/11 20:04:23  ogrebeny
// 1. Fix the sectors initialization.
// 2. Remove exceptions from the geom code.
//
// Revision 1.23  2005/04/29 03:06:03  balewski
// *** empty log message ***
//
// Revision 1.22  2004/06/03 20:59:54  zolnie
// - phi angle now adjusted to [-pi,pi] interval in accordace to TVecror3 convention
// - replaced Jan's interesting implementation of direction2tower method with
// a resurrected getTower (formerly getHit) method see EEmcGeomSimple.h
//
// Revision 1.21  2004/06/01 21:20:49  balewski
// direction2tower ()
//
// Revision 1.20  2004/05/25 15:32:36  zolnie
// phi angles adjusted to [0,2pi] interval
//
// Revision 1.19  2004/05/24 18:33:39  zolnie
// comment cleanup, added a small exception class
// more argument checking, exception thrown when argument invalid
//
// Revision 1.18  2004/05/20 21:12:07  zolnie
// added a static instance of EEmcGeomSimple
//
// Revision 1.17  2003/09/17 22:05:33  zolnie
// delete mumbo-jumbo
//
// Revision 1.16  2003/09/11 19:41:06  zolnie
// updates for gcc3.2
//
// Revision 1.15  2003/09/05 15:04:24  zolnie
// remove Stiostream/iostream from the source code
//
// Revision 1.14  2003/09/02 17:57:56  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.13  2003/07/01 14:13:25  balewski
// simplified formulano clue
//
// Revision 1.12  2003/05/23 22:13:04  zolnie
// SUN does not like inlines (why??)
//
// Revision 1.11  2003/04/25 15:53:52  zolnie
// always initalize
//
// Revision 1.10  2003/04/23 18:11:19  balewski
// 'continous' eta & phi bins added
//
// Revision 1.9  2003/03/22 22:44:57  zolnie
// make it standalone library
//
// Revision 1.8  2003/03/06 18:54:21  zolnie
// improvements for track/tower matching
//
// Revision 1.7  2003/02/20 21:47:25  zolnie
// *** empty log message ***
//
// Revision 1.4  2003/01/19 03:47:10  zolnie
// still further improvements
//
// Revision 1.3  2003/01/18 02:35:53  zolnie
// further modifications
//
// Revision 1.1  2003/01/16 19:33:50  zolnie
// added a simple Geom class to conver a track hit -> tower hit
//

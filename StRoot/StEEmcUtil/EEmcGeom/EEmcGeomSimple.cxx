// $Id: EEmcGeomSimple.cxx,v 1.17 2003/09/17 22:05:33 zolnie Exp $
// $Log: EEmcGeomSimple.cxx,v $
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
#include <cmath>

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
// ######################################################################
ClassImp(EEmcGeomSimple)


EEmcGeomSimple::EEmcGeomSimple() 
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

EEmcGeomSimple::~EEmcGeomSimple() 
{
  if(mEtaBin) delete [] mEtaBin;
}

// counter-clockwise (mClock==-1)
//    12:  105deg ->   75deg
//     3:  195deg ->  165deg
//     6:  285deg ->  255deg
//     9:   15deg ->  345deg
void
EEmcGeomSimple::useDefaultGeometry() 
{
  // default EtaBins
  const Float_t defaultEtaBin[] = {
    2.0    , 
    1.9008 , 1.8065 , 1.7168 , 1.6317 , 1.5507 , 1.4738 ,
    1.4007 , 1.3312 , 1.2651 , 1.2023 , 1.1427 , 1.086  ,
    0.0 
  };

  mNumSec  = kEEmcNumSectors;
  mNumSSec = kEEmcNumSubSectors;
  mNumEta  = kEEmcNumEtas;

  if(mEtaBin) delete [] mEtaBin;
  mEtaBin = new Float_t[mNumEta+1];
  for(UInt_t i=0;i<=mNumEta;i++) mEtaBin[i] = defaultEtaBin[i];

  mZ1     =  kEEmcZPRE1; // preshower
  mZ2     =  kEEmcZPOST; // postshower
  mZSMD   =  kEEmcZSMD;  // 
  mPhi0   =  75.0*M_PI/180.0;       
  mClock  =  CounterClockwise;  
}

TVector3 
EEmcGeomSimple::getTowerCenter(const UInt_t sec, const UInt_t sub, const UInt_t etabin) const 
{
  Double_t  phi   = getPhiMean(sec,sub);
  Double_t  eta   = getEtaMean(etabin);
  if(eta<0.0) return TVector3();
  Double_t  z     = getZMean();
  Double_t  rho   = z*tan(2.0*atan(exp(-1.0*eta)));  

  // create vector pointing toward the center of the tower
  return TVector3(rho*cos(phi),rho*sin(phi),z);
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

  Double_t  eta   = getEtaMean(ietaBin) - (xetaBin-ietaBin)*2*getEtaHalfWidth(ietaBin);
  //  printf("getDirection(xetaBin=%f, xphiBin=%f)--> eta=%f, phi=%f\n",xetaBin,xphiBin,eta,phi);
  
  if(eta<0.0) return TVector3();
  Double_t  z     = getZMean();
  Double_t  rho   = z/sinh(eta);  

  //printf("getDirection(xetaBin=%f, xphiBin=%f)--> eta=%f, phi=%f, x=%f, y=%f z=%f\n",xetaBin,xphiBin,eta,phi,rho*cos(phi),rho*sin(phi),z);
  // create vector pointing toward the center of the tower
  return TVector3(rho*cos(phi),rho*sin(phi),z);
}



#if 0
// =========================================================================
// gets a hit vector r checks if inside the EEmc
// and returns sector (0..mNumSec-1), subsector (0..mNumSSec-1) 
// and eta(0..mNumEta)
// =========================================================================
int 
EEmcGeomSimple::getHit(const StThreeVectorD& point,  StEmcRawHit &hit) const
{
  double dPhiSec  = 2.0*M_PI/mNumSec;
  
  // some shorcuts
  double  rZ    = point.z();
  double  rEta  = point.pseudoRapidity();
  double  rPhi  = point.phi() - mPhi0;

  // check if inside 
  if(rZ  <mZ1              || mZ2<rZ          ) return 0; 
  if(rEta<mEtaBin[mNumEta] || mEtaBin[0]<rEta ) return 0; 
  
  UInt_t eta = 0;
  for(eta=0;eta<=mNumEta;eta++) if(mEtaBin[eta]<rEta) break;
  --eta; // step back

  // ------------------------------------------------------------------------
  // this code is a wonder code  - I do not know how it works ;) /paz/
  // get the sector number - 1
  int k = isClockwise() ? (int)floor(rPhi/dPhiSec) : (int)ceil(rPhi/dPhiSec);
  // adjust to 0..(mNumSec-1)
  UInt_t sec    = (mNumSec + mClock*k) % mNumSec; 
  // get the subsector
  UInt_t subsec = (int) ( ((k*dPhiSec - rPhi)/dPhiSec*mNumSSec) ) % mNumSSec;
  // ------------------------------------------------------------------------

  hit.setId(kEndcapEmcTowerId,sec,eta,subsec);
  return 1;
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

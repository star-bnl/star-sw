// $Id: EEmcGeomSimple.cxx,v 1.8 2003/03/06 18:54:21 zolnie Exp $
// $Log: EEmcGeomSimple.cxx,v $
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
#include <math.h>

#include "SystemOfUnits.h"
#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"

#include "StEmcRawHit.h"
#include "StTrackGeometry.h"
#include "StTrack.h"

#include "EEmcGeomDefs.h"
#include "EEmcGeomSimple.h"

// ######################################################################
//         *** WARNING NOT TESTED FOR mClock==1 (clock-wise) ***
// ######################################################################
ClassImp(EEmcGeomSimple)


EEmcGeomSimple::EEmcGeomSimple() 
{
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

  if(mEtaBin) delete mEtaBin;
  mEtaBin = new Float_t[mNumEta+1];
  for(UInt_t i=0;i<=mNumEta;i++) mEtaBin[i] = defaultEtaBin[i];

  mZ1     =  kEEmcZPRE1; // preshower
  mZ2     =  kEEmcZPOST; // postshower
  mZSMD   =  kEEmcZSMD;  // 
  mPhi0   =  75.0*degree;       
  mClock  =  CounterClockwise;  
}

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

// $Id: EEmcGeomSimple.cxx,v 1.4 2003/01/19 03:47:10 zolnie Exp $
// $Log: EEmcGeomSimple.cxx,v $
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
  for(int i=0;i<=mNumEta;i++) mEtaBin[i] = defaultEtaBin[i];

  mZ1     =  270.190*centimeter; // preshower
  mZ2     =  306.158*centimeter; // postshower
  mPhi0   =  75.0*degree;       
  mClock  =  CounterClockwise;  

}

// =========================================================================
// gets a hit vector r checks if inside the EEmc
// and returns sector (0..mNumSec-1), subsector (0..mNumSSec-1) and eta(0..mNumEta)
// 
// =========================================================================
int 
EEmcGeomSimple::getHit(const StThreeVectorD& r,  StEmcRawHit &hit)
{
  const double dPhiSec  = 2.0*M_PI/mNumSec;
  
  // some shorcuts
  double  rZ    = r.z();
  double  rEta  = r.pseudoRapidity();
  double  rPhi  = r.phi() - mPhi0;

  // check if inside 
  if(rZ  <mZ1              || mZ2<rZ          ) return 0; 
  if(rEta<mEtaBin[mNumEta] || mEtaBin[0]<rEta ) return 0; 

  
  int eta = 0;
  for(eta=0;eta<=mNumEta;eta++) if(mEtaBin[eta]<rEta) break;
  --eta; // step back

  // ------------------------------------------------------------------------
  // this code is a wonder code  - I do not know how it works ;) /paz/
  // get the sector number - 1
  int  k  = isClockwise() ? (int) floor(rPhi/dPhiSec) : (int) ceil(rPhi/dPhiSec); 
  // adjust to 0..(mNumSec-1)
  int sec    = (mNumSec + mClock*k) % mNumSec; 
  // get the subsector
  int subsec = (int) ( ((k*dPhiSec - rPhi)/dPhiSec*mNumSSec) ) % mNumSSec ;
  // ------------------------------------------------------------------------

  hit.setId(kEndcapEmcTowerId,sec,eta,subsec);

  return 1;

}

// 
// a wrapper for the above when a StTrack and z is given 
// (implicitely assumed that the center of the world is at z==0
//
int   
EEmcGeomSimple::getHit(const StTrack& track , double z, StEmcRawHit &hit)
{
  StPhysicalHelixD   helix = track.geometry()->helix();
  double s  = ( z - helix.origin().z() ) / sin( helix.dipAngle())  ;
  return getHit(helix.at(s),hit);
}

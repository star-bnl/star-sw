//******************************************************************************
//                                                                            
// StEmcPosition.cxx
//
// Authors: Marcia Maria de Moura
//
// Initial version: 2001/12/21
//
//******************************************************************************

#include "StEmcPosition.h"
#include <math.h>
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "StPhysicalHelixD.hh"

#include "StMcEvent.hh"
#include "StMcEventTypes.hh"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StEvent.h"
#include "StEventTypes.h"
#include "StEmcGeom.h"

ClassImp(StEmcPosition)

//------------------------------------------------------------------------------
StEmcPosition::StEmcPosition():TObject()
{  
  mBemcGeom = StEmcGeom::getEmcGeom("bemc");  
}
//------------------------------------------------------------------------------
StEmcPosition::~StEmcPosition()
{
}
//------------------------------------------------------------------------------
Bool_t StEmcPosition::projTrack( StThreeVectorD* atFinal, StThreeVectorD* momentumAtFinal, 
                            StTrack* track, Double_t magField, Double_t radius )
{
  StThreeVectorD Zero(0,0,0);
  *atFinal=Zero;
  *momentumAtFinal=Zero;
  
  const StThreeVectorF& origin = track->geometry()->origin();
  const StThreeVectorF& momentum = track->geometry()->momentum();
  Double_t charge = track->geometry()->charge();
  StPhysicalHelixD* helix = new StPhysicalHelixD( momentum, origin, magField*tesla, charge );
  pairD pathLength = helix->pathLength( radius );
  Double_t s = 0;
  if ( pathLength.first > 0 ) s = pathLength.first;
  else 
  {
    if ( pathLength.second > 0 ) s = pathLength.second;
  }
  
  if ( s > 0 )  
  {
    *atFinal = helix->at( s );
    *momentumAtFinal = helix->momentumAt( s, magField*tesla );
    if ( charge == 0 ) *momentumAtFinal = momentum;
    delete helix;
    return kTRUE;
  }
  //else cout << " Projection of track failed - invalid radius " << endl;
  delete helix;
  return kFALSE;    
}
//------------------------------------------------------------------------------
Bool_t StEmcPosition::projTrack( StThreeVectorD* atFinal, StThreeVectorD* momentumAtFinal, 
                            StMcTrack* track, Double_t magField, Double_t radius )
{
  StThreeVectorD Zero(0,0,0);
  *atFinal=Zero;
  *momentumAtFinal=Zero;
  
  const StThreeVectorF& origin = track->startVertex()->position();
  const StThreeVectorF& momentum = track->momentum();
  Double_t charge = track->particleDefinition()->charge();
  StPhysicalHelixD* helix = new StPhysicalHelixD( momentum, origin, magField*tesla, charge );
  pairD pathLength = helix->pathLength( radius );
  Double_t s = 0;
  if ( pathLength.first > 0 ) s = pathLength.first;
  else 
  {
    if ( pathLength.second > 0 ) s = pathLength.second;
  }

  if ( s > 0 )  
  {
    *atFinal = helix->at( s );
    *momentumAtFinal = helix->momentumAt( s, magField*tesla );
    if ( charge == 0 ) *momentumAtFinal = momentum;
    delete helix;
    return kTRUE;
  }
  // else cout << " Projection of track failed - invalid radius " << endl;
  delete helix;
  return kFALSE;    
}

//------------------------------------------------------------------------------
Bool_t StEmcPosition::trackOnEmc( StThreeVectorD* position, StThreeVectorD* momentum,
                            StTrack* track, Double_t magField, Double_t emcRadius )
{  
  // There's no check for primary or secondary tracks

  if (!track->geometry()) return kFALSE;  
  
  const StThreeVectorD& origin = track->geometry()->origin();
  Float_t xO = origin.x();
  Float_t yO = origin.y();
  Float_t distToOrigin = sqrt( pow(xO, 2) + pow(yO, 2) );    
  if ( distToOrigin < emcRadius )
  {
    Bool_t projTrackOk = projTrack( position, momentum, track, magField, emcRadius );
    if ( projTrackOk )  
    {
      Int_t m = 0, e = 0, s = 0;
      Float_t phi = position->phi();
      Float_t eta = position->pseudoRapidity();
      if ( mBemcGeom->getBin(phi, eta, m, e, s) == 0  && s != -1 ) return kTRUE;      
    }
  } 

  return kFALSE;
}
//------------------------------------------------------------------------------
Bool_t StEmcPosition::trackOnEmc( StThreeVectorD* position, StThreeVectorD* momentum,
                            StMcTrack* track, Double_t magField, Double_t emcRadius )

{  
  Float_t startVertexX = track->startVertex()->position().x();
  Float_t startVertexY = track->startVertex()->position().y();
  Float_t startVtxToOrigin = sqrt( pow( startVertexX, 2 ) + pow( startVertexY, 2 ) );
  
  if ( !track->stopVertex() && startVtxToOrigin < emcRadius )    
  {
   Bool_t projTrackOk = projTrack( position, momentum, track, magField, emcRadius );
   if ( projTrackOk )  
    {
      Int_t m = 0, e = 0, s = 0;
      Float_t phi = position->phi();
      Float_t eta = position->pseudoRapidity();
      if ( mBemcGeom->getBin(phi, eta, m, e, s) == 0 && s != -1 ) return kTRUE;
    }
  } 
      
  // Checking if stopVertex exists
  Float_t stopVtxToOrigin = -1;
  if ( track->stopVertex() )     
  {
    Float_t stopVertexX = track->stopVertex()->position().x();
    Float_t stopVertexY = track->stopVertex()->position().y();
    stopVtxToOrigin = sqrt( pow( stopVertexX,2 ) + pow( stopVertexY,2 ) );
  }
  
  if (stopVtxToOrigin >= emcRadius)
  {
    Bool_t projTrackOk = projTrack( position, momentum, track, magField, emcRadius );
    if ( projTrackOk )  
    {
      Int_t m = 0, e = 0, s = 0;
      Float_t phi = position->phi();
      Float_t eta = position->pseudoRapidity();
      if ( mBemcGeom->getBin(phi, eta, m, e, s) == 0 && s != -1 ) return kTRUE;
    }
  }  
  
  return kFALSE;
}
//------------------------------------------------------------------------------
Int_t StEmcPosition::getTowerEtaPhi( Double_t eta, Double_t phi, 
                                  Float_t* towerEta, Float_t* towerPhi )
{
  *towerEta = 0; *towerPhi = 0;
  Float_t tempTowerEta = 0, tempTowerPhi = 0;
  Int_t m = 0, e = 0, s = 0, towerId = -1;
  
  mBemcGeom->getBin(phi, eta, m, e, s);
  if (m==0) return -1;
  if (s<0) s=1;
  mBemcGeom->getId(m, e, s, towerId);
  mBemcGeom->getEtaPhi(towerId, tempTowerEta, tempTowerPhi);
  *towerEta = tempTowerEta;
  *towerPhi = tempTowerPhi;
  return 0;
}
//------------------------------------------------------------------------------
Int_t StEmcPosition::getNextTowerId(Float_t trackEta, Float_t trackPhi, 
                                 Int_t nTowersdEta, Int_t nTowersdPhi)
{
  // Some local variables
  Int_t m = 0, e = 0, s = 0, nextTowerId = 0;
  Float_t trackTowerEta = 0, trackTowerPhi = 0;
  Float_t towersdEtaWdt = 0, towersdPhiWdt = 0;

  towersdEtaWdt = 0.05;
  towersdPhiWdt = 3./180.*pi;

  if ( getTowerEtaPhi( trackEta, trackPhi, &trackTowerEta, &trackTowerPhi ) == 0)
  {
    // Calculating eta and phi of neighbour tower
    Float_t nextTowerEta = trackTowerEta + nTowersdEta * towersdEtaWdt;

    if ( fabs(nextTowerEta) > 1.0 ) return 0;
    Float_t nextTowerPhi = trackTowerPhi + nTowersdPhi * towersdPhiWdt;

    // Getting tower id of neighbour tower
    mBemcGeom->getBin( nextTowerPhi, nextTowerEta, m, e, s );
    if (s<0) s=1;
    mBemcGeom->getId( m, e, s, nextTowerId );

    return nextTowerId;
  }
  else 
    return 0;
}
//------------------------------------------------------------------------------
Float_t StEmcPosition::getDistTowerToTrack( Double_t trackEta, Double_t trackPhi, 
                                         Int_t nTowersdEta, Int_t nTowersdPhi )

{     
  Int_t towerId = 0;
  Float_t towerEta = 0, towerToTrackdEta = 0; 
  Float_t towerPhi = 0, towerToTrackdPhi = 0; 
  Float_t mdistTowerToTrack = 0;

  towerId = getNextTowerId( trackEta, trackPhi, nTowersdEta, nTowersdPhi );
  if (towerId != 0)
  {
    // Getting eta and phi of neighbour tower
    mBemcGeom->getEtaPhi(towerId, towerEta, towerPhi);
    towerToTrackdEta = towerEta-trackEta;
    towerToTrackdPhi = towerPhi-trackPhi;
      
    mdistTowerToTrack = sqrt( pow(towerToTrackdEta, 2) + pow(towerToTrackdPhi, 2) );
  
    return mdistTowerToTrack;
  }
  else
    return -1;
}
//------------------------------------------------------------------------------
StThreeVectorF StEmcPosition::getPosFromVertex( StVertex* vertex,Int_t TowerId )
{
  StThreeVectorF Zero(0,0,0);
  if(TowerId<1 || TowerId>4800) return Zero;
  
  Float_t xTower,yTower,zTower;
  StThreeVectorF position = vertex->position();
  mBemcGeom->getXYZ(TowerId, xTower, yTower, zTower);
  StThreeVectorF towerPosition(xTower, yTower, zTower);
  StThreeVectorF PositionFromVertex = towerPosition - position;
  
  return PositionFromVertex;
}
//------------------------------------------------------------------------------
StThreeVectorF StEmcPosition::getPosFromVertex( StMcVertex* vertex,Int_t TowerId )
{
  StThreeVectorF Zero(0,0,0);
  if(TowerId<1 || TowerId>4800) return Zero;
  
  Float_t xTower,yTower,zTower;
  StThreeVectorF position = vertex->position();
  mBemcGeom->getXYZ(TowerId, xTower, yTower, zTower);
  StThreeVectorF towerPosition(xTower, yTower, zTower);
  StThreeVectorF PositionFromVertex = towerPosition - position;
  
  return PositionFromVertex;
}
//------------------------------------------------------------------------------
Float_t StEmcPosition::getThetaFromVertex( StVertex* vertex,Int_t TowerId )
{
  StThreeVectorF p=getPosFromVertex(vertex,TowerId );
  return p.theta();
}
//------------------------------------------------------------------------------
Float_t StEmcPosition::getThetaFromVertex( StMcVertex* vertex,Int_t TowerId )
{
  StThreeVectorF p=getPosFromVertex(vertex,TowerId );
  return p.theta();
}
//------------------------------------------------------------------------------
Float_t StEmcPosition::getEtaFromVertex( StVertex* vertex,Int_t TowerId )
{
  StThreeVectorF p=getPosFromVertex(vertex,TowerId );
  return p.pseudoRapidity();
}
//------------------------------------------------------------------------------
Float_t StEmcPosition::getEtaFromVertex( StMcVertex* vertex,Int_t TowerId )
{
  StThreeVectorF p=getPosFromVertex(vertex,TowerId );
  return p.pseudoRapidity();
}
//------------------------------------------------------------------------------
Float_t StEmcPosition::getPhiFromVertex( StVertex* vertex,Int_t TowerId )
{
  StThreeVectorF p=getPosFromVertex(vertex,TowerId );
  return p.phi();
}
//------------------------------------------------------------------------------
Float_t StEmcPosition::getPhiFromVertex( StMcVertex* vertex,Int_t TowerId )
{
  StThreeVectorF p=getPosFromVertex(vertex,TowerId );
  return p.phi();
}

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
#include "StEmcUtil/geometry/StEmcGeom.h"

ClassImp(StEmcPosition)

//------------------------------------------------------------------------------
StEmcPosition::StEmcPosition():TObject()
{  
  mGeom[0] = StEmcGeom::getEmcGeom("bemc");  
  mGeom[1] = StEmcGeom::getEmcGeom("bprs");  
  mGeom[2] = StEmcGeom::getEmcGeom("bsmde");  
  mGeom[3] = StEmcGeom::getEmcGeom("bsmdp");  
}
//------------------------------------------------------------------------------
StEmcPosition::~StEmcPosition()
{
}
//------------------------------------------------------------------------------
Bool_t StEmcPosition::projTrack(StThreeVectorD* atFinal, StThreeVectorD* momentumAtFinal, 
                            StTrack* track, Double_t magField, Double_t radius, Int_t option)
{
  StThreeVectorD Zero(0,0,0);
  *atFinal=Zero;
  *momentumAtFinal=Zero;

  const StThreeVectorF& origin = track->geometry()->origin();
  const StThreeVectorF& momentum = track->geometry()->momentum();
  Double_t charge = track->geometry()->charge();
  StPhysicalHelixD* helix = new StPhysicalHelixD(momentum, origin, magField*tesla, charge);
  pairD pathLength = helix->pathLength(radius);

  Double_t s,s1,s2; 
  s=0;
  s1 = pathLength.first;
  s2 = pathLength.second;

  Bool_t goProj;
  goProj = kFALSE;

  if (finite(s1) == 0 && finite(s2) == 0) { delete helix; return kFALSE;} // Track couldn't be projected!

  if (option == 1)  // Selects positive path lenght to project track forwards along its helix relative to
                    // first point of track. The smaller solution is taken when both are positive
  {
    if (s1 >= 0 && s2 >= 0) {s = s1; goProj = kTRUE; }
    if (s1 >= 0 && s2 < 0) { s = s1; goProj = kTRUE; }
    if (s1 < 0 && s2 >= 0) { s = s2; goProj = kTRUE; }
  }
  
  if (option == -1) // Selects negative path lenght to project track backwards along its helix relative to
                    // first point of track. The smaller absolute solution is taken when both are negative 
  {
    if (s1 <= 0 && s2 <= 0) { s = s2; goProj = kTRUE; }
    if (s1 <= 0 && s2 > 0) { s = s1; goProj = kTRUE; }
    if (s1 > 0 && s2 <= 0) { s = s2; goProj = kTRUE; }
  }
	
  if (goProj) 
  {
    *atFinal = helix->at( s );
    *momentumAtFinal = helix->momentumAt( s, magField*tesla );
    if (charge == 0) *momentumAtFinal = momentum;
  }
	delete helix;
  return goProj;
}
//------------------------------------------------------------------------------
Bool_t StEmcPosition::projTrack(StThreeVectorD* atFinal, StThreeVectorD* momentumAtFinal, 
                            StMcTrack* mcTrack, Double_t magField, Double_t radius, Int_t option)
{
  StThreeVectorD Zero(0,0,0);
  *atFinal=Zero;
  *momentumAtFinal=Zero;

  const StThreeVectorF& origin = mcTrack->startVertex()->position();
  const StThreeVectorF& momentum = mcTrack->momentum();
  Double_t charge = mcTrack->particleDefinition()->charge();
  StPhysicalHelixD* helix = new StPhysicalHelixD(momentum, origin, magField*tesla, charge);
  pairD pathLength = helix->pathLength(radius);

  Double_t s,s1,s2;  
  s=0;
  s1 = pathLength.first;
  s2 = pathLength.second;

  Bool_t goProj;
  goProj = kFALSE;

  if (finite(s1) == 0 && finite(s2) == 0) { delete helix; return kFALSE;} // Track couldn't be projected!

  if (option == 1)  // Selects positive path lenght to project track forwards along its helix relative to
                    // first point of track. The smaller solution is taken when both are positive
  {                 
    if (s1 >= 0 && s2 >= 0) { s = s1; goProj = kTRUE; }
    if (s1 >= 0 && s2 < 0) { s = s1; goProj = kTRUE; }
    if (s1 < 0 && s2 >= 0) { s = s2; goProj = kTRUE; }
  }

  if (option == -1) // Selects negative path lenght to project track backwards along its helix relative to
                    // first point of track. The smaller absolute solution is taken when both are negative 
  {
    if (s1 <= 0 && s2 <= 0) { s = s2; goProj = kTRUE; }
    if (s1 <= 0 && s2 > 0) { s = s1; goProj = kTRUE; }
    if (s1 > 0 && s2 <= 0) { s = s2; goProj = kTRUE; }
  }

	if (goProj) 
  {
    *atFinal = helix->at( s );
    *momentumAtFinal = helix->momentumAt( s, magField*tesla );
    if (charge == 0) *momentumAtFinal = momentum;
  }
  delete helix;
  return goProj;
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
  Float_t distToOrigin = ::sqrt( ::pow(xO, 2) + ::pow(yO, 2) );    
  if ( distToOrigin < emcRadius )
  {
    Bool_t projTrackOk = projTrack( position, momentum, track, magField, emcRadius );
    if ( projTrackOk )  
    {
      Int_t m = 0, e = 0, s = 0;
      Float_t phi = position->phi();
      Float_t eta = position->pseudoRapidity();
      if ( mGeom[0]->getBin(phi, eta, m, e, s) == 0  && s != -1 ) return kTRUE;      
    }
  } 

  return kFALSE;
}
//------------------------------------------------------------------------------
Bool_t StEmcPosition::trackOnEmc( StThreeVectorD* position, StThreeVectorD* momentum,
                            StMcTrack* mcTrack, Double_t magField, Double_t emcRadius )
{  
  Float_t startVertexX = mcTrack->startVertex()->position().x();
  Float_t startVertexY = mcTrack->startVertex()->position().y();
  Float_t startVtxToOrigin = ::sqrt( ::pow( startVertexX, 2 ) + ::pow( startVertexY, 2 ) );

  if ( !mcTrack->stopVertex() && startVtxToOrigin < emcRadius )    
  {
   Bool_t projTrackOk = projTrack( position, momentum, mcTrack, magField, emcRadius );
   if ( projTrackOk )  
    {
      Int_t m = 0, e = 0, s = 0;
      Float_t phi = position->phi();
      Float_t eta = position->pseudoRapidity();
      if ( mGeom[0]->getBin(phi, eta, m, e, s) == 0 && s != -1 ) return kTRUE;
    }
  } 

  // Checking if stopVertex exists
  Float_t stopVtxToOrigin = -1;
  if ( mcTrack->stopVertex() )     
  {
    Float_t stopVertexX = mcTrack->stopVertex()->position().x();
    Float_t stopVertexY = mcTrack->stopVertex()->position().y();
    stopVtxToOrigin = ::sqrt( ::pow( stopVertexX,2 ) + ::pow(stopVertexY,2) );
  }
  
  if (stopVtxToOrigin >= emcRadius)
  {
    Bool_t projTrackOk = projTrack( position, momentum, mcTrack, magField, emcRadius );
    if ( projTrackOk )  
    {
      Int_t m = 0, e = 0, s = 0;
      Float_t phi = position->phi();
      Float_t eta = position->pseudoRapidity();
      if ( mGeom[0]->getBin(phi, eta, m, e, s) == 0 && s != -1 ) return kTRUE;
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
  
  mGeom[0]->getBin(phi, eta, m, e, s);
  if (m==0) return -1;
  if (s<0) s=1;
  mGeom[0]->getId(m, e, s, towerId);
  mGeom[0]->getEtaPhi(towerId, tempTowerEta, tempTowerPhi);
  *towerEta = tempTowerEta;
  *towerPhi = tempTowerPhi;
  return 0;
}
//------------------------------------------------------------------------------
Int_t StEmcPosition::getNextTowerId(Float_t Eta, Float_t Phi, Int_t nTowersdEta, Int_t nTowersdPhi)
{
  Int_t m,e,s;
  mGeom[0]->getBin( Phi, Eta, m, e, s );
	if(m>0 && m<=120)
	{
		if(s<0) s=1;
		return getNextTowerId(m,e,s,nTowersdEta,nTowersdPhi);
	}
	return 0;
}
//------------------------------------------------------------------------------
Int_t StEmcPosition::getNextTowerId(Int_t id, Int_t nTowersdEta, Int_t nTowersdPhi)
{
	if(id<1 || id>4800) return 0;
	Int_t m,e,s;
	mGeom[0]->getBin(id,m,e,s);
	return getNextTowerId(m,e,s,nTowersdEta,nTowersdPhi);
}
//------------------------------------------------------------------------------
Int_t StEmcPosition::getNextTowerId(Int_t m, Int_t e, Int_t s, Int_t nTowersdEta, Int_t nTowersdPhi)
{
	if(m<1 || m>120) return 0;
	if(e<1 || e>20) return 0;
	if(s<1 || s>2) return 0;
	return getNextId(1,m,e,s,nTowersdEta,nTowersdPhi);
}
//------------------------------------------------------------------------------
Int_t StEmcPosition::getNextId(Int_t det,Int_t m, Int_t e, Int_t s, Int_t nEta, Int_t nPhi)
{
	if(det<1 || det>4) return 0;
	if(m<1 || m>120) return 0;
	if(s<1 || s>mGeom[det-1]->NSub()) return 0;
	if(e<1 || e>mGeom[det-1]->NEta()) return 0;
	
	Int_t ef=e+nEta;
	Int_t sf=s+nPhi;
	Int_t mf=m;
	
	Int_t NE=mGeom[det-1]->NEta();
	Int_t NS=mGeom[det-1]->NSub();
	
	if(abs(ef)>NE) return 0;
	
  do
	{
		if(sf<=0)
		{
			sf += NS;
			mf--;
			if(mf==60) mf  = 120;
			if(mf==0)  mf  = 60;
		}
		if(sf>NS)
		{
			sf -= NS;
			mf++;
			if(mf==61)  mf = 1;
			if(mf==121) mf = 61;
		}
	} while(sf<=0 || sf>NS);
	
	if(ef<=0)
	{
		ef = 1-ef;
		sf = NS-sf+1;
		if(ef>NE) return 0;
	  Int_t rid,etmp,stmp;
	  Float_t eta,phi;
	  mGeom[det-1]->getId(mf, ef, sf, rid);
    mGeom[det-1]->getEtaPhi(rid, eta, phi);
		mGeom[det-1]->getBin(phi,-eta,mf,etmp,stmp);
	}
	
	Int_t rid;
	if(mf<1 || mf>120) return 0;
	if(ef<1 || ef>NE) return 0;
	if(sf<1 || sf>NS) return 0;
	mGeom[det-1]->getId(mf, ef, sf, rid);
	return rid;

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
    mGeom[0]->getEtaPhi(towerId, towerEta, towerPhi);
    towerToTrackdEta = towerEta-trackEta;
    towerToTrackdPhi = towerPhi-trackPhi;
      
    mdistTowerToTrack = ::sqrt( ::pow(towerToTrackdEta, 2) + ::pow(towerToTrackdPhi, 2) );
  
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
  mGeom[0]->getXYZ(TowerId, xTower, yTower, zTower);
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
  mGeom[0]->getXYZ(TowerId, xTower, yTower, zTower);
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

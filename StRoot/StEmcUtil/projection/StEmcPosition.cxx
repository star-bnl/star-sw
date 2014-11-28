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

//StMuDstMaker:
#include "StMuDSTMaker/COMMON/StMuTrack.h"

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
								const StMuTrack* const track, double magField, double radius, int option) const
{
    StThreeVectorD Zero(0,0,0);
    *atFinal=Zero;
    *momentumAtFinal=Zero;
	
    /* this was for StTrack
		const StThreeVectorF& origin = track->geometry()->origin();
	const StThreeVectorF& momentum = track->geometry()->momentum();
	double charge = track->geometry()->charge();
	StPhysicalHelixD helix(momentum, origin, magField*tesla, charge);
    */
    StPhysicalHelixD helix = track->outerHelix();
    const StThreeVectorF momentum = track->momentum();
    pairD pathLength = helix.pathLength(radius);
    double charge = track->charge();
	
    double s,s1,s2; 
    s=0;
    s1 = pathLength.first;
    s2 = pathLength.second;
	
    Bool_t goProj;
    goProj = kFALSE;
	
    if (finite(s1) == 0 && finite(s2) == 0) { return kFALSE;} // Track couldn't be projected!
	
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
	    *atFinal = helix.at( s );
	    *momentumAtFinal = helix.momentumAt( s, magField*tesla );
	    if (charge == 0) *momentumAtFinal = momentum;
	}
    return goProj;
}
//------------------------------------------------------------------------------
Bool_t StEmcPosition::projTrack(StThreeVectorD* atFinal, StThreeVectorD* momentumAtFinal, 
                            const StPhysicalHelixD* const helix, Double_t magField, Double_t radius, Int_t option) const
{
  StThreeVectorD Zero(0,0,0);
  *atFinal=Zero;
  *momentumAtFinal=Zero;

  pairD pathLength = helix->pathLength(radius);

  Double_t s,s1,s2; 
  s=0;
  s1 = pathLength.first;
  s2 = pathLength.second;

  Bool_t goProj;
  goProj = kFALSE;

  if (finite(s1) == 0 && finite(s2) == 0) { return kFALSE;} // Track couldn't be projected!

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
  }
  return goProj;
}

//------------------------------------------------------------------------------
Bool_t StEmcPosition::projTrack(StThreeVectorD* atFinal, StThreeVectorD* momentumAtFinal, 
                            const StTrack* const track, Double_t magField, Double_t radius, Int_t option) const 
{
  StThreeVectorD Zero(0,0,0);
  *atFinal=Zero;
  *momentumAtFinal=Zero;

  const StThreeVectorF& origin = track->outerGeometry()->origin();
  const StThreeVectorF& momentum = track->outerGeometry()->momentum();
  Double_t charge = track->outerGeometry()->charge();
  StPhysicalHelixD helix(momentum, origin, magField*tesla, charge);
  pairD pathLength = helix.pathLength(radius);

  Double_t s,s1,s2; 
  s=0;
  s1 = pathLength.first;
  s2 = pathLength.second;

  Bool_t goProj;
  goProj = kFALSE;

  if (finite(s1) == 0 && finite(s2) == 0) { return kFALSE;} // Track couldn't be projected!

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
    *atFinal = helix.at( s );
    *momentumAtFinal = helix.momentumAt( s, magField*tesla );
    if (charge == 0) *momentumAtFinal = momentum;
  }
  return goProj;
}
//------------------------------------------------------------------------------
Bool_t StEmcPosition::projTrack(StThreeVectorD* atFinal, StThreeVectorD* momentumAtFinal, 
                            const StMcTrack* const mcTrack, Double_t magField, Double_t radius, Int_t option) const 
{
  StThreeVectorD Zero(0,0,0);
  *atFinal=Zero;
  *momentumAtFinal=Zero;

  const StThreeVectorF& origin = mcTrack->startVertex()->position();
  const StThreeVectorF& momentum = mcTrack->momentum();
  Double_t charge = mcTrack->particleDefinition()->charge();
  StPhysicalHelixD helix(momentum, origin, magField*tesla, charge);
  pairD pathLength = helix.pathLength(radius);

  Double_t s,s1,s2;  
  s=0;
  s1 = pathLength.first;
  s2 = pathLength.second;

  Bool_t goProj;
  goProj = kFALSE;

  if (finite(s1) == 0 && finite(s2) == 0) { return kFALSE;} // Track couldn't be projected!

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
    *atFinal = helix.at( s );
    *momentumAtFinal = helix.momentumAt( s, magField*tesla );
    if (charge == 0) *momentumAtFinal = momentum;
  }
  return goProj;
}
//------------------------------------------------------------------------------
Bool_t StEmcPosition::trackOnEmc( StThreeVectorD* position, StThreeVectorD* momentum, const StMuTrack* const track, double magField, double emcRadius ) const
{  
    // There's no check for primary or secondary tracks
	
    /* this was for StTrack
	if (!track->geometry()) return kFALSE;  
	const StThreeVectorD& origin = track->geometry()->origin();
    */
    StPhysicalHelixD helix = track->outerHelix();
    const StThreeVectorD& origin = helix.origin();
	
    
    float xO = origin.x();
    float yO = origin.y();
    float distToOrigin = ::sqrt( ::pow(xO, 2) + ::pow(yO, 2) );    
    if ( distToOrigin < emcRadius )
	{
		//		LOG_DEBUG << "inside emcRadius" << endm;
	    Bool_t projTrackOk = projTrack( position, momentum, track, magField, emcRadius );
	    if ( projTrackOk )  
		{
			//			LOG_DEBUG << "projTrackOk==1" << endm;
		    int m = 0, e = 0, s = 0;
		    float phi = position->phi();
		    float eta = position->pseudoRapidity();
			//			LOG_DEBUG << "eta,phi = "<<eta<<","<<phi<<endm;
			//			LOG_DEBUG << mGeom[0]->getBin(phi, eta, m, e, s) << endm;
			//			LOG_DEBUG <<"m:e:s = "<<m<<":"<<e<<":"<<s<<endm;
		    if ( mGeom[0]->getBin(phi, eta, m, e, s) == 0  && s != -1 ) return kTRUE;      
		}
	} 
	
    return kFALSE;
}
//------------------------------------------------------------------------------
Bool_t StEmcPosition::trackOnEmc( StThreeVectorD* position, StThreeVectorD* momentum,
                            const StTrack* const track, Double_t magField, Double_t emcRadius ) const
{  
  // There's no check for primary or secondary tracks
  
  if (!track->outerGeometry()) return kFALSE;  

  const StThreeVectorD& origin = track->outerGeometry()->origin();
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
                            const StMcTrack* const mcTrack, Double_t magField, Double_t emcRadius ) const
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
Int_t StEmcPosition::getTowerEtaPhi(const Double_t eta, const Double_t phi, 
                                  Float_t* towerEta, Float_t* towerPhi ) const
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
Int_t StEmcPosition::getNextTowerId(const Float_t eta, const Float_t phi, const Int_t nTowersdEta, const Int_t nTowersdPhi) const
{
  Int_t m,e,s;
  mGeom[0]->getBin( phi, eta, m, e, s );
	if(m>0 && m<=120)
	{
		if(s<0) s=1;
		return getNextTowerId(m,e,s,nTowersdEta,nTowersdPhi);
	}
	return 0;
}
//------------------------------------------------------------------------------
Int_t StEmcPosition::getNextTowerId(const Int_t softId, const Int_t nTowersdEta, const Int_t nTowersdPhi) const
{
	if(softId<1 || softId>4800) return 0;
	Int_t m,e,s;
	mGeom[0]->getBin(softId,m,e,s);
	return getNextTowerId(m,e,s,nTowersdEta,nTowersdPhi);
}
//------------------------------------------------------------------------------
Int_t StEmcPosition::getNextTowerId(const Int_t m, const Int_t e, const Int_t s, const Int_t nTowersdEta, const Int_t nTowersdPhi) const
{
	if(m<1 || m>120) return 0;
	if(e<1 || e>20) return 0;
	if(s<1 || s>2) return 0;
	return getNextId(1,m,e,s,nTowersdEta,nTowersdPhi);
}
//------------------------------------------------------------------------------
Int_t StEmcPosition::getNextId(const Int_t det, const Int_t m, const Int_t e, const Int_t s, const Int_t nEta, const Int_t nPhi) const
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
Int_t StEmcPosition::getNextId(const Int_t det, const Int_t softId, const Int_t nEta, const Int_t nPhi)const
{
  if(det<1 || det>4) return 0;
  Int_t m,e,s;
  if(softId<1)return 0;
  if((det == 1 || det == 2) && softId > 4800)return 0;
  if((det == 3 || det == 4) && softId > 18000)return 0;
  mGeom[det-1]->getBin(softId,m,e,s);
  if(m>0 && m<=120)
    {
      if(s<0) s=1;
      return getNextId(det,m,e,s,nEta,nPhi);
    }
  return 0;
}
//------------------------------------------------------------------------------
Float_t StEmcPosition::getDistTowerToTrack( Double_t trackEta, Double_t trackPhi, 
                                         Int_t nTowersdEta, Int_t nTowersdPhi ) const

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
StThreeVectorF StEmcPosition::getPosFromVertex( const StVertex* const vertex,Int_t TowerId ) const
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
StThreeVectorF StEmcPosition::getPosFromVertex( const StThreeVectorF& position,int TowerId ) const
{
    StThreeVectorF Zero(0,0,0);
    if(TowerId<1 || TowerId>4800) return Zero;
	
    float xTower,yTower,zTower;
    //StThreeVectorF position = vertex->position(); //modified to work with StMuDst instead of StEvent
    mGeom[0]->getXYZ(TowerId, xTower, yTower, zTower);
    StThreeVectorF towerPosition(xTower, yTower, zTower);
    StThreeVectorF PositionFromVertex = towerPosition - position;
	
    return PositionFromVertex;
}
//------------------------------------------------------------------------------
StThreeVectorF StEmcPosition::getPosFromVertex( const StMcVertex* const vertex,Int_t TowerId ) const
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
Float_t StEmcPosition::getThetaFromVertex( const StVertex* const vertex,Int_t TowerId ) const
{
  StThreeVectorF p=getPosFromVertex(vertex,TowerId );
  return p.theta();
}
//------------------------------------------------------------------------------
Float_t StEmcPosition::getThetaFromVertex( const StThreeVectorF& vertex,int TowerId ) const
{
    StThreeVectorF p=getPosFromVertex(vertex,TowerId );
    return p.theta();
}
//------------------------------------------------------------------------------
Float_t StEmcPosition::getThetaFromVertex( const StMcVertex* const vertex,Int_t TowerId ) const
{
  StThreeVectorF p=getPosFromVertex(vertex,TowerId );
  return p.theta();
}
//------------------------------------------------------------------------------
Float_t StEmcPosition::getEtaFromVertex( const StVertex* const vertex,Int_t TowerId ) const
{
  StThreeVectorF p=getPosFromVertex(vertex,TowerId );
  return p.pseudoRapidity();
}
//------------------------------------------------------------------------------
Float_t StEmcPosition::getEtaFromVertex( const StThreeVectorF& vertex,int TowerId ) const
{
    StThreeVectorF p=getPosFromVertex(vertex,TowerId );
    return p.pseudoRapidity();
}
//------------------------------------------------------------------------------
Float_t StEmcPosition::getEtaFromVertex( const StMcVertex* const vertex,Int_t TowerId ) const
{
  StThreeVectorF p=getPosFromVertex(vertex,TowerId );
  return p.pseudoRapidity();
}
//------------------------------------------------------------------------------
Float_t StEmcPosition::getPhiFromVertex( const StVertex* const vertex,Int_t TowerId ) const
{
  StThreeVectorF p=getPosFromVertex(vertex,TowerId );
  return p.phi();
}
//------------------------------------------------------------------------------
Float_t StEmcPosition::getPhiFromVertex( const StThreeVectorF& vertex,int TowerId ) const
{
    StThreeVectorF p=getPosFromVertex(vertex,TowerId );
    return p.phi();
}
//------------------------------------------------------------------------------
Float_t StEmcPosition::getPhiFromVertex( const StMcVertex* const vertex,Int_t TowerId ) const
{
  StThreeVectorF p=getPosFromVertex(vertex,TowerId );
  return p.phi();
}

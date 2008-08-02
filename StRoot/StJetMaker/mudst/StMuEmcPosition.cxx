#include "StMuEmcPosition.h"
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
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

//StMuDstMaker:
#include "StMuDSTMaker/COMMON/StMuTrack.h"


  ClassImp(StMuEmcPosition)

      StMuEmcPosition::StMuEmcPosition():TObject()
{  
    mGeom[0] = StEmcGeom::getEmcGeom("bemc");  
    mGeom[1] = StEmcGeom::getEmcGeom("bprs");  
    mGeom[2] = StEmcGeom::getEmcGeom("bsmde");  
    mGeom[3] = StEmcGeom::getEmcGeom("bsmdp");  
}

StMuEmcPosition::~StMuEmcPosition()
{
}

bool StMuEmcPosition::projTrack(StThreeVectorD* atFinal, StThreeVectorD* momentumAtFinal, 
				const StMuTrack* track, double magField, double radius, int option)
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

    bool goProj;
    goProj = kFALSE;

    if (finite(s1) == 0 && finite(s2) == 0) { return kFALSE;} // StjTrack couldn't be projected!

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

bool StMuEmcPosition::projTrack(StThreeVectorD* atFinal, StThreeVectorD* momentumAtFinal, 
				StMcTrack* mcTrack, double magField, double radius, int option)
{
    StThreeVectorD Zero(0,0,0);
    *atFinal=Zero;
    *momentumAtFinal=Zero;

    const StThreeVectorF& origin = mcTrack->startVertex()->position();
    const StThreeVectorF& momentum = mcTrack->momentum();
    double charge = mcTrack->particleDefinition()->charge();
    StPhysicalHelixD helix(momentum, origin, magField*tesla, charge);
    pairD pathLength = helix.pathLength(radius);

    double s,s1,s2;  
    s=0;
    s1 = pathLength.first;
    s2 = pathLength.second;

    bool goProj;
    goProj = kFALSE;

    if (finite(s1) == 0 && finite(s2) == 0) { return kFALSE;} // StjTrack couldn't be projected!

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

bool StMuEmcPosition::trackOnEmc( StThreeVectorD* position, StThreeVectorD* momentum, const StMuTrack* track, double magField, double emcRadius )
{  
  return trackOnBEmc(position, momentum, track, magField, emcRadius);
}

bool StMuEmcPosition::trackOnBEmc( StThreeVectorD* position, StThreeVectorD* momentum, const StMuTrack* track, double magField, double emcRadius )
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
	    bool projTrackOk = projTrack( position, momentum, track, magField, emcRadius );
	    if ( projTrackOk )  
		{
		    int m = 0, e = 0, s = 0;
		    float phi = position->phi();
		    float eta = position->pseudoRapidity();
		    if ( mGeom[0]->getBin(phi, eta, m, e, s) == 0  && s != -1 ) return kTRUE;      
		}
	} 

    return kFALSE;
}

bool StMuEmcPosition::trackOnEmc( StThreeVectorD* position, StThreeVectorD* momentum,
				  StMcTrack* mcTrack, double magField, double emcRadius )
{
  return trackOnBEmc(position, momentum, mcTrack, magField, emcRadius );
}

bool StMuEmcPosition::trackOnBEmc( StThreeVectorD* position, StThreeVectorD* momentum,
				  StMcTrack* mcTrack, double magField, double emcRadius )
{  
    float startVertexX = mcTrack->startVertex()->position().x();
    float startVertexY = mcTrack->startVertex()->position().y();
    float startVtxToOrigin = ::sqrt( ::pow( startVertexX, 2 ) + ::pow( startVertexY, 2 ) );

    if ( !mcTrack->stopVertex() && startVtxToOrigin < emcRadius )    
	{
	    bool projTrackOk = projTrack( position, momentum, mcTrack, magField, emcRadius );
	    if ( projTrackOk )  
		{
		    int m = 0, e = 0, s = 0;
		    float phi = position->phi();
		    float eta = position->pseudoRapidity();
		    if ( mGeom[0]->getBin(phi, eta, m, e, s) == 0 && s != -1 ) return kTRUE;
		}
	} 

    // Checking if stopVertex exists
    float stopVtxToOrigin = -1;
    if ( mcTrack->stopVertex() )     
	{
	    float stopVertexX = mcTrack->stopVertex()->position().x();
	    float stopVertexY = mcTrack->stopVertex()->position().y();
	    stopVtxToOrigin = ::sqrt( ::pow( stopVertexX,2 ) + ::pow(stopVertexY,2) );
	}
  
    if (stopVtxToOrigin >= emcRadius)
	{
	    bool projTrackOk = projTrack( position, momentum, mcTrack, magField, emcRadius );
	    if ( projTrackOk )  
		{
		    int m = 0, e = 0, s = 0;
		    float phi = position->phi();
		    float eta = position->pseudoRapidity();
		    if ( mGeom[0]->getBin(phi, eta, m, e, s) == 0 && s != -1 ) return kTRUE;
		}
	}  

    return kFALSE;
}

// Project track onto EEMC at SMD depth (magnetic field must be in Tesla)
bool StMuEmcPosition::trackOnEEmc(StThreeVectorD* position, StThreeVectorD* momentum, const StMuTrack* track, double magField, double z) const
{
  if (track->eta() < 0) return false;
  StPhysicalHelixD outerHelix = track->outerHelix();
  if (fabs(outerHelix.origin().z()) > fabs(z)) return false;
  StThreeVectorD r(0,0,z);
  StThreeVectorD n(0,0,1);
  double s = outerHelix.pathLength(r,n);
  if (!finite(s)) return false;
  if (s == StHelix::NoSolution) return false;
  if (s < 0) return false;
  *position = outerHelix.at(s);
  int sector, subsector, etabin;
  if (!EEmcGeomSimple::Instance().getTower(position->xyz(), sector, subsector, etabin)) return false;
  *momentum = outerHelix.momentumAt(s, magField*tesla);
  return true;
}

int StMuEmcPosition::getTowerEtaPhi( double eta, double phi, float* towerEta, float* towerPhi )
{
    *towerEta = 0; *towerPhi = 0;
    float tempTowerEta = 0, tempTowerPhi = 0;
    int m = 0, e = 0, s = 0, towerId = -1;
  
    mGeom[0]->getBin(phi, eta, m, e, s);
    if (m==0) return -1;
    if (s<0) s=1;
    mGeom[0]->getId(m, e, s, towerId);
    mGeom[0]->getEtaPhi(towerId, tempTowerEta, tempTowerPhi);
    *towerEta = tempTowerEta;
    *towerPhi = tempTowerPhi;
    return 0;
}

int StMuEmcPosition::getNextTowerId(float Eta, float Phi, int nTowersdEta, int nTowersdPhi)
{
    int m,e,s;
    mGeom[0]->getBin( Phi, Eta, m, e, s );
    if(m>0 && m<=120)
	{
	    if(s<0) s=1;
	    return getNextTowerId(m,e,s,nTowersdEta,nTowersdPhi);
	}
    return 0;
}

int StMuEmcPosition::getNextTowerId(int id, int nTowersdEta, int nTowersdPhi)
{
    if(id<1 || id>4800) return 0;
    int m,e,s;
    mGeom[0]->getBin(id,m,e,s);
    return getNextTowerId(m,e,s,nTowersdEta,nTowersdPhi);
}

int StMuEmcPosition::getNextTowerId(int m, int e, int s, int nTowersdEta, int nTowersdPhi)
{
    if(m<1 || m>120) return 0;
    if(e<1 || e>20) return 0;
    if(s<1 || s>2) return 0;
    return getNextId(1,m,e,s,nTowersdEta,nTowersdPhi);
}

int StMuEmcPosition::getNextId(int det,int m, int e, int s, int nEta, int nPhi)
{
    if(det<1 || det>4) return 0;
    if(m<1 || m>120) return 0;
    if(s<1 || s>mGeom[det-1]->NSub()) return 0;
    if(e<1 || e>mGeom[det-1]->NEta()) return 0;
	
    int ef=e+nEta;
    int sf=s+nPhi;
    int mf=m;
	
    int NE=mGeom[det-1]->NEta();
    int NS=mGeom[det-1]->NSub();
	
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
	    int rid,etmp,stmp;
	    float eta,phi;
	    mGeom[det-1]->getId(mf, ef, sf, rid);
	    mGeom[det-1]->getEtaPhi(rid, eta, phi);
	    mGeom[det-1]->getBin(phi,-eta,mf,etmp,stmp);
	}
	
    int rid;
    if(mf<1 || mf>120) return 0;
    if(ef<1 || ef>NE) return 0;
    if(sf<1 || sf>NS) return 0;
    mGeom[det-1]->getId(mf, ef, sf, rid);
    return rid;

}

float StMuEmcPosition::getDistTowerToTrack( double trackEta, double trackPhi, 
					    int nTowersdEta, int nTowersdPhi )

{     
    int towerId = 0;
    float towerEta = 0, towerToTrackdEta = 0; 
    float towerPhi = 0, towerToTrackdPhi = 0; 
    float mdistTowerToTrack = 0;

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

StThreeVectorF StMuEmcPosition::getPosFromVertex( const StThreeVectorF& position,int TowerId )
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

StThreeVectorF StMuEmcPosition::getPosFromVertex( StMcVertex* vertex,int TowerId )
{
    StThreeVectorF Zero(0,0,0);
    if(TowerId<1 || TowerId>4800) return Zero;
  
    float xTower,yTower,zTower;
    StThreeVectorF position = vertex->position();
    mGeom[0]->getXYZ(TowerId, xTower, yTower, zTower);
    StThreeVectorF towerPosition(xTower, yTower, zTower);
    StThreeVectorF PositionFromVertex = towerPosition - position;
  
    return PositionFromVertex;
}

float StMuEmcPosition::getThetaFromVertex( const StThreeVectorF& vertex,int TowerId )
{
    StThreeVectorF p=getPosFromVertex(vertex,TowerId );
    return p.theta();
}

float StMuEmcPosition::getThetaFromVertex( StMcVertex* vertex,int TowerId )
{
    StThreeVectorF p=getPosFromVertex(vertex,TowerId );
    return p.theta();
}

float StMuEmcPosition::getEtaFromVertex( const StThreeVectorF& vertex,int TowerId )
{
    StThreeVectorF p=getPosFromVertex(vertex,TowerId );
    return p.pseudoRapidity();
}

float StMuEmcPosition::getEtaFromVertex( StMcVertex* vertex,int TowerId )
{
    StThreeVectorF p=getPosFromVertex(vertex,TowerId );
    return p.pseudoRapidity();
}

float StMuEmcPosition::getPhiFromVertex( const StThreeVectorF& vertex,int TowerId )
{
    StThreeVectorF p=getPosFromVertex(vertex,TowerId );
    return p.phi();
}

float StMuEmcPosition::getPhiFromVertex( StMcVertex* vertex,int TowerId )
{
    StThreeVectorF p=getPosFromVertex(vertex,TowerId );
    return p.phi();
}

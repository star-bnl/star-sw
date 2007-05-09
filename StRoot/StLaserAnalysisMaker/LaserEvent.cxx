// 
#include <assert.h>
#include "TRandom.h"
#include "TDirectory.h"
#include "LaserEvent.h"
#if 1
#include "StProbPidTraits.h"
#include "StDedxPidTraits.h"
#endif
#include "StVertex.h"
#include "StPrimaryVertex.h"
#include "StTrack.h"
#include "StPrimaryTrack.h"
#include "StGlobalTrack.h"
#include "StTrackNode.h"
#include "StTpcDb/StTpcDb.h"
#include "StEventTypes.h"
#include "TGeoMatrix.h"
ClassImp(LaserRaft);
ClassImp(EventHeader);
ClassImp(LaserEvent);
ClassImp(Vertex);
ClassImp(Track);
ClassImp(Zees);
TClonesArray *LaserEvent::fgVertices = 0;
TClonesArray *LaserEvent::fgTracks = 0;
TClonesArray *LaserEvent::fgzEast = 0;
TClonesArray *LaserEvent::fgzWest = 0;
TClonesArray *LaserEvent::fgFit = 0;
//______________________________________________________________________________
LaserB::LaserB(const LaserRaft &laser) : Sector(laser.Sector), Raft(laser.Raft), Bundle(laser.Bundle), Mirror(laser.Mirror),
					 XyzL(laser.XyzL), XyzU(laser.XyzU), dirL(laser.dirL), dirU(laser.dirU),
					 Theta(laser.Theta), Phi(laser.Phi){
}
//______________________________________________________________________________
LaserEvent::LaserEvent()
{
   // Create an LaserEvent object.
   // When the constructor is invoked for the first time, the class static
   // variable fgTracks is 0 and the TClonesArray fgTracks is created.

   if (!fgVertices) fgVertices = new TClonesArray("Vertex", 1000);
   fVertices = fgVertices;
   fNvertex = 0;
   if (!fgTracks) fgTracks = new TClonesArray("Track", 1000);
   fTracks = fgTracks;
   fNtrack = 0;

   if (!fgzEast) fgzEast = new TClonesArray("Zees", 1000);
   fzEast = fgzEast;
   fNzEast = 0;
   if (!fgzWest) fgzWest = new TClonesArray("Zees", 1000);
   fzWest = fgzWest;
   fNzWest = 0;
   if (!fgFit) fgFit = new TClonesArray("FitDV", 1000);
   fgFit->ExpandCreate(12);
   fFit = fgFit;
   fNFit = 0;
}

//______________________________________________________________________________
LaserEvent::~LaserEvent()
{
   Clear("C");
}

//______________________________________________________________________________
Vertex *LaserEvent::AddVertex(StPrimaryVertex *vertex) {
  if (! vertex) return 0;
  TClonesArray &vertices = *fVertices;
  Vertex *vx = new(vertices[fNvertex++]) Vertex(vertex);
  const TGeoHMatrix &Tpc2Global = gStTpcDb->Tpc2GlobalMatrix();
  Tpc2Global.MasterToLocal(vx->Xyz.xyz(),vx->XyzL.xyz());
  if (vx->WestEast) {
    Zees *z = 0;
    if (vx->WestEast == 1) z = new((*fgzWest)[fNzWest++]) Zees();
    else                   z = new((*fgzEast)[fNzEast++]) Zees();
    z->zP = vx->XyzL.z();
    z->zL = 0;
    Double_t dv = fEvtHdr.DriftVel();
    Double_t ZE = fEvtHdr.DriftDistance();
    z->dP = ZE - TMath::Abs(z->zP);
    z->tD = z->dP/dv;
    static const  Double_t R = 320; // Radius of middle of supersector 
    z->tL = (2*ZE + TMath::Sqrt(ZE*ZE + R*R))/TMath::Ccgs();
  }
  return vx;
}
//______________________________________________________________________________
Track *LaserEvent::AddTrack(Int_t sector, StTrack *track, LaserB *laser) {
  if (! track) return 0;
  TClonesArray &tracks = *fTracks;
  Track *t = new(tracks[fNtrack++]) Track(sector, track, laser);
  const TGeoHMatrix &Tpc2Global = gStTpcDb->Tpc2GlobalMatrix();
  Tpc2Global.MasterToLocal(t->XyzP.xyz(),t->XyzPL.xyz());
  StThreeVectorD g3 = track->outerGeometry()->momentum();
  t->dirP = g3.unit();
  Tpc2Global.MasterToLocalVect(t->dirP.xyz(),t->dirPL.xyz());
  if (t->Flag == 2 && laser->IsValid) {
    Int_t s2 = (t->mSector-1)/2;
    if (s2 >= 0 && s2 < 12) {
      TClonesArray &fits = *fFit;
      FitDV *fit = (FitDV *) fits[s2]; 
      fit->Sector = t->mSector;
      Int_t N = fit->N + 1;
      Double32_t x = t->Laser.XyzL.z();
      Double32_t y = t->XyzPL.z() - x;
      Double_t xM  = fit->xM  + x;
      Double_t yM  = fit->yM  + y;
      Double_t x2M = fit->x2M + x*x;
      Double_t y2M = fit->y2M + y*y;
      Double_t xyM = fit->xyM + x*y;
      fit->N   = N;
      fit->xM  = xM;
      fit->yM  = yM; 
      fit->x2M = x2M;
      fit->y2M = y2M;
      fit->xyM = xyM;
    }
    Zees *z = 0;
    if (t->mSector <= 12) z = new((*fgzWest)[fNzWest++]) Zees();
    else                  z = new((*fgzEast)[fNzEast++]) Zees();
    z->zP = t->XyzPL.z();
    z->zL = t->Laser.XyzL.z();
    Double_t dv = fEvtHdr.DriftVel();
    Double_t ZE = fEvtHdr.DriftDistance();
    z->dP = ZE - TMath::Abs(z->zP);
    z->tD = z->dP/dv;
    z->tL = (ZE - TMath::Abs(z->zL))/TMath::Ccgs();
  }
  return t;
}
//______________________________________________________________________________
void LaserEvent::Clear(Option_t *option) {
   fTracks->Clear(option);
   fVertices->Clear(option);
   fgzEast->Clear(option);
   fgzWest->Clear(option);
   fgFit->Clear(option);
}
//______________________________________________________________________________
void LaserEvent::Reset() {
// Static function to reset all static objects for this event
//   fgTracks->Delete(option);
   delete fgTracks; fgTracks = 0;
   delete fgVertices; fgVertices = 0;
   SafeDelete(fgzEast);
   SafeDelete(fgzWest);
   SafeDelete(fgFit);
}

//______________________________________________________________________________
void LaserEvent::SetHeader(Int_t i, Int_t run, Int_t date, Int_t time)
{
   fNvertex = 0;
   fNtrack = 0;
   fNzEast = fNzWest = fNFit =0;
   fEvtHdr.Set(i, run, date, time);
}
//______________________________________________________________________________
void LaserEvent::SetHeader(Int_t i, Int_t run, Int_t date, Int_t time,
     Float_t tzero, Float_t drivel, Float_t clock)
{
   fNvertex = 0;
   fNtrack = 0;
   fNzEast = fNzWest = fNFit = 0;
   fEvtHdr.Set(i, run, date, time);
   fEvtHdr.SetE(tzero, drivel, clock);
}
//______________________________________________________________________________
void LaserEvent::SetHeader(Int_t i, Int_t run, Int_t date, Int_t time,
     Float_t tzero, Float_t drivel, Float_t clock, Float_t trigger)
{
   fNvertex = 0;
   fNtrack = 0;
   fNzEast = fNzWest = fNFit = 0;
   fEvtHdr.Set(i, run, date, time);
   fEvtHdr.SetE(tzero, drivel, clock, trigger);
}
//______________________________________________________________________________
Track::Track(Int_t sector, StTrack *track, LaserB *theLaser) : Flag(0),mType(kUndefinedVtxId),  mSector(sector), 
							       XyzP(), thePath(0), dPhi(-999), dTheta(-999) {
  StTrackNode*   node = track->node();
  StGlobalTrack* gTrack = static_cast<StGlobalTrack*>(node->track(global));
  fgeoIn = *((StHelixModel *) gTrack->geometry());
  fgeoOut = *((StHelixModel *) gTrack->outerGeometry());
  StPhysicalHelixD helixO = fgeoOut.helix();
  fTheta = fgeoOut.momentum().theta();
  fPhi   = fgeoOut.momentum().phi();
  if (theLaser) {
    thePath = helixO.pathLength(theLaser->XyzG.x(),theLaser->XyzG.y());
    XyzP   = helixO.at(thePath);
    Flag   = 2;
    Laser  = *theLaser; 
  }
  if (gTrack) {
    StPrimaryTrack *pTrack = 	static_cast<StPrimaryTrack*>(node->track(primary));
    if (pTrack) {
      StPrimaryVertex* vertex = (StPrimaryVertex*) pTrack->vertex();
      if (vertex) {
	mType = vertex->type();
	Vertex = vertex->position();
	Flag = 1;
      }
    }
    mKey = gTrack->key();
    mFlag = gTrack->flag();
    mNumberOfPossiblePointsTpc = gTrack->numberOfPossiblePoints(kTpcId);
    mImpactParameter = gTrack->impactParameter();
    mLength = gTrack->length();
#if 1
    StTrackFitTraits&  fitTraits =  gTrack->fitTraits();
    mNumberOfFitPointsTpc = fitTraits.numberOfFitPoints(kTpcId);
    mPrimaryVertexUsedInFit = fitTraits.primaryVertexUsedInFit();

    StSPtrVecTrackPidTraits &traits = gTrack->pidTraits();
    unsigned int size = traits.size();
    StDedxPidTraits *pid = 0;
    for (unsigned int i = 0; i < size; i++) {
      if (! traits[i]) continue;
      if ( traits[i]->IsZombie()) continue;
      pid = dynamic_cast<StDedxPidTraits*>(traits[i]);
      if (pid && pid->detector() == kTpcId && pid->method() == kTruncatedMeanId) {
	fNdEdx = pid->numberOfPoints();      // Number of points used in dE/dx calc 
	fdEdx =  pid->mean();       
	break;
      }
    }
#endif
  }
}
//______________________________________________________________________________
Vertex::Vertex(StPrimaryVertex *vertex) : mType(kUndefinedVtxId), WestEast(0), 
					  Xyz(), numberOfDaughter(0){
  if (vertex) {
    mType = vertex->type();
    Xyz = vertex->position();
    numberOfDaughter = vertex->numberOfDaughters();
    if (numberOfDaughter > 24) {
      for (UInt_t i = 0; i < numberOfDaughter; i++) {
	StTrack *track = vertex->daughter(i);
	if (! track ) continue;
	StPtrVecHit hvec = track->detectorInfo()->hits();
	for (unsigned int j=0; j<hvec.size(); j++) {// hit loop
	  if (hvec[j]->detector() != kTpcId) continue;
	  StTpcHit *tpcHit = static_cast<StTpcHit *> (hvec[j]);
	  Int_t sector = tpcHit->sector();
	  Int_t WE = sector <= 12 ? 1 : 2;
	  if (! WestEast) WestEast = WE;
	  else {
	    if (WE != WestEast) {
	      WestEast = 0;
	      break;
	    }
	  }
	}
      }
    }
  }
}
//________________________________________________________________________________
Int_t Track::Matched() {
  Int_t iok = 0;
  dPhi = -999;
  dTheta = -999;
  iok = 1 << 10;
  if (Flag != 2) return iok; 
  iok = 10;
  if (Laser.Sector < 1 || Laser.Sector > 24 ||
      Laser.Mirror < 1 || Laser.Mirror > 7) return iok; 
  iok = 0;
  Int_t status = 0;
  if (TMath::Abs(dU.x()) > 0.05) status |= 1 << 3;
  if (TMath::Abs(dU.y()) > 0.05) status |= 1 << 4;
  if (thePath < 5 || thePath > 25)    status |= 1 << 5;
  if (mNumberOfFitPointsTpc  < 25)    status |= 1 << 6;
  dTheta = Laser.ThetaG-fgeoOut.dipAngle()-TMath::Pi()/2;
#if 0
  if (TMath::Abs(dTheta) > 0.030)                  status |= 1 << 7;;
#endif
  dPhi = Laser.PhiG - fgeoOut.psi();
  if (dPhi >=  TMath::Pi()) dPhi -= 2*TMath::Pi();
  if (dPhi <= -TMath::Pi()) dPhi += 2*TMath::Pi();
  //  if (SectorMirror[m][i].sigma > 0 && TMath::Abs(dPhi) > 5*SectorMirror[m][i].sigma) status |= 1 << 8;
#if 0
  if ( TMath::Abs(dPhi) > 0.020)  status |= 1 << 9;
#endif
  return iok + 10*status;
}

//$Id: LaserEvent.cxx,v 1.10 2018/04/11 02:43:21 smirnovd Exp $
// $Log: LaserEvent.cxx,v $
// Revision 1.10  2018/04/11 02:43:21  smirnovd
// Enable TPC/iTPC switch via St_tpcPadConfig
//
// This is accomplished by substituting St_tpcPadPlanes with St_tpcPadConfig.
// A sector ID is passed to St_tpcPadConfig in order to extract parameters for
// either TPC or iTPC
//
// Revision 1.9  2014/03/13 21:59:44  fisyak
// add cluster position in Local Sector Coordinate System
//
// Revision 1.8  2014/02/13 18:21:28  fisyak
// Add protection against cicling in fitting
//
// Revision 1.7  2011/01/10 20:36:12  fisyak
// Use sector/padrow in global => local transformation
//
// Revision 1.6  2008/06/02 13:48:02  fisyak
// Add  t0 handlers for Tpx/Tpc time offsets
//
// Revision 1.5  2007/12/10 19:54:02  fisyak
// Add Id and Log, correct spelling error in README
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
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StEventTypes.h"
#include "TGeoMatrix.h"
#include "StDetectorDbMaker/St_tpcPadConfigC.h"
ClassImp(LaserRaft);
ClassImp(EventHeader);
ClassImp(LaserEvent);
ClassImp(Vertex);
ClassImp(Track);
ClassImp(Hit);
TClonesArray *LaserEvent::fgVertices = 0;
TClonesArray *LaserEvent::fgTracks = 0;
TClonesArray *LaserEvent::fgHits = 0;
TClonesArray *LaserEvent::fgFit = 0;
//______________________________________________________________________________
LaserB::LaserB(const LaserRaft &laser) : Sector(laser.Sector), Raft(laser.Raft), Bundle(laser.Bundle), Mirror(laser.Mirror),
					 XyzL(laser.XyzL), XyzU(laser.XyzU), XyzB(laser.XyzB), 
					 dirL(laser.dirL), dirU(laser.dirU),dirB(laser.dirB),
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

   if (!fgHits) fgHits = new TClonesArray("Hit", 1000);
   fHits = fgHits;
   fNhit = 0;

   if (!fgFit) fgFit = new TClonesArray("FitDV", 1000);
   fgFit->ExpandCreate(12);
   fFit = fgFit;
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
  return vx;
}
//______________________________________________________________________________
Track *LaserEvent::AddTrack(Int_t sector, StTrack *track, LaserB *laser, Double_t z) {
  if (! track) return 0;
  TClonesArray &tracks = *fTracks;
  Track *t = new(tracks[fNtrack++]) Track(sector, track, laser, z);
  return t;
}
//______________________________________________________________________________
Hit *LaserEvent::AddHit(StTpcHit *hit, Int_t trackKey) {
  if (! hit) return 0;
  TClonesArray &hits = *fHits;
  Hit *t = new(hits[fNhit++]) Hit(hit,trackKey);
  return t;
}
//________________________________________________________________________________
void LaserEvent::AddTrackFit(Track *t) {
  if (! t) return;
  Int_t sector = t->Laser.Sector;
  Int_t bundle =  t->Laser.Bundle;
  Int_t mirror =  t->Laser.Mirror;
  Int_t s2 = (sector-1)/2;
  if (s2 >= 0 && s2 < 12) {
    TClonesArray &fits = *fFit;
    FitDV *fit = (FitDV *) fits[s2]; 
    fit->Sector = sector;
    Int_t N = fit->N;
    Double32_t x = t->Laser.XyzL.z();
    Double32_t y = t->XyzPL.z() - x;
    if (N < 42) {
      fit->X[N] = x;
      fit->Y[N] = y;
      fit->Bundle[N] =  bundle;
      fit->Mirror[N] =  mirror;
      fit->N   = N+1;
    }
  }
}
//______________________________________________________________________________
void LaserEvent::Clear(Option_t *option) {
   fTracks->Clear(option);
   fHits->Clear(option);
   fVertices->Clear(option);
   fgFit->Clear(option);
}
//______________________________________________________________________________
void LaserEvent::Reset() {
  // Static function to reset all static objects for this event
  SafeDelete(fgTracks);
  SafeDelete(fgHits);
  SafeDelete(fgVertices);
  SafeDelete(fgFit);
}

//______________________________________________________________________________
void LaserEvent::SetHeader(Int_t i, Int_t run, Int_t date, Int_t time)
{
   fNvertex = 0;
   fNtrack = 0;
   fNhit = 0;
   fEvtHdr.Set(i, run, date, time);
}
//______________________________________________________________________________
void LaserEvent::SetHeader(Int_t i, Int_t run, Int_t date, Int_t time,
     Float_t tzero, Float_t drivel, Float_t clock)
{
   SetHeader(i, run, date, time);
   fEvtHdr.SetE(tzero, drivel, clock);
}
//______________________________________________________________________________
void LaserEvent::SetHeader(Int_t i, Int_t run, Int_t date, Int_t time,
     Float_t tzero, Float_t drivel, Float_t clock, Float_t trigger)
{
   SetHeader(i, run, date, time);
   fEvtHdr.SetE(tzero, drivel, clock, trigger);
}
//______________________________________________________________________________
Track::Track(Int_t sector, StTrack *track, LaserB *theLaser, Double_t z) : 
  Flag(0),mType(kUndefinedVtxId),  mSector(sector), 
  fpTInv(-999), thePath(0), dPhi(-999), dTheta(-999), zLastHit(z) {
  StTrackNode*   node = track->node();
  StGlobalTrack* gTrack = static_cast<StGlobalTrack*>(node->track(global));
  fgeoIn = *((StHelixModel *) gTrack->geometry());
  fgeoOut = *((StHelixModel *) gTrack->outerGeometry());
  fDca    = *((StDcaGeometry *) gTrack->dcaGeometry());
  StThreeVectorD g3 = fgeoOut.momentum();
  fpTInv = fgeoOut.charge()/g3.perp();
  fTheta = fgeoOut.momentum().theta();
  fPhi   = fgeoOut.momentum().phi();
  StPhysicalHelixD helixO = fgeoOut.helix();
  thePath = helixO.pathLength(Laser.XyzG.x(),Laser.XyzG.y());
  if (theLaser) Laser = *theLaser;
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
void Track::SetPredictions(TGeoHMatrix *Raft2Tpc, TGeoHMatrix *Bundle2Tpc, TGeoHMatrix *Mirror2Tpc) {
  if ( Flag ) return;
  if ( Laser.Sector < 1 || Laser.Sector > 24) return;
  Flag   = 2;
  const TGeoHMatrix &Tpc2Global = gStTpcDb->Tpc2GlobalMatrix();
  StPhysicalHelixD helixO = fgeoOut.helix();
  thePath = helixO.pathLength(Laser.XyzG.x(),Laser.XyzG.y());
  XyzP   = helixO.at(thePath);
  Tpc2Global.MasterToLocal(XyzP.xyz(),XyzPL.xyz());
  StThreeVectorD g3 = fgeoOut.momentum();
  dirP = g3.unit();
  Tpc2Global.MasterToLocalVect(dirP.xyz(),dirPL.xyz());
  dU = StThreeVectorD(999.,999.,999.);
  if (Raft2Tpc) {
    Raft2Tpc->MasterToLocal(XyzPL.xyz(),XyzPU.xyz());
    Raft2Tpc->MasterToLocalVect(dirPL.xyz(),dirPU.xyz());
    dU = XyzPU - Laser.XyzU;
  }
  if (Bundle2Tpc) {
    Bundle2Tpc->MasterToLocal(XyzPL.xyz(),XyzPB.xyz());
    Bundle2Tpc->MasterToLocalVect(dirPL.xyz(),dirPB.xyz());
  }
  if (Mirror2Tpc) {
    Mirror2Tpc->MasterToLocal(XyzPL.xyz(),XyzPM.xyz());
    Mirror2Tpc->MasterToLocalVect(dirPL.xyz(),dirPM.xyz());
  }
  Flag += Matched();
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
#if 1
  if (TMath::Abs(dTheta) > 0.030)                  status |= 1 << 7;;
#endif
  dPhi = Laser.PhiG - fgeoOut.psi();
  if (dPhi >=  TMath::Pi()) dPhi -= 2*TMath::Pi();
  if (dPhi <= -TMath::Pi()) dPhi += 2*TMath::Pi();
  //  if (SectorMirror[m][i].sigma > 0 && TMath::Abs(dPhi) > 5*SectorMirror[m][i].sigma) status |= 1 << 8;
#if 1
  if ( TMath::Abs(dPhi) > 0.020)  status |= 1 << 9;
#endif
  static const Double_t pTInv0 = 4.78815e-03;
  static const Double_t DpTInv0 = 9.75313e-03;
  if (TMath::Abs(fpTInv - pTInv0) > 3.0*DpTInv0) status |= 1 << 10;
  return iok + 10*status;
}
//________________________________________________________________________________
Hit::Hit(StTpcHit *tpcHit, Int_t trKey)  : sector(0),row(0),charge(0),flag(0),usedInFit(0), trackKey(trKey) {
  if (tpcHit) {
    hit = *tpcHit;
    sector = tpcHit->sector();
    row = tpcHit->padrow();;
    charge = tpcHit->charge();
    flag = tpcHit->flag();
    usedInFit = tpcHit->usedInFit();
    xyz = tpcHit->position(); // from StTpcHitMover
    static StTpcCoordinateTransform transform(gStTpcDb);
    StGlobalCoordinate glob(xyz.x(),xyz.y(),xyz.z());
    static StTpcLocalSectorCoordinate  local;
    transform(glob,local,tpcHit->sector(),tpcHit->padrow()); 
    xyzL = StThreeVectorF(local.position().x(),local.position().y(),local.position().z());
    static StTpcLocalCoordinate  localTpc;
    transform(glob,localTpc,tpcHit->sector(),tpcHit->padrow()); 
    xyzTpcL = StThreeVectorF(localTpc.position().x(),localTpc.position().y(),localTpc.position().z());
    Double_t xyzs[3];
    gStTpcDb->SupS2Tpc(tpcHit->sector()).MasterToLocal(localTpc.position().xyz(),xyzs);
    xyzS = StThreeVectorF(xyzs[0],xyzs[1],xyzs[2]);
    pad  = tpcHit->pad() - St_tpcPadConfigC::instance()->padsPerRow(sector,row)/2 - 1;
    tbk  = tpcHit->timeBucket();
  }
}

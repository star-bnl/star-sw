// 

#include "TRandom.h"
#include "TDirectory.h"
#include "LaserEvent.h"
#include "StProbPidTraits.h"
#include "StDedxPidTraits.h"
#include "StVertex.h"
#include "StPrimaryVertex.h"
#include "StTrack.h"
#include "StPrimaryTrack.h"
#include "StGlobalTrack.h"
#include "StTrackNode.h"
ClassImp(EventHeader)
ClassImp(LaserEvent)
ClassImp(Vertex)
ClassImp(Track)
TClonesArray *LaserEvent::fgVertices = 0;
TClonesArray *LaserEvent::fgTracks = 0;
//______________________________________________________________________________
LaserB::LaserB(const Raft_t &laser) : Sector(laser.Sector), Raft(laser.Raft), Bundle(laser.Bundle), Mirror(laser.Mirror),
				      XyzL(laser.X, laser.Y, laser.Z), Theta(laser.Theta), dPhi(laser.Phi)  {
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
}

//______________________________________________________________________________
LaserEvent::~LaserEvent()
{
   Clear();
}

//______________________________________________________________________________
void LaserEvent::AddVertex(StPrimaryVertex *vertex) {
  if (! vertex) return;
  TClonesArray &vertices = *fVertices;
  new(vertices[fNvertex++]) Vertex(vertex);
}
//______________________________________________________________________________
void LaserEvent::AddTrack(Int_t sector, StTrack *track, LaserB *laser) {
  if (! track) return;
  TClonesArray &tracks = *fTracks;
  Track *t = new(tracks[fNtrack++]) Track(sector, track, laser);
  if (t->Flag == 1) {
    FitDV *fit = 0;
    if (t->mSector <= 12) fit = &West;
    else                  fit = &East;
    fit->N++;
    Double32_t x = t->Laser.XyzG.z();
    Double32_t y = t->XyzP.z() - x;
    fit->xM += x;
    fit->yM += y;
    fit->x2M += x*x;
    fit->y2M += y*y;
    fit->xyM += x*y;
  }
}
//______________________________________________________________________________
void LaserEvent::Clear(Option_t *option) {
   fTracks->Clear(option);
   fVertices->Clear(option);
}
//______________________________________________________________________________
void LaserEvent::Reset() {
// Static function to reset all static objects for this event
//   fgTracks->Delete(option);
   delete fgTracks; fgTracks = 0;
   delete fgVertices; fgVertices = 0;
}

//______________________________________________________________________________
void LaserEvent::SetHeader(Int_t i, Int_t run, Int_t date, Int_t time)
{
   fNvertex = 0;
   fNtrack = 0;
   fEvtHdr.Set(i, run, date, time);
}
//______________________________________________________________________________
void LaserEvent::SetHeader(Int_t i, Int_t run, Int_t date, Int_t time,
     Float_t tzero, Float_t drivel, Float_t clock)
{
   fNvertex = 0;
   fNtrack = 0;
   fEvtHdr.Set(i, run, date, time);
   fEvtHdr.SetE(tzero, drivel, clock);
}
//______________________________________________________________________________
void LaserEvent::SetHeader(Int_t i, Int_t run, Int_t date, Int_t time,
     Float_t tzero, Float_t drivel, Float_t clock, Float_t trigger)
{
   fNvertex = 0;
   fNtrack = 0;
   fEvtHdr.Set(i, run, date, time);
   fEvtHdr.SetE(tzero, drivel, clock, trigger);
}
//______________________________________________________________________________
Track::Track(Int_t sector, StTrack *track, LaserB *theLaser) : Flag(0), mSector(sector), 
							       XyzP(), thePath(0), dPhi(-999), dDip(-999) {
  StTrackNode*   node = track->node();
  StGlobalTrack* gTrack = static_cast<StGlobalTrack*>(node->track(global));
  fgeoIn = *((StHelixModel *) gTrack->geometry());
  fgeoOut = *((StHelixModel *) gTrack->outerGeometry());
  StPhysicalHelixD helixO = fgeoOut.helix();
  if (theLaser) {
    thePath = helixO.pathLength(theLaser->XyzG.x(),theLaser->XyzG.y());
    XyzP   = helixO.at(thePath);
    Flag   = 1;
    Laser  = *theLaser; 
    fTheta = fgeoOut.momentum().theta();
    fPhi   = fgeoOut.momentum().phi();
    Flag  += Matched();
  }
  if (gTrack) {
    StPrimaryTrack *pTrack = 	static_cast<StPrimaryTrack*>(node->track(primary));
    if (pTrack) {
      StPrimaryVertex* vertex = (StPrimaryVertex*) pTrack->vertex();
      if (vertex) {
	mType = vertex->type();
	Vertex = vertex->position();
	Flag = 2;
      }
    }
    mKey = gTrack->key();
    mFlag = gTrack->flag();
    mNumberOfPossiblePointsTpc = gTrack->numberOfPossiblePoints(kTpcId);
    mImpactParameter = gTrack->impactParameter();
    mLength = gTrack->length();

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
  }
}
//______________________________________________________________________________
Vertex::Vertex(StPrimaryVertex *vertex) : mType(kUndefinedVtxId), 
					  Xyz(), numberOfDaughter(0){
  if (vertex) {
    mType = vertex->type();
    Xyz = vertex->position();
    numberOfDaughter = vertex->numberOfDaughters();
  }
}
//________________________________________________________________________________
Int_t Track::Matched() {
  Int_t iok = 0;
  struct SectorCorr_t {
    Int_t sector2;
    Double_t phi;
    Double_t dphi;
    Double_t sigma;
    Double_t dsigma;
  };
  SectorCorr_t SectorCorr0[12] = {
    // Mirror 4
    {2,     -0.0186195,     1.4527e-05,     0.00184551,     1.02723e-05}, 
    {4,     -0.0196896,     2.33502e-05,    0.00184777,     1.65113e-05},
    {6,     -0.005116,      2.93135e-05,    0.00260989,     2.07281e-05},
    {8,     0.00437859,     2.77778e-05,    0.00329948,     1.96424e-05},
    {10,    0,      0,      0,      0},
    {12,    0.000916182,    2.43739e-05,    0.00234977,     1.72353e-05},
    {14,    -0.0180773,     2.52572e-05,    0.00275963,     1.78601e-05},
    {16,    0.00312329,     1.55691e-05,    0.00204276,     1.10091e-05},
    {18,    -0.00609319,    1.41944e-05,    0.00136081,     1.00371e-05},
    {20,    0.00649718,     2.39854e-05,    0.00278396,     1.69605e-05},
    {22,    0.0201383,      2.10855e-05,    0.00128673,     1.49099e-05},
    {24,    0.0374981,      4.71165e-05,    0.00324044,     3.33172e-05}
  };
  SectorCorr_t SectorMirror[7][12] = {
    { //Mirror 1
      { 2,    0,      0,      0,      0},
      { 4,    0,      0,      0,      0},
      { 6,    0,      0,      0,      0},
      { 8,    0,      0,      0,      0},
      {12,    0,      0,      0,      0},
      {14,    0,      0,      0,      0},
      {16,    0,      0,      0,      0},
      {18,    0,      0,      0,      0},
      {20,    0,      0,      0,      0},
      {22,    0,      0,      0,      0},
      {24,    0,      0,      0,      0}
    },
    {//Mirror 2
      {2,  1.47897e-02,   2.50330e-05, 5.15724e-04,   2.14312e-05},// dominant peak  0.00566472,     0.00047955,     0.0125602,      0.000339109},
      {4, -6.08707e-02,   2.16937e-05, 6.93992e-04,   1.60809e-05},// single peak   -0.0609268,     2.06028e-05,    0.000791607,    1.4584e-05},
      {6,     0,      0,      0,      0},
      {8, -3.50111e-03,   3.93290e-05, 1.60574e-03,   2.70401e-05},// dominant peak-0.00664828,    0.000132061,    0.00628642,     9.33846e-05},
      {10,    0,      0,      0,      0},
      {12,-1.45117e-02,   5.64167e-05, 1.54124e-03,   3.65524e-05},// dominant peak -0.00821915,    0.000208234,    0.00970468,     0.00014725},
      {14,-3.28631e-02,   9.47326e-05, 1.92888e-03,   6.99189e-05},// single peak  -0.0331542,     0.000115018,    0.00245071,     8.13329e-05},
      {16, 2.65153e-02,   1.00910e-05, 5.09772e-04,   5.99093e-06},// dominant peak    0.0208911,      0.000809766,    0.0810765,      0.000671043},
      {18, 2.26290e-02,   1.61950e-05, 5.65004e-04,   8.09284e-06},// single peak    0.0226052,      1.64671e-05,    0.000650616,    1.1386e-05},
      {20,-6.06357e-02,   1.44520e-05, 7.23663e-04,   1.35932e-05},// dominant peak -0.0143382,     0.000654378,    0.0481428,      0.000464407},
      {22, 6.01550e-03,   9.13966e-05, 1.07660e-03,   6.33931e-05},// dominant peak 0.0116422,      0.000241984,    0.00731575,     0.000171114},
      {24,-5.20394e-02,   2.44742e-05, 9.68631e-04,   1.61593e-05} // single peak   -0.0520774,     2.32609e-05,    0.00107706,     1.64484e-05},
    },
    {//Mirror 3
      {2,  0.0172837,  9.29745e-06, 0.000480627, 6.46204e-06},// dominant peak  0.0253127,      0.000130236,    0.0110478,      9.20956e-05},
      {4,  0.00212475, 5.27541e-05, 0.00229639,  3.73615e-05},// dominant peak  0.00355427,     0.000111547,    0.00517944,     7.88774e-05},
      {6, -0.00222559, 6.13796e-06, 6.34452e-05, 1.34243e-06},// dominant peak   0.00362372,     0.000262839,    0.0216966,      0.000185867},
      {8,  0.0433267,  9.80298e-06, 0.00051389,  6.37242e-06},// multiple peaks   0.0261529,      0.000166797,    0.0148252,      0.000117951},
      {10,    0,      0,      0,      0},
      {12, 0.0246517, 1.91944e-05, 0.00101986, 1.51846e-0 },// dominant peak  0.0371178,      0.000394592,    0.0280604,      0.000279036},
      {14, 0.0047278, 0.000260374, 0.00157437, 0.000349249},// dominant peak   0.00253371,     0.000508345,    0.00479571,     0.000359431},
      {16, 0.00466532,1.16948e-05, 0.000832494,7.36983e-06},// dominant peak  0.000905332,    4.07978e-05,    0.00421818,     2.88493e-05},
      {18,-0.00221707,6.30134e-05, 0.00184436, 3.70755e-05},// single peak -0.00226442,    5.92677e-05,    0.00191133,     4.19086e-05},
      {20,-0.00400158,6.86993e-05, 0.00225299, 4.08153e-05},//  multiple peaks     -0.0195078,     0.00102821,     0.0795717,      0.000842814},
      {22, 0.00392405,0.00010365,  0.00119326, 0.000110543},// single peak -0.0034619,     0.0001722,      0.00249542,     0.000121762},
      {24, 0.00557869,2.23633e-05, 0.000991408,1.30714e-05} // single peak 0.00547582,     2.62727e-05,    0.00137273,     1.8578e-05},
    },
    {//Mirror 4
      {2,  -2.0143e-05, 1.65561e-05, 0.00171236, 7.53284e-06},// single peak   -4.46201e-06,   1.45853e-05,    0.00182683,     1.03135e-05},
      {4,  -0.000256831,2.38597e-05, 0.00101118, 2.03843e-05},// single peak   -0.000404097,   2.02919e-05,    0.00147685,     1.43488e-05},
      {6,   0.001134,   1.91263e-05, 0.00134625, 1.07783e-05},// single peak  0.000750153,    2.58916e-05,    0.00209322,     1.83084e-05},
      {8,  -0.00081522, 1.04845e-05, 0.000613908,1.08238e-05},//  multiple peaks -1.54491e-05,   2.72895e-05,    0.00322305,     1.9297e-05},
      {10,    0,      0,      0,      0},
      {12,  0.000987496,1.66857e-05, 0.00104992, 1.11315e-05},// single peak 0.000355621,    2.22063e-05,    0.00201857,     1.57025e-05},
      {14,    -0.00122242,    4.50794e-05,    0.00344171,     3.18769e-05},// double peak ?
      {16,  0.000131596,2.89793e-05, 0.00185695, 1.72874e-05},// single peak 0.000109311,    1.80858e-05,    0.00217127,     1.27887e-05},
      {18,  0.000805035,1.34267e-05, 0.000764655,7.61076e-06},// single peak 0.000650699,    1.40839e-05,    0.00109203,     9.95901e-06},
      {20, -0.00240851, 1.75617e-05, 0.00109031, 1.29384e-05},// dominant peak  -0.000293356,   2.83839e-05,    0.00285453,     2.00709e-05},
      {22, -0.000366494,1.51986e-05, 0.000679965,1.06328e-05},// single peak
      {24,  0.00159829, 1.97649e-05, 0.00106986, 1.39759e-05} // single peak
    },
    {//Mirror 5 
      {2,   0.0202492, 1.21405e-05, 0.000651143, 8.64812e-06},// dominant peak  -0.0477053,     0.00105037,     0.0881546,      0.000890414},
      {4,  -0.127498,  4.41647e-05, 0.0015842,   2.73322e-05},// dominant peak -0.11625,       0.00263391,     0.0772389,      0.0018637},
      {6,  -0.0111096, 1.71769e-05, 0.000746844, 1.27916e-05},// dominant peak -0.0264714,     0.00102381,     0.0801099,      0.00084246},
      {8,   0.0115083, 1.07084e-05, 0.000388096, 4.30121e-06},// bad fit dominant peak  0.0145057,      0.000549461,    0.0546474,      0.000394043},
      {10,    0,      0,      0,      0},
      {12,  0.0797944, 2.68801e-05, 0.000825459, 2.09022e-05},// dominant peak 0.000852137,    0.00198958,     0.102371,       0.00205952},
      {14,    -0.00656188,    3.06239e-05,    0.00137091,     2.16544e-05},// single peak
      {16, -0.00293398, 1.50938e-05, 0.000800728, 1.4194e-05},// dominant peak  0.000220087,    5.00353e-05,    0.00372452,     3.53814e-05},
      {18,    0,      0,      0,      0}, //only 4 entries
      {20,  0.0247521,  1.0784e-05,  0.000544184, 8.77057e-06},// single peak  0.0247561,      1.25756e-05,    0.000640794,    8.9043e-06},
      {22,     0,      0,      0,      0},// only 12 entries  0.00257692,     0.000254232,    0.000916646,    0.000179642},
      {24,    -0.0035,        0.00010766,     0.000854507,    7.6101e-05}// single peak only 30 entries 
    },
    {//Mirror 6 
      {2,     -0.0211806,     8.82803e-05,    0.00193211,     6.24233e-05},// single peak
      {4,      0.00960692, 0.000803011, 0.00207609, 0.000503285},// single peak  0.00873809,     0.000184719,    0.00146617,     0.000130616},
      {6,     -0.019809,      6.20976e-05,    0.00178037,     4.39096e-05},// single peak
      {8,     -0.0244286,     3.08233e-05,    0.00143618,     2.17956e-05},// single peak
      {10,    0,      0,      0,      0},
      {12,    0.00980073, 9.14786e-05, 0.00101579, 6.82576e-05},// dominant peaks 0.02598,        0.00102381,     0.0169779,      0.000723964},
      {14,    0,      0,      0,      0},
      {16,    -0.000329188, 6.08403e-05, 0.000869988, 4.33747e-05},// single peak -0.000268182,   7.28943e-05,    0.0010812,      5.15429e-05},
      {18,    -0.00445835, 0.000235526, 0.000686245, 0.000139439},// dominant  -0.00399438,    6.93754e-05,    0.000925585,    4.9056e-05},
      {20,     0.00150319, 0.000115474, 0.000571378, 8.76884e-05},// double peak 0.0029, 0.000228035,    0.00161245,     0.000161221},
      {22,    0,      0,      0,      0},
      {24,    0.00230525,     5.80364e-05,    0.0015616,      4.10378e-05}// single peak
    },
    {//Mirror 7
      { 2,    0,      0,      0,      0},
      { 4,    0,      0,      0,      0},
      { 6,    0,      0,      0,      0},
      { 8,    0,      0,      0,      0},
      {12,    0,      0,      0,      0},
      {14,    0,      0,      0,      0},
      {16,    0,      0,      0,      0},
      {18,    0,      0,      0,      0},
      {20,    0,      0,      0,      0},
      {22,    0,      0,      0,      0},
      {24,    0,      0,      0,      0}
    }
  };
  dPhi = -999;
  dDip = -999;
  iok = 999;
  if (Flag != 1) return iok; 
  iok = 10;
  if (Laser.Sector < 1 || Laser.Sector > 24 ||
      Laser.Mirror < 1 || Laser.Mirror > 7) return iok; 
  iok = 20;
  
  Int_t i = (Laser.Sector-1)/2;
  Int_t m = Laser.Mirror - 1;
  iok = 0;
  if (SectorCorr0[i].sigma <= 0) iok += 100;
  if (SectorMirror[m][i].sigma <= 0) iok += 200;
  if (TMath::Abs(XyzP.x()-Laser.XyzG.x()) > 0.1) iok += 400;
  if (TMath::Abs(XyzP.y()-Laser.XyzG.y()) > 0.1) iok += 800;
  if (thePath < 5 || thePath > 12)               iok += 1600;
  if (mNumberOfFitPointsTpc  < 25)               iok += 3200;
  dDip = Laser.ThetaG-fgeoOut.dipAngle()-TMath::Pi()/2;
  //  if (TMath::Abs(dDip) > 0.050)                  iok += 6400;
  dPhi = Laser.PhiG - fgeoOut.psi() - SectorCorr0[i].phi - SectorMirror[m][i].phi;
  if (dPhi >=  TMath::Pi()) dPhi -= 2*TMath::Pi();
  if (dPhi <= -TMath::Pi()) dPhi += 2*TMath::Pi();
  //  if (SectorMirror[m][i].sigma > 0 && TMath::Abs(dPhi) > 5*SectorMirror[m][i].sigma) iok += 12800;
  return iok;
}

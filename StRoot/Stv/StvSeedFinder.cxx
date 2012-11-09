#include "StvSeedFinder.h"
#include "TSystem.h"
#include "StvDraw.h"
#include "StvHit.h"
#include "vector"
#include "StarVMC/GeoTestMaker/StTGeoHelper.h"

ClassImp(StvSeedFinder)
//_____________________________________________________________________________
StvSeedFinder::StvSeedFinder(const char *name):TNamed(name,"")
{ fDraw=0;fDoShow=0;
  fMinHits = 10; fGoodHits = 10;
}
//_____________________________________________________________________________
void StvSeedFinder::Clear(const char*)
{
 fSeedHits.clear();
 if(fDraw) fDraw->Clear();
}
//_____________________________________________________________________________
void StvSeedFinder::Show()
{
  if (!fDraw) fDraw = NewDraw();
  fDraw->Trak(fHelix,fSeedHits,kGlobalTrack);
  fDraw->UpdateModified();
}
//_____________________________________________________________________________
void StvSeedFinder::ShowRest(EDraw3DStyle style)
{
   if (!fDraw) fDraw = NewDraw();
   std::vector<StvHit*> myHits;  
   const StVoidArr *hitArr =  StTGeoHelper::Inst()->GetSeedHits();
   int nHits =  hitArr->size();
   for (int iHit=0;iHit<nHits;iHit++) {
     StvHit *stiHit = (StvHit*)(*hitArr)[iHit];
     if (stiHit->timesUsed()) continue;
     myHits.push_back(stiHit);
   }
   fDraw->Hits(myHits,style);
   fDraw->UpdateModified();
}
//_____________________________________________________________________________
StvDraw *StvSeedFinder::NewDraw()
{
   StvDraw *dr = new StvDraw();
   dr->SetBkColor(kWhite);
   return dr;
}
//_____________________________________________________________________________
void StvSeedFinder::DoShow(int lev)
{
  fDoShow = lev;
  if (fDoShow) {if (!fDraw) fDraw=NewDraw();}
  else         { delete fDraw;fDraw=0      ;}

}

//_____________________________________________________________________________
const THelixTrack *StvSeedFinder::Approx()
{
static int nCall=0; nCall++;

#ifdef APPROX_DEBUG
static TCanvas *myCanvas=0;
static TH1  *H[2];
if(!myCanvas) {
   myCanvas=new TCanvas("Approx","",600,800);
   H[0] = new TH1F("Xi2","Xi2", 100,0,5);
   H[1] = new TProfile("nHits ","nHits",  30,0,30);
   myCanvas->Divide(1,2);
   for (int i=0;i<2;i++) {myCanvas->cd(i+1); H[i]->Draw();}
}
#endif // APPROX_DEBUG

const double kTpcHitErr = 0.2;
const double BAD_XI2=333*kTpcHitErr*kTpcHitErr;
const double BAD_RHO=0.1;

//		Loop over nodes and collect global xyz

  THelixFitter &circ = fHelix;
  circ.Clear();
  int nNode=fSeedHits.size();
  for (int iNode = 0; iNode<nNode;++iNode) {
    const StvHit * hit = fSeedHits[iNode];
    circ.Add(hit->x()[0],hit->x()[1],hit->x()[2]);
  }  
  double Xi2 =circ.Fit();
  if (Xi2>BAD_XI2) return 0; //Xi2 too bad, no updates
  if (fabs(circ.GetRho()) >BAD_RHO) return 0;
  int startSeed = 0;
  if (fHelix.Pos()[0]*fHelix.Dir()[0]+fHelix.Pos()[1]*fHelix.Dir()[1]>0) {
    fHelix.Backward(); startSeed = fSeedHits.size()-1;
  }
  const float *fx = fSeedHits[startSeed]->x();
  const double dx[3]={fx[0],fx[1],fx[2]};
  double l = fHelix.Path(dx); fHelix.Move(l*1.2);
  return &fHelix;
}    
//_____________________________________________________________________________
void StvSeedFinders::Clear()
{
  for (int i=0;i<(int)size();i++) {(*this)[i]->Clear();}
}

//_____________________________________________________________________________
void StvSeedFinders::Reset()
{
  for (int i=0;i<(int)size();i++) {(*this)[i]->Reset();}
}


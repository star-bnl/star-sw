#include "StvSeedFinder.h"
#include "TSystem.h"
#include "StvDraw.h"
#include "StvHit.h"
#include "StvUtil/StvHitErrCalculator.h"
#include "vector"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"

ClassImp(StvSeedFinder)
//_____________________________________________________________________________
StvSeedFinder::StvSeedFinder(const char *name):TNamed(name,"")
{ fDraw=0;fDoShow=0;
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
   const StVoidArr *hitArr =  StTGeoProxy::Inst()->GetSeedHits();
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

const double BAD_XI2=100;
const double BAD_RHO=0.1;

//		Loop over nodes and collect global xyz

  THelixFitter circ;
  int nNode=fSeedHits.size();
  for (int iNode = 0; iNode<nNode;++iNode) {
    const StvHit * hit = fSeedHits[iNode];
    circ.Add(hit->x()[0],hit->x()[1],hit->x()[2]);
  }  
  double Xi2 =circ.Fit();
  if (Xi2>BAD_XI2*100) 			return 0; //Xi2 too bad, no updates
  if (fabs(circ.GetRho()) >BAD_RHO) 	return 0; //Too big curvature
  int startSeed = 0;
  if (circ.Pos()[0]*circ.Dir()[0]+circ.Pos()[1]*circ.Dir()[1]>0) {
    circ.Backward(); startSeed = nNode-1;
  }
  const float *fx = fSeedHits[startSeed]->x();
  const double dx[3]={fx[0],fx[1],fx[2]};
  double l = circ.Path(dx); circ.Move(l*1.2);

//		Now refit with errors
  int step = (startSeed)? -1:1;
  fHelix.Clear();
  for (int iNode = startSeed; (iNode<nNode && iNode>=0) ;iNode+=step) {
    const StvHit * hit = fSeedHits[iNode];
    const float *fx = hit->x();
    const double dx[3]={fx[0],fx[1],fx[2]};
    double l = circ.Path(dx); circ.Move(l);
//		Set position for helix
    fHelix.Add(dx[0],dx[1],dx[2]);
//		Set position errors for helix
    const StHitPlane *hp = hit->detector();
    const Mtx33F_t &hd = hp->GetDir(fx);
    StvHitErrCalculator* myHitErrCalc = (StvHitErrCalculator*)hp->GetHitErrCalc();
    myHitErrCalc->SetTrack(circ.Dir());
    double hRR[3];
    myHitErrCalc->CalcDcaErrs(fx,hd,hRR);
    double cos2l = circ.GetCos(); cos2l*=cos2l;
    if (hRR[0]<1e-4) hRR[0]=1.;
    fHelix.AddErr( hRR[0],hRR[2]/cos2l);
  }
  Xi2 =fHelix.Fit();
  if (Xi2>BAD_XI2) return 0; 	//Xi2 too bad, no updates
  fHelix.MakeErrs();
  l = fHelix.Path(dx); 
  l*= (nNode+1.)/(nNode-1.);
  fHelix.Move(l);
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


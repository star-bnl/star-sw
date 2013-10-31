#include "StvSeedFinder.h"
#include "TSystem.h"
#include "TVector3.h"
#include "TSystem.h"
#include "StvUtil/StvDebug.h"
#include "StvDraw.h"
#include "StvHit.h"
#include "StvUtil/StvHitErrCalculator.h"
#include "vector"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"

//Constants for THelixFitter (Approx)
static const double kBAD_XI2cm2 = 0.3*0.3	// max Xi2 in cm**2 without errs
                  , kBAD_XI2    = 8;		// max Xi2 (with errs)
const double BAD_RHO=0.1;

ClassImp(StvSeedFinder)
//_____________________________________________________________________________
StvSeedFinder::StvSeedFinder(const char *name):TNamed(name,"")
{ fDraw=0;
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
  StvConstHits &ch = (StvConstHits &)fSeedHits;
  fDraw->Road(fHelix,ch,kGlobalTrack,10.);
  fDraw->UpdateModified();
  fDraw->Wait();
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
   fDraw->Wait();
}
//_____________________________________________________________________________
StvDraw *StvSeedFinder::NewDraw()
{
   StvDraw *dr = new StvDraw();
   dr->SetBkColor(kWhite);
   return dr;
}

//_____________________________________________________________________________
const THelixTrack *StvSeedFinder::Approx()
{
static int nCall=0; nCall++;
//		Loop over nodes and collect global xyz

  fHelix.Clear();
  THelixFitter circ;
  int nNode=fSeedHits.size();
  const float *fBeg = fSeedHits.front()->x();
  const float *fEnd = fSeedHits.back ()->x();
  double r2Beg = fBeg[0]*fBeg[0]+fBeg[1]*fBeg[1];
  double r2End = fEnd[0]*fEnd[0]+fEnd[1]*fEnd[1];
  assert(r2Beg > r2End);

  for (int iNode = 0; iNode<nNode;++iNode) {
    const StvHit * hit = fSeedHits[iNode];
    circ.Add(hit->x()[0],hit->x()[1],hit->x()[2]);
  }  
  fXi2[0] =circ.Fit();
  if (fXi2[0]>kBAD_XI2cm2) 		return 0; //Xi2 too bad, no updates
  if (fabs(circ.GetRho()) >BAD_RHO) 	return 0; //Too big curvature

  const double dBeg[3]={fBeg[0],fBeg[1],fBeg[2]};
  double l = circ.Path(dBeg); circ.Move(l);

//		Now refit with errors
  for (int iNode = 0; iNode<nNode ;iNode++) {
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
    int ans = myHitErrCalc->CalcDcaErrs(fx,hd,hRR);
    if (ans) {// touching case
       fHelix.AddErr( 1.,1.);
    } else {
      double cos2l = circ.GetCos(); cos2l*=cos2l;
      if (hRR[0]<1e-4) hRR[0]=1.;
      fHelix.AddErr( hRR[0],hRR[2]/cos2l);
    }
  }
  fXi2[1] =fHelix.Fit();
  if (fXi2[1]>kBAD_XI2) return 0; 	//Xi2 too bad, no updates
  fHelix.MakeErrs();
  l = fHelix.Path(dBeg); 
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
//_____________________________________________________________________________
void StvSeedFinder::FeedBack(int success)
{
TVector3 dbSeed(fHelix.Dir());
double seedEta = dbSeed.Eta(); 
double mi,ma;
   KNNMiMax(mi,ma);
   if (success==0) {
     StvDebug::Count("BaddSeedXi2",fXi2[1]);
     StvDebug::Count("BaddSeedEta",seedEta);
     StvDebug::Count("BaddMinKnn",mi);
     StvDebug::Count("BaddMaxKnn",ma);
     StvDebug::Count("BaddMa:MiKnn",mi,ma);
   }
   else if(success==-1) {
     StvDebug::Count("FailSeedXi2",fXi2[1]);
     StvDebug::Count("FailSeedEta",seedEta);
     StvDebug::Count("FailMinKnn",mi);
     StvDebug::Count("FailMaxKnn",ma);
     StvDebug::Count("FailMa:MiKnn",mi,ma);
   }
   else {
     StvDebug::Count("GoodSeedXi2",fXi2[1]);
     StvDebug::Count("GoodSeedEta",seedEta);
     StvDebug::Count("GoodMinKnn",mi);
     StvDebug::Count("GoodMaxKnn",ma);
     StvDebug::Count("GoodMa:MiKnn",mi,ma);
   }
}
//_____________________________________________________________________________
void StvSeedFinder::KNNMiMax(double &mi,double &ma)
{
  enum {kNN = 2};
  mi = 1e11,ma=0;
  int sz = fSeedHits.size();
  for (int jk=0;jk<sz-kNN;jk++)
  {
    const float *xl = fSeedHits[jk    ]->x();
    const float *xr = fSeedHits[jk+kNN]->x();
    double d = 0;
    for (int j=0;j<3;j++) d+= pow(xl[j]-xr[j],2);
    if (mi>d) mi = d;
    if (ma<d) ma = d;
  }
  double cosL = fHelix.GetCos();
  mi = sqrt(mi)*cosL/kNN;
  ma = sqrt(ma)*cosL/kNN;


}
 
 
 


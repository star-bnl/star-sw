#include "StvSeedFinder.h"
#include "TSystem.h"
#include "TVector3.h"
#include "TSystem.h"
#include "StvUtil/StvDebug.h"
#include "StvDraw.h"
#include "StvTrack.h"
#include "StvNode.h"
#include "StvHit.h"
#include "StvUtil/StvHitErrCalculator.h"
#include "vector"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"
#include "StvUtil/StvKNNUtil.h"
#include "Stv/StvConst.h"

//Constants for THelixFitter (Approx)
static const double kBAD_XI2cm2 = 0.9*0.9	// max Xi2 in cm**2 without errs
                  , kBAD_XI2    = 50		// max Xi2 (with errs)
                  , kBAD_RHO=0.1		// max curvature
                  , kMIN_Rxy=50;		// minimal radius for seed hits

ClassImp(StvSeedFinder)

//_____________________________________________________________________________
StvSeedFinder::StvSeedFinder(const char *name):TNamed(name,"")
{ 
  fDraw=0;
  fMinHits = 5;
  fMaxHits = 10;
  fSgn = 1;
  fIdTruth = 0;
  SetVtx(0);
}
//_____________________________________________________________________________
void StvSeedFinder::SetCons(const StvKonst_st *kons)
{
  fMinHits= kons->mMinSeedHits;
  fMaxHits= kons->mMaxSeedHits;
}
//_____________________________________________________________________________
void StvSeedFinder::Clear(const char*)
{
 SetVtx(0);
 fSeedHits.clear();
 if(fDraw) fDraw->Clear();
}
//_____________________________________________________________________________
void StvSeedFinder::SetVtx(const float vtx[3])
{
  if (vtx) { memcpy(fVtx,vtx,sizeof(fVtx)); return; }
  memset(fVtx,0,sizeof(fVtx));
  fVtx[2]= 3e33;
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
     if (stiHit->isUsed()) continue;
     myHits.push_back(stiHit);
   }
   fDraw->Hits(myHits,style);
   fDraw->UpdateModified();
   fDraw->Wait();
}
//_____________________________________________________________________________
void StvSeedFinder::ShowIn()
{
   if (!fDraw) fDraw = NewDraw();
   std::vector<StvHit*> myHits;  
   const StVoidArr *hitArr =  StTGeoProxy::Inst()->GetSeedHits();
   int nHits =  hitArr->size();
   for (int iHit=0;iHit<nHits;iHit++) {
     StvHit *stiHit = (StvHit*)(*hitArr)[iHit];
     if (Reject(stiHit->x())) continue; 
     myHits.push_back(stiHit);
   }
   fDraw->Hits(myHits,kUnusedHit);
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
  if (fSgn*r2Beg < fSgn*r2End) return 0;

  for (int iNode = 0; iNode<nNode;++iNode) {
    const StvHit * hit = fSeedHits[iNode];
    circ.Add(hit->x()[0],hit->x()[1],hit->x()[2]);
  }  
  fXi2[0] =circ.Fit();
  if (fXi2[0]>kBAD_XI2cm2) 		return 0; //Xi2 too bad, no updates
  if (fabs(circ.GetRho()) >kBAD_RHO) 	return 0; //Too big curvature

  const double dBeg[3]={fBeg[0],fBeg[1],fBeg[2]};
  double l = circ.Path(dBeg); circ.Move(l);

//		Now refit with errors
  for (int iNode = 0; iNode<nNode ;iNode++) {
    const StvHit * hit = fSeedHits[iNode];
    if (!hit) continue;
    const float *fx = hit->x();
    const double dx[3]={fx[0],fx[1],fx[2]};
    double l = circ.Path(dx); circ.Move(l);
//		Set position for helix
    fHelix.Add(dx[0],dx[1],dx[2]);
//		Set position errors for helix
    const StHitPlane *hp = hit->detector();
    StvHitErrCalculator* myHitErrCalc = (StvHitErrCalculator*)hp->GetHitErrCalc();
    myHitErrCalc->SetTrack(circ.Dir());
    double hRR[3];
//     const Mtx33F_t &hd = hp->GetDir(fx);
//     int ans = myHitErrCalc->CalcDcaErrs(fx,hd,hRR);
    int ans = myHitErrCalc->CalcDcaErrs(hit,hRR);
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
  if (fSgn>0) {
    l = fHelix.Path(dBeg); 
    l-= 0.1; fHelix.Move(l);
  } else {
    const double dEnd[3]={fEnd[0],fEnd[1],fEnd[2]};
    l = fHelix.Path(dEnd); 
    l+= 0.1; fHelix.Move(l);
    fHelix.Backward();
  }
  return &fHelix;
}    
#include "StarRoot/TIdTruUtil.h"
//_____________________________________________________________________________
void StvSeedFinder::FeedBack(const StvTrack *tk)
{
  if (StvDebug::Debug()<2) return;
  TIdTruUtil idu;

  for (int ih=0;ih<(int)fSeedHits.size();ih++) {
    auto *hit = fSeedHits[ih];
    idu.Add(hit->idTru());
  }
  double qua = idu.GetQua()*100;
  if (!idu.GetIdTru()) qua=0;
  StvDebug::Count("SeedAllQua",qua);
  if (!tk) { //
  StvDebug::Count("SeedBadXi2:Xi2E",fXi2[1],fXi2[0]);
  StvDebug::Count("SeedBadQua",qua);

  } else {
  StvDebug::Count("GooXi2:Xi2E",fXi2[1],fXi2[0]);
  
  StvDebug::Count("SeedGooQua",qua);
  double tqua = tk->GetQua()*100;
  StvDebug::Count("GlobGooQua",tqua);
  StvDebug::Count("GlobGooQua::SeedGooQua",qua,tqua);
  const StvNode *node = tk->GetNode(StvTrack::kFirstPoint);
  double P[3];
  node->GetFP().getMom(P);
  
  double eta = TVector3(P).Eta();
  int nHits = tk->GetNHits(kPxlId);
  nHits    += tk->GetNHits(kIstId);
  nHits    += tk->GetNHits(kSstId);
  StvDebug::Count("GoodEta",eta);
  if (nHits>=2) StvDebug::Count("HftEta",eta);
  }
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
void StvSeedFinders::SetCons(const StvKonst_st *kons)
{
  for (int i=0;i<(int)size();i++) {(*this)[i]->SetCons(kons);}
}
//_____________________________________________________________________________
void StvSeedFinders::SetVtx(const float vtx[3])
{
  for (int i=0;i<(int)size();i++) {(*this)[i]->SetVtx(vtx);}
}
 
//_____________________________________________________________________________
void StvSeedFinders::Add(StvSeedFinder *sf)
{
  push_back(sf);
}
 


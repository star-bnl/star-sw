#include "TSystem.h"
#define _THelixNew_
#include "StvSeedFinder.h"
#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TGeoMaterial.h"
#include "StvToolkit.h"
#include "StvUtil/StvELossTrak.h"
#include "StvUtil/StvDebug.h"
#include "StvUtil/StvGrappa.h"
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
                  , kBAD_RHO=1./66.9		// max curvature(Pt=0.1GeV
                  , kMIN_Rxy=50;		// minimal radius for seed hits

ClassImp(StvSeedFinder)
StvSeedFinder* StvSeedFinder::fgCurrFinder = 0;
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
  fDraw->Zhow(&fSeedHits);
}
//_____________________________________________________________________________
void StvSeedFinder::ShowRest(int style)
{
   if (!fDraw) fDraw = NewDraw();
   if (!style) style = kHit;
   StvHits myHits;  
   const StVoidArr *hitArr =  StTGeoProxy::Inst()->GetSeedHits();
   int nHits =  hitArr->size();
   for (int iHit=0;iHit<nHits;iHit++) {
     StvHit *stiHit = (StvHit*)(*hitArr)[iHit];
     if (stiHit->isUsed()) continue;
     const float *f = stiHit->x();
     fDraw->Add(f[0],f[1],f[2],style);
   }
   fDraw->Show();
}
//_____________________________________________________________________________
void StvSeedFinder::ShowIn()
{
   if (!fDraw) fDraw = NewDraw();
   StvHits myHits;  
   const StVoidArr *hitArr =  StTGeoProxy::Inst()->GetSeedHits();
   int nHits =  hitArr->size();
   for (int iHit=0;iHit<nHits;iHit++) {
     StvHit *stiHit = (StvHit*)(*hitArr)[iHit];
     if (Reject(stiHit->x())) continue; 
     myHits.push_back(stiHit);
   }
   fDraw->Show(&myHits,kHit);
}
//_____________________________________________________________________________
StvGrappa *StvSeedFinder::NewDraw()
{
   StvGrappa *dr = new StvGrappa();
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
  if (fSgn*r2Beg <= fSgn*r2End) return 0;

  for (int iNode = 0; iNode<nNode;++iNode) {
    const StvHit * hit = fSeedHits[iNode];
    circ.Add(hit->x()[0],hit->x()[1],hit->x()[2]);
  }  
  fXi2[0] =circ.Fit();
  if (fXi2[0]>kBAD_XI2cm2) 		return 0; //Xi2 too bad, no updates
  if (fabs(circ.GetRho()*circ.GetCos()) >kBAD_RHO) 	return 0; //Too big curvature

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
    TkDir_t tkdir;
    double H[3]={0,0,1};
    THelix3d::MakeTkDir(circ.Dir(),H,tkdir);
    myHitErrCalc->SetTkDir(tkdir);

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
//_____________________________________________________________________________
void StvSeedFinder::Init( StvTrack *tk) const
{
enum {kKalmanFactor = 4};//Tested 4 is the best
static StTGeoProxy *prx = StTGeoProxy::Inst();
static StvToolkit  *kit = StvToolkit::Inst();

  double Xi2 = fHelix.Chi2();
  THelixTrack circ(fHelix);
  circ.Backward(); 	//Make direction In to Out
  double s=0,xyz[3]; 
  int iNode = 0;
  StvNode *node=0,*preNode = 0;
  for (int ihit=0;ihit<(int)fSeedHits.size();ihit++) {
    iNode++;
    StvHit *hit = fSeedHits[ihit];
    const auto *xhit = hit->x();
    for (int i=0; i<3;i++) {xyz[i] = xhit[i];}
    double ds = circ.Path(xyz);
    circ.Move(ds);
    s+=ds;
    node = kit->GetNode();
    tk->push_front(node);
    node->SetLen(s);
    node->SetHit(hit);
    const StHitPlane *hitPlane = hit->detector();
    node->SetHitPlane(hitPlane);
    node->SetType(StvNode::kRegNode);
    StvNodePars &pars = node->GetFP(0);
    StvFitErrs  &errs = node->GetFE(0);
    pars.set(&circ);
    errs.Set(&circ);
    for (int i=0,li=0;i< 5;li+=++i) {errs[li+i]*=double(kKalmanFactor);}
    node->SetXDive(pars.pos());	
    for (int i=0;i<2;i++) {
      node->SetFit(pars,errs,i);
      node->SetPre(pars,errs,i);
      node->SetXi2(Xi2,i);
    }
//	Fill hitErrs    
    auto *hitErrCalc = (StvHitErrCalculator*)hitPlane->GetHitErrCalc();
    assert(hitErrCalc);
    hitErrCalc->SetTkDir(pars.getTkDir());
    double dcaHitErrs[3];
    int ans = hitErrCalc->CalcDcaErrs(hit,dcaHitErrs);
    assert(!ans);
    node->SetHE(dcaHitErrs);
    if (fabs(ds)<=0) continue;
//		Fill ELoss
    StvELossTrak *eloss = kit->GetELossTrak();
    int voluId = hitPlane->GetVoluId();
    TGeoVolume *volu = gGeoManager->GetVolume(voluId);
    const TGeoMaterial *mate = volu->GetMaterial();
    eloss->Set(mate,pars.getP());
    eloss->Add(ds);
    node->SetELoss(eloss,0);
  }

  return;
} 



#if 0
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
    const StvNode *node = tk->GetNode(StvTrack::kFirstPoint);
    double P[3];
    node->GetFP().getMom(P);

    double eta = TVector3(P).Eta();
    int nHits = tk->GetNHits(kPxlId);
    nHits    += tk->GetNHits(kIstId);
    nHits    += tk->GetNHits(kSstId);
  }
}
#endif //0
#if 1
//_____________________________________________________________________________
void StvSeedFinder::FeedBack(const StvTrack *tk)
{
}

#endif //1
//_____________________________________________________________________________
void StvSeedFinder::DrawHelix() 
{
  StvDebug::ClearGra();

  THelixTrack circ(&fHelix);
  int nNode=fSeedHits.size();
  const float *fBeg = fSeedHits.front()->x();


  const double dBeg[3]={fBeg[0],fBeg[1],fBeg[2]};
  double l = circ.Path(dBeg); circ.Move(l);

//		Now Draw helix
  for (int iNode = 0; iNode<nNode ;iNode++) {
    const StvHit * hit = fSeedHits[iNode];
    if (!hit) continue;
    const float *fx = hit->x();
    const double dx[3]={fx[0],fx[1],fx[2]};
    double l = circ.Path(dx); circ.Move(l);
    const double *px = circ.Pos();
    StvDebug::AddGra(px[0],px[1],px[2],3);
    StvDebug::AddGra(fx[0],fx[1],fx[2],4);
  }
  StvDebug::ShowGra();
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


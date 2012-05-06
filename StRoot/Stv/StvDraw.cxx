#include <string.h>
#include "TSystem.h"
#include "TBrowser.h"
#include "TCernLib.h"
#include "TVector3.h"
#include "StvDraw.h"
#include "Stv/StvTrack.h"
#include "Stv/StvNode.h"
#include "Stv/StvHit.h"
#include "StvUtil/StvNodePars.h"
#include "Stv/StvStl.h"
#include "THelixTrack.h"
#include "StarVMC/GeoTestMaker/StTGeoHelper.h"

#include "Stv/StvToolkit.h"

StvDraw *StvDraw::fgStvDraw=0;
//_____________________________________________________________________________
//_____________________________________________________________________________
StvDraw::StvDraw(const char *name):StDraw3D(name)
{
 fgStvDraw = this; mNDoIt=0;mNPow2=1;
 SetBkColor(kWhite);
}
//_____________________________________________________________________________
TObject *StvDraw::Hits(const std::vector<StvHit*> &hits, EDraw3DStyle sty)
{
const std::vector<const StvHit*>&vc = (std::vector<const StvHit*>&)hits;
return Hits(vc,sty);
}


//_____________________________________________________________________________
TObject *StvDraw::Hits(const std::vector<const StvHit*> &hits, EDraw3DStyle sty)
{
  int n = hits.size();
  std::vector<float> vec(n*3);
  const float *f;float *v=&vec[0];
  for (int i=0;i<n; i++) {
    const StvHit *h=hits[i];
    if (!h) continue;
    f = h->x();v[0]=f[0];v[1]=f[1];v[2]=f[2];v+=3;
  }
  vec.resize(v-&vec[0]);
  TObject *to = Points(vec,sty);
  DoIt();
//  ProcessEvents();
  return to;
}
//_____________________________________________________________________________
TObject *StvDraw::Hits(const std::vector<const float*> &hits, EDraw3DStyle sty)
{
  int n = hits.size();
  std::vector<float> vec(n*3);
  const float *f;float *v=&vec[0];
  for (int i=0;i<n; i++) {
    f = hits[i]; if(!f) continue; 
    v[0]=f[0];v[1]=f[1];v[2]=f[2];v+=3;
  }
  vec.resize(v-&vec[0]);
  return Points(vec,sty);
}
//_____________________________________________________________________________
TObject *StvDraw::Trak(const THelixTrack &helx,const std::vector<StvHit*>  &hits, EDraw3DStyle sty)
{
const std::vector<const StvHit*>&vc = (std::vector<const StvHit*>&)hits;
return Trak(helx,vc,sty);
}
//_____________________________________________________________________________
TObject *StvDraw::Trak(const THelixTrack &helx,const std::vector<const StvHit*>  &hits, EDraw3DStyle sty)
{
  int n = hits.size(); if (!n) return 0;
  for (int i=0;i<n; i++) {
    printf("%d %g\n",i,atan2(hits[i]->x()[1],hits[i]->x()[0]));}

  std::vector<float> myTrak;  
  THelixTrack th(helx);
  const float *h = hits[0]->x();
  double l = th.Path(h[0],h[1]); th.Move(l);
  h = hits[n-1]->x();
  l = th.Path(h[0],h[1]);
  double dl = l/100;
  for (int i=0;i<=100;i++) {
    const double *x = th.Pos();
    for (int j=0;j<3;j++) {myTrak.push_back(x[j]);}
    th.Move(dl);
  }
                Hits(hits  ,kUsedHit);
  TObject *to = Line(myTrak,sty);
  DoIt();
  return to;
}
//_____________________________________________________________________________
TObject *StvDraw::Trak(const std::vector<float> &pnts, EDraw3DStyle sty)
{
  int n = pnts.size(); if (!n) return 0;
  return Line(pnts,sty);
}
//_____________________________________________________________________________
void  StvDraw::Trak(const StvTrack *tk, EDraw3DStyle sty)
{
  StvConstHits myHits,ihHits;
  StvPoints  myPoits;
  const StvNode *lNode=0,*rNode;
  for (StvNodeConstIter it = tk->begin();it != tk->end();++it) {
    rNode = *it;
//    ((StvNodePars&)(rNode->GetFP())).ready();
    const StvHit  *hit  = rNode->GetHit();
    if (hit) {
      if (rNode->GetXi2()<1000) {myHits+=hit;} else {ihHits+=hit;}}
      
    if (!lNode) { myPoits+=rNode->GetFP().P;}
    else        { Join(lNode,rNode,myPoits);}

    const double *P = rNode->GetFP().P;
    if (hit)    {// make connection to hit
      const float  *H = hit->x();
      float con[6] = {H[0],H[1],H[2],P[0],P[1],P[2]};
      Line (2,con);            
    }  	
    {// only short line to mark node
       const double *D = &rNode->GetFP()._cosCA;
       float con[6] = {P[0]         ,P[1]         ,P[2]
                     ,P[0]-D[1]*0.1,P[1]+D[0]*0.5,P[2]};
       Line (2,con);            
    }
    lNode = rNode;
  }
  Hits(myHits,kUsedHit);
  Hits(ihHits,kUnusedHit);
  Trak(myPoits,sty);

}
//_____________________________________________________________________________
void StvDraw::Zhow(const StvTrack *tk){Inst()->Road(tk,10);}
//_____________________________________________________________________________
void  StvDraw::Road(const StvTrack *tk, double wide, EDraw3DStyle sty)
{
  Trak(tk, sty);
  const StvNode *fist = tk->front();
  assert(fist);  
  const StvNode *last = tk->back();
  assert(last);  
  const StvNodePars &parF = fist->GetFP();
  const StvNodePars &parL = last->GetFP();
  TVector3 myFst(parF.P);
  TVector3 myDir(parL._x-parF._x,parL._y-parF._y,parL._z-parF._z);
  double dxy = myDir.Perp();;
  double dis = myDir.Mag();;
  double fi0 = myDir.Phi();
  double rho = parF._curv;
  double dfi = asin(dxy*rho/2);
  double len = (fabs(dfi)<1e-3)? dxy:2*fabs(dfi/rho);

  double dir[3]={cos(fi0-dfi),sin(fi0-dfi),myDir[2]/len};
  THelixTrack hlx(parF.P,dir,rho);
  double sag2 = fabs(dxy*dxy/4*rho)+wide; sag2*=sag2;
  
  StVoidArr *vHits = StTGeoHelper::Inst()->GetAllHits();
  int nHits = vHits->size();
  std::vector<const StvHit*> unHits;
  myDir=myDir.Unit();
  for (int ih=0;ih<nHits;ih++) {//loop hit
    const StvHit *hit = (const StvHit*)(*vHits)[ih];
    TVector3 hi(hit->x()); 
    TVector3 hl = hi-myFst;
    double dot = hl.Dot(myDir);
    if (dot<0 || dot >dis) 		continue;
    if ((hl.Cross(myDir)).Mag2()>sag2)	continue;
    double *d = &(hi[0]);
    double dca = hlx.Dca(d);
    if (dca>wide) 			continue;
    unHits.push_back(hit);
  } //End hit loop
  if (!unHits.size()) 			return;
  Hits(unHits,kUnusedHit);
}


//_____________________________________________________________________________
void StvDraw::Join(const StvNode *left,const StvNode *rite,StvPoints &poits)
{
static const double maxStep=0.1;
  const StvNodePars &lFP = left->GetFP();
  const StvNodePars &rFP = rite->GetFP();
  THelixTrack hLeft,hRite;
  lFP.get(&hLeft);
  rFP.get(&hRite);
  double lenL =  hLeft.Path(rFP._x,rFP._y);

  int nStep = fabs(lenL/2)/maxStep; if (nStep <3) nStep = 3;
  double step = lenL/2/nStep;
  for (int is=0;is<=nStep;is++) {
    poits +=hLeft.Pos();
    hLeft.Move(step);
  }
  hLeft.Move(-step);

  double lenR =  hRite.Path(hLeft.Pos()[0],hLeft.Pos()[1]); hRite.Move(lenR);
  step = -lenR/nStep;
  if (step<0) {poits +=rFP.P; return;}
  assert(step>0);
  Points(1,hRite.Pos(),kTrackBegin);
  
  for (int is=0;is<=nStep;is++) {
    poits +=hRite.Pos();
    hRite.Move(step);
  }
}
//_____________________________________________________________________________
void StvDraw::Show(const StvTrack *tk){Inst()->Trak(tk);}
//_____________________________________________________________________________
void StvDraw::Klear(){Inst()->Clear();}
//_____________________________________________________________________________
void StvDraw::DoIt()
{
  mNDoIt++; if (mNDoIt<=mNPow2) return;
  mNPow2<<=1;
  UpdateModified(); ProcessEvents();
}
//_____________________________________________________________________________
void StvDraw::Clear(const char *)
{
  mNDoIt=0;  mNPow2=1;
  StDraw3D::Clear();
}
//_____________________________________________________________________________
int StvDraw::ProcessEvents()
{
  int ans = gSystem->ProcessEvents();
  return ans;
}
//_____________________________________________________________________________
void StvDraw::Wait()
{
    if (Jnst()) Jnst()->UpdateModified();
    fprintf(stderr,"StvDraw::Waiting...\n");
    while(!ProcessEvents()){gSystem->Sleep(200);}; 
}
//_____________________________________________________________________________
void StvDraw::KBrowse(const TObject *to)
{
  new TBrowser("StvDraw",(TObject*)to);
}
//_____________________________________________________________________________
void  StvDraw::All(const char *opt)
{
  if (!opt || !*opt) opt = "THh";

  if (strstr(opt,"T")) {
    StvTracks &tks = StvToolkit::Inst()->GetTracks();
    for (StvTrackConstIter it=tks.begin(); it!=tks.end(); ++it) {
      const StvTrack *tk = *it;
      Trak(tk);
  }  }

  if (strstr(opt,"h")) {
    StVoidArr *vHits = StTGeoHelper::Inst()->GetAllHits();  
    std::vector<const StvHit*> sHits;
    for (int ihit=0;ihit<(int)vHits->size();ihit++) {
      const StvHit *stvHit = (StvHit*)(*vHits)[ihit];
      if (stvHit->timesUsed()) continue;
      sHits.push_back(stvHit);
    }
    Hits(sHits,kUnusedHit);
  }  
}


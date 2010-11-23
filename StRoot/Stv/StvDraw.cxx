#include "TSystem.h"
#include "StvDraw.h"
#include "Stv/StvTrack.h"
#include "Stv/StvNode.h"
#include "Stv/StvHit.h"
#include "StvUtil/StvNodePars.h"
#include "Stv/StvStl.h"
#include "THelixTrack.h"
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
  int n = hits.size();
  std::vector<float> vec(n*3);
  const float *f;float *v=&vec[0];
  for (int i=0;i<n; i++) {
    const StvHit *h=hits[i];
    if (!h) continue;
    f = h->x_g();v[0]=f[0];v[1]=f[1];v[2]=f[2];v+=3;
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
  int n = hits.size(); if (!n) return 0;
  std::vector<float> myTrak;  
  THelixTrack th(helx);
  const float *h = hits[0]->x_g();
  double l = th.Path(h[0],h[1]); th.Move(l);
  h = hits[n-3]->x_g();
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
  StvHits myHits;
  StvPoints  myPoits;
  const StvNode *lNode=0,*rNode;
  for (StvNodeConstIter it = tk->begin();it != tk->end();++it) {
    rNode = *it;
    StvHit  *hit  = rNode->GetHit();
    if (hit) myHits+=hit;
    if (!lNode) { myPoits+=rNode->GetFP().P;}
    else        { Join(lNode,rNode,myPoits);}
    if (hit)    {// make connection to hit
      const double *P = rNode->GetFP().P;
      const float  *H = hit->x_g();
      float con[6] = {H[0],H[1],H[2],P[0],P[1],P[2]};
      Line (2,con);            
    }
    lNode = rNode;
  }
  Hits(myHits,kUsedHit);
  Trak(myPoits,sty);

}
//_____________________________________________________________________________
void StvDraw::Show(const StvTrack *tk){Inst()->Trak(tk);}
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
void StvDraw::Wait()
{
    if (!Jnst()) return;
    Jnst()->UpdateModified();
    fprintf(stderr,"StvDraw::Waiting...\n");
    while(!ProcessEvents()){gSystem->Sleep(200);}; 
}
//_____________________________________________________________________________
void StvDraw::Join(const StvNode *left,const StvNode *rite,StvPoints &poits)
{
static const double maxStep=0.1;
  double lDir[3],rDir[3];
  const StvNodePars &lFP = left->GetFP();
  const StvNodePars &rFP = rite->GetFP();
  lFP.getDir(lDir);
  rFP.getDir(rDir);
  THelixTrack hLeft(lFP.P,lDir,lFP._curv);
  THelixTrack hRite(rFP.P,rDir,rFP._curv);
  double len =  hLeft.Path(rFP.P);
  hRite.Move(-len);
  int nStep = fabs(len)/maxStep;
  if (nStep <6) nStep = 6;
  double step = len/nStep;
  for (int is=1;is<=nStep;is++) {
    hLeft.Move(step);
    double dL = hRite.Path(hLeft.Pos());
    hRite.Move(dL);
    double wtL = pow(nStep-is,2);
    double wtR = pow(      is,2);
    if (is*2<nStep) {wtL=1;wtR=0;} else {wtL=0;wtR=1;}  
    double x[3];
    for (int i=0;i<3;i++) { 
      x[i] = (hLeft.Pos()[i]*wtL+hRite.Pos()[i]*wtR)/(wtL+wtR);}
    poits +=x;
  }
}

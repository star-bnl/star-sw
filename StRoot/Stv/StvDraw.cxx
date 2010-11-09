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
  for (StvNodeConstIter it = tk->begin();it != tk->end();++it) {
    const StvNode *node = *it;
    StvHit  *hit  = node->GetHit();
    myPoits+=node->GetFP().P;
    if (hit) myHits+=hit;
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

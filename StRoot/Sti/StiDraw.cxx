#include "TSystem.h"
#include "StiDraw.h"
#include "Sti/StiHit.h"
#include "Sti/StiNodePars.h"
#include "THelixTrack.h"
StiDraw *StiDraw::fgStiDraw=0;
//_____________________________________________________________________________
//_____________________________________________________________________________
StiDraw::StiDraw(const char *name):StDraw3D(name)
{
 fgStiDraw = this; mNDoIt=0;mNPow2=1;
}
//_____________________________________________________________________________
TObject *StiDraw::Hits(const std::vector<StiHit*> &hits, EDraw3DStyle sty)
{
  int n = hits.size();
  std::vector<float> vec(n*3);
  const float *f;float *v=&vec[0];
  for (int i=0;i<n; i++) {
    const StiHit *h=hits[i];
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
TObject *StiDraw::Hits(const std::vector<const float*> &hits, EDraw3DStyle sty)
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
TObject *StiDraw::Trak(const THelixTrack &helx,const std::vector<StiHit*>  &hits, EDraw3DStyle sty)
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
TObject *StiDraw::Trak(const std::vector<float> &pnts, EDraw3DStyle sty)
{
  enum {kNSteps = 5};
  int n = pnts.size(); if (!n) return 0;
  if (n<9) { return Line(pnts,sty);}
  std::vector<float> myTrak;  
  for (int j=0;j+6<n;j+=3) {
    const float *d0 = &pnts[j+0];
    const float *d1 = &pnts[j+3];
    const float *d2 = &pnts[j+6];
    float l0=0,l1=0,l2=0;
    for (int i=0;i<3;i++) {l1+=(d1[i]-d0[i])*(d1[i]-d0[i]);
                           l2+=(d2[i]-d1[i])*(d2[i]-d1[i]);}
    l1=sqrt(l1);l2=sqrt(l2); l2+=l1;
    double step = ((j+9)>=n)? (l2)/kNSteps: l1/kNSteps;
    for (int k=0;k<=kNSteps;k++) {
      float l = step*k;
      float w0 = (l-l1)*(l-l2)/((l0-l1)*(l0-l2));
      float w1 = (l-l0)*(l-l2)/((l1-l0)*(l1-l2));
      float w2 = (l-l0)*(l-l1)/((l2-l0)*(l2-l1));
      for (int i=0;i<3;i++) {myTrak.push_back(w0*d0[i]+w1*d1[i]+w2*d2[i]);}
    }
  }
  TObject *to = Line(myTrak,sty);
  DoIt();
  return to;
}
//_____________________________________________________________________________
void StiDraw::DoIt()
{
  mNDoIt++; if (mNDoIt<=mNPow2) return;
  mNPow2<<=1;
  UpdateModified(); ProcessEvents();
}
//_____________________________________________________________________________
void StiDraw::Clear(const char *)
{
  mNDoIt=0;  mNPow2=1;
  StDraw3D::Clear();
}
//_____________________________________________________________________________
int StiDraw::ProcessEvents()
{
  int ans = gSystem->ProcessEvents();
  return ans;
}
void StiDraw::Wait()
{
    if (!Jnst()) return;
    Jnst()->UpdateModified();
    fprintf(stderr,"StiDraw::Waiting...\n");
    while(!ProcessEvents()){gSystem->Sleep(200);}; 
}

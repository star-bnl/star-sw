#include "StiHitTest.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "TVectorD.h"
#include "TMatrixDSym.h"
#include "TMath.h"
#include "StiHit.h"
#include "StiKalmanTrack.h"
#include "StiUtilities/StiDebug.h"
#include "StEvent/StEnumerations.h"

//______________________________________________________________________________
void StiHitTest::reset()
{
  memset(fBeg,   0,fEnd-fBeg+1);
  fW[0]=-1;
}

//______________________________________________________________________________
void StiHitTest::add(double x,double y,double z)
{
  double xx[3]; xx[0]=x;xx[1]=y;xx[2]=z;
  add(xx);
}
//______________________________________________________________________________
void StiHitTest::add(double x[3])
{
  fN++;
  for (int i=0;i<3;i++) {
    fX[i] += x[i];
    for (int j=0;j<3;j++) {
      fM[i][j] += x[i]*x[j];
  }}
}
//______________________________________________________________________________
void StiHitTest::doIt()
{
  if (!fN) 		return;
  if (fW[0]>=0.) 	return;
  for (int i=0;i<3;i++) {fX[i]/=fN;}
  double *d = fM[0];
  for (int i=0;i<9;i++) {d[i]/=fN;}
  for (int i=0;i<3;i++) {
    for (int j=0;j<3;j++) {
      fM[i][j] -= fX[i]*fX[j];
  }}
  TMatrixDSym Sym(3,fM[0],"");
  TVectorD vals(3);
  TMatrixD vecs = Sym.EigenVectors(vals);
//  vals.Print();
  memcpy(fW,   vals.GetMatrixArray(),sizeof(fW));
  vecs.Transpose(vecs);
  memcpy(fV[0],vecs.GetMatrixArray(),sizeof(fV));
}
//______________________________________________________________________________
double StiHitTest::width(int idx)
{
  doIt();
  return fW[idx];
}

//______________________________________________________________________________
const double *StiHitTest::vector(int idx)
{
  doIt();
  return fV[idx];
}
//______________________________________________________________________________
double StiHitTest::yAngle() const
{
   double dy = fV[2][1]; double dx = fV[2][0];
   if (dx<0) {dx=-dx;dy=-dy;}
   return TMath::ATan2(dy,dx);
}
//______________________________________________________________________________
double StiHitTest::zAngle() const
{
   double dz = fV[2][2]; double dx = fV[2][0];
   if (dx<0) {dx=-dx;dz=-dz;}
   return TMath::ATan2(dz,dx);
}
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
void StiHftHits::hftHist(const char *name, const StiKalmanTrack* tk)
{
//PXL 1 -- 2 < R < 4 cm
//PXL 2 -- 7 < R < 9 cm
//IST 3 -- 12 < R < 17 cm
//SST 4 -- 21 < R < 28 cm 
enum {kMinRadTpc = 50};
if (StiDebug::Debug()<2) return;
int nTpcHits = tk->getPointCount(kTpcId);
if (nTpcHits<11) return;

static double Rxy[]={5,10,19,30,0};
  int nHits=0,nHft=0,tally[9]={0};
  double lastR=0;
  TString forRxy(name); forRxy+="_xLoc";
  StiKTNIterator it;
  for (it=tk->rbegin();it!=tk->rend();it++)  {
    StiKalmanTrackNode *node = &(*it);
    if (!node->isValid()) 	continue;
    const StiDetector *detector = node->getDetector();
    if (!detector) continue;
    double rxy = node->getRxy();
    if (rxy>kMinRadTpc) 	break;
    if (!detector->isActive()) 	continue;
    nHft++;
    const StiHit *hit = node->getHit();
    if (!hit ) 			continue;
    if (!hit->detector())    	continue;
    if (node->getChi2()>1000)	continue;
    rxy = sqrt(pow(hit->x(),2)+pow(hit->y(),2));
    if (rxy>kMinRadTpc) 	break;
    if (rxy<lastR) 		{StiDebug::Count(name,"????"); return;}//wrong order, do not count at all
    lastR = rxy;
    int ih=0;
    rxy = hit->x();
    for (;Rxy[ih];ih++) { if (rxy<Rxy[ih]) break;}
    tally[ih]++; nHits++;
    StiDebug::Count(forRxy.Data(),rxy);
  }
  if (nHft) {
assert(strstr(name,"After") || nHits==0 || nHits >=3);///???
    TString ts(name);ts+="_siHits:siDets";
    StiDebug::Count(ts.Data(),100.*nHits/nHft);
  }
  if (!nHits) {
    if (nHft) {StiDebug::Count(name,"0000");}
    else      {StiDebug::Count(name,"xxxx");}
    return;
  }
  TString ts;
  for (int i=0;i<4;i++) {
    if (tally[i]>3) tally[i]=3;
    ts += tally[i];
  }
  StiDebug::Count(name,ts.Data());
}


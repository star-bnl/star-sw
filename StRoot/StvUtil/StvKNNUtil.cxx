#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <assert.h>
#include "StvKNNUtil.h"
//_____________________________________________________________________________
 StvKNNUtil::StvKNNUtil(int nVr,int nGb): mNVars(nVr),mKNNgb(nGb)
{
assert(nGb<=kKNNgbMax);
assert(nVr<=kKNNvrMax);
 Reset();
}
//_____________________________________________________________________________
void StvKNNUtil::Reset()
{
mIdxBestWost[0]=-1;   mIdxBestWost[1]=-1;
mBestWost[0]   =3e33; mBestWost[1]   =-3e33;
mEnt.clear();
}
//_____________________________________________________________________________
int StvKNNUtil::Add(ULong_t id,const float *vars)
{
  int nWas = mEnt.size();
  mEnt.resize(nWas+1);
  StvKNNAux &bk = mEnt.back();
  bk.mId = id;
  memcpy(bk.mVar,vars,sizeof(bk.mVar[0])*mNVars);
  for (int i=0;i<mKNNgb;i++) { bk.mQa[i].qa=1e11; bk.mQa[i].ix=-1;}
  
  for (int iEnt=0;iEnt<nWas;iEnt++) {	//ent loop
    
    StvKNNAux &nt = mEnt[iEnt];
    double qa = Dist(nt.mVar,bk.mVar); 
    int kuda = iEnt,kogo=nWas;
    for (int jk=0; jk<2;jk++) {	//neighbour loop
      if (jk) { kuda = nWas,kogo=iEnt; }
      StvKNNAux::QaIx_t *q = mEnt[kuda].mQa;
      if (qa>q[0].qa) continue;
      int before = 1;
      for (;before<mKNNgb;before++){if (qa>=q[before].qa) break;}
      if (before-1>0) {memcpy(q,q+1,(before-1)*sizeof(q[0]));}
      q[before-1].qa = qa; q[before-1].ix = kogo;
//		Now update min/max
      if (mBestWost[0]>q[0].qa) {mBestWost[0]=q[0].qa;mIdxBestWost[0]=kuda;}
    }//end neighbour loop
  }//end ent loop
  return mEnt.size();

}   
//_____________________________________________________________________________
double StvKNNUtil::Dist(const float *a,const float *b) const 
{ double s=0; 
  for(int i=0;i<mNVars;i++) {s+=pow(a[i]-b[i],2);}
  return s;
}
//_____________________________________________________________________________
double StvKNNUtil::GetBest(ULong_t *id,ULong_t *ngb) const
{  
/// returns the max density of points

   double ans = mBestWost[0];
   if (mIdxBestWost[0]<0) 	return ans;
   if (id) *id = mEnt[mIdxBestWost[0]].mId;
   if (!ngb) return ans;
   const StvKNNAux::QaIx_t *q = mEnt[mIdxBestWost[0]].mQa;
   for (int i=0;i<mKNNgb;i++) {
     int ix = q[i].ix;
     ngb[i] = 0; if (ix<0) continue;
     ngb[i] = mEnt[ix].mId;
  }
  return ans;
}
//_____________________________________________________________________________
double StvKNNUtil::GetWost(ULong_t *id,ULong_t *ngb) const
{  
   if (mBestWost[1]<0) {
     int n = mEnt.size();
     for (int i=0;i<n;i++) {
       double qa = mEnt[i].mQa[0].qa;
       assert(qa<1e11);
       if (qa<mBestWost[1]) continue;
       mBestWost[1]=qa;mIdxBestWost[1]=i;
   } }
   double ans = mBestWost[1];


   if (mIdxBestWost[1]<0) 	return ans;
   if (id) *id = mEnt[mIdxBestWost[1]].mId;
   if (!ngb) return ans;
   const StvKNNAux::QaIx_t *q = mEnt[mIdxBestWost[1]].mQa;
   for (int i=0;i<mKNNgb;i++) {
     int ix = q[i].ix;
     ngb[i] = 0; if (ix<0) continue;
     ngb[i] = mEnt[ix].mId;
  }
  return ans;
}
//_____________________________________________________________________________
double StvKNNUtil::GetBest(int &idx,int *ngb) const
{  
   double ans = mBestWost[0];
   idx = mIdxBestWost[0];
   if (!ngb) return ans;
   const StvKNNAux::QaIx_t *q = mEnt[mIdxBestWost[0]].mQa;
   for (int i=0;i<mKNNgb;i++) {
     ngb[i] = q[i].ix;
  }
  return ans;
}

//_____________________________________________________________________________
double StvKNNUtil::GetWost(int &id,int *ngb) const
{  
   if (mBestWost[1]<0) {
     int n = mEnt.size();
     for (int i=0;i<n;i++) {
       double qa = mEnt[i].mQa[0].qa;
       assert(qa<1e11);
       if (qa<mBestWost[1]) continue;
       mBestWost[1]=qa;mIdxBestWost[1]=i;
   } }
   double ans = mBestWost[1];
   id = mIdxBestWost[1];
   if (!ngb) return ans;
   const StvKNNAux::QaIx_t *q = mEnt[mIdxBestWost[1]].mQa;
   for (int i=0;i<mKNNgb;i++) {
     ngb[i] = q[i].ix;
  }
  return ans;
}
//_____________________________________________________________________________
double StvKNNUtil::GetByIdx(int idx,int *ngb) const
{  
   double ans = mEnt[idx].mQa[0].qa;
   if (!ngb) return ans;
   const StvKNNAux::QaIx_t *q = mEnt[idx].mQa;
   for (int i=0;i<mKNNgb;i++) {
     ngb[i] = q[i].ix;
  }
  return ans;
}
//_____________________________________________________________________________
double StvKNNUtil::BestPos(float *var) const
{
  const StvKNNAux::QaIx_t *q = mEnt[mIdxBestWost[0]].mQa;
  float myVar[kKNNvrMax]={0};
  for (int i=0;i<mKNNgb;i++) {
    int ix = q[i].ix;
    const float *v = mEnt[ix].mVar;
    for (int j=0;j<mNVars;j++) {myVar[j]+=v[j];}
  }
  for (int j=0;j<mNVars;j++) {myVar[j]/=mKNNgb;}
  double s = 0; for (int j=0;j<mNVars;j++) {s+=pow(myVar[j],2);}     
  if (var) memcpy(var, myVar,sizeof(*var)*mNVars);
  return sqrt(s);
}
//_____________________________________________________________________________
double StvKNNUtil::WostDis(const float *var) const
{
  int nEnt = mEnt.size();
  double maxDis = 0;
  for (int ie=0;ie<nEnt;ie++) {
    const float *v = mEnt[ie].mVar;
    double s = 0; for (int j=0;j<mNVars;j++) {s+=pow(v[j],2);}     
    if (s>maxDis) maxDis=s;
  }
  return sqrt(maxDis);  
}

//_____________________________________________________________________________
StvKNNAux::StvKNNAux()
{
  memset(this,0,sizeof(*this)); 
}


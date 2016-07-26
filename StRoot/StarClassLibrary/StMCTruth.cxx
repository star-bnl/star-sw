/***************************************************************************
 *
 * $Id: StMCTruth.cxx,v 1.5 2016/07/25 17:33:49 jwebb Exp $
 *
 * Author: Victor Perev, Jun 2005
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StMCTruth.cxx,v $
 * Revision 1.5  2016/07/25 17:33:49  jwebb
 * Init members in ctor / coverity
 *
 * Revision 1.4  2016/07/25 17:18:57  jwebb
 * Comment out deadcode / coverity
 *
 * Revision 1.3  2009/12/17 08:37:26  fisyak
 * account signature change snce root 5.24
 *
 * Revision 1.2  2009/08/28 16:38:26  fine
 * fix the compilation issues under SL5_64_bits  gcc 4.3.2
 *
 * Revision 1.1  2005/07/19 22:39:58  perev
 * IdTruth classes
 *
 *
 **************************************************************************/
#include "assert.h"
#include "StMCTruth.h"
#include "TMath.h"
#include "TExMap.h"
#include <cassert>
//__________________________________________________________________________________________________
StMCTruth &StMCTruth::operator=(int word)
{  
  trackId = word&((1<<16)-1);
  trackWt = word>>16;
  return *this;
}
//__________________________________________________________________________________________________
StMCTruth::operator int() const
{
  return trackId | (trackWt<<16);
}
//__________________________________________________________________________________________________
StMCPivotTruth::StMCPivotTruth(int normInput) : 
  fN(0), fNorm(normInput), 
  mTrackIds{0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0}, 
  mTrackWts{0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0}, 
  mTrackNum{0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0},
  qwe(0)
{
 fNorm = normInput;
 Reset();
}

//__________________________________________________________________________________________________
void StMCPivotTruth::Add(int trackId, double wt)
{

//   assert(trackId);
   wt = fabs(wt);
//   assert(wt);
   for (int i=0;i<fN;i++) {
     if (mTrackIds[i]!=trackId) continue;
     mTrackWts[i]+=wt; 		
     mTrackNum[i]+=1;
     return;
   }
   if (fN>=HOWMANY) 		return;
   mTrackIds[fN] = trackId;
   mTrackWts[fN] = (float)wt;
   mTrackNum[fN] = 1;
   fN++;

}
//__________________________________________________________________________________________________
void StMCPivotTruth::Add(StMCTruth truth)
{
  fNorm=1;
  Add(truth.trackId,truth.trackWt);
}
//__________________________________________________________________________________________________
void StMCPivotTruth::Add(StMCTruth truth, double wt)
{
  fNorm=0;
  Add(truth.trackId,truth.trackWt*wt);
}
//__________________________________________________________________________________________________
StMCTruth StMCPivotTruth::Get(int byCount) const
{
   int trackId=0,zeroId=byCount>>1;
   double wt=0,sum=0,sun=0,sum0=0,sun0=0;
   const float *wts = (byCount&1)? mTrackNum:mTrackWts;
   for (int i=0;i<fN;i++) {
     if (mTrackIds[i]==0) { sun0=mTrackNum[i];sum0=wts[i];continue;}
     sun += mTrackNum[i];
     sum += wts[i];
     if (wts[i]<wt) continue;
     trackId = mTrackIds[i];
     wt      = wts[i];
   }
   if (zeroId) { sum+=sum0; wt+=sum0; sun+=sun0;}
   sum = ((byCount&1) || fNorm==0)? sum/100 : sun;
   wt /=(sum+1e-20);
   return StMCTruth(trackId,int(wt+0.5));
}

//__________________________________________________________________________________________________
StMCPivotTruthMap::StMCPivotTruthMap(int normInput)
{
 fNorm = normInput;
 fMap = new TExMap;
 fIter=0;
}
//__________________________________________________________________________________________________
StMCPivotTruthMap::~StMCPivotTruthMap()
{
  LongKey_t key,val;
  TExMapIter it(fMap);
  while (it.Next(key,val)) { delete (StMCPivotTruth*)val; }
  delete fMap; fMap=0;
  delete fIter; fIter=0;
}
//__________________________________________________________________________________________________
void StMCPivotTruthMap::Add(LongKey_t token, int trackId, double wt)
{
  LongKey_t& word = (*fMap)(TMath::Hash(&token,sizeof(token)),token);
  StMCPivotTruth *&pivo = (StMCPivotTruth *&)word;
  if (!pivo) pivo = new StMCPivotTruth(fNorm);
  pivo->Add(trackId,wt);
}
//__________________________________________________________________________________________________
void StMCPivotTruthMap::Add(LongKey_t token, StMCTruth truth)
{
  Add(token,truth.trackId,truth.trackWt);
}
//__________________________________________________________________________________________________
StMCTruth StMCPivotTruthMap::Get(LongKey_t token,int byCount) const
{
  LongKey_t word = fMap->GetValue(TMath::Hash(&token,sizeof(token)),token);
  assert(word);
  //if (!word) return StMCTruth(0,0); // deadcode 
  
  StMCPivotTruth *pivo = (StMCPivotTruth*)word;
  return pivo->Get(byCount);
}

//__________________________________________________________________________________________________
StMCTruth StMCPivotTruthMap::Iter(LongKey_t &token) const
{
  LongKey_t val;
  if (token == -1L) {
    if (!fIter) fIter = new TExMapIter(fMap);
    fIter->Reset();
  }
  if (!fIter->Next(token,val)) {token = -1; return 0;}
  StMCPivotTruth *pivo = (StMCPivotTruth*)val;
  return pivo->Get();
}


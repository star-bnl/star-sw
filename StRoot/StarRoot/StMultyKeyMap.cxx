#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <assert.h>
static int gMyId=0;

#include "StMultyKeyMap.h"
#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif
//______________________________________________________________________________
StMultyKeyMap::StMultyKeyMap(int nkeys,int nbins) 
{
  mNKey=nkeys;
  mNBin=nbins;
  mJKey = 1946;
  mLink.resize(mNBin,    0);
  mDow.resize (mNKey, 1e11);
  mStp.resize (mNKey,-1e11);
  SetMap(this);
}
//______________________________________________________________________________
StMultyKeyMap::~StMultyKeyMap() 
{
  
}
//______________________________________________________________________________
void StMultyKeyMap::Clear(const char *) 
{
  mSize = 0;
  mJKey = 1946;

  StMultyKeyDivd::Clear();
  mArr.clear();
}
//______________________________________________________________________________
void StMultyKeyMap::Add(const void *obj,const double *keys)
{
   float buf[200];
   for (int i=0;i<mNKey;i++) {buf[i]=(float)keys[i];}
   Add(obj,buf);
}
//______________________________________________________________________________
void StMultyKeyMap::Add(const void *obj,const float *keys)
{
assert(obj);
  StMultyKeyNode *node = MakePair();
  for (int ik=0;ik<mNKey;ik++) {
    if (mDow[ik]>keys[ik]) mDow[ik]=keys[ik]; 
    if (mStp[ik]<keys[ik]) mStp[ik]=keys[ik]; 
  }
  node->Set(obj,keys);
  mArr.push_back(node);
  mSize = mArr.size();
}
//______________________________________________________________________________
int StMultyKeyMap::GetJKey()
{
   mJKey+=1000000007; return mJKey%mNKey;
}
//______________________________________________________________________________
StMultyKeyPair *StMultyKeyMap::MakePair()
{
  StMultyKeyPair *pair = 0;
  if (!mPairs.empty()) {// reuse
    pair = mPairs.front(); mPairs.pop_front();}
  else {
    pair = new StMultyKeyPair();
    pair->mKeys.resize(mNKey,0);
    pair->SetMap(this);
  }
  return pair;
}
//______________________________________________________________________________
StMultyKeyDivd *StMultyKeyMap::MakeNode()
{
  StMultyKeyDivd *node = 0;
  if (!mNodes.empty()) {// reuse
    node = mNodes.front();mNodes.pop_front();}
  else {
    node = new StMultyKeyDivd();
    node->SetMap(this);
  }
  node->mLink.resize(mNBin,0);

  return node;
}
//______________________________________________________________________________
int StMultyKeyMap::MakeTree(int keepArray)
{
   int nNodes = mArr.size();
   if (!nNodes) return 0;
   for (int ik=0;ik<mNKey;ik++) {mStp[ik] = 1.001*(mStp[ik]-mDow[ik])/mNBin;}

   SetIKey(0);

   for (int i=0;i<nNodes;i++) {Add(mArr[i]);}

   if (keepArray) return nNodes;
std::vector<StMultyKeyNode*> tmp(0);
   mArr.swap(tmp);	//destroy internal array completely;
   return nNodes;
}
//______________________________________________________________________________
StMultyKeyDivd::StMultyKeyDivd()
{
  mIKey = 0;
} 
//______________________________________________________________________________
StMultyKeyDivd::~StMultyKeyDivd()
{
//delete [] mKeys;
  delete mLink[0];
  delete mLink[1];
} 
//______________________________________________________________________________
//______________________________________________________________________________
int StMultyKeyNode::GetNumb() const
{ 
   int nb = 0;
   if (GetKeys()) return 1;
   int nk = GetNKey();
   for (int ik=0;ik<nk;ik++) {if (Link(ik)) nb += Link(ik)->GetNumb();}
   return nb;
}
//______________________________________________________________________________
//______________________________________________________________________________
void StMultyKeyDivd::Clear(const char *)
{
  int nk = GetNKey();
  for (int ik=0;ik<nk;ik++) {if (mLink[ik]) mLink[ik]->Clear();mLink[ik]=0;}
  mIKey = 0;
  if (this == mMap) return;
  mMap->mNodes.push_front(this);
}
//______________________________________________________________________________
StMultyKeyPair::StMultyKeyPair()
{
  mId = ++gMyId; mObj = 0;
} 
//______________________________________________________________________________
void StMultyKeyPair::Clear(const char *)
{
  mKeys.clear(); mObj = 0;
  mMap->mPairs.push_front(this);
}
//______________________________________________________________________________
void StMultyKeyPair::Set(const void *obj,const float *keys)
{
   mObj = obj;; 
   mKeys.clear();
   int nk = mMap->GetNKey();
   for (int i=0;i<nk;i++) {mKeys.push_back(keys[i]);}
}
//______________________________________________________________________________
void StMultyKeyDivd::Add(StMultyKeyNode *pair)
{
static int nCall=0; nCall++;
  const float *keyA = pair->GetKeys();
  int nBin = mMap->GetNBin();
  int ibin = (keyA[mIKey]-mDow[mIKey])/mStp[mIKey];
assert(ibin>=0 && ibin<nBin);
  StMultyKeyNode *node = mLink[ibin];
  const float *keyB = 0;
  if (!node)				{ mLink[ibin] = pair; return;}
  else if (!(keyB=node->GetKeys())) 	{ node->Add(pair);    return;}

//		Node is Pair
  StMultyKeyDivd *bode = mMap->MakeNode();
  mLink[ibin]=bode;
  bode->mDow = mDow; bode->mStp = mStp; 
  bode->mDow[mIKey] += mStp[mIKey]*ibin; 
  bode->mStp[mIKey] /= nBin; 
  bode->mIKey=mMap->GetJKey();;
  bode->Add(pair);
  bode->Add(node);
  return;
}  
//______________________________________________________________________________
double StMultyKeyNode::Quality(int *numb) const
{
static int nCall=0; nCall++;
   if (GetKeys()) { *numb = 1; return 0;}

   int ni=0,nt=0,ni2=0;
   double qi=0,qa=0;
   int nb = GetNBin();
   for (int ib=0;ib<nb;ib++) {
     if (!Link(ib)) continue;
     qi = Link(ib)->Quality(&ni);
     nt +=ni; ni2 += ni*ni; qa +=qi*ni;
   }
   qa = (qa + nt*nt-ni2)/nt;
   if (numb) {*numb = nt;} else { qa/=nt;}
   return qa;
}
//______________________________________________________________________________
int StMultyKeyNode::MaxDeep(int *deep) const
{
   if (GetKeys()) { *deep = 1; return 0;}
   int nk = GetNKey();int maxi=0;
   for (int ik=0;ik<nk;ik++) {
     int ni = (deep)? *deep+1:1;
     
     int mxi = ni;
     if (Link(ik)) mxi = Link(ik)->MaxDeep(&ni);
     if (maxi<mxi) maxi=mxi;
   }
   
   return maxi;
}
//______________________________________________________________________________
int StMultyKeyNode::ls(const char *file) const
{

   FILE *out=stdout;
   if (!file) file="";
   if (file && file[0]) out=fopen(file,"w");

   StMultyKeyMapIter iter(this);
   
   int n=0; 
   for (const StMultyKeyNode *node=0;(node = *iter);++iter) {
     n++;
     if (!out) continue;
     int lev = iter.Level();
     const float *keys = node->GetKeys();
     fprintf(out,"%4d - ",n);
     fprintf(out,"Lev(%d) \t(%10p keys=)",lev,(void*)node);
     int nk = node->GetNKey();
       for (int k=0;k<nk;k++) {fprintf(out,"%g ",keys[k]);}
     fprintf(out,"\n");
   } 
   return n;
}


//______________________________________________________________________________
//______________________________________________________________________________
StMultyKeyMapIter::StMultyKeyMapIter(const StMultyKeyNode *node,const float *kMin,const float *kMax)
  :mMinMax(0),mStk(32)
{
  if (!node) return;
  Set(node,kMin,kMax);
}
//______________________________________________________________________________
void StMultyKeyMapIter::Reset()
{
  memset(mTouched,0,sizeof(mTouched));
  mStk.resize(32);
  mLev = 1;
  int ini = InitLev();
  if (ini<0 && FullCheck()==0) return;
  mLev = 2;  ++(*this);
}
//______________________________________________________________________________
void StMultyKeyMapIter::Set(const StMultyKeyNode *node,const float *kMin,const float *kMax)
{
  memset(mTouched,0,sizeof(mTouched));
  mTop = node;
  mStk.resize(100);
  mNK = node->GetNKey();
  mNB = node->GetNBin();
  mKMin=0;mKMax=0;
  if (kMin) {
    mMinMax.resize(2*mNK);
    mKMin = &mMinMax[0];
    mKMax  = mKMin+mNK;
    int sk = mNK*sizeof(mKMin[0]);
    memcpy(mKMin,kMin,sk);
    memcpy(mKMax,kMax,sk);
  }
  mLev = 0; mStk[0].node=0;
  mLev = 1; mStk[1].node=node;
  int ini = InitLev();
  mLev = 2;
  if (ini<0 && !FullCheck()) return;
  mLev = 2;  ++(*this);
}
//______________________________________________________________________________
int StMultyKeyMapIter::InitLev()
{
  const StMultyKeyNode *node = mStk[mLev].node;
  mStk[mLev].rite = 0;
  mStk[mLev].ibin = 0;
  if (node->GetKeys()) return -1;
  int iKey = node->GetIKey();
  mStk[mLev].rite = mNB-1;
  mStk[mLev].ibin =-1;
  if (mKMin) {
    float dow = node->GetDow()[iKey];
    float stp = node->GetStp()[iKey];
    int left = (mKMin[iKey]-dow)/stp; if (left<   0) {left=    0;}; if(left>=mNB) return 1;
    int rite = (mKMax[iKey]-dow)/stp; if (rite>=mNB) {rite=mNB-1;}; if(rite<   0) return 2;
    mStk[mLev].rite = rite;
    mStk[mLev].ibin = left-1;
  }
  return 0;
}
//______________________________________________________________________________
void StMultyKeyMapIter::Update(const float *kMin,const float *kMax)
{
  int sk = mNK*sizeof(mKMin[0]);
  if (kMin) memcpy(mKMin,kMin,sk);
  if (kMax) memcpy(mKMax,kMax,sk);
}
//______________________________________________________________________________
StMultyKeyMapIter::~StMultyKeyMapIter()
{
}
//______________________________________________________________________________
StMultyKeyMapIter &StMultyKeyMapIter::operator++()
{
  --mLev;
  while (mLev) {
    myStk_t &stk =mStk[mLev]; 
    const StMultyKeyNode* node  = stk.node;
    const StMultyKeyNode* binNode = 0;
//	Find non zero bin
    while (++stk.ibin<=stk.rite) {
      binNode = node->Link(stk.ibin); if (binNode) break;
    }
    if (!binNode) 	{ mLev--; 	continue;}	//did not find
    mStk[++mLev].node = binNode;
    int ini = InitLev();
    if (ini==0) 	{		continue;}	//normal case 
    if (ini>0) 		{ mLev--; 	continue;}
    if (FullCheck())	{ mLev--; 	continue;}
    break;						// found the good one
  }
  return *this;
}  
//______________________________________________________________________________
int StMultyKeyMapIter::FullCheck() 
{
//  check node 0=good,1=bad

  const StMultyKeyNode *node = mStk[mLev].node;
  if (!mKMin) return 0;
  const float *fk = node->GetKeys();
  mTouched[2]++;
  for (int k=0;k<mNK;k++) {  
    if (mKMin[k]>fk[k] || fk[k] >= mKMax[k]) return 1;
  }
  return 0;
}  
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
#include "TRandom.h"
#include "TStopwatch.h"
#include <map>

void StMultyKeyMap::Test()
{
printf("StMultyKeyMap::Test() started\n");
   float rnd; 
   int nEVTS=50000;
   StMultyKeyMap map(1);
   for (int i=0;i<nEVTS;i++) {
     rnd = 1 - (i+1.)/nEVTS; 
     map.Add((void*)(-1),&rnd);
   }
   map.MakeTree();
   map.ls();
   double qa = map.Quality();
   printf(" Quality of tree = %g\n\n",qa);

   StMultyKeyMapIter iter(map.GetTop());
   int n = 0;
   float pre=0;

   printf("\n%d evts No bounds\n",nEVTS);
   for (StMultyKeyNode *node=0;(node = *iter);++iter)
   {
     const float * keys = node->GetKeys();
     if(!keys) continue;
     n++; 
     rnd = keys[0];
//     printf("%4d - %g %p\n",n,rnd,node);
     assert(pre<=rnd);
     pre = rnd;
   }
assert(n==nEVTS);
printf("\nNo bounds OK, touched %d %d %d\n"
      ,iter.Touched()[0],iter.Touched()[1],iter.Touched()[2]);


   float kMin=0.5,kMax=0.6;
   iter.Set(map.GetTop(),&kMin,&kMax);
   pre=0;n=0;
   int nEst = int((nEVTS)*(kMax-kMin)+0.5);
printf("\n%d ~evts bounds=%g %g\n",nEst,kMin,kMax);
   for (StMultyKeyNode *node=0;(node = *iter);++iter)
   {
     rnd = node->GetKeys()[0];
     n++; rnd = node->GetKeys()[0];
////     printf("%4d - %g \n",n,rnd);
     assert(pre<=rnd);
     assert((kMin<=rnd) && (rnd < kMax));
     pre = rnd;
   }
printf("\nGot %d. Bounds OK, Touched %d %d %d\n",n
      ,iter.Touched()[0],iter.Touched()[1],iter.Touched()[2]);

}
//______________________________________________________________________________
void StMultyKeyMap::Test2()
{
printf("StMultyKeyMap::Test2() started\n");
   int nBin = 50;
   StMultyKeyMap map(4,nBin);
   float key[4];
   int nEvts = 50000;

   TStopwatch TW;
   TW.Start();
//		Fill the map
   for (int iEv=0;iEv<nEvts ;iEv++) {
     for (int ik=0;ik<4;ik++) { key[ik]= gRandom->Rndm();}
     map.Add((void*)1,key);
   }  
   map.MakeTree(1946);
   TW.Stop();
   printf ( "MakeTree Cpu = %g\n",TW.CpuTime());
   TW.Print();
// map.ls();
//		Check map quality
   double qa = map.Quality();
   int maxDeep = map.MaxDeep();
   int etaDeep = log(nEvts)/log(2.)+0.5;
   printf(" Quality of tree = %g maxDeep=%d etaDeep %d",qa,maxDeep,etaDeep);


   float dow[4]={0,  0.1,0.2,0.3};
   float upp[4]={0.2,0.3,0.4,0.5};
   double ev = nEvts;for (int i=0;i<4;i++){ev*=(upp[i]-dow[i]);};
printf("\n%d ~evts \n",int(ev+0.5));
   int nk = map.GetNKey();
   int nSel = 0,nBad=0;
   StMultyKeyMapIter iter(map.GetTop(),dow,upp);

   TW.Start(1);
   int nkl = 10000;
for (int jkl=0;jkl<nkl;jkl++) {
   iter.Reset();
   nSel = 0;nBad=0;
   for (StMultyKeyNode *node=0;(node = *iter);++iter)
   {
     nSel++; int good = 0;
//      const float *key = node->GetKeys();
//      for (int j=0;j<nk;j++) {if (key[j]>=dow[j] && key[j]<upp[j]) good++;}
//      nBad += (good!=nk); 
//     printf("%4d - %g %g %g %g \n",nSel,key[0],key[1],key[2],key[3]);
   }
}//end jkl
   TW.Stop();
   printf ( "Search Cpu = %g\n",TW.CpuTime()/nkl);
   int nb = map.Size();
   StMultyKeyNode **nodes = map.GetArr();
   int nMust=0;
   for (int i=0;i<nb;i++) 
   {
     StMultyKeyNode *node = nodes[i];
     const float *key = node->GetKeys();
     int good = 1;
     for (int k=0;k<nk;k++) {  
       if (dow[k]<=key[k] && key[k] < upp[k]) continue;
       good=0;break;
     }
     if (!good) continue;

     nMust++;
     break;
   }
   map.Clear();
printf("\nSelected  %d bad %d and must be %d\n",nSel,nBad,nMust);
printf("Touched %d %d %d\n"
      ,iter.Touched()[0],iter.Touched()[1],iter.Touched()[2]);

}




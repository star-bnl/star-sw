#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <assert.h>

#include <algorithm>
#include <numeric>
static int gMyId=0,gMyInst=0;

#include "StMultiKeyMapM.h"
#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif
static void random_shuffle(std::vector<StMultiKeyNode*> &arr); // shuffle elements 
//______________________________________________________________________________
StMultiKeyMapM::StMultiKeyMapM(int nkeys) 
{
  mNKey=nkeys;
  mTop = 0;
}
//______________________________________________________________________________
StMultiKeyMapM::~StMultiKeyMapM() 
{
  delete mTop;mTop=0;
}
//______________________________________________________________________________
void StMultiKeyMapM::Clear(const char *) 
{
  delete mTop;mTop=0;
  mArr.clear();
}
//______________________________________________________________________________
void StMultiKeyMapM::Add(const void *obj,const double *keys)
{
   float buf[200];
   for (int i=0;i<mNKey;i++) {buf[i]=(float)keys[i];}
   Add(obj,buf);
}
//______________________________________________________________________________
void StMultiKeyMapM::Add(const void *obj,const float *keys)
{
assert(obj);
  assert(!mTop);
  StMultiKeyNode *node = new StMultiKeyNode(mNKey);
  node->Set(obj,keys);
  mArr.push_back(node);
}
//______________________________________________________________________________
double StMultiKeyMapM::StMultiKeyMapM::Quality() 
{
  assert(mTop);
  return mTop->Quality();
}
//______________________________________________________________________________
int StMultiKeyMapM::MakeTree(int keepArray)
{
   int nNodes = mArr.size();
   if (!nNodes) return 0;
//   std::random_shuffle( mArr.begin(),mArr.end() ); // shuffle elements 
   random_shuffle(mArr); 
   int jl = 0;
   if (!mTop )  {mTop =  mArr[0];jl=1;mTop->SetIKey(0);}
   unsigned int myRnd = 1946;
   for (int i=jl;i<nNodes;i++) {
     myRnd+=1000000007; mArr[i]->SetIKey(myRnd%mNKey);
     mTop->Add(mArr[i]);
   }

   if (keepArray) return nNodes;
std::vector<StMultiKeyNode*> tmp(0);
//    assert(nNodes == mTop->Size());
   mArr.swap(tmp);	//destroy internal array completely;
   return nNodes;
}
//______________________________________________________________________________
int StMultiKeyMapM::ls(const char *file) const
{
  return mTop->ls(file);
}
//______________________________________________________________________________
int StMultiKeyMapM::Size() const
{
  if (mTop) return mTop->Size();
  else      return mArr.size();
}
//______________________________________________________________________________
//______________________________________________________________________________
StMultiKeyNode::StMultiKeyNode(int nKeys)
{
  Init();
  mNKey=nKeys; 
} 
//______________________________________________________________________________
StMultiKeyNode::StMultiKeyNode(const StMultiKeyNode &fr)
{
  Init();
  mNKey = fr.mNKey;
  if (fr.mObj) Set(fr.mObj,fr.mKeys);
} 
//______________________________________________________________________________
void StMultiKeyNode::Init()
{
  memset(&mNKey,0,(char*)&mKeys-&mNKey+sizeof(mKeys));
  mId = ++gMyId; gMyInst++; mIKey = -1;
}
//______________________________________________________________________________
void StMultiKeyNode::Clear()
{
  memset(&mIKey,0,(char*)&mObj - &mIKey); mIKey = -1;
}
//______________________________________________________________________________
StMultiKeyNode::~StMultiKeyNode()
{
  delete [] mKeys;
  delete mLink[0];
  delete mLink[1];
  gMyInst--;
} 
//______________________________________________________________________________
int StMultiKeyNode::GetNInst() 
{ return gMyInst;}
//______________________________________________________________________________
void StMultiKeyNode::Set(const void *obj,const float *keys)
{
   Clear();
   mObj = obj; mIKey=-1;
   if (!mKeys) mKeys = new float[mNKey]; 
   memcpy(mKeys,keys,sizeof(mKeys[0])*mNKey);
}
//______________________________________________________________________________
void StMultiKeyNode::Set(const void *obj,const double *keys)
{
   float buf[200];
   for (int i=0;i<mNKey;i++) {buf[i]=(float)keys[i];}
   Set(obj,buf);
}
//______________________________________________________________________________
void StMultiKeyNode::Add(const void *obj,const float *keys)
{
  StMultiKeyNode *node = new StMultiKeyNode(mNKey);
  node->Set(obj,keys);
  Add(node);
}
//______________________________________________________________________________
void StMultiKeyNode::Add(StMultiKeyNode *node)
{
static int nCall=0; nCall++;
  assert(this != node);
  int way = (node->mKeys[int(mIKey)] > GetKey());
  if (mLink[way])            { mLink[way]->Add(node);}
  else                       { mLink[way] = node    ;}
  mNumb[way]++;
  return;
}  
//______________________________________________________________________________
double StMultiKeyNode::Quality() 
{
   double qa = 0;
   int nTot = GetNumb(0)+GetNumb(1)+1;
   StMultiKeyMapMIter iter(this);
   
   int n=0; 
   for (StMultiKeyNode *node=0;(node = *iter);++iter) {
     n++;
     int nL = node->GetNumb(0);
     int nR = node->GetNumb(1);
     if (!nL || !nR) continue;
     qa += (2.*nL*nR+nL+nR)/nTot;
   }
printf("Quality() nodes %d\n",n);
   return qa/nTot;
}
//______________________________________________________________________________
int StMultiKeyNode::ls(const char *file) const
{
   FILE *out=stdout;
   if (!file) file="";
   if (file && file[0]) out=fopen(file,"w");

   StMultiKeyMapMIter iter((StMultiKeyNode*)this);
   
   int n=0; 
   for (const StMultiKeyNode *node=0;(node = *iter);++iter) {
     n++;
     if (!out) continue;
     int nL = node->GetNumb(0);
     int nR = node->GetNumb(1);
     int lev = iter.Level();
     if (node==this) {fprintf(out,"%4d * ",n);}
     else            {fprintf(out,"%4d - ",n);}
     fprintf(out,"Lev(%d) \t(%10p) \tL(%10p(%d)) \tR(%10p(%d)) \tDiv(%g)"
           ,lev,(void*)node
	   ,(void*)node->mLink[0],nL
	   ,(void*)node->mLink[1],nR
           ,node->GetKey());
     if (node->mObj) {
       for (int j=0;j<mNKey;j++) {fprintf(out," %g",node->mKeys[j]);}}
     fprintf(out,"\n");
   } 
   return n;
}


//______________________________________________________________________________
//______________________________________________________________________________
StMultiKeyMapMIter::StMultiKeyMapMIter(const StMultiKeyNode *node,const float *kMin,const float *kMax)
  :mMinMax(0),mStk(32)
{
  if (!node) return;
  Set(node,kMin,kMax);
}
//______________________________________________________________________________
void StMultiKeyMapMIter::Set(const StMultiKeyNode *node,const float *kMin,const float *kMax)
{
  memset(mTouched,0,sizeof(mTouched));
  mStk.resize(32);
  mNK = node->GetNKey();
  mKMin=0;mKMax=0;
  if (kMin) {
    mMinMax.resize(2*mNK);
    mKMin = &mMinMax[0];
    mKMax  = mKMin+mNK;
    int sk = mNK*sizeof(mKMin[0]);
    memcpy(mKMin,kMin,sk);
    memcpy(mKMax,kMax,sk);
  }
  mLev = 0; mStk[0]=0;
  
  Left(node);
  if (FullCheck()) ++(*this);
}
//______________________________________________________________________________
void StMultiKeyMapMIter::Update(const float *kMin,const float *kMax)
{
  int sk = mNK*sizeof(mKMin[0]);
  if (kMin) memcpy(mKMin,kMin,sk);
  if (kMax) memcpy(mKMax,kMax,sk);
}
//______________________________________________________________________________
StMultiKeyMapMIter::~StMultiKeyMapMIter()
{
}
//______________________________________________________________________________
StMultiKeyMapMIter &StMultiKeyMapMIter::operator++()
{
  while (mLev) {
    const StMultiKeyNode* node  = mStk[mLev];
    const StMultiKeyNode* rLink = node->RLink();
    mLev--;  
    if (rLink && (!mKMin || !FilterRite(node))) Left(rLink);
    if (!FullCheck()) break;
  }
  return *this;
}  
//______________________________________________________________________________
void StMultiKeyMapMIter::Left(const StMultiKeyNode *node) 
{
  while(node) {
    if ((int)mStk.size() <=mLev) mStk.resize(mLev*2);
    mStk[++mLev] = node;
    if (!node->LLink()) break;
    if (mKMin && FilterLeft(node)) break;
    node = node->LLink();
  }
  return;
}
//______________________________________________________________________________
int StMultiKeyMapMIter::FullCheck() 
{
  const StMultiKeyNode *node = mStk[mLev];

  if (!node ) return 0;
  if (!mKMin) return 0;
//  if (!node->GetObj()) { ++(*this); return;} 
  const float *fk = node->GetKeys();
  mTouched[2]++;
  for (int k=0;k<mNK;k++) {  
    if (mKMin[k]>fk[k] || fk[k] >= mKMax[k]) return 1;
  }
  return 0;
}  
//______________________________________________________________________________
int StMultiKeyMapMIter::FilterLeft(const StMultiKeyNode *node) const
{
  int   ik = node->GetIKey();
  float fk = node->GetKey();
  mTouched[0]++;
  return ( mKMin[ik]>fk);
}
//______________________________________________________________________________
int StMultiKeyMapMIter::FilterRite(const StMultiKeyNode *node) const
{
  int ik = node->GetIKey();
  float fk = node->GetKey();
  mTouched[1]++;
  return (mKMax[ik]<=fk);
}
//______________________________________________________________________________
void random_shuffle(std::vector<StMultiKeyNode*> &arr)
{
static const unsigned int us=1000000007;
  int n = arr.size(); if (n<=3) return;
  unsigned int u=n/2;
  int jr=n-1;
  while (0<jr) {
    int jj = (u+=us)%jr;
    StMultiKeyNode *v = arr[jj]; arr[jj]=arr[jr]; arr[jr]=v; jr--;
  }
}
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
#include "TRandom.h"
void StMultiKeyMapM::Test()
{
printf("StMultiKeyMapM::Test() started\n");
   float rnd; 
   int nEVTS=1000;
   StMultiKeyMapM map(1);
   for (int i=0;i<nEVTS;i++) {
     rnd = 1 - (i+1.)/nEVTS; 
     map.Add((void*)(-1),&rnd);
   }
   map.MakeTree();
   map.ls();
   double qa = map.Quality();
   printf(" Quality of tree = %g\n\n",qa);

   StMultiKeyMapMIter iter(map.GetTop());
   int n = 0;
   float pre=0;

printf("\n%d evts No bounds\n",nEVTS);
   for (StMultiKeyNode *node=0;(node = *iter);++iter)
   {
     n++; rnd = node->GetKeys()[0];
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
   for (StMultiKeyNode *node=0;(node = *iter);++iter)
   {
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
void StMultiKeyMapM::Test2()
{
printf("StMultiKeyMapM::Test2() started\n");
   StMultiKeyMapM map(4);
   float key[4];
   int nEvts = 10000;
   for (int iEv=0;iEv<nEvts ;iEv++) {
     for (int ik=0;ik<4;ik++) { key[ik]= gRandom->Rndm();}
     map.Add((void*)1,key);
   }  

   map.MakeTree(1946);
//   map.ls();
   double qa = map.Quality();
   printf(" Quality of tree = %g\n\n",qa);


   float dow[4]={0,  0.1,0.2,0.3};
   float upp[4]={0.2,0.3,0.4,0.5};

   StMultiKeyMapMIter iter(map.GetTop(),dow,upp);
   double ev = nEvts;for (int i=0;i<4;i++){ev*=(upp[i]-dow[i]);};
printf("\n%d ~evts \n",int(ev+0.5));
   int nk = map.GetNKey();
   int nSel = 0,nBad=0;
   for (StMultiKeyNode *node=0;(node = *iter);++iter)
   {
     nSel++; int good = 0;
     const float *key = node->GetKeys();
     for (int j=0;j<nk;j++) {if (key[j]>=dow[j] && key[j]<upp[j]) good++;}
     nBad += (good!=nk); 
//     printf("%4d - %g %g %g %g \n",nSel,key[0],key[1],key[2],key[3]);
   }
   int nb = map.Size();
   StMultiKeyNode **nodes = map.GetArr();
   int nMust=0;
   for (int i=0;i<nb;i++) 
   {
     StMultiKeyNode *node = nodes[i];
     const float *key = node->GetKeys();
     int good = 1;
     for (int k=0;k<nk;k++) {  
       if (dow[k]<=key[k] && key[k] < upp[k]) continue;
       good=0;break;
     }
     if (!good) continue;
     nMust++;
   }
   map.Clear();
printf("\nSelected  %d bad %d and must be %d\n",nSel,nBad,nMust);
printf("Touched %d %d %d\n"
      ,iter.Touched()[0],iter.Touched()[1],iter.Touched()[2]);

}


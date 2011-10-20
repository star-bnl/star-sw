#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <assert.h>

#include <algorithm>
#include <numeric>
static int gMyId=0;

#include "StMultiKeyMap.h"
#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif
static void random_shuffle(std::vector<StMultiKeyNode*> &arr); // shuffle elements 
//______________________________________________________________________________
StMultiKeyMap::StMultiKeyMap(int nkeys) 
{
  mNKey=nkeys;
  mTop = 0;
}
//______________________________________________________________________________
StMultiKeyMap::~StMultiKeyMap() 
{
  delete mTop;mTop=0;
}
//______________________________________________________________________________
void StMultiKeyMap::Clear(const char *) 
{
  delete mTop;mTop=0;
  mArr.clear();
}
//______________________________________________________________________________
void StMultiKeyMap::Add(const void *obj,const double *keys)
{
   if (!obj) obj = (void*)(-1);
   float buf[200];
   for (int i=0;i<mNKey;i++) {buf[i]=(float)keys[i];}
   Add(obj,buf);
}
//______________________________________________________________________________
void StMultiKeyMap::Add(const void *obj,const float *keys)
{
  if (!obj) obj = (void*)(-1);
  assert(!mTop);
  StMultiKeyNode *node = new StMultiKeyNode(mNKey);
  node->Set(obj,keys);
  mArr.push_back(node);
}
//______________________________________________________________________________
double StMultiKeyMap::StMultiKeyMap::Quality() 
{
  assert(mTop);
  return mTop->Quality();
}
//______________________________________________________________________________
void StMultiKeyMap::MakeTree()
{
   assert(!mTop);
   int nNodes = mArr.size();
   if (!nNodes) return;
//   std::random_shuffle( mArr.begin(),mArr.end() ); // shuffle elements 
   random_shuffle(mArr); 
   mTop =  mArr[0];
   for (int i=1;i<nNodes;i++) {mTop->Add(mArr[i]);}

std::vector<StMultiKeyNode*> tmp(0);
   assert(nNodes == mTop->Size());
   mArr.swap(tmp);	//destroy internal array completely;
   return;
}
//______________________________________________________________________________
int StMultiKeyMap::ls(const char *file) const
{
  return mTop->ls(file);
}
//______________________________________________________________________________
int StMultiKeyMap::Size() const
{
  if (mTop) return mTop->Size();
  else      return mArr.size();
}
//______________________________________________________________________________
void StMultiKeyMap::Test()
{
printf("StMultiKeyMap::Test() started\n");
   float rnd; 
   int nEVTS=1000;
   StMultiKeyMap map(1);
   for (int i=0;i<nEVTS;i++) {
     rnd = (i+1.)/nEVTS; 
     map.Add(0,&rnd);
   }
   map.MakeTree();
   double qa = map.Quality();
   printf(" Quality of tree = %g\n\n",qa);
   map.ls();

   StMultiKeyMapIter iter(map.GetTop());
   int n = 0;
   float pre=0;

printf("\n%d evts No bounds\n",nEVTS);
   for (StMultiKeyNode *node=0;(node = *iter);++iter)
   {
     n++; rnd = node->GetKeys()[0];
////     printf("%4d - %g \n",n,rnd);
     assert(pre<=rnd);
     pre = rnd;
   }
assert(n==nEVTS);
printf("\nNo bounds OK, touched %d %d\n",iter.Touched()[0],iter.Touched()[1]);


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
printf("\nGot %d. Bounds OK, Touched %d %d\n",n,iter.Touched()[0],iter.Touched()[1]);

}
//______________________________________________________________________________
void StMultiKeyMap::Test2()
{
printf("StMultiKeyMap::Test2() started\n");
   StMultiKeyMap map(4);
   float key[4];
   for (int ix=0;ix<100;ix+=10) {
     key[0]=ix;key[1]=ix+10;
     for (int iy=0;iy<200;iy+=10) {
     key[2]=iy;key[3]=iy+10;
     map.Add(0,key);
   } }
   map.MakeTree();
   double qa = map.Quality();
   printf(" Quality of tree = %g\n\n",qa);
   map.ls();


   float sel[4]={15,25,105,115};
   StMultiKeyMapIter iter(map.GetTop(),sel,0);
printf("\n4 ~evts \n");
   int n = 0;
   for (StMultiKeyNode *node=0;(node = *iter);++iter)
   {
     n++; 
     const float *key = node->GetKeys();
     printf("%4d - %g %g %g %g \n",n,key[0],key[1],key[2],key[3]);
   }
printf("\nGot %d. Bounds OK , Touched %d %d\n",n,iter.Touched()[0],iter.Touched()[1]);

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
  mId = ++gMyId;
}
//______________________________________________________________________________
void StMultiKeyNode::Clear()
{
  memset(&mIKey,0,(char*)&mObj - &mIKey);
}
//______________________________________________________________________________
StMultiKeyNode::~StMultiKeyNode()
{
  if (mObj) delete [] mKeys;
  delete mLink[0];
  delete mLink[1];
} 
//______________________________________________________________________________
void StMultiKeyNode::Set(const void *obj,const float *keys)
{
   Clear();
   if (!obj) obj = (void*)(-1);
   mObj = obj; mIKey=0;
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
  node->mIKey = (mIKey+1000003)%mNKey;
  int way = (node->mKeys[int(mIKey)] <= GetKey())? 0:1;
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
   StMultiKeyMapIter iter(this);
   
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
   if (file[0]=='-') out=0;
   else if (file[0]) out=fopen(file,"r");

   StMultiKeyMapIter iter((StMultiKeyNode*)this);
   
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
StMultiKeyMapIter::StMultiKeyMapIter(const StMultiKeyNode *node,const float *kMin,const float *kMax)
  :mMinMax(0),mStk(32)
{
  if (!node) return;
  Set(node,kMin,kMax);
}
//______________________________________________________________________________
void StMultiKeyMapIter::Set(const StMultiKeyNode *node,const float *kMin,const float *kMax)
{
  mTouched[0]=0;mTouched[1]=0;
  mStk.resize(32);
  mBoundsOn=(kMin && !kMax);	//MapTree is a tree of boundaries. 
  mNK = node->GetNKey();
  mKMin=0;mKMax=0;
  if (kMin) {
    mMinMax.resize(2*mNK);
    mKMin = &mMinMax[0];
    mKMax  = (mBoundsOn) ? 0: mKMin+mNK;
    int sk = mNK*sizeof(mKMin[0]);
    memcpy(mKMin,kMin,sk);
    if (mKMax) memcpy(mKMax,kMax,sk);
  }
  mLev = 0; mStk[0]=0;
  Down(node);
  SelfCheck();
}
//______________________________________________________________________________
void StMultiKeyMapIter::Update(const float *kMin,const float *kMax)
{
  int sk = mNK*sizeof(mKMin[0]);
  if (kMin) memcpy(mKMin,kMin,sk);
  if (kMax) memcpy(mKMax,kMax,sk);
}
//______________________________________________________________________________
StMultiKeyMapIter::~StMultiKeyMapIter()
{
}
//______________________________________________________________________________
StMultiKeyMapIter &StMultiKeyMapIter::operator++()
{
  if (!mLev)	return *this;
  const StMultiKeyNode* node = mStk[mLev];
  const StMultiKeyNode* rLink  = node->RLink();
  mLev--;
  if (!rLink)			goto RETN;
  if (mKMin && FilterRite(node))goto RETN;
  Down(rLink);
  return *this;
RETN:
  SelfCheck();
  return *this;
}  
//______________________________________________________________________________
void StMultiKeyMapIter::Down(const StMultiKeyNode *node) 
{
  while(node) {
    if ((int)mStk.size() <=mLev) mStk.resize(mLev*2);
    mStk[++mLev] = node;
    if (!node->LLink()) break;
    if (mKMin && FilterLeft(node)) break;
    node = node->LLink();
  }
  SelfCheck();
}
//______________________________________________________________________________
void StMultiKeyMapIter::SelfCheck() 
{
  const StMultiKeyNode *node = mStk[mLev];
  if (!node ) return;
  if (!node->GetObj()) {++(*this); return;}
  if (!mKMin) return;
  const float *fk = node->GetKeys();
  switch (mBoundsOn) {
    case 0:
      mTouched[1]++;
      for (int k=0;k<mNK;k++) {  
	if (mKMin[k]<=fk[k] && fk[k] < mKMax[k]) continue;
	 ++(*this); return;
      }
    return;

    case 1:
      mTouched[1]++;
      for (int k=0;k<mNK;k++) {  
        float *lim = mKMin+(k&(-2));
        switch (k&1) {
          case 0: if (fk[k]>lim[1]) {++(*this); return;} break;
          case 1: if (fk[k]<lim[0]) {++(*this); return;} break;
        }//end switch
      }//end for k
    }//end boundOn switch 
  return;
}  
//______________________________________________________________________________
int StMultiKeyMapIter::FilterLeft(const StMultiKeyNode *node) const
{
  int   ik = node->GetIKey();
  float fk = node->GetKey();
  assert(ik>=0);

  switch (mBoundsOn) {
  
  case 0: //Normal case
    if ( mKMin[ik]>fk) return 1; return 0;

  case 1:
    float *lim = mKMin+(ik&(-2));
    switch(ik&1) {
    
      case 0: //key is a lower boundary
        return 0; 
	
      case 1://key is a upper boundary
        mTouched[0]++;
        if (lim[0]>fk) return 1; return 0;
    }//end ik switch
  }//end bounds switch
  return 0;
}
//______________________________________________________________________________
int StMultiKeyMapIter::FilterRite(const StMultiKeyNode *node) const
{
  int ik = node->GetIKey();
  assert(ik>=0);
  float fk = node->GetKey();
  switch (mBoundsOn) {

   case 0:   mTouched[0]++;
             if (mKMax[ik]<fk) return 1; return 0;

   case 1: float *lim = mKMin+(ik&(-2));
    switch(ik&1) {
    
      case 0: //key is a lower boundary
         mTouched[0]++; if (lim[1]<fk) return 1; return 0;
	
      case 1://key is a upper boundary
        return 0;
    }//end ik switch
  }//end bounds switch
  return 0;
}
//______________________________________________________________________________
void random_shuffle(std::vector<StMultiKeyNode*> &arr)
{
  int n = arr.size(); if (n<=3) return;
  unsigned int u=n/2,us=1000000007;
  int jr=n-1;
  while (0<jr) {
    int jj = (u+=us)%jr;
    StMultiKeyNode *v = arr[jj]; arr[jj]=arr[jr]; arr[jr]=v; jr--;
  }
}
//______________________________________________________________________________
//______________________________________________________________________________



#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include "StMultiArray.h"	
//______________________________________________________________________________
StMultiArrayBase::StMultiArrayBase(int n1,int n2)
{
  int des[]={n1,n2};
  Init(des,2);
}  
//______________________________________________________________________________
StMultiArrayBase::StMultiArrayBase(int n1,int n2,int n3)
{
  int des[]={n1,n2,n3};
  Init(des,3);
}  
//______________________________________________________________________________
StMultiArrayBase::StMultiArrayBase(int n1,int n2,int n3,int n4)
{
  int des[]={n1,n2,n3,n4};
  Init(des,4);
}  
//______________________________________________________________________________
StMultiArrayBase::StMultiArrayBase(int n1,int n2,int n3,int n4,int n5)
{
  int des[]={n1,n2,n3,n4,n5};
  Init(des,5);
}  
//______________________________________________________________________________
StMultiArrayBase::StMultiArrayBase(int n1,int n2,int n3,int n4,int n5,int n6)
{
  int des[]={n1,n2,n3,n4,n5,n6};
  Init(des,6);
}  
//______________________________________________________________________________
StMultiArrayBase::StMultiArrayBase(int n1,int n2,int n3,int n4,int n5,int n6,int n7)
{
  int des[]={n1,n2,n3,n4,n5,n6,n7};
  Init(des,7);
}  
//______________________________________________________________________________
StMultiArrayBase::StMultiArrayBase(int n1,int n2,int n3,int n4,int n5,int n6,int n7,int n8)
{
  int des[]={n1,n2,n3,n4,n5,n6,n7,n8};
  Init(des,8);
}  
//______________________________________________________________________________
void StMultiArrayBase::Init(int *sz,int n)
{
  memset(this,0,sizeof(*this));
  mNDes = n;
  mDes[n]=1;
  for (int i=n-1;i>=0;i--) {mDes[i]=mDes[i+1]*sz[i];}
}
//______________________________________________________________________________
void StMultiArrayBase::AddIdx(int i)
{
   mSft +=mDes[++mTally]*i;
   assert(mTally<=mNDes+1);
}
//______________________________________________________________________________
int StMultiArrayBase::GetIdx()
{
   assert(mTally==mNDes);
   int i=mSft; mSft=0; mTally=0;
   return i;
}
//______________________________________________________________________________
int StMultiArrayBase::GetKdx()
{
   assert(mTally==mNDes-1);
   int i=mSft; mSft=0; mTally=0;
   return i;
}
//______________________________________________________________________________
void StMultiArrayBase::Clear()
{
   mSft=0; mTally=0;
}
//______________________________________________________________________________
void StMultiArrayBase::Test()
{
  int A[2][3][4];
  for (int i1=0;i1<2;i1++) {
  for (int i2=0;i2<3;i2++) {
  for (int i3=0;i3<4;i3++) {A[i1][i2][i3] = 100*i1+10*i2+i3;}}};

  StMultiArray<int > MA(2,3,4);
  MA = A[0][0];
  for (int i1=0;i1<2;i1++) {
  for (int i2=0;i2<3;i2++) {
  for (int i3=0;i3<4;i3++) {
    int v = MA[i1][i2][i3];
    int* vp = MA[i1][i2];
    printf("%d = %d = %d\n",100*i1+10*i2+i3,v,vp[i3]);
  }}}
  
}

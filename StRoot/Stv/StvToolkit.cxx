#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "StvToolkit.h"
#include "Stv/Factory/StvFactory.h"
#include "Stv/StvHit.h"
#include "Stv/StvNode.h"
#include "Stv/StvTrack.h"
#include "StvUtil/StvELossTrak.h"
#include "Stv/StvStl.h"
#include "Stv/StvDraw.h"
#include "StarMagField.h"
#include "StvMaker/StvHitLoader.h"
#include "Stv/StvSeedFinder.h"
#include "Stv/StvTrackFinder.h"

class StvHitFactory  : public StvFactory<StvHit ,StvHit > 		{public:};
class StvNodeFactory : public StvFactory<StvNode,StvNode> 		{public:};
class StvTrackFactory: public StvFactory<StvTrack,StvTrack> 		{public:};
class StvELossTrakFactory: public StvFactory<StvELossTrak,StvELossTrak> {public:};
class StvVertexFactory   : public StvFactory<StvVertex ,StvVertex > 	{public:};


StvToolkit *StvToolkit::mgInstance = 0;

//_____________________________________________________________________________
StvToolkit::StvToolkit()
{
  assert(!mgInstance);
  mgInstance = this;
  memset(mBeg,0,mEnd-mBeg+1);
  mTraks = new StvTracks;
}

//_____________________________________________________________________________
StvToolkit * StvToolkit::Inst()
{
  if (!mgInstance) mgInstance = new StvToolkit();

  return mgInstance;
}

//_____________________________________________________________________________
StvHit *StvToolkit::GetHit()
{
  if (!mHitFactory) {
    mHitFactory = (StvHitFactory*)StvHitFactory::myInstance();
    mHitFactory->setMaxIncrementCount(4000000);
    mHitFactory->setFastDelete();
  }
  return mHitFactory->getInstance();	
}
//_____________________________________________________________________________
StvHit *StvToolkit::GetVertex()
{
  if (!mVertexFactory) {
    mVertexFactory = (StvVertexFactory*)StvVertexFactory::myInstance();
    mVertexFactory->setMaxIncrementCount(100);
    mVertexFactory->setFastDelete();
  }
  return mVertexFactory->getInstance();	
}
//_____________________________________________________________________________
void StvToolkit::FreeHit(StvHit *&stiHit)
{
  StvHitFactory::Free(stiHit);	stiHit=0;
}
//_____________________________________________________________________________
StvTrack *StvToolkit::GetTrack()
{
  if (!mTrackFactory) {
    mTrackFactory = (StvTrackFactory*)StvTrackFactory::myInstance();
    mTrackFactory->setMaxIncrementCount(40000);
  }
  return mTrackFactory->getInstance();	
}
//_____________________________________________________________________________
StvELossTrak *StvToolkit::GetELossTrak()
{
  if (!mELossTrakFactory) {
    mELossTrakFactory = (StvELossTrakFactory*)StvELossTrakFactory::myInstance();
    mELossTrakFactory->setMaxIncrementCount(4000000);
  }
  return mELossTrakFactory->getInstance();	
}
//_____________________________________________________________________________
void StvToolkit::FreeTrack(StvTrack *&stiTrack)
{
  StvTrackFactory::Free(stiTrack);	stiTrack=0;
}
//_____________________________________________________________________________
void StvToolkit::FreeELossTrak(StvELossTrak *&stiELossTrak)
{
  StvELossTrakFactory::Free(stiELossTrak);	stiELossTrak=0;
}
//_____________________________________________________________________________
StvNode *StvToolkit::GetNode()
{
  if (!mNodeFactory) {
    mNodeFactory = (StvNodeFactory*)StvNodeFactory::myInstance();
    mNodeFactory->setMaxIncrementCount(4000000);
    mNodeFactory->setFastDelete();
  }
  return mNodeFactory->getInstance();	
}
//_____________________________________________________________________________
void StvToolkit::FreeNode(StvNode *&stiNode)
{
  StvNodeFactory::Free(stiNode);	stiNode=0;
}
//_____________________________________________________________________________
StvTracks &StvToolkit::GetTracks(){return *mTraks;}
//_____________________________________________________________________________
void StvToolkit::Show() const
{
   StvToolkit *This = (StvToolkit*)this;
   for (StvTrackConstIter it = This->GetTracks().begin();
                          it!= This->GetTracks().end(); ++it) {
     const StvTrack *tk = *it;tk->Show();
   }
}
//_____________________________________________________________________________
void StvToolkit::Clear(const char*)
{
//if (StvDraw::Jnst())  StvDraw::Jnst()->Clear();
  if (mTraks)		mTraks->clear();
  if (mHitLoader)   	mHitLoader->Clear();
  if (mSeedFinders)  	mSeedFinders->Clear();
  if (mTrakFinder)  	mTrakFinder->Clear();

  if (mTrackFactory)	mTrackFactory->clear();
  if (mELossTrakFactory) mELossTrakFactory->clear();
  if (mNodeFactory) 	mNodeFactory->clear();
  if (mHitFactory)  	mHitFactory->clear();
  if (mVertexFactory)   mVertexFactory->clear();
  StvTrack::mgId=0;
}
/*! Calculate/return the z component of mag field 
  <p>
  Calculate/return the z component of mag field
  <p>
  Field is calcualated via StarMagField class and cashed. 
*/
//_____________________________________________________________________________
void StvToolkit::Print(const char*)
{
  if (mTrackFactory)	mTrackFactory->print();
  if (mELossTrakFactory)mELossTrakFactory->print();
  if (mNodeFactory) 	mNodeFactory->print();
  if (mHitFactory)  	mHitFactory->print();
  if (mVertexFactory)   mVertexFactory->print();
}
//_____________________________________________________________________________
void StvToolkit::Reset()
{
  if (mSeedFinders)  	mSeedFinders->Reset();
  if (mTrakFinder)  	mTrakFinder->Reset();

}//______________________________________________________________________________
double StvToolkit::GetHz(const double *x) const
{
  static const Double_t EC = 2.99792458e-4;
   double h[3];
   StarMagField::Instance()->BField((x),h);
   h[2] = EC*h[2];
   if (fabs(h[2]) < 3e-33) h[2]=3e-33;
   return h[2];
}
//______________________________________________________________________________
double StvToolkit::GetHz(const float *x) const
{ double xx[3]={x[0],x[1],x[2]};
  return GetHz(xx);
}
//______________________________________________________________________________
void StvToolkit::SetSeedFinder(StvSeedFinder *seedFinder )
{
  assert(seedFinder);
  if (!mSeedFinders) mSeedFinders = new StvSeedFinders;
  mSeedFinders->push_back(seedFinder);
}
//______________________________________________________________________________
int StvToolkit::Alive(void *obj)
{
  return FactoryB::Alive(obj);
}

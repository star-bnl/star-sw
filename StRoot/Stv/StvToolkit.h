/** 
 * @file  StvToolkit.h
 */
#ifndef StvToolkit_H
#define StvToolkit_H 1

/** 
 * @class StvToolkit
 * @brief Definition of toolkit
 */
class StvGeoLoader;
class StvHitLoader;
class StvSeedFinder;
class StvSeedFinders;
class StvTrackFinder;
class StvEventFiller;
class StvHit;
class StvHitFactory;
class StvHitRrFactory;
class StvVertexFactory;

class StvNode;
class StvNodeFactory;

class StvTrack;
class StvTrackFactory;
class StvTracks;

class StvELossTrak;
class StvELossTrakFactory;

class StvToolkit 
{
protected:
  StvToolkit(); 
public:
void Clear(const char* opt="");
void Print(const char* opt="");
void Reset();

void Init ();
void Finish();

StvGeoLoader 	*GeoLoader();
StvHitLoader 	*HitLoader()  const	{return mHitLoader ;}
StvSeedFinders  *SeedFinders() const	{return mSeedFinders;}
StvTrackFinder  *TrackFinder() const	{return mTrakFinder;}
StvEventFiller  *EventFiller() const	{return mEventFiller;}
double          GetHz(const double *x) const;
double          GetHz(const float  *x) const;
double          GetHA(const double *x) const;
double          GetHA(const float  *x) const;

StvTracks      &GetTracks();
void            Show() const;


//		Factories for Stv objects
StvHit          *GetHit();       
StvHit          *GetHitRr();       
StvHit          *GetVertex();       
void            FreeHit(StvHit*     &stiHit );       
StvNode         *GetNode();       
void            FreeNode(StvNode*   &stiNode);       
StvTrack       *GetTrack();       
void            FreeTrack(StvTrack* &stiTrak);       
StvELossTrak   *GetELossTrak();       
void            FreeELossTrak(StvELossTrak* &stiELossTrak);       


void SetHitLoader  (StvHitLoader   *loadHits   ){ mHitLoader   = loadHits   ;}
void SetSeedFinders(StvSeedFinders *seedFinders){ mSeedFinders = seedFinders;}
void SetTrackFinder(StvTrackFinder *trackFinder){ mTrakFinder  = trackFinder;}
void SetEventFiller(StvEventFiller *eventFiller){ mEventFiller = eventFiller;}

public:
static StvToolkit* Inst(); 
static        int  Alive(void *obj); 

protected:
char           	mBeg[1];
StvGeoLoader   	*mGeoLoader;
StvHitLoader   	*mHitLoader;
StvSeedFinders  *mSeedFinders;
StvTrackFinder 	*mTrakFinder;
StvTracks 	*mTraks;
StvEventFiller  *mEventFiller;
StvHitFactory  	*mHitFactory;
StvHitRrFactory *mHitRrFactory;
StvNodeFactory  *mNodeFactory;
StvTrackFactory *mTrackFactory;
StvELossTrakFactory *mELossTrakFactory;
StvVertexFactory    *mVertexFactory;
// Mag field 
mutable double mX[3],mH[3];
char            mEnd[1];

protected:
static StvToolkit* mgInstance;
};

#endif


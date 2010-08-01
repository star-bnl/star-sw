/** 
 * \file  StvTrack.h
 * \brief Definition StvTrack
 * 
 */
#ifndef StvTrack_H
#define StvTrack_H 1

#include "StEvent/StEnumerations.h"
#include "Stv/StvStl.h"

class StvHitCount;
class StvHit;
class StvTrack: public StvNodes
{
 public:
 enum EPointType {kDcaPoint,kFirstPoint,kLastPoint,kPrimPoint}; 
 public:
  StvTrack(); 
  
///    	Destructor
  virtual ~StvTrack();
  void reset();
  void unset();
  
  /// returns node related ipt 0=DCA node, 1=1st point. 2=last point, 3=Primary vertex 
      StvNode *GetNode(EPointType poTy);
const StvNode *GetNode(EPointType poTy) const;

	 /// Return the number of possible hits with this track.
   int GetNPoss(StDetectorId detectorId=kUnknownId) const;

	 /// Returns the number of hits associated and used in the fit of this track.
   int GetNHits(StDetectorId detectorId=kUnknownId) const;  

	 /// Returns the number of hits associated and used in the fit of this track.
   int CountHits(StvHitCount &cnt) const;  

   /*!
     Returns the track length (in centimeters) from the :
      - first point (kFirstPoint) default;
      - DCA point (kDcaPoint) ;
      - Primary vertex (kPrimPoint) ;
     to the last point on track. 
    */
   double GetLength(EPointType ept=kFirstPoint) const;

   int ReleaseHits();
double GetXi2() const;   	// chi2/ndf of fit,        all nodes

  void SetFlag(int flag) 	{mFlag = flag;}
   int GetFlag() const   	{return mFlag;}
   int GetId()   const   	{return mId  ;}

  void SetPrimary(int iprim) 	{mPrimary = iprim;}
   int IsPrimary() const 	{return mPrimary ;}
  void Print(const char *opt) const;


  void Show() const;  

protected:
 int mFlag;  
public:
int mId; 
int mPrimary;
static int mDebug; 	// Debug level
static int mgId; 	// static track counter

};
/// Hit counting
class StvHitCount
{ 
enum {kTotHits=10,kGoodHits=5,kContHits=2,kContNits=5};
public:
StvHitCount()		{Clear();}
void Clear()		{memset(mBeg,0,mEnd-mBeg+1);}
void AddHit();
void AddNit();
int  Reject();
int  Skip() const;
public:
char mBeg[1] ;
int nPossHits;	// Number of possible hits;
int nTotHits ;	// Total number of hits
int nGoodHits;  // Number of good hits (hits in sequences > kContHits
int nSeqHits ;	// Number of hit sequences
int nSeqShort;	// Number of too short hit sequences
int nSeqNits ;	// Number of Non Hit(Nit) sequences
int nSeqLong ;	// Number of too long Non Hit(Nit) sequences
int nContHits;	// Number of hits in current Hit sequence
int nContNits;	// Number of nits in current nonHit sequence
char mEnd[1] ;
};

#endif


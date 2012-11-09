/** 
 * \file  StvTrack.h
 * \brief Definition StvTrack
 * 
 */
#ifndef StvTrack_H
#define StvTrack_H 1

#include "StEvent/StEnumerations.h"
#include "Stv/StvStl.h"

class StvHit;
class StvTrack: public StvNodes
{
 public:
 enum EPointType {kDcaPoint,kFirstPoint,kLastPoint,kPrimPoint,kMaxXi2}; 
 public:
  StvTrack(); 
  
///    	Destructor
  virtual ~StvTrack();
  void reset();
  void unset();

  /// Set Type of End tracking
  void SetTypeEnd (int tyEnd)  {mTypeEnd = tyEnd;} 
  
  /// returns node related ipt 0=DCA node, 1=1st point. 2=last point, 3=Primary vertex 
      StvNode *GetNode(EPointType poTy);
const StvNode *GetNode(EPointType poTy) const;

	 /// Returns the number of hits associated and used in the fit of this track.
   int GetNHits(StDetectorId detectorId=kUnknownId) const;  
	 /// Return the number of possible hits with this track.
   int GetNPoss(StDetectorId detectorId=kUnknownId) const;
	 /// Returns the number of fits associated direction.0=OutIn,1=InOut,2=Joined
   int GetNFits(int dir) const;  

	 /// Returns the End Type i.e reason of the end of tracking
         /// 0=Dca,1 = too many continues nits,2 = too many total nits
   int GetTypeEnd () const	{return mTypeEnd;} 

   	/// Delete all the nodes started form given
  void CutTail(const StvNode *start=0);

   /*!
     Returns the track length (in centimeters) from the :
      - first point (kFirstPoint) default;
      - DCA point (kDcaPoint) ;
      - Primary vertex (kPrimPoint) ;
     to the last point on track. 
    */
   double GetLength(EPointType ept=kFirstPoint) const;
      int GetCharge() const;

   int ReleaseHits();		// release hits from track
  void Reverse();		// Inverese node order. For debug only
double GetXi2() const;   	// chi2/ndf of fit,        all nodes
double GetXi2P() const;   	// chi2 of fit to primary vertex
double GetRes() const;		// Average residual

  void SetFlag(int flag) 	{mFlag = flag;}
   int GetFlag() const   	{return mFlag;}
   int GetId()   const   	{return mId  ;}

  void SetPrimary(int iprim) 	{mPrimary = iprim;}
   int IsPrimary() const 	{return mPrimary ;}
  void Print(const char *opt) const;

  double Approx(int mode=0);
  double ToBeam() const;
  int Check(const char *tit="",int dirs=3) const; 
  void Show() const;  

protected:
 int mFlag;  
public:
unsigned int  mId; 
unsigned char mPrimary;
unsigned char mTypeEnd;	// Type of end tracking. 0=Dca,
                        // 1 = too many continues nits,2 = too many total nits
static int mDebug; 	// Debug level
static int mgId; 	// static track counter

};

#endif


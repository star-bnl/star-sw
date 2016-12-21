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
  StvTrack &operator=(const StvTrack &from);
  StvTrack(const StvTrack &from)	{*this=from;} 
  
///    	Destructor
  virtual ~StvTrack();
  void reset();
  void unset();

  /// Set Type of End tracking
  void SetTypeEnd (int tyEnd)  {mTypeEnd = tyEnd;} 
  
  /// returns node related ipt 0=DCA node, 1=1st point. 2=last point, 3=Primary vertex 
      StvNode *GetNode(EPointType poTy);
const StvNode *GetNode(EPointType poTy) const;
	 /// Returns  node with biggest KNN distance
      StvNode *GetMaxKnnNode() ;
	 /// Returns the number of hits associated and used in the fit of this track.
   int GetNHits(StDetectorId detectorId=kUnknownId) const;  
	 /// Return the number of possible hits with this track.
   int GetNPoss(StDetectorId detectorId=kUnknownId) const;
	 /// Returns the number of fits associated direction.0=OutIn,1=InOut,2=Joined
   int GetNFits(int dir) const;  

	 /// Returns the End Type i.e reason of the end of tracking
         /// 0=Dca,1 = too many continues nits,2 = too many total nits
   int GetTypeEnd () const	{return mTypeEnd;} 

	 /// Returns the quality of track for montecarlo case
double GetQua() const;  

   	/// Delete all the nodes started form given
  void CutTail(const StvNode *start=0);

   	/// Delete all the hitless nodes a the the begining and at the end
  void CutEnds();

   /*!
     Returns the track length (in centimeters) from the :
      - first point (kFirstPoint) default;
      - DCA point (kDcaPoint) ;
      - Primary vertex (kPrimPoint) ;
     to the last point on track. 
    */
   double GetLength(EPointType ept=kFirstPoint) const;
      int GetCharge() const;

  void Reverse();		// Inverese node order. For debug only
double GetXi2() const;   	// chi2/ndf of fit,        all nodes
double GetXi2P() const;   	// chi2 of fit to primary vertex
double GetXi2W() const;   	// chi2 of the worst node
double GetRes() const;		// Average residual
double GetXi2Aux() const;   	// chi2/ndf of fit, special version or track ordering       all nodes

  void SetFlag(int flag) 	{mFlag = flag;}
   int GetFlag() const   	{return mFlag;}
   int GetId()   const   	{return mId  ;}
  void AddId(int info)      	{mId+=1000000*info;}

  void SetPrimary(int iprim) 	{mPrimary = iprim;}
   int IsPrimary() const 	{return mPrimary ;}
  void Print(const char *opt) const;

   int SetUsed(); 	
   int SetUnused(); 	

  double ToBeam() const;
  int Check(const char *tit="",int dirs=3) const; 
  void Show() const;  

protected:
unsigned char mBeg[1];
unsigned char mPrimary;
unsigned char mTypeEnd;	// Type of end tracking. 0=Dca,
          int mFlag;  
mutable float mXi2W;
mutable float mXi2Aux;
unsigned char mEnd[1];
unsigned int  mId; 
public:
static int mDebug; 	// Debug level
static int mgId; 	// static track counter

};

#endif


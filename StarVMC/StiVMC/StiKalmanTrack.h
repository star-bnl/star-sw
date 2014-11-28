/** 
 * \file  StiKalmanTrack.h
 * \brief Definition of Kalman Track
 * 
 * Subclass of StiKalmanTrack defining a Kalman track to be used by the Kalman Track Finder.
 *
 * \author Claude A Pruneau, Wayne State University, 
 * \date   March 2001
 * \copyright 2001, STAR  Experiment at BNL, All rights reserved.  
 *  
 * Permission to use, copy, modify and distribute this software and its
 * documentation strictly for non-commercial purposes is hereby granted 
 * without fee, provided that the above copyright notice appears in all
 * copies and that both the copyright notice and this permission notice
 * appear in the supporting documentation. The authors make no claims 
 * about the suitability of this software for any purpose. It is     
 * provided "as is" without express or implied warranty.             
 */
#ifndef StiKalmanTrack_H
#define StiKalmanTrack_H 1
#include "Stiostream.h"
//STD
#include <vector>
#include <assert.h>
using namespace std;
//Sti
#include "StiFactory.h"
#include "StiKTNIterator.h"
#include "StThreeVectorD.hh"
#include "StMCTruth.h"
class StiHit;
class StiKalmanTrackNode;
/*! Find extension (track) to the given track seed in the given direction */
/*! Return Ok      if operation was successful */
//______________________________________________________________________________

/*! 
  \class StiKalmanTrack
  \brief Definition of Kalman Track
  
  A concrete subclass of StiKalmanTrack defining a Kalman track to be 
  used by the Kalman Track Finder.
  
  The track reconstruction is driven by an instance of class 
  StiKalmanTrackFinder while the Kalman state of the track at any 
  given location is held by instances of the StiKalmanTrackNode 
  class. The use of nodes allows, in principle, to have, during the 
  track search, and reconstruction, tracks that behave as trees 
  rather than simple linear or sequential structures. 
  <p>
  Users should not invoke the ctor of this class directly but should 
  instead call the "getObject" method of the StiKalmantrackFactory 
  class to get instances of this class. The StiKalmanTrackFactory holds 
  (and owns, i.e. has responsibility for memory management) of a large 
  array of re-usable track objects. Instances of this class should only be 
  obtained from the factory as this eliminates (or at the very least 
  minimizes the risk) of memory leaks.	 
  <p>
  This class holds a static pointer to a track node factory. The factory 
  is invoked whenever instances of StiKalmanTrackNode are needed. The class 
  holds pointers to the fisrt and last node associated with a track. Given
  that the reconstruction proceeds primarily outside-in, the first node 
  is the outer most point associated with this track. 
  The last node is the inner most point associated with the track. 
  <p>
  This class includes a host of convenience methods to calculate track 
  properties such as the number of hits on the track (PointCount), 
  the track length (TrackLength), etc. Many of those properties
  are not stored internally but rather calculated on the fly from the 
  appropriate nodes on the tracks. This offers the advantage that it is 
  not necessary to recalculate these various properties systematically
  each time a fit or re-fit is performed but once when the information
  is actually needed. 
  
  \see StiKalmanTrackNode
  \see StiKalmanTrackFinder
  \author Claude A Pruneau (Wayne State University)
*/
/** 
    \enum Direction
    \brief Definition of directions used in track finding and fitting.
    This enumeration defines the Outside-In and Inside-Out directions 
    used in track finding and fitting.
*/
class StiKalmanTrack {
 public:
  
  enum StiKalmanTrackProperty {kCharge=0,
			       kMass,
			       kChi2,
			       kDca2,
			       kDca3,
			       kFlag,
			       kPrimaryDca,
			       kPointCount,
			       kFitPointCount,
			       kGapCount,
			       kTrackLength,
			       kMaxPointCount,
			       kIsPrimary,
			       kTpcDedx,
			       kSvtDedx,
			       kCurvature,
			       kP,
			       kPt,
			       kRapidity,
			       kPseudoRapidity,
			       kPhi,
			       kTanL  };
  /*! 
    Constructor
    Delegates the initialization of the object to the Reset method. Note that users should not call this 
    ctor directly but should instead invoke to the "getInstance" method of the Factory<StiKalmanTrack> class 
    to get instances of this class. The StiKalmanTrackFactory holds (and owns, i.e. has responsibility for
    memory management) of a large array of re-usable track objects. Instances of this class should only be 
    obtained from the factory as this eliminates (or at the very least minimizes the risk) of memory leaks.
  */
  StiKalmanTrack() : mId (0), firstNode(0), lastNode(0), mSeedHitCount(0), mFlag(0), m(-1.) {  /* nops */ }
  virtual ~StiKalmanTrack()    {  }
  /*! Set the factory used for the creation of kalman track nodes. */
  static void     SetMaxRefiter(Int_t maxRefiter) {mgMaxRefiter = maxRefiter;}
  virtual Int_t   Approx(Int_t mode);
  virtual Int_t   Fit (Int_t direction, TRVector *refFitPar = 0);
  virtual Bool_t  Find(Int_t direction=kOutsideIn,Double_t rmin=0);
  virtual Int_t   Fit();
  virtual Int_t   Smooth(Int_t direction);
  void                ExtendTrackToVertices(const std::vector<StiHit*> &vertices);
  StiKalmanTrackNode *ExtendToVertex(StiHit* vertex);
  void                ExtendTrackToVertex(StiHit* vertex);
 private:
  class QAFind {
  public: 
    Double_t rmin;  //minimal radius allowed for search
    Double_t sum; 	//summ of chi2
    Int_t    hits;  //total number of hits
    Int_t    nits;  //total number of no hits
    Int_t    wits;  //total weight of precision hits
    Int_t    qa;	// quality flag for current level
    //   qa =  1 == new hit accepted
    //   qa =  0 == no hits was expected. dead material or edge
    //   qa = -1 == hit expected but not found
    //   qa = -2 == close to beam, stop processing of it
    //   qa = -3 == fake track, stop processing of it
    //   qa = -4 == track can not be continued, stop processing of it
    
    QAFind()		{Reset();                  }
    void 	Reset()			{rmin=0; sum=0; hits =0; nits=0; qa=0;wits=0;}
  };
  virtual void    Find(Int_t direction,StiKalmanTrackNode *leadNode,QAFind &qa);
  void NodeQA(StiKalmanTrackNode *node, Int_t position,Int_t active,QAFind &qa);
  Int_t CompQA(QAFind &qaBest,QAFind &qaTry,Double_t maxChi2);
 public:
  void Reset();
  void Unset();
  /*! Calculates and returns the momentum and error of the track */
  void    Momentum(double p[3], double e[6]) const {InnerMostHitNode()->Momentum(p,e);}
  /*! Calculates and returns the momentum of the track at the inner most node */
  Double_t  P() const {return InnerMostHitNode()->P();}
  /*! Calculates and returns the transverse momentum of the track at the inner most node */
  Double_t  Pt() const {return InnerMostHitNode()->Pt();}
  /*! Return the curvature of the track at its inner most point. */
  Double_t  Curvature() const { return InnerMostHitNode()->Curvature();}
  /*! Return the rapidity of the track if the mass is known. */
  Double_t  Rapidity() const {
    Double_t p[3]; StiKalmanTrackNode *  inner = InnerMostHitNode(); inner->Momentum(p,0);
    Double_t mass = getMass();  assert(mass > 0);
    Double_t e = ::sqrt(mass*mass+p[0]*p[0]+p[1]*p[1]+p[2]*p[2]);
    return 0.5*::log(e+p[2]/e-p[2]);
  }
  /*! Return the pseudorapidity of the track. */
  Double_t  PseudoRapidity() const {
    Double_t tanTheta = tan(M_PI/4.- InnerMostHitNode()->DipAngle()/2. ); 
    assert(tanTheta>0.); return -::log(tanTheta);
  }
  /*! Return azimuthal angle at inner most point of the track. */
  Double_t  Phi()            const {Double_t p[3]; InnerMostHitNode()->Momentum(p,0); return atan2(p[1],p[0]);}
  /*! Returns the tangent of the dip angle of the track determined at the inner most point of the track. */
  Double_t  TanL()           const {return InnerMostHitNode()->TanL();}
  /*! Returns the distance of closest approach of this track to the given hit. */
  Double_t  Dca()    const {return _dca;}
  virtual Double_t  Dca(const StiHit * vertex)    const;
  void setDca(Double_t dca) { _dca = dca;}
  /*! Returns the distance of closest approach of this track to the give track. */
  Double_t Dca(StiKalmanTrack *t)   const {return 0;}
  /*! Return the number of hits associated with this track. */
  Int_t PointCount(Int_t detectorId=0) const;
  /*! Returns the number of hits associated and used in the fit of this track. */
  Int_t FitPointCount(Int_t detectorId=0) const;  
  /*! Returns all the PointCount far all detectors and types of nodes */
  void AllPointCount(Int_t count[1][3],Int_t maxDetId) const;
  /*! Return the number of gaps on this track. */
  Int_t GapCount() const;
  /*!
    Returns the track length (in centimeters) from the first to the last point on 
    track. The main vertex is included in the calculation if associated with 
    the track.
  */
  Double_t TrackLength() const;
  /*!
    Returns the track length (in centimeters) from the first point to beam line
    (x=y=0).  
    pnt is the nearest point to beam and dir direction of tracl in this point
    The main vertex is included in the calculation if associated with 
    the track.
  */
  Double_t NearBeam(StThreeVectorD *pnt=0,StThreeVectorD *dir=0) const;
  /*!
    Returns the maximum number of points that can possibly be on the track given
    its track parameters, i.e. its position in the detector. The calculation 
    accounts for sublayers that are not active, and nominally active volumes 
    that were turned off or had no data for some reason.
  */
  Int_t MaxPointCount(Int_t detectorId=0) const;
  
  Int_t SeedHitCount() const {return mSeedHitCount;}
  void SetSeedHitCount(Int_t c) {mSeedHitCount=c;}
  
  // Convenience Accessor using a switch
  virtual double  Value(int key) const;
  int    Id() const {return mId;}
  void   SetId(int id)  {mId=id;}
  /*!
   * Identifies the track as a primary or secondary track. The track
   * is defined as primary if it contains a primary vertex i.e. if the
   * vertex was included as a point to the track because it had low enough
   * a incremental chi2.
   */
  virtual bool IsPrimary() const;
  
  Double_t calculateTrackLength() const;
  Double_t calculateTrackSegmentLength(const StiKalmanTrackNode &p1, const StiKalmanTrackNode &p2) const;
  Int_t calculatePointCount() const;
  Int_t calculateMaxPointCount() const;
  
  StiKTNBidirectionalIterator  begin()  const {assert(firstNode); return StiKTNBidirectionalIterator::begin(firstNode);}
  const StiKTNBidirectionalIterator& end()    const {return StiKTNBidirectionalIterator::end();}
  StiKTNBidirectionalIterator  rbegin() const {assert(lastNode); return StiKTNBidirectionalIterator::rbegin(lastNode);}
  const StiKTNBidirectionalIterator& rend()   const {return StiKTNBidirectionalIterator::rend();}
  void  RemoveLastNode();  
  
  /*! Accessor method returns the inner/outer most node associated with the track. 
    inot: 0=inner, 1=outer; 
    qua : 0=nocheck, 1=with hit inside, 2=chi2 non infinit
    Same for NNodes(qua) */
  StiKalmanTrackNode * InnOutMostNode(Int_t inot,Int_t qua)  const;
  /*! Accessor method returns the outer most node associated with the track. */
  StiKalmanTrackNode * OuterMostNode(Int_t qua=0)  const {return InnOutMostNode(1,qua);}
  /*! Accessor method returns the inner most node associated with the track.*/
  StiKalmanTrackNode * InnerMostNode(Int_t qua=0)   const {return InnOutMostNode(0,qua);}
  
  /*! Accessor method returns the outer most hit node associated with the track.*/
  StiKalmanTrackNode * OuterMostHitNode(Int_t qua=0)  const;
  /*! Accessor method returns the inner most hit node associated with the track.*/
  StiKalmanTrackNode * InnerMostHitNode(Int_t qua=0)   const;
  Int_t                  NNodes(Int_t qua=0) const;
  /*! Accessor method returns the first node associated with the track.*/
  StiKalmanTrackNode * FirstNode()  const { return firstNode; };
  /*! Accessor method returns the last node associated with the track.*/
  // Assumes the track has been pruned.
  StiKalmanTrackNode * LastNode()   const { return  lastNode; };
  
  void  SetLastNode (StiKalmanTrackNode *n) { lastNode  = n; };
  void  SetFirstNode(StiKalmanTrackNode *n) { firstNode = n; };   
  void  SetFirstLastNode(StiKalmanTrackNode *n);   
  
  
  /*! Add a kalman track node to this track as a child to the last node of the track,  Return the added node */
  virtual void Add(StiKalmanTrackNode * node,Int_t direction);
  
  /*! Convenience method to Initialize a track based on seed information */
  Int_t Initialize(const vector<StiHit*> &);
  
  /*! Method to return the pointer to the fitter parameters. */
  
  StThreeVector<double> MomentumAtOrigin() const;
  StThreeVector<double> Point(Int_t firstLast=0) const;
  
  virtual vector<StiHit*> Hits();
  virtual vector<const StMeasuredPoint*> StHits() const;
  virtual vector<StiKalmanTrackNode*> Nodes(Int_t detectorGroupId) const;
  
  
  Double_t  getMass() const { return m;} /*! Return the mass hypothesis used in the resconstruction of this track.*/
  
  Int_t     Charge()const;   // charge of the particle
  Double_t  Chi2() const;   // chi2/ndf of fit 
  Double_t  Dca2(StiKalmanTrack *t) const {return 0;}   // distance of closest approach to given track - 2D calc
  Double_t  Dca3(StiKalmanTrack *t) const {return 0;}   // distance of closest approach to given track - 3D calc
  void ReserveHits();
  void SetFlag(long v) { mFlag = v; }
  long Flag() const { return mFlag; }
  
  StiKalmanTrackNode * ExtrapolateToBeam();
  StiKalmanTrackNode * ExtrapolateToRadius(Double_t radius);
  StMCTruth Truth(Double_t rXYMin=0, Double_t rXYMax=1000) const;  
  virtual  void Print(Option_t *opt="") const {cout << *this << endl;}
  static void SetDebug(Int_t m = 0) {_debug = m;}
  static Int_t  Debug() {return _debug;}
  StiKalmanTrack &operator=(const StiKalmanTrack &tk);
  void SetComb(int comb=7)		{mComb = comb;}
  int  Comb() const			{return mComb;}
  
  
 protected:
  friend ostream& operator<<(ostream& os, const StiKalmanTrack& track);
  Int_t mComb; //=silicon+4*tpc
  // silicon/tpc 0=no combinatoric , no tree search
  //             1=combinatoric , only hits count
  //             2=combinatoric , no hits also counts
  
  static Int_t mgMaxRefiter;		//max number of refit iteratins allowed
  static Factory<StiKalmanTrackNode> * trackNodeFactory;
  
  Int_t             mId;
  Int_t             mIdDb;
  StiKalmanTrackNode * firstNode;
  StiKalmanTrackNode * lastNode;
  
  Int_t     mSeedHitCount; //number of points used to seed the track
  long      mFlag;         //A flag to pack w/ topo info
  Double_t  m;             // mass hypothesis
  
  Double_t  _dca;
  
 public:
  Double_t _vChi2;//
  static Int_t _debug; // Debug level
};
#endif

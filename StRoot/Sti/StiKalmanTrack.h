/** 
 * \file  StiKalmanTrack.h
 * \brief Definition of Kalman Track
 * 
 * Subclass of StiTrack defining a Kalman track to be used by the Kalman Track Finder.
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

#include <cassert>
//STD
#include <vector>
//Sti
#include "Sti/Base/Factory.h"
#include "Sti/StiKTNIterator.h"
#include "Sti/StiTrack.h"
#include "Sti/StiTrackNodeHelper.h"

#include "StThreeVectorD.hh"
#include "StMCTruth.h"
class StiHit;
class StiTrackNode;
class StiKalmanTrackNode;

/*! 
  \class StiKalmanTrack
  \brief Definition of Kalman Track
  
  A concrete subclass of StiTrack defining a Kalman track to be 
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
  is invoked whenever instances of StiTrackNode are needed. The class 
  holds pointers to the fisrt and last node associated with a track. Given
  that the reconstruction proceeds primarily outside-in, the first node 
  is the outer most point associated with this track. 
  The last node is the inner most point associated with the track. 
  <p>
  This class includes a host of convenience methods to calculate track 
  properties such as the number of hits on the track (getPointCount), 
  the track length (getTrackLength), etc. Many of those properties
  are not stored internally but rather calculated on the fly from the 
  appropriate nodes on the tracks. This offers the advantage that it is 
  not necessary to recalculate these various properties systematically
  each time a fit or re-fit is performed but once when the information
  is actually needed. 
  
  \see StiKalmanTrackNode
  \see StiKalmanTrackFinder
  \author Claude A Pruneau (Wayne State University)
*/
enum nodeQA {kKeepHit=1,kGoodHit=2};


class StiKalmanTrack : public StiTrack 
{
 public:

  /// Modes for calculating approximate track parameters
  enum { kAppGud =1, kAppRR=2, kAppUpd = 4, kAppUPD = 8 };
  
  /*! 
    Constructor
    Delegates the initialization of the object to the reset method. Note that users should not call this 
    ctor directly but should instead invoke to the "getInstance" method of the Factory<StiKalmanTrack> class 
    to get instances of this class. The StiKalmanTrackFactory holds (and owns, i.e. has responsibility for
    memory management) of a large array of re-usable track objects. Instances of this class should only be 
    obtained from the factory as this eliminates (or at the very least minimizes the risk) of memory leaks.
  */
  StiKalmanTrack() :
    firstNode(0),
    lastNode(0),
    mSeedHitCount(0),
    mFlag(0),
    mMass(-1.)
    {  /* nops */ }
  
  /*! 
    Destructor
    Nothing to be done as instances of this class do not "own" the objects 
    (i.e. nodes) its members point to.
  */
  virtual ~StiKalmanTrack()
    {  }
  
  /// Set the factory used for the creation of kalman track nodes.
  static void setKalmanTrackNodeFactory(Factory<StiKalmanTrackNode>*);
  static void setMaxRefiter(int maxRefiter);

  void reset();
  void unset();
  
  /// Calculates and returns the momentum and error of the track 
  void    getMomentum(double p[3], double e[6]) const ;
  
  /// Calculates and returns the momentum of the track at the inner most node 
  double  getP() const;
  
  /// Calculates and returns the transverse momentum of the track at the inner most node 
  double  getPt() const;
  
  /// Return the curvature of the track at its inner most point. 
  double  getCurvature() const; 
  
  /// Return the rapidity of the track if the mass is known.
  double  getRapidity() const;
  
  /// Return the pseudorapidity of the track.
  double  getPseudoRapidity() const;
  
	/// Return azimuthal angle at inner most point of the track.
  double  getPhi()            const;
  
  /// Returns the tangent of the dip angle of the track determined at the inner most point of the track.
	double  getTanL()           const;

  /*!
   * Returns the distance of closest approach of this track to the given hit.
   * @see StiHit
   * @return dca in cm.
   */
   double  getDca()    const;
   virtual double  getDca(const StiHit * vertex)    const;

   void setDca(double dca);
   
  /*!
   * Returns the distance of closest approach of this track to the give track.
   * @return dca in cm.
   */
   double getDca(StiTrack *t)   const;

  /*!
   * Returns the combinatoric style used in track construction .
   * and set this value.
   */
   int     combUsed() const 	{return mCombUsed  ;}
   void setCombUsed(int comb)  	{mCombUsed = comb&7;}
  
  /*! 
   * Returns the distance of closest approach of this track to the primary vertex 
   * @return dca
   */
	 /// Return the number of hits associated with this track.
   int getPointCount(int detectorId=0) const;

	 /// Returns the number of hits associated and used in the fit of this track.
   int getFitPointCount(int detectorId=0) const;  

	 /// Returns all the PointCount far all detectors and types of nodes
   void getAllPointCount(int count[1][3],int maxDetId) const;
   
	 /// Return the number of gaps on this track. 
   int getGapCount() const;

   /*!
     Returns the track length (in centimeters) from the first to the last point on 
     track. The main vertex is included in the calculation if associated with 
     the track.
    */
   double getTrackLength() const;
   /*!
     Returns the track length (in centimeters) from the first point to beam line
     (x=y=0).  
     pnt is the nearest point to beam and dir direction of tracl in this point
     The main vertex is included in the calculation if associated with 
     the track.
    */
   double getNearBeam(StThreeVectorD *pnt=0,StThreeVectorD *dir=0) const;
  
   /*!
     Returns the maximum number of points that can possibly be on the track given
     its track parameters, i.e. its position in the detector. The calculation 
     accounts for sublayers that are not active, and nominally active volumes 
     that were turned off or had no data for some reason.
    */
   int getMaxPointCount(int detectorId=0) const;

   UShort_t getSeedHitCount() const {return mSeedHitCount;}
   void   setSeedHitCount(UShort_t c) {mSeedHitCount=c;}

  /*!
   * Identifies the track as a primary or secondary track. The track
   * is defined as primary if it contains a primary vertex i.e. if the
   * vertex was included as a point to the track because it had low enough
   * a incremental chi2.
   */
   int isPrimary() const	{return mVertex;}
   void setPrimary(int vertex) 	{mVertex=vertex;}

	double getTrackRadLength() const;

	StiKTNBidirectionalIterator  begin()  const;
  const StiKTNBidirectionalIterator& end()    const;
	StiKTNBidirectionalIterator  rbegin() const;
  const StiKTNBidirectionalIterator& rend()   const;
  void  removeLastNode();  

   /// Accessor method returns the inner/outer most node associated with the track.
   /// inot: 0=inner, 1=outer; 
   /// qua : 0=nocheck, 1=with hit inside, 2=chi2 non infinit
   		/// Same for getNNodes(qua)
   StiKalmanTrackNode * getInnOutMostNode(int inot,int qua)  const;
   		/// Accessor method returns the outer most node associated with the track.
   StiKalmanTrackNode * getOuterMostNode(int qua=0)  const;
   		/// Accessor method returns the inner most node associated with the track.
   StiKalmanTrackNode * getInnerMostNode(int qua=0)   const;

   		/// Accessor method returns the outer most hit node associated with the track.
   StiKalmanTrackNode * getOuterMostHitNode(int qua=0)  const;
   		/// Accessor method returns the inner most hit node associated with the track.
   StiKalmanTrackNode * getInnerMostHitNode(int qua=0)   const;
   StiKalmanTrackNode * getInnerMostDetHitNode(int detectorId)   const;
   int                  getNNodes(int qua=0) const;
   int                  releaseHits(double rMin=0,double rMax=50);
   /// Accessor method returns the first node associated with the track.
   StiKalmanTrackNode * getFirstNode()  const { return firstNode; };
   /// Accessor method returns the last node associated with the track.
   // Assumes the track has been pruned.
   StiKalmanTrackNode * getLastNode()   const { return  lastNode; };
   void  setLastNode (StiKalmanTrackNode *n) { lastNode  = n; };
   void  setFirstNode(StiKalmanTrackNode *n) { firstNode = n; };   
   void  setFirstLastNode(StiKalmanTrackNode *n);   
   void  removeNode(StiKalmanTrackNode *n){ n->remove((StiTreeNode**)&firstNode,(StiTreeNode**)&lastNode);}   
   
   
   /// Add a kalman track node to this track as a child to the last node of the track
   /// Return the added node 
   virtual void add(StiTrackNode * node,int direction,StiTrackNode *near=0);

  /// Convenience method to initialize a track based on seed information 
  virtual int initialize (const vector<StiHit*> &);
  virtual int initialize0(const std::vector<StiHit*> &hits, StiNodePars *firstPars=0, StiNodePars *lastPars=0, StiNodeErrs *firstErrs=0, StiNodeErrs *lastErrs=0);
    
  virtual vector<StiHit*> getHits();
  virtual vector<const StMeasuredPoint*> stHits() const;
  virtual vector<StiKalmanTrackNode*> getNodes(int detectorGroupId) const;
	 

  double  getMass()  const;   	// mass when pid known
  int     getCharge()const;   	// charge of the particle
  double  getChi2()  const;   	// chi2/ndf of fit 
  double  getChi2Max() const;   // maximal chi2 
  double  getXi2  () const { return mXi2;}   // helix chi2 
  double  getDca2(StiTrack *t) const;   // distance of closest approach to given track - 2D calc
  double  getDca3(StiTrack *t) const;   // distance of closest approach to given track - 3D calc

  int  refit();
  int  refitL();
  void reserveHits(int yes=1);
  StiTrackNode *extendToVertex(StiHit* vertex);
  void setFlag(int v);
  int  getFlag() const;

  StiKalmanTrackNode * extrapolateToBeam();
  StiKalmanTrackNode * extrapolateToRadius(double radius);
  int approx(int mode = (kAppRR | kAppUPD));
  
  void reduce();

  void print(const char *opt="") const;
  static void setDebug(int d = 0) {_debug = d;}
  static int  debug() {return _debug;}
  StiKalmanTrack &operator=(const StiKalmanTrack &tk);
  int rejectByHitSet()  const;
  int idTruth(int *qu=0) const;

 void test(const char *txt="")  const;
  
  typedef enum{ // type of return value for the refit() procedure
    kNoErrors = 0,
    kRefitFail,
    kNotEnoughUsed,
    kInNodeNotValid,
    kBadQA,
    kVertexNodeInvalid,
    kNodeNotValid,
    kTooManyDroppedNodes
  } TRefitStatus;

  
protected:
  friend ostream& operator<<(ostream& os, const StiKalmanTrack& track);

  // hidden static variables for refit & refiL
  static StiTrackNodeHelper sTNH;
  static double diff(const StiNodePars &p1,const StiNodeErrs &e1
                    ,const StiNodePars &p2,const StiNodeErrs &e2,int &igor);
  // end of hidden static variables for refit & refiL

  /**
   * Two return values can be obtained by calling this protected version of
   * refit(). By default the original value is used in the publicly available
   * refit() of this class while the other is used in derived class
   * StiCAKalmanTrack.
   */

protected:
    
  static int mgMaxRefiter;		//max number of refit iteratins allowed
  static Factory<StiKalmanTrackNode> * trackNodeFactory;
  
  StiKalmanTrackNode * firstNode;
  StiKalmanTrackNode * lastNode;

  UShort_t  mSeedHitCount; //number of points used to seed the track (seed quality)
  char      mCombUsed; 	  // save which combinatoric style was used
  int     mVertex;
  int     mFlag;         //A flag to pack w/ topo info
  double  mMass;         // mass hypothesis
  double  mXi2;
  double  _dca;

 public:
  double _vChi2;//
  static int _debug; // Debug level

};

/*! Return the mass hypothesis used in the resconstruction of this track.
*/
inline double  StiKalmanTrack::getMass() const
{ 
  return mMass;  
}

inline void StiKalmanTrack::setFlag(int v) 
{
  mFlag = v;
}

inline int StiKalmanTrack::getFlag() const 
{
  return mFlag;
}

/*! 
  Calculates and returns the momentum and error of the track 
  <p>
  This method calculates and returns in the two arrays provided as arguments the 
  3-momentum and error of the track in Star global coordinates. The 3-momentum 
  is calculated at the inner most point associated with the track. The inner-most 
  point may or may not be the main vertex of the event. Care should thus be exercised 
  while using this method. 
  <p>
  The error is calculated (and returned) only if a non null array is passed as a second
  argument. It is thus possible to get the momentum without a lengthy calculation 
  of the error matrix. The error error matrix corresponds to a full covariance matrix.
  The definition of the error matrix is described in the introduction of this class
  definition. Note that the actual calculation of the momentum and associated error 
  is delegated to the track node class and uses the inner most node of the track.
*/
inline void StiKalmanTrack::getMomentum(double p[3], double e[6]) const
{
  // return the momentum of the track at the inner most node held by this track
  // which may (or not) be the primary vertex. 
  // this will need to be refined...
  getInnerMostHitNode()->getMomentum(p,e);
}

/*!
   Calculates and returns the momentum of the track at the inner most node 
   held by this track which may or (or not) be the primary vertex. 
*/
inline double  StiKalmanTrack::getP() const
{
  return getInnerMostHitNode()->getP();
}

/*!
   Calculates and returns the transverse momentum of the track at the inner most node 
   held by this track which may or (or not) be the primary vertex. 
*/
inline double  StiKalmanTrack::getPt() const
{
  return getInnerMostHitNode()->getPt();
}

/*!
  Calculates and returns the track curvature at the inner most node held by this track.
  <p>
  Obtains the curvature from the inner most hit node associated with this track.
*/
inline double StiKalmanTrack::getCurvature() const
{
  return getInnerMostHitNode()->getCurvature();
}

/*!
  Returns the rapidity of the track if the mass is known.
  <p>
  <ol>
  <li>Obtains the momentum from the inner most hit node associated with the track.</li>
  <li>Obtains the mass of this track using the getMass() method. If the mass returned
  is negative, throws a runtime_error exception.</li>
  </ol>
  \throws runtime_error
  \return rapidity
*/
inline double  StiKalmanTrack::getRapidity()       const 
{
  // returns the rapidity of the particle at the inner most node held by this track
  // which may (or not) be the primary vertex. 
  double p[3];
  StiKalmanTrackNode *  inner = getInnerMostHitNode();
  inner->getMomentum(p,0);
  double mass = getMass();
  assert(mass >= 0);
  double e = ::sqrt(mass*mass+p[0]*p[0]+p[1]*p[1]+p[2]*p[2]);
  assert(e > p[2]);
  return 0.5*::log(e+p[2]/e-p[2]);
}

/*!
  Returns the pseudo-rapidity of the track.
  <p>
  <ol>
  <li>Obtains the helix pitch angle from the inner most hit node associated with the track.</li>
  <li>Calculate/return the pseudo-rapidity using the pitch angle.</li>
  </ol>
  \return pseudo-rapidity
*/
inline double  StiKalmanTrack::getPseudoRapidity() const
{
  // Return pseudo rapidity of the particle at the inner most node held by this track
  // which may (or not) be the primary vertex. 
  double tanTheta = tan(M_PI/4.- getInnerMostHitNode()->getDipAngle()/2. );
  assert(tanTheta>0.);
  return -::log(tanTheta);
}

/*! 
  Returns the azimuthal angle of the track determined at the inner most point of the track
  hich may or may not be a vertex.
  \return phi in radian
*/
inline double  StiKalmanTrack::getPhi()            const 
{
  double p[3];
  getInnerMostHitNode()->getGlobalMomentum(p,0);
  return atan2(p[1],p[0]);
}

/*!
	Return tan(lambda) of the particle at the inner most node held by this track
	which may (or not) be the primary vertex. 
	\return tan(lambda)
*/
inline double  StiKalmanTrack::getTanL()           const 
{
  return getInnerMostHitNode()->getTanL();
}

/*! Calculate and return the distance of closest approach to given hit
   <h3>Notes</h3> 
   <ol>
   <li>No implementation.</li>
   <li>Returns 0</li>
   </ol>
*/
inline double  StiKalmanTrack::getDca()    const
{
  return _dca;
}

inline void  StiKalmanTrack::setDca(double dca)  
{
  _dca = dca;
}


/*! Calculate and return the distance of closest approach to given track
   <h3>Notes</h3> 
   <ol>
   <li>No implementation.</li>
   <li>Returns 0</li>
   </ol>
*/
inline double  StiKalmanTrack::getDca(StiTrack *t)   const
{
  return 0;
}

/*! Calculate and return the distance of closest approach to given track - 2D calc
   <h3>Notes</h3> 
   <ol>
   <li>No implementation.</li>
   <li>Returns 0</li>
   </ol>
*/
inline double  StiKalmanTrack::getDca2(StiTrack *t)   const
{
    return 0;
}

/*! Calculate and return the distance of closest approach to given track - 3D calc
   <h3>Notes</h3> 
   <ol>
   <li>No implementation.</li>
   <li>Returns 0</li>
   </ol>
*/
inline double  StiKalmanTrack::getDca3(StiTrack *t)   const
{
    return 0;
}


/*! Convenience method used to return a track node iterator initialized to the track first node.
  \return Bidirectional Itertator of KalmanTrackNodes 
  \throws runtime_error 	
*/
inline StiKTNBidirectionalIterator StiKalmanTrack::begin() const 
{
  assert(firstNode);
  return StiKTNBidirectionalIterator::begin(firstNode);
}
inline StiKTNBidirectionalIterator StiKalmanTrack::rbegin() const 
{
  assert(lastNode);
  return StiKTNBidirectionalIterator::rbegin(lastNode);
}

/*! Convenience method used to return a track node iterator initialized to the track last node.
	\return Bidirectional Itertator of KalmanTrackNodes 
	\throws runtime_error 	
*/
inline const StiKTNBidirectionalIterator &StiKalmanTrack::end() const 
{
  return StiKTNBidirectionalIterator::end();
}
inline const StiKTNBidirectionalIterator &StiKalmanTrack::rend() const 
{
  return StiKTNBidirectionalIterator::rend();
}

/*! Accessor method to get the dca.
  <h3>Note</h3> 
  <ol>
  <li>Not implemented</li>
  </ol>
*/

/*! Accessor method returns the outer most node associated with the track.
   <h3>Notes</h3>
   <ol>
   <li>Node returned depends on the direction of tracking. </li>
   <li>Return firstNode if tracking was done outside-in, lastNode otherwise.</li>
   <li>No check done to determine whether returned value is non null.</li>
   </ol>
	 \return outer most node on this track
*/
inline StiKalmanTrackNode * StiKalmanTrack::getOuterMostNode(int qua)  const 
{
  return getInnOutMostNode(1,qua);
}

/*! Accessor method returns the inner most node associated with the track.
   <h3>Notes</h3>
   <ol>
   <li>Node returned depends on the direction of tracking. </li>
   <li>Return firstNode if tracking was done inside-out, lastNode otherwise.</li>
   <li>No check done to determine whether returned value is non null.</li>
   </ol>
	 \return outer most node on this track
*/
inline StiKalmanTrackNode * StiKalmanTrack::getInnerMostNode(int qua)   const 
{ 
  return getInnOutMostNode(0,qua);
}

#endif


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
//Sti
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
//STD
#include <math.h>
#include <vector>
#include <stdexcept>
using namespace std;

#include "StDetectorId.h"

//Sti
#include "Factory.h"
#include "StiKalmanTrackNode.h"
#include "StiHitContainer.h"
#include "StiKTNIterator.h"
#include "StiHit.h"
#include "StiTrack.h"
#include "StiKalmanTrackFinderParameters.h"

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
class StiKalmanTrack : public StiTrack 
{
 public:
  
  /*! 
    Constructor
    Delegates the initialization of the object to the reset method. Note that users should not call this 
    ctor directly but should instead invoke to the "getInstance" method of the Factory<StiKalmanTrack> class 
    to get instances of this class. The StiKalmanTrackFactory holds (and owns, i.e. has responsibility for
    memory management) of a large array of re-usable track objects. Instances of this class should only be 
    obtained from the factory as this eliminates (or at the very least minimizes the risk) of memory leaks.
  */
  StiKalmanTrack() :
    trackingDirection(kOutsideIn),
    fittingDirection(kOutsideIn),
    firstNode(0),
    lastNode(0),
    mSeedHitCount(0),
    mFlag(0),
    m(-1.)
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
  static void setParameters(StiKalmanTrackFinderParameters* p);

  void    reset();
  
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
   double  getDca(StiHit *h=0)    const;

  /*!
   * Returns the distance of closest approach of this track to the give track.
   * @return dca in cm.
   */
   double getDca(StiTrack *t)   const;
  
  /*! 
   * Returns the distance of closest approach of this track to the primary vertex 
   * @return dca
   */
   double getPrimaryDca() const;

	 /// Return the number of hits associated with this track.
   int getPointCount() const;

	 /// Returns the number of hits associated and used in the fit of this track.
   int getFitPointCount() const;  
   
	 /// Return the number of gaps on this track. 
   int getGapCount() const;

   /*!
     Returns the track length (in centimeters) from the first to the last point on 
     track. The main vertex is included in the calculation if associated with 
     the track.
    */
   double getTrackLength() const;
   
   /*!
     Returns the maximum number of points that can possibly be on the track given
     its track parameters, i.e. its position in the detector. The calculation 
     accounts for sublayers that are not active, and nominally active volumes 
     that were turned off or had no data for some reason.
    */
   int getMaxPointCount() const;

   int getSeedHitCount() const;
   void setSeedHitCount(int c);

  /*!
   * Identifies the track as a primary or secondary track. The track
   * is defined as primary if it contains a primary vertex i.e. if the
   * vertex was included as a point to the track because it had low enough
   * a incremental chi2.
   */
  bool isPrimary() const;

	double calculateTrackLength() const;
	double calculateTrackSegmentLength(const StiKalmanTrackNode &p1, const StiKalmanTrackNode &p2) const;
	int calculatePointCount() const;
	int calculateMaxPointCount() const;

  double getTpcDedx() const;
  double getSvtDedx() const;

	StiKTNBidirectionalIterator begin() const;
	StiKTNBidirectionalIterator end() const;

   /// Accessor method returns the outer most node associated with the track.
   StiKalmanTrackNode * getOuterMostNode()  const;
   /// Accessor method returns the inner most node associated with the track.
   StiKalmanTrackNode * getInnerMostNode()   const;

   /// Accessor method returns the outer most hit node associated with the track.
   StiKalmanTrackNode * getOuterMostHitNode()  const;
   /// Accessor method returns the inner most hit node associated with the track.
   StiKalmanTrackNode * getInnerMostHitNode()   const;

   /// Accessor method returns the first node associated with the track.
   StiKalmanTrackNode * getFirstNode()  const { return firstNode; };
   /// Accessor method returns the last node associated with the track.
   // Assumes the track has been pruned.
   StiKalmanTrackNode * getLastNode()   const { return lastNode; };

   void  setLastNode(StiKalmanTrackNode *n) { lastNode = n; };
   
   /// Returns the direction (kInsideOut, kOutsideIn) used in the reconstruction of this track.
   StiDirection getTrackingDirection() const { return  trackingDirection;};
   
   /// Returns the direction (kInsideOut, kOutsideIn) used in the fit of this track.
   StiDirection getFittingDirection() const { return   fittingDirection;};
   
   /// Sets the direction (kInsideOut, kOutsideIn) used in the reconstruction of this track.
   void setTrackingDirection(StiDirection direction) { trackingDirection = direction;}
   
   /// Sets the direction (kInsideOut, kOutsideIn) used in the fit of this track.
   void setFittingDirection(StiDirection direction) { fittingDirection = direction;}
   
   /// Method used to add a hit to this track
   /*! Method used to add an arbitrary hit to a track. A track node
       is first obtained from the KalmantrackNode factory. The hit is added to
       the node and the node is added to the track as a child to the last node 
       of the track.
   */
   StiKalmanTrackNode * addHit(StiHit *h);
  
  /// Method to insert a hit in this track
  StiKalmanTrackNode * insertHit(StiHit *hInserted, StiHit * targetParent);
  
  /// Remove given hit from this track
  void removeHit(StiHit *h);
  
  /// Remove all hits and nodes currently associated with this track.
  void removeAllHits();
  
  /// Work method used to find the node containing the given hit.
  StiKalmanTrackNode * findHit(StiHit * h);
  
  /// Convenience method to initialize a track based on seed information 
  void initialize(double curvature,
									double tanl,
									const StThreeVectorD& origin,
									const hitvector &);
  
  /// Work method returns the node closest to the given position.
  /*! Work method returns the node closest to the given position. 
    The given position is a radial distance calculated in the local
    reference frame of the detector.
  */
  StiKalmanTrackNode *  getNodeNear(double x) const;
  
  /*! Convenience method returns a point corresponding to the node
    of this track which is the closest to the given position.
  */
  StThreeVector<double> getPointNear(double x) const;
  StThreeVector<double> getGlobalPointNear(double x) const;
  StThreeVector<double> getGlobalPointAt(double x) const;
  
   StThreeVector<double> getMomentumAtOrigin() const;
   StThreeVector<double> getMomentumNear(double x);
   StThreeVector<double> getHitPositionNear(double x) const;

    ///return hits;
	 //virtual vector<StHit*> stHits() const;
	 virtual vector<StMeasuredPoint*> stHits() const;
	 virtual vector<StiKalmanTrackNode*> getNodes(StDetectorId det) const;
	 
  // Function to reverse the node geometry of a track
  void swap();


  double  getMass() const;   // mass when pid known
  int     getCharge()const;   // charge of the particle
  double  getChi2() const;   // chi2 of fit
  double  getDca2(StiTrack *t) const;   // distance of closest approach to given track - 2D calc
  double  getDca3(StiTrack *t) const;   // distance of closest approach to given track - 3D calc

  bool find(int direction=kOutsideIn);
  void prune();
  void reserveHits();
  bool extendToVertex(StiHit* vertex);

  void setFlag(long v);
  long getFlag() const;

protected:
    
  static StiKalmanTrackFinderParameters * pars;
  static Factory<StiKalmanTrackNode> * trackNodeFactory;
  
  StiDirection trackingDirection;
  StiDirection fittingDirection;
  StiKalmanTrackNode * firstNode;
  StiKalmanTrackNode * lastNode;

  int     mSeedHitCount; //number of points used to seed the track
  long    mFlag;         //A flag to pack w/ topo info
  double  m;             // mass hypothesis


};

/*! Return the mass hypothesis used in the resconstruction of this track.
*/
inline double  StiKalmanTrack::getMass() const
{ 
  return m;  
}

inline int StiKalmanTrack::getSeedHitCount() const
{
  return mSeedHitCount;
}

inline void StiKalmanTrack::setSeedHitCount(int c) 
{
  mSeedHitCount=c;
}

inline void StiKalmanTrack::setFlag(long v) 
{
  mFlag = v;
}

inline long StiKalmanTrack::getFlag() const 
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
  return getInnerMostHitNode()->fP3;
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
  if (mass<0)
    throw runtime_error("StiKalmanTrack::getRapidity() - particle mass unknown");
  double e = sqrt(mass*mass+p[0]*p[0]+p[1]*p[1]+p[2]*p[2]);
  if (e<=p[2])
    throw runtime_error("StiKalmanTrack::getRapidity() - Error: e<=pz");
  return 0.5*log(e+p[2]/e-p[2]);
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
  return -log(tan(M_PI/4.-(getInnerMostHitNode()->getTanL()/2.)));
}

/*! 
  Returns the azimuthal angle of the track determined at the inner most point of the track
  hich may or may not be a vertex.
  \return phi in radian
*/
inline double  StiKalmanTrack::getPhi()            const 
{
  double p[3];
  getInnerMostHitNode()->getMomentum(p,0);
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

inline double StiKalmanTrack::getTpcDedx() const
{
  return 0.; // to be changed...
}

inline double StiKalmanTrack::getSvtDedx() const
{
  return 0.; // to be changed...
}
/*! Calculate and return the distance of closest approach to given hit
   <h3>Notes</h3> 
   <ol>
   <li>No implementation.</li>
   <li>Returns 0</li>
   </ol>
*/
inline double  StiKalmanTrack::getDca(StiHit * h)    const
{
	return 0;
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
  if (!firstNode)
    throw runtime_error("StiKalmanTrack::begin() - ERROR - firstNode==0");
  return StiKTNBidirectionalIterator(firstNode);
}

/*! Convenience method used to return a track node iterator initialized to the track last node.
	\return Bidirectional Itertator of KalmanTrackNodes 
	\throws runtime_error 	
*/
inline StiKTNBidirectionalIterator StiKalmanTrack::end() const 
{
  if (!firstNode)
    throw runtime_error("StiKalmanTrack::end() - ERROR - firstNode==0");
  return StiKTNBidirectionalIterator(lastNode);
}

/*! Accessor method to get the dca.
  <h3>Note</h3> 
  <ol>
  <li>Not implemented</li>
  </ol>
*/
inline double StiKalmanTrack::getPrimaryDca() const
{
  return 0;
}

/*! Accessor method returns the outer most node associated with the track.
   <h3>Notes</h3>
   <ol>
   <li>Node returned depends on the direction of tracking. </li>
   <li>Return firstNode if tracking was done outside-in, lastNode otherwise.</li>
   <li>No check done to determine whether returned value is non null.</li>
   </ol>
	 \return outer most node on this track
*/
inline StiKalmanTrackNode * StiKalmanTrack::getOuterMostNode()  const 
{
  return (trackingDirection==kOutsideIn)?firstNode:lastNode;
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
inline StiKalmanTrackNode * StiKalmanTrack::getInnerMostNode()   const 
{ 
  return (trackingDirection==kInsideOut)?firstNode:lastNode;
}


#endif


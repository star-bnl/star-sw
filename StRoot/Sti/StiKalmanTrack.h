/** 
 * @file  StiKalmanTrack.h
 * @brief Definition of Kalman Track
 * 
 * Subclass of StiTrack defining a Kalman track to be used by the Kalman Track Finder.
 *
 * @author Claude A Pruneau, Wayne State University, 
 * @date   March 2001
 * @copyright 2001, STAR  Experiment at BNL, All rights reserved.  
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
//SCL
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"
//STD
#include <math.h>
#include <vector>
using namespace std;

//Sti
#include "StiObjectFactoryInterface.h"
#include "StiTrack.h"
#include "StiKalmanTrackNode.h"
#include "StiHitContainer.h"
#include "StiKTNIterator.h"
#include "StiHit.h"

class StHit;

/** 
 * @enum StiDirection
 * @brief Definition of directions used in track finding and fitting.
 * This enumeration defines the Outside-In and Inside-Out directions 
 * used in track finding and fitting.
 */
enum StiDirection {kOutsideIn=0, kInsideOut};

/** 
 * @class StiKalmanTrack
 * @brief Definition of Kalman Track
 * 
 * Subclass of StiTrack defining a Kalman track to be used by the Kalman Track Finder.
 *
 * A concrete class used in the reconstruction of tracks within the Star detector. The track reconstruction 
 * is driven by an instance of class StiKalmanTrackFinder while the Kalman state of the track at any given 
 * location is held by instances of the StiKalmanTrackNode class. The use of nodes allows, in principle to
 * have, during the track search, and reconstruction, tracks that behave as trees rather than simple linear
 * or sequential structures. 
 * 
 * This class holds a static pointer to a track node factory. The factory is invoked whenever instances of 
 * StiTrackNode are needed. The class holds pointers to the fisrt and last node associated with a track. Given
 * that the reconstruction proceeds outside-in, the first node is the outer most point associated with this track. 
 * The last node is the inner most point associated with the track. The class also currently holds two double meant 
 * to correspond to the svt and tpc dedx respectively. Those are however not used to store this information and 
 * should be considered depracted.
 */
class StiKalmanTrack : public StiTrack 
{
public:
  
  /** 
   * Constructor
   * Delegates the initialization of the object to the reset method. Note that users should not call this 
   * ctor directly but should instead invoke to the "getObject" method of the StiKalmantrackFactory class 
   * to get instances of this class. The StiKalmanTrackFactory holds (and owns, i.e. has responsibility for
   * memory management) of a large array of re-usable track objects. Instances of this class should only be 
   * obtained from the factory as this eliminates (or at the very least minimizes the risk) of memory leaks.
   */
  StiKalmanTrack() 
    {
      reset();
    };
  
  /** 
   * Destructor
   * Nothing to be done as instances of this class do not "own" the objects (i.e. nodes) its members point to.
   */
  virtual ~StiKalmanTrack()
    {
    };

  /** 
   * Set the factory used for the creation of kalman track nodes.
   * @see StiFactory
   */
  static void setKalmanTrackNodeFactory(StiObjectFactoryInterface<StiKalmanTrackNode>*);
  
  //Action method for polymorphic graphical behavior
  virtual void update();
  
  /** 
   * Reset the class members to their default state.
   * This method is called by the ctor of the class to initialize the
   * members of the class to an "empty" or null track state. The
   * method must also be called everytime an instance of this class is
   * retrieved from its factory in order to set the first and last
   * nodes to "null" thus guaranteeing that the track object is empty
   * i.e. does not represent any track and is thus ready for a new
   * search and reconstruction.  It is guaranteed that a call to
   * reset() fully propogates up the inheritance tree.
   */
  void    reset();
  
  /** 
   * Calculates and returns the momentum and error of the track 
   * This method calculates and returns in the two arrays provided as arguments the 
   * 3-momentum and error of the track in Star global coordinates. The 3-momentum 
   * is calculated at the inner most point associated with the track. The inner-most 
   * point may or may not be the main vertex of the event. Care should thus be exercised 
   * while using this method. 
   *
   * The error is calculated (and returned) only if a non null array is passed as a second
   * argument. It is thus possible to get the momentum without a lengthy calculation 
   * of the error matrix. The error error matrix corresponds to a full covariance matrix.
   * The definition of the error matrix is described in the introduction of this class
   * definition. Note that the actual calculation of the momentum and associated error 
   * is delegated to the track node class and uses the inner most node of the track.
   */
  void    getMomentum(double p[3], double e[6]) const ;
  
  /**
   * Calculates and returns the transverse momentum of the track at the inner most node 
   * held by this track which may or (or not) be the primary vertex. 
   */
  double  getPt()             const;
  
  /// Convenience method used to return the curvature of the track at its inner most point. 
  double  getCurvature()      const; 
  
  /**
   * Returns the rapidity of the track if the mass is known, Throws a runtime_error
   * exception otherwise.
   * @return rapidity
   */
  double  getRapidity()       const;
  
  /**
   * Returns the pseudorapidity of the track.
   * @return pseudorapidity
   */
  double  getPseudoRapidity() const;
  
  /** 
   * Returns the azimuthal angle of the track determined at the inner most point of the track
   * which may or may not be a vertex.
   * @return phi in radian
   */
  double  getPhi()            const;
  
  /**
   * Returns the tangent of the dip angle of the track determined at the inner most point of the track
   * which may or may not be a vertex.
   * @return tan(lambda)
   */
   double  getTanL()           const;

  /**
   * Returns the distance of closest approach of this track to the given hit.
   * @see StiHit
   * @return dca in cm.
   */
   double  getDca(StiHit *h=0)    const;

  /**
   * Returns the distance of closest approach of this track to the give track.
   * @return dca in cm.
   */
   double getDca(StiTrack *t)   const;
  
  /** 
   * Returns the distance of closest approach of this track to the primary vertex 
   * @return dca
   */
   double getPrimaryDca() const;

  /**
   * Returns the number of hits associated with this track.
   * @return number of hits associated with the track
   */
   int getPointCount() const;

  /**
   * Returns the number of hits associated with this track.
   * @return number of hits associated with the track
   */
   int getFitPointCount() const;  
   
  /**
   * Returns the number of gaps on this track. The gaps correspond to missing hits 
   * in active layers along the track
   * @return number of gaps along this track
   */
   int getGapCount() const;

	 /**
    * Returns the track length (in centimeters) from the first to the last point on 
		* track. The main vertex is included in the calculation if associated with 
		* the track.
		*/
	 double getTrackLength() const;

	 /**
		* Returns the maximum number of points that can possibly be on the track given
		* its track parameters, i.e. its position in the detector. The calculation 
		* accounts for sublayers that are not active, and nominally active volumes 
		* that were turned off or had no data for some reason.
		*/
	 int getMaxPointCount() const;

  /**
   * Identifies the track as a primary or secondary track. The track
   * is defined as primary if it contains a primary vertex i.e. if the
   * vertex was included as a point to the track because it had low enough
   * a incremental chi2.
   */
  bool isPrimary() const;

	double calculateTrackLength() const;
	double calculateTrackSegmentLength(StiKalmanTrackNode *p1, StiKalmanTrackNode *p2) const;
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
   /** Method used to add an arbitrary hit to a track. A track node
       is first obtained from the KalmantrackNode factory. The hit is added to
       the node and the node is added to the track as a child to the last node 
       of the track.
   */
   StiKalmanTrackNode * addHit(StiHit *h);
  
  /// Method to insert a hit in this track
  StiKalmanTrackNode * insertHit(StiHit *hInserted, StiHit * targetParent);
  
  /// 
  void removeHit(StiHit *h);
  
  /// Remove all hits and nodes currently associated with this track.
  void removeAllHits();
  
  /// Work method used to find the node containing the given hit.
  /**
    Current implementation only considers the first child of each node
    and must therefore be revised.
  */
  StiKalmanTrackNode * findHit(StiHit * h);
  
  /// Convenience method to initialize a track based on seed information 
  /** Method to initialize this track based on given arguments. 
    curvature : estimated curvature of the track
    tanl      : tangent of the estimated pitch angle of this track
    origin    : 3-coordinates of the origin of this track
    hitvector : stl vector containing a series of hits to be inserted in this track
    
    This method gets node from the node factory and inserts the given hits 
    in those nodes in the given sequence assumed (but not checked) to be 
    in radially decreasing order. The kalman state of each track node is set
    on the basis of the curvature, tanl, and origin provided. The error matrix
    of each node is set a diagonal matrix with unit value.
  */
  void initialize(double curvature,
		  double tanl,
		  const StThreeVectorD& origin,
		  const hitvector &);
  
  /// Work method returns the node closest to the given position.
  /** Work method returns the node closest to the given position. 
    The given position is a radial distance calculated in the local
    reference frame of the detector.
  */
  StiKalmanTrackNode *  getNodeNear(double x) const;
  
  /** Convenience method returns a point corresponding to the node
    of this track which is the closest to the given position.
  */
  StThreeVector<double> getPointNear(double x) const;
  StThreeVector<double> getGlobalPointNear(double x) const;
  StThreeVector<double> getGlobalPointAt(double x) const;
  
   StThreeVector<double> getMomentumAtOrigin() const;
   StThreeVector<double> getMomentumNear(double x);
   StThreeVector<double> getHitPositionNear(double x) const;

    ///return hits;
    virtual vector<StHit*> stHits() const;
  
  // Function to reverse the node geometry of a track
  void swap();


  double  getMass() const;   // mass when pid known
  int     getCharge()const;   // charge of the particle
  double  getChi2() const;   // chi2 of fit
	double  getDca2(StiTrack *t) const;   // distance of closest approach to given track - 2D calc
	double  getDca3(StiTrack *t) const;   // distance of closest approach to given track - 3D calc

protected:
    
  static StiObjectFactoryInterface<StiKalmanTrackNode> * trackNodeFactory;
  
  StiDirection trackingDirection;
  StiDirection fittingDirection;
  StiKalmanTrackNode * firstNode;
  StiKalmanTrackNode * lastNode;
};

#endif


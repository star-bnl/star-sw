/*! \class StiKalmanTrack
  A concrete class used in the reconstruction of tracks within the Star detector. The track reconstruction 
  is driven by an instance of class StiKalmanTrackFinder while the Kalman state of the track at any given 
  location is held by instances of the StiKalmanTrackNode class. The use of nodes allows, in principle to
  have, during the track search, and reconstruction, tracks that behave as trees rather than simple linear
  or sequential structures. 
 
  This class holds a static pointer to a track node factory. The factory is invoked whenever instances of 
  StiTrackNode are needed. The class holds pointers to the fisrt and last node associated with a track. Given
  that the reconstruction proceeds outside-in, the first node is the outer most point associated with this track. 
  The last node is the inner most point associated with the track. The class also currently holds two double meant 
  to correspond to the svt and tpc dedx respectively. Those are however not used to store this information and 
  should be considered depracted.

  \author Claude A Pruneau (Wayne State Unviersity)
*/

#ifndef StiKalmanTrack_H
#define StiKalmanTrack_H 1

//SCL
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"

//STD
#include <math.h>
//Sti
#include "StiObjectFactoryInterface.h"
#include "StiTrack.h"
#include "StiKalmanTrackNode.h"
#include "StiHitContainer.h"


enum StiTrackingDirection {kOutsideIn=0, kInsideOut};

class StiKalmanTrack : public StiTrack 
{
public:
  
    // constructor/destructor/copy/etc
  
    /// Constructor. 
    /*! Delegates the initialization of the object to the reset method. Note that users should not call this 
      ctor directly but should instead invoke to the "getObject" method of the StiKalmantrackFactory class 
      to get instances of this class. The StiKalmanTrackFactory holds (and owns, i.e. has responsibility for
      memory management) of a large array of re-usable track objects. Instances of this class should only be 
      obtained from the factory as this eliminates (or at the very least minimizes the risk) of memory leaks.
    */
    StiKalmanTrack() 
    {
	reset();
    };

    /// destructor
    /*! Nothing to be done as instances of this class do not "own" the objects (i.e. nodes) its members point to.
     */
    virtual ~StiKalmanTrack()
    {
    };

    /// Static method used to identify (i.e. set) the factory used for the creation of kalman track nodes.
    static void setKalmanTrackNodeFactory(StiObjectFactoryInterface<StiKalmanTrackNode>*);
	
    //Action method for polymorphic graphical behavior
    virtual void update();
    
    // Action methods
    // Implementation of virtual methods inherited from StiTrack

    /// Reset the class members to their default state.
    /*! This method is called by the ctor of the class to initialize the members of the 
      class to an "empty" or null track state. The method must also be called everytime
      an instance of this class is retrieved from its factory in order to set the first 
      and last nodes to "null" thus guaranteeing that the track object is empty i.e. does
      not represent any track and is thus ready for a new search and reconstruction.  
      It is guaranteed that a call to reset() fully propogates up the inheritance tree.
    */
    virtual void    reset();

    /// Calculates and returns the momentum and error of the track 
    /*! This method calculates and returns in the two arrays provided as arguments the 
      3-momentum and error of the track in Star global coordinates. The 3-momentum 
      is calculated at the inner most point associated with the track. The inner-most 
      point may or may not be the main vertex of the event. Care should thus be exercised 
      while using this method. 

      The error is calculated (and returned) only if a non null array is passed as a second
      argument. It is thus possible to get the momentum without a lengthy calculation 
      of the error matrix. The error error matrix corresponds to a full covariance matrix.
      The definition of the error matrix is described in the introduction of this class
      definition. Note that the actual calculation of the momentum and associated error 
      is delegated to the track node class and uses the inner most node of the track.
    */
    virtual void    getMomentum(double p[3], double e[6]) const ;

    /// Calculates and returns the transverse momentum of the track.
    /*!
      Calculates and returns the transverse momentum of the track at the inner most node 
      held by this track which may or (or not) be the primary vertex. 
    */
    virtual double  getPt()             const;
	
    /// Convenience method used to return the curvature of the track at its inner most point. 
    virtual double  getCurvature()      const; 

    /// Convenience method used to calculate and return the rapidity of the particle
    /*!
      Calculates and returns the rapidity of the track based on the inner most node associated 
      with the track. If a non zero mass is set for this track, the rapidity returned is the
      true particle rapidity; otherwise the pseudorapidity is calculated and returned. 

      A value of -999 is returned if "E-Pz" is less or equal to zero.
    */
    virtual double  getRapidity()       const;

    /// Convenience method used to calculate and return the pseu-do rapidity of the track.
    /*!
      Calculates and returns the pseudo-rapidity of the track based on the inner most node associated 
      with the track. 
    */
    virtual double  getPseudoRapidity() const;

    /// Convenience method used to calculate and return the azimuthal angle of the track.
    /*!	Calculates and returns the azimuthal angle of the track momemtum vector based on the 
      inner most node associated with the track. 
    */		
    virtual double  getPhi()            const;

    /// Convenience method used to calculate and return the pitch angle of the track.
    /*!	Calculates and returns the pitch angle of the track momemtum vector based on the 
      inner most node associated with the track. 
    */		
    virtual double  getTanL()           const;                      // tan(lambda)

    /// Implementation returns 0
    virtual double  getDca(StiHit *h=0)    const;

    /// Implementation returns 0
    virtual double  getDca2(StiTrack *t)   const;

    /// Implementation returns 0
    virtual double  getDca3(StiTrack *t)   const;

    /// Accessor method returns the mass set for this track.
    virtual double  getMass() const;

    /// Accessor method returns the charge of this track.
    /*! This method should only be called after the track is fully reconstructed given that
      the charge is evaluated (as positive or negative) only after the reconstruction is 
      completed.
    */
    virtual int     getCharge() const;

    /// Accessor method returns the chi2 of this track.
    /*! This accessor method returns the chi2 of the track based on the inner most node
      associated with the track. Can be called at any time during or after the reconstruction 
      of a track.
    */
    virtual double  getChi2() const;  
	
    /*! Accessor method returns the number of nodes containing a hit and used in the
      determination of the chi2 and track parameters.
    */
    virtual int     getFitPointCount() const;

    /*! Accessor method returns the number of nodes containing a hit. 
     */
    virtual int     getPointCount()      const;  

    /// Accessor method to set the charge of this track.
    virtual void  setCharge(int v);

    /// Accessor method to set the chi2 of this track.
    virtual void  setChi2(double v);        

  // Methods of this class
  
  /// Accessor method returns the outer most node associated with the track.
  StiKalmanTrackNode * getOuterMostNode()  const;
  /// Accessor method returns the inner most node associated with the track.
  StiKalmanTrackNode * getInnerMostNode()   const;
  /// Accessor method returns the first node associated with the track.
  StiKalmanTrackNode * getRootNode()  const { return rootNode; };
  /// Accessor method returns the first node, e.g. rootNode associated with the track.
  StiKalmanTrackNode * getFirstNode()  const { return rootNode; };
  /// Accessor method returns the last node associated with the track.
  // Assumes the track has been pruned.
  StiKalmanTrackNode * getLastNode()   const;

  /// Returns the direction (kInsideOut, kOutsideIn) used in the reconstruction, or in the most 
  /// fit to the track.
  StiTrackingDirection getTrackingDirection() const { return  trackingDirection;};

  /// Sets the direction (kInsideOut, kOutsideIn) used in the reconstruction, or in the most 
  /// fit to the track.
  void setTrackingDirection(StiTrackingDirection direction) { trackingDirection = direction;}
  
  /// Method used to add a hit to this track
  /*! Method used to add an arbitrary hit to a track. A track node
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
  /*!
    Current implementation only considers the first child of each node
    and must therefore be revised.
  */
  StiKalmanTrackNode * findHit(StiHit * h);
  
  /// Convenience method to initialize a track based on seed information 
  /*! Method to initialize this track based on given arguments. 
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
  
  virtual StThreeVector<double> getMomentumAtOrigin() const;
  virtual StThreeVector<double> getMomentumNear(double x);
  virtual StThreeVector<double> getHitPositionNear(double x) const;
  
  // Function to reverse the node geometry of a track
  void swap();

protected:
    
  static StiObjectFactoryInterface<StiKalmanTrackNode> * trackNodeFactory;
  
  StiTrackingDirection trackingDirection;
  StiKalmanTrackNode * rootNode;
};

#endif


//StiDedxCalculator
// C.Pruneau

/*! \class StiDedxCalculator
  StiDedxCalculator is a simple utility class that, given an StiTrack,
  calculates the ionization (dedx) via a truncated mean measurement.  The
  truncated mean method uses the following algorithm:\n
  1) The ionization samples (hits) are retrieved from the track. \n
  2) For each hit the charge (de) must be associated with the pathlength
  (dx) of the particle in the active volume of the detector.  From this
  association we define de/dx_i, where i represents an index corresponding
  to a given hit. \n
  3) The hits (dedx_i) are placed in a vector and sorted in ascending
  order. \n
  4) A fraction 'f' of the hits are kept, and 1-f is truncated from the
  vector. \n
  5) Of the remaining samples, we calculate the mean value <dedx>.\n

  \author C. Pruneau
  \author M.L. Miller (Yale Software)
  \author B. Norman (Kent State)

 */
/*! \example StiDedxCalculator_ex.cxx */

#ifndef StiDedxCalculator_H
#define StiDedxCalculator_H 1

#include <vector>
using std::vector;
#include <math.h>

#include "StDetectorId.h"

class StiTrackNode;
class StiKalmanTrackNode;
class StiKalmanTrack;
class StiTrack;

class StiDedxCalculator 
{
 public:

    ///For internal convenience.
    typedef vector<StiKalmanTrackNode*> StiKalmanTrackNodeVec;
    ///For internal convenience.
    typedef vector<double> DoubleVec;
    
    ///Default Constructor.
    StiDedxCalculator();
    
    ///Default Destructor.
    virtual ~StiDedxCalculator(){};

    //Sets

    ///Set the truncation fraction.
    void setFractionUsed(double);
    void setDetectorFilter(StDetectorId detector);

    //Action
    
    ///Calculate Dedx for the track.
    virtual void getDedx(const StiKalmanTrack* track,
			  double &dEdx, double &dEdxE,
			  double &nPointsUsed);
    //return some info
    StDetectorId whichDetId();
    double       whatUseFraction();

 private:

    double mFraction;
    StDetectorId mDetector;
    DoubleVec mVector;
};

// helper struct
/* \struct NodeDedxCalculator
   NodeDedxCalculator is a simple functor that takes a StiHit pointer
   and returns a value that corresponds to the de/dx in KeV/cm for that
   hit.  To change the behavior of the calculation, simply modify the
   operator() method.

   \author M.L. Miller (Yale Software)
 */
struct NodeDedxCalculator
{
    ///Perform the dedx calculation for a given TrackNode.
    double operator()(const StiKalmanTrackNode*);
};


//inlines

/*! The truncation fraction is a number between 0. and 1.  If a track
  has n samples associated with it (n hits) then the truncation fraction
  represents the fraction of n <b>that is used</b> to calculate the
  ionization.  For example, if a track has n=10 hits, then setting
  the truncation fraction to .7 corresoponds to using 7 measurements with
  the lowest dedx per hit.
 */
inline void StiDedxCalculator::setFractionUsed(double val)
{
    mFraction = val;
}
inline void StiDedxCalculator::setDetectorFilter(StDetectorId detector)
{
    mDetector = detector;
}


inline StDetectorId StiDedxCalculator::whichDetId(){return mDetector;};
inline double       StiDedxCalculator::whatUseFraction(){return mFraction;};


#endif

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

#ifndef StiTrack_H
#define StiTrack_H 1

//std
#include <math.h>
#include <vector>
using namespace std;

//SCL
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"

//Sti
#include "StiTrackFinder.h"
#include "StiTrackFitter.h"
#include "Filter.h"

class StiHit;
class StHit;
class StMeasuredPoint;
class StiTrackFinder;
class StiTrackFitter;
class StiTrack;

/** 
    \enum Direction
    \brief Definition of directions used in track finding and fitting.
    This enumeration defines the Outside-In and Inside-Out directions 
    used in track finding and fitting.
*/
enum StiDirection {kOutsideIn=0, kInsideOut};

typedef Filter<StiTrack> StiTrackFilter;


/*! 
   \class StiTrack
   \brief  Abstract definition of a Track
   
   Abstract definition of a track used in the Sti package.
   <p>
   \author Claude A Pruneau (Wayne State University)
*/
class StiTrack 
{
public:

  enum StiTrackProperty {kCharge=0,
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
			 kisPrimary,
			 kTpcDedx,
			 kSvtDedx,
			 kCurvature,
			 kP,
			 kPt,
			 kRapidity,
			 kPseudoRapidity,
			 kPhi,
			 kTanL  };
  static void setTrackFinder(StiTrackFinder * finder);
  static void setTrackFitter(StiTrackFitter * fitter);
  static StiTrackFinder * getTrackFinder();
  static StiTrackFitter * getTrackFitter();

  StiTrack()    
		{ /* nops */  }
  virtual ~StiTrack()  { /* nops */  }
  virtual void fit(int direction=kOutsideIn); 
  virtual bool find(int direction=kOutsideIn);
  virtual void reset()=0;
  virtual void getMomentum(double p[3], double e[6]) const =0;
  virtual StThreeVector<double> getMomentumAtOrigin() const =0; //3-momentum at first point
  virtual StThreeVector<double> getMomentumNear(double x) =0; //3-momentum at arb. point
  virtual StThreeVector<double> getHitPositionNear(double x) const =0; //3-position at arb. point
  virtual double  getCurvature()      const=0;   // transverse curvature
  virtual double  getP()              const=0;   // transverse momentum
  virtual double  getPt()             const=0;   // transverse momentum
  virtual double  getRapidity()       const=0;   // rapidity
  virtual double  getPseudoRapidity() const=0;   // pseudo rapidity
  virtual double  getPhi()            const=0;   // azimuthal angle
  virtual double  getTanL()           const=0;   // tan(lambda)
  virtual double  getDca(StiHit *h=0)  const=0;   // distance of closest approach to given point/hit
  virtual double  getDca2(StiTrack *t) const=0;   // distance of closest approach to given track - 2D calc
  virtual double  getDca3(StiTrack *t) const=0;   // distance of closest approach to given track - 3D calc
  virtual int     getPointCount() const=0;
  virtual int     getFitPointCount() const=0; 
  virtual int     getGapCount() const=0;
  virtual int     getMaxPointCount() const=0;
  /// number of hits used to seed the track
  virtual int     getSeedHitCount() const =0;
  virtual void    setSeedHitCount(int c)=0;
  virtual double  getTrackLength() const=0;
  virtual vector<StMeasuredPoint*> stHits() const=0;
  /// Get mass of the particle that produced this track
  virtual double  getMass() const=0;
  /// Get charge of the particle that produced this track
  virtual int     getCharge() const=0;
  /// Get chi2 of this track
  virtual double  getChi2() const=0;
  virtual void    setFlag(long v)=0;
  virtual long    getFlag() const=0;

  // Convenience Accessor using a switch
  virtual double  getValue(int key) const;

 protected:
  static StiTrackFinder * trackFinder;
  static StiTrackFitter * trackFitter;
  
  friend ostream& operator<<(ostream& os, const StiTrack& track);
};


#endif

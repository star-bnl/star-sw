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

//sti
#include "StiTrackFitter.h"

class StiHit;
class StHit;
class StMeasuredPoint;

/*! 
   \class StiKalmanTrack
   \brief  Abstract definition of a Track
   
   Abstract definition of a track used in the Sti package.
	 <p>
	 \author Claude A Pruneau (Wayne State University)
*/
class StiTrack 
{
public:

	//enum StiTrackType {kGlobal=0, kPrimary};
  // static methods

  static void setTrackFitter(StiTrackFitter * fitter);
  static StiTrackFitter * getTrackFitter();

  // constructor/destructor/copy/etc
  
  StiTrack()
		{
			reset();
		};

  virtual ~StiTrack()
		{
		}
  
  // action methods
  
  virtual void reset();
	virtual void fit(); 
	
	//This is for full state (3 mom + error matrix) 
	virtual void    getMomentum(double p[3], double e[6]) const =0;
	
	//Simple gets
	virtual StThreeVector<double> getMomentumAtOrigin() const =0; //3-momentum at first point
	virtual StThreeVector<double> getMomentumNear(double x) =0; //3-momentum at arb. point
	virtual StThreeVector<double> getHitPositionNear(double x) const =0; //3-position at arb. point
	
	virtual double  getPt()             const                   =0;   // transverse momentum
	virtual double  getRapidity()       const                   =0;   // rapidity
	virtual double  getPseudoRapidity() const                   =0;   // pseudo rapidity
	virtual double  getPhi()            const                   =0;   // azimuthal angle
	virtual double  getTanL()           const                   =0;   // tan(lambda)
	virtual double  getDca(StiHit *h=0)  const=0;   // distance of closest approach to given point/hit
	virtual double  getDca2(StiTrack *t) const=0;   // distance of closest approach to given track - 2D calc
	virtual double  getDca3(StiTrack *t) const=0;   // distance of closest approach to given track - 3D calc
	
	virtual int getPointCount() const=0;
	virtual int getFitPointCount() const=0; 
	virtual int getGapCount() const=0;
	virtual double getTrackLength() const=0;
	virtual int getMaxPointCount() const=0;

	
	/// number of hits used to seed the track
	int getSeedHitCount() const {return mSeedHitCount;}
	
	//StiHit * getVertex() const;  // return pointer to vertex associated with this track if any. 

	//virtual vector<StHit*> stHits() const=0;
	virtual vector<StMeasuredPoint*> stHits() const=0;
	
  // accessor methods
  
	/// Get mass of the particle that produced this track
	virtual double  getMass() const
		{ 
			return m;
		}
	
	/// Get charge of the particle that produced this track
	virtual int     getCharge() const=0;
	
	/// Get chi2 of this track
	virtual double  getChi2() const=0;
	
	/// Set charge  of the particle that produced this track
	//void  setCharge(int v)
	//	{
	//		q = v;
	//	}
	
	/// Set chi2 of the track
	//void  setChi2(double v)
	//	{
	//chi2 = v;
	//	}
	
	//void  setVertex(StiHit *v);
	void setSeedHitCount(int c) {mSeedHitCount=c;}
	
	//Flag:
	void setFlag(long v) {mFlag = v;}
	long getFlag() const {return mFlag;}

 protected:
	static StiTrackFitter * trackFitter;
	
	friend ostream& operator<<(ostream& os, const StiTrack& track);
	
	//int q;          // charge of the track 
	//int nPts;       // number of points on the track
	//int nFitPts;    // number of points included in the fit of the track
	int mSeedHitCount; //number of points used to seed the track
	long mFlag; //A flag to pack w/ topo info
	//StiHit * vertex; // parent vertex of this track
	double  m;          // mass hypothesis
	//double  chi2;
};

/*
inline StiHit * StiTrack::getVertex() const
{
  return vertex;
}

inline void  StiTrack::setVertex(StiHit *v)
{
  vertex = v;
}
*/

inline void StiTrack::fit()
{
  trackFitter->fit(this);
}

#endif

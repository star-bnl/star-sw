#ifndef StiMcTrack_H
#define StiMcTrack_H 1
#include "StiTrack.h"

class StMcTrack;

class StiMcTrack : public StiTrack 
{
 public:  StiMcTrack();
  virtual ~StiMcTrack();
  virtual void fit(int direction=kOutsideIn); 
  virtual bool find(int direction=kOutsideIn);
  virtual void reset();
  virtual void getMomentum(double p[3], double e[6]) const ;
  virtual StThreeVector<double> getMomentumAtOrigin() const ; //3-momentum at first point
  virtual StThreeVector<double> getMomentumNear(double x) ; //3-momentum at arb. point
  virtual StThreeVector<double> getHitPositionNear(double x) const ; //3-position at arb. point
  virtual double  getCurvature()      const;
  virtual double  getP()              const;
  virtual double  getPt()             const;   // transverse momentum
  virtual double  getRapidity()       const;   // rapidity
  virtual double  getPseudoRapidity() const;   // pseudo rapidity
  virtual double  getPhi()            const;   // azimuthal angle
  virtual double  getTanL()           const;   // tan(lambda)
  virtual double  getDca(StiHit *h)  const;   // distance of closest approach to given point/hit
  virtual double  getDca2(StiTrack *t) const;   // distance of closest approach to given track - 2D calc
  virtual double  getDca3(StiTrack *t) const;   // distance of closest approach to given track - 3D calc
  virtual int     getPointCount() const;
  virtual int     getFitPointCount() const; 
  virtual int     getGapCount() const;
  virtual int     getMaxPointCount() const;
  /// number of hits used to seed the track
  virtual int     getSeedHitCount() const ;
  virtual void    setSeedHitCount(int c);
  virtual double  getTrackLength() const;
  virtual vector<StMeasuredPoint*> stHits() const;
  /// Get mass of the particle that produced this track
  virtual double  getMass() const;
  /// Get charge of the particle that produced this track
  virtual int     getCharge() const;
  /// Get chi2 of this track
  virtual double  getChi2() const;
  virtual void    setFlag(long v);
  virtual long    getFlag() const;
  void setStMcTrack(const StMcTrack * track);
  const StMcTrack * getStMcTrack(const StMcTrack * track) const;
	bool extendToVertex(StiHit* vertex);
 protected:

  const StMcTrack * mcTrack;
};

inline bool StiMcTrack::extendToVertex(StiHit* vertex)
{
	// no ops 
	return false;
}


/// Set the given StMcTrack as the track this wrapper points to
inline  void StiMcTrack::setStMcTrack(const StMcTrack * track)
{
  mcTrack =  track;
}

inline const StMcTrack * StiMcTrack::getStMcTrack(const StMcTrack * track) const
{
  return mcTrack;
}

#endif

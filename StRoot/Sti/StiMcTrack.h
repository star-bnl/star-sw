#ifndef StiMcTrack_H
#define StiMcTrack_H 1
#include "StiTrack.h"

class StMcTrack;
 
class StiMcTrack : public StiTrack
{
 public:  StiMcTrack();
  virtual ~StiMcTrack();
          int fit(int direction=kOutsideIn); 
          bool find(int direction=kOutsideIn);
          void reset();
          void unset(){;}
          void getMomentum(double p[3], double e[6]) const ;
          StThreeVector<double> getMomentumAtOrigin() const ; //3-momentum at first point
          StThreeVector<double> getMomentumNear(double x) ; //3-momentum at arb. point
          StThreeVector<double> getHitPositionNear(double x) const ; //3-position at arb. point
          double  getCurvature()      const;
          double  getP()              const;
          double  getPt()             const;   // transverse momentum
          double  getRapidity()       const;   // rapidity
          double  getPseudoRapidity() const;   // pseudo rapidity
          double  getPhi()            const;   // azimuthal angle
          double  getTanL()           const;   // tan(lambda)
          double  getDca()  const;             // distance of closest approach to given point/hit
          double  getDca2(StiTrack *t) const;   // distance of closest approach to given track - 2D calc
          double  getDca3(StiTrack *t) const;   // distance of closest approach to given track - 3D calc
          double  getDca(const StiHit*)const {return 0;} //WarnOff
          int     getPointCount() const;
          int     getFitPointCount(int detectorId=0) const; 
          int     getGapCount() const;
          int     getMaxPointCount(int detectorId=0) const;
  /// number of hits used to seed the track
          int     getSeedHitCount() const ;
          void    setSeedHitCount(int c);
          double  getTrackLength() const;
          vector<const StMeasuredPoint*> stHits() const;
  /// Get mass of the particle that produced this track
          double  getMass() const;
  /// Get charge of the particle that produced this track
          int     getCharge() const;
  /// Get chi2 of this track
          double  getChi2() const;
          void    setFlag(long v);
          long    getFlag() const;
          void    setStMcTrack(const StMcTrack * track);
  const StMcTrack * getStMcTrack(const StMcTrack * track) const;
  StiTrackNode *extendToVertex(StiHit* vertex){return 0;}
  vector<StiHit*> getHits();
  void addHit(StiHit *hit);
  bool isPrimary() const;
  int  refit()	{return 0;}
  int  refitL()	{return 0;}

 protected:
  const StMcTrack * mcTrack;
  vector<StiHit*> _hits;
};

inline void  StiMcTrack::addHit(StiHit*hit)
{
  _hits.push_back(hit);
}

inline vector<StiHit*> StiMcTrack::getHits()
{
  return _hits;
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


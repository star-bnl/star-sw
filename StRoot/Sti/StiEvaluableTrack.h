//StiEvaluableTrack.h
//M.L. Miller (Yale Software)
//06/01

/*! \class StiEvaluableTrack
  StiEvaluableTrack is a class that is used to evaluate the tracker when run on simulated
  data.  StiEvaluableTrack is a StiKalmanTrack.  Additionally it encapsulates the relationship
  between the found StiKalmanTrack, the StMcTrack (monte-carlo) from which it came, and the
  StGlobalTrack (found by existing STAR tracking) that was deembed to the best match to the
  monte-carlo track.  The best match is decided using the existing STAR package
  StAssociationMaker, and the lower bound requirement for the match to be deemed successful
  are determined by StiEvaluableTrackSeedFinder.


  \author M.L. Miller (Yale Software)
 */

/*! \example StiEvaluableTrack_ex.cxx
 */

#ifndef StiEvaluableTrack_HH
#define StiEvaluableTrack_HH

#include "StiKalmanTrack.h"

class StTrackPairInfo;

class StiEvaluableTrack : public StiKalmanTrack
{
public:
    StiEvaluableTrack();
    virtual ~StiEvaluableTrack();

    ///Set StTrackPairInfo pointer member
    void setStTrackPairInfo(StTrackPairInfo*);

    ///Get a pointer to StTrackPairInfo object.
    /*! StTrackPairInfo represents the union of a monte-carlo track and a StGlobalTrac.
      This association has been made via StAssociationMaker.
    */    
    StTrackPairInfo* stTrackPairInfo() const;

    ///Reset the class members to default state.
    /*! This is used so that StiEvaluableTrack objects can be owned and served by
      Factory.  It is guarunteed that a call to reset() fully propogates
      up the inheritance tree.
    */    
    virtual void reset();
    
protected:
    ///A pointer to the union of a monte-carlo track and a StGlobalTrack.
    StTrackPairInfo* mPair;
    
private:
};

inline void StiEvaluableTrack::setStTrackPairInfo(StTrackPairInfo* val)
{
    mPair = val;
}

inline StTrackPairInfo* StiEvaluableTrack::stTrackPairInfo() const
{
    return mPair;
}


#endif

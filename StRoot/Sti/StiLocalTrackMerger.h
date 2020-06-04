//StiLocalTrackMerger.h
//M.L. Miller (Yale Software)
//12/01

/*! \class StiLocalTrackMerger
  StiLocalTrackMerger is the most naive implementation to merge split tracks.  It
  implements the algorithm previously used in the TPT module, identifying split
  tracks via successive one dimensional tests in the 5 helix dimensions.
  This comparison is based on the charge of the particle, the radius of
  curvature, the dip angle, and the center of the circle as projected onto the transverse
  (x-y) plane.  Additionally, a test is perfomed to calculate the DCA of the two tracks
  to a  common point.

  \author M.L. Miller (Yale Software)
*/

#ifndef StiLocalTrackMerger_HH
#define StiLocalTrackMerger_HH

#include "StiTrackMerger.h"
#include "StiKalmanTrack.h"
#include "StiKalmanTrackNode.h"

class StiTrackContainer;

class StiLocalTrackMerger : public StiTrackMerger
{
public:

    ///One must provide a valid pointer to the track container.
    StiLocalTrackMerger(StiTrackContainer*);
    
    virtual ~StiLocalTrackMerger();

    ///Set the search window in radius.
    void setDeltaR(double);
    
    ///Merge the tracks in the track container.
    virtual void mergeTracks();

protected:
    StiLocalTrackMerger(); //This is not implemented
    bool sameTrack(StiKalmanTrack* lhs, StiKalmanTrack* rhs);
    bool configureMaxTrack(StiKalmanTrack* lowerTrack);

private:
    StiKalmanTrack mMaxTrack;
    StiKalmanTrackNode mMaxTrackNode;
    
    double mDeltaR;
    
};

//inlines

inline void StiLocalTrackMerger::setDeltaR(double val)
{
    mDeltaR = val;
}
#endif

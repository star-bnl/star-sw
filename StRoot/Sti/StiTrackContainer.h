//StiTrackContainer.h
//M.L. Miller (Yale Software)
//05/01

/*!
  \class StiTrackContainer
  
  StiTrackContainer is meant to provide the interface between the StiTracker and the persistency model.
  That is, StiTrackContainer will be the only thing that StiTracker will have to know about and
  StiTrackContainer will interface to (most likely) StEvent, unless we decide to make StiTrackContainer
  persistent.

  \author M.L. Miller (Yale Software)
  \note StiTrackContainer is implemented as a singleton.
  
*/

#ifndef StiTrackContainer_HH
#define StiTrackContainer_HH

#include <map>
using namespace std;

class StiKalmanTrack;

///Define the Less-Than operator for track ordering in the track container.
struct StiKalmanTrackLessThan
{
    bool operator()(const StiKalmanTrack* lhs, const StiKalmanTrack* rhs) const;
};

typedef map<StiKalmanTrack*, StiKalmanTrack*, StiKalmanTrackLessThan> KalmanTrackMap;
typedef KalmanTrackMap::value_type KalmanTrackMapValType;

class StiTrackContainer : public KalmanTrackMap
{
public:
    
    friend class nobody;
    
    ///Singleton access
    static StiTrackContainer* instance();
    static void kill();
    
    ///Preserve simple interface to add tracks
    void push_back(StiKalmanTrack*);
    
protected:
    StiTrackContainer();
    virtual ~StiTrackContainer();
    
private:
    static StiTrackContainer* sinstance;
};

#endif

#ifndef StiKalmanTrackFinder_H
#define StiKalmanTrackFinder_H 1

#include "Sti/Base/SubjectObserver.h"
#include "Sti/StiTrackFinder.h"

class Messenger;
class StiDetector;
class StiDetectorBuilder;
class StiDetectorContainer;
class StiToolkit;
class StiSeedFinder;
class StiKalmanTrackFactory;
class StiHitContainer;
class StiTrackContainer;
class StiSeedFinder;
class StiTrack;
class StiMcTrack;
class StiKalmanTrack;
class StiKalmanTrackNode;
class StEvent;
class StMcEvent;
class StiStEventFiller;
class StiKalmanTrackFinderParameters;
template<class Factorized>class Factory;
template<class Event,class Detector>class StiHitLoader;

class StiKalmanTrackFinder : public StiTrackFinder, public Observer
{
public:
  StiKalmanTrackFinder(StiToolkit*toolkit);
  virtual ~StiKalmanTrackFinder();
  
  virtual void update();
  virtual void changed(Subject* changedSubject);

  /// Initialize the finder
  virtual void initialize();
  /// Load given event, and find all tracks of that event
  /// Optionally load the given (associated) monte carlo event
  virtual void findTracks(StEvent * event,StMcEvent * mcEvent); 
  /// Load given event and MC event
  virtual void loadEvent(StEvent * event,StMcEvent * mcEvent);
  /// Load hits from the given source
  virtual void loadHits(StEvent * event);
  /// Load tracks from the given source
  virtual void loadTracks(StEvent * event);
  /// Load MC tracks from the given source
  virtual void loadMcTracks(StMcEvent * mcEvent);
  /// Find all tracks of the currently loaded event
  virtual void findTracks(); 
  /// Find/extend the given track, in the given direction
  virtual bool find(StiTrack *track, int direction);
  /// Find the next track 
  virtual void findNextTrack(); 
  /// Find the next track segment
  virtual void findNextTrackSegment();
  /// Fit all tracks crruently loaded 
  virtual void fitTracks(); 
  /// Fit the next track available
  virtual void fitNextTrack();
  /// Extent all tracks to the given vertex
  virtual void extendTracksToVertex(StiHit* vertex);
  /// Reset the tracker
  virtual void reset();
  /// Clear the tracker
  virtual void clear();
  /// Get the number of number of track seed used by the seed finder
  virtual int getTrackSeedFoundCount() const;
  /// Get the number of track found
  virtual int getTrackFoundCount() const;
  /// Get the number of track found that satisfy the given filter
  virtual int getTrackFoundCount(Filter<StiTrack> * filter) const;
  /// Get the track filter currently used by the tracker
  virtual Filter<StiTrack> * getTrackFilter() const;

  /// Get the vertex finder used by this track finder
  virtual StiVertexFinder * getVertexFinder();
  /// Set the vertex finder used by this tracker
  virtual void setVertexFinder(StiVertexFinder *);

  /// Depracated
  virtual Filter<StiTrack> * getGuiTrackFilter() const;
  /// Depracated
  virtual Filter<StiTrack> * getGuiMcTrackFilter() const;

  /// Set Tracking Mode used for Interactive Tracking
  void setTrackingMode(StiFindStep m);
  /// Get Tracking Mode used for Interactive Tracking
  StiFindStep getTrackingMode() const;
  
  void setParameters(StiKalmanTrackFinderParameters *par);
  StiKalmanTrackFinderParameters * getParameters();
  
  void doInitLayer(int trackingDirection);
  void doNextDetector();
  void doFinishLayer();
  void doFinishTrackSearch();
  void doNextTrackStep();

  
  

protected:

    void getNewState();
    void printState();

    StiToolkit                  * _toolkit;
    Filter<StiTrack>            * _trackFilter;
    Filter<StiTrack>            * _guiTrackFilter;
    Filter<StiTrack>            * _guiMcTrackFilter;
    StiSeedFinder               * _trackSeedFinder;
    Factory<StiKalmanTrackNode> * _trackNodeFactory;
    Factory<StiKalmanTrack>     * _trackFactory;
    Factory<StiMcTrack>         * _mcTrackFactory;
    Factory<StiHit>             * _hitFactory;
    StiDetectorContainer        * _detectorContainer;
    StiHitLoader<StEvent,StiDetectorBuilder> * _hitLoader;
    StiHitContainer             * _hitContainer;
    StiTrackContainer           * _trackContainer;
    StiTrackContainer           * _mcTrackContainer;
    StiVertexFinder             * _vertexFinder;
    StiStEventFiller            * _eventFiller;
    StEvent                     * _event;
    StMcEvent                   * _mcEvent;
    StiKalmanTrackFinderParameters * _pars;
    Messenger                      & _messenger;

private:
    
    StiFindStep mode;
    int       state;
    int       visitedDet ;
    int       position;
    int       lastMove;
    int       nAdded;
    
    double    chi2;
    double    bestChi2;
    StiKalmanTrack         * track;
    StiKalmanTrackNode * sNode;
    StiKalmanTrackNode * tNode;
    StiKalmanTrackNode * bestNode;
    StiKalmanTrackNode * leadNode;
    StiHit * bestHit;
    StiHit * hit;
    const StiDetector * sDet;
    const StiDetector * tDet;
    const StiDetector * leadDet;
    bool trackDone;
    bool scanningDone;
    bool hasHit;
    bool hasDet;

};

inline Filter<StiTrack> * StiKalmanTrackFinder::getTrackFilter() const
{
  return _trackFilter;
}

inline Filter<StiTrack> * StiKalmanTrackFinder::getGuiTrackFilter() const
{
  return _guiTrackFilter;
}

inline Filter<StiTrack> * StiKalmanTrackFinder::getGuiMcTrackFilter() const
{
  return _guiMcTrackFilter;
}

inline void StiKalmanTrackFinder::setTrackingMode(StiFindStep m)
{
  mode = m;
}

inline StiFindStep StiKalmanTrackFinder::getTrackingMode() const 
{
  return mode;
}

#endif




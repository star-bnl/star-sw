///\File StiKalmanTrackFinder.h
///\Author Claude A Pruneau (Wayne State U) 
#ifndef StiKalmanTrackFinder_H_INCLUDED
#define StiKalmanTrackFinder_H_INCLUDED
#include "Sti/StiTrackFinder.h"
#include "Sti/StiKalmanTrackFinderParameters.h"
#include "Sti/Base/Named.h"
#include "Sti/Base/Described.h"
#include "Sti/Base/Loadable.h"

class StiDetector;
class StiDetectorBuilder;
class StiDetectorContainer;
class StiToolkit;
class StiTrackSeedFinder;
class StiKalmanTrackFactory;
class StiHitContainer;
class StiTrackContainer;
class StiTrack;
class StiMcTrack;
class StiKalmanTrack;
class StiKalmanTrackNode;
class StEvent;
class StMcEvent;
class StiStEventFiller;
template<class Factorized>class Factory;
template<class Event,class McEvent,class Detector>class StiHitLoader;


///\class StiKalmanTrackFinder  
///
///\author  Claude Pruneau, Wayne State University                        
///\date March 2001                                                    
///
///\note The Kalman Filter Code imbedded in this class was given
///to us gracioulsy by Jouri Belikov from the ALICE       
///collaboration. i.e. code reproduced with autorization. 
///
class StiKalmanTrackFinder : public Loadable, public StiTrackFinder, public Named, public Described
{
public:
  StiKalmanTrackFinder(StiToolkit*toolkit);
  virtual ~StiKalmanTrackFinder();
  /// Initialize the finder
  virtual void initialize();
  /// Set default tracking parameter values
  virtual void setDefaults();
  /// Find all tracks of the currently loaded event
  virtual void findTracks(); 
  /// Find/extend the given track, in the given direction
  virtual bool find(StiTrack *track, int direction);
  /// Find the next track 
  virtual void findNextTrack(); 
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
  /// Get the track filter currently used by the tracker
  virtual Filter<StiTrack> * getTrackFilter() const;
  /// Get the vertex finder used by this track finder
  virtual StiVertexFinder * getVertexFinder();
  /// Set the vertex finder used by this tracker
  virtual void setVertexFinder(StiVertexFinder *);
  void setParameters(const StiKalmanTrackFinderParameters &par);
  virtual EditableParameters & getParameters();
  void doInitLayer(int trackingDirection);
  void doNextDetector();
  void doFinishLayer();
  void doFinishTrackSearch();
  void doNextTrackStep();
  
  void load(const string & userFileName, StMaker & source)
    {
      Loadable::load(userFileName,source);
    }
  void loadDS(TDataSet&);
  void loadFS(ifstream&);
  
  StiKalmanTrackFinderParameters  _pars;
  
 protected:

    void printState();
    StiToolkit                  * _toolkit;
    Filter<StiTrack>            * _trackFilter;
    StiTrackSeedFinder          * _trackSeedFinder;
    Factory<StiKalmanTrackNode> * _trackNodeFactory;
    Factory<StiKalmanTrack>     * _trackFactory;
    Factory<StiMcTrack>         * _mcTrackFactory;
    Factory<StiHit>             * _hitFactory;
    StiDetectorContainer        * _detectorContainer;
    StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder> * _hitLoader;
    StiHitContainer             * _hitContainer;
    StiTrackContainer           * _trackContainer;
    StiTrackContainer           * _mcTrackContainer;
    StiVertexFinder             * _vertexFinder;
    StiStEventFiller            * _eventFiller;
    StEvent                     * _event;
    StMcEvent                   * _mcEvent;

private:
    
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

/*
inline void StiKalmanTrackFinder::setTrackingMode(StiFindStep m)
{
  mode = m;
}

inline StiFindStep StiKalmanTrackFinder::getTrackingMode() const 
{
  return mode;
}
*/

#endif




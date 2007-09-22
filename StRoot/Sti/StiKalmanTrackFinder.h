///\File StiKalmanTrackFinder.h
///\Author Claude A Pruneau (Wayne State U) 
#ifndef StiKalmanTrackFinder_H_INCLUDED
#define StiKalmanTrackFinder_H_INCLUDED
#include "Sti/StiTrackFinder.h"
#include "Sti/StiKalmanTrackFinderParameters.h"
#include "Sti/Base/Named.h"
#include "Sti/Base/Described.h"
#include "Sti/Base/Loadable.h"

class TStopwatch;
class StiDetector;
class StiDetectorBuilder;
class StiDetectorContainer;
class StiToolkit;
class StiKalmanTrackFactory;
class StiHitContainer;
class StiTrackContainer;
class StiTrack;
class StiKalmanTrack;
class StiKalmanTrackNode;
class StEvent;
class StiStEventFiller;
template<class Factorized>class Factory;
template<class Event,class Detector>class StiHitLoader;


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
  StiKalmanTrackFinder(StiToolkit *toolkit);
  virtual ~StiKalmanTrackFinder();
  /// Initialize the finder
  virtual void initialize();
  /// Set timing of tracking
          void setTiming();
  /// Set default tracking parameter values
  virtual void setDefaults();
  /// Find all tracks of the currently loaded event
  virtual void findTracks(); 
  /// Find/extend the given track, in the given direction
          bool find(StiTrack *track, int direction, double rmin=0);
  /// Find the next track 
  virtual StiTrack * findTrack(double rMin=0); 
  /// Extend seeds to tracks
  void extendSeeds (double rMin);
  void extendTracks(double rMin);
  /// Extend track
  int extendTrack(StiKalmanTrack *track,double rMin);
  /// Extent all tracks to the given vertex
          void extendTracksToVertex(StiHit* vertex);
          void extendTracksToVertices(const std::vector<StiHit*> &vertices);
  /// Reset the tracker
  virtual void reset();
  virtual void unset(){;}

  /// Clear the tracker
  virtual void clear();
  /// Finish the tracker
  virtual void finish() const;
  /// Get the track filter currently used by the tracker
  virtual Filter<StiTrack> * getTrackFilter();
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
  static void setDebug(int m = 0) {_debug = m;}
  static int  debug() {return _debug;}
private:
class QAFind;
  void find(StiKalmanTrack *track, int direction,StiKalmanTrackNode *node,QAFind &qa);
  void nodeQA(StiKalmanTrackNode *node, int position,int active,QAFind &qa);
  int  compQA(QAFind &qaBest,QAFind &qaTry,double maxChi2);
 
 protected:

    void printState();
    StiToolkit                  * _toolkit;
    Filter<StiTrack>            * _trackFilter;
    StiTrackFinder              * _trackSeedFinder;
    Factory<StiKalmanTrackNode> * _trackNodeFactory;
    Factory<StiKalmanTrack>     * _trackFactory;
    Factory<StiHit>             * _hitFactory;
    StiDetectorContainer        * _detectorContainer;
    StiHitLoader<StEvent,StiDetectorBuilder> * _hitLoader;
    StiHitContainer             * _hitContainer;
    StiTrackContainer           * _trackContainer;
    StiVertexFinder             * _vertexFinder;
    StiStEventFiller            * _eventFiller;
    StEvent                     * _event;

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
    TStopwatch *mTimg[3]; //seeds,traks,prims
    static int   _debug;
};

inline Filter<StiTrack> * StiKalmanTrackFinder::getTrackFilter() 
{
  return _trackFilter;
}


class CloserAngle
{
  public:
  CloserAngle(double refAngle);
  bool operator()(const StiDetector*lhs, const StiDetector* rhs);
 protected:
  double _refAngle;
};

#endif




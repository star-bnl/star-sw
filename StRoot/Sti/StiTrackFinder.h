#ifndef StiTrackFinder_H
#define StiTrackFinder_H 1

class StEvent;
class StMcEvent;
class StiHit;
class StiTrack;
class StiVertexFinder;
class EditableParameters;
template<class Filtered>class Filter;

enum StiFindStep {StepByLayer=1,StepByDetector=2 };

/*!
An abstract class defining  the interface to the track finder.
*/
class StiTrackFinder 
{
public:
  /// Initialize the finder
  virtual void initialize()=0;
  /// Load given event, and find all tracks of that event
  //virtual void findTracks(StEvent * event,StMcEvent * mcEvent)=0; 
  /// ReLoad current event
  //virtual void reloadEvent()=0;
  /// Load given event and MC event
  //virtual void loadEvent(StEvent * event,StMcEvent * mcEvent)=0;
  /// Load tracks from the given source
  //virtual void loadTracks(StEvent * event)=0;
  /// Load MC tracks from the given source
  //virtual void loadMcTracks(StMcEvent * mcEvent)=0;
  /// Load hits from the given source
  //virtual void loadHits(StEvent * event)=0;
  /// Find all tracks of the currently loaded event
  virtual void findTracks()=0; 
  /// Find/extend the given track, in the given direction
  virtual bool find(StiTrack *track, int direction) = 0;
  /// Find the next track 
  virtual void findNextTrack()=0;
  /// Find the next track segment
  virtual void findNextTrackSegment()=0;
  /// Fit all tracks crruently loaded 
  virtual void fitTracks()=0; 
  /// Fit the next track available
  virtual void fitNextTrack()=0;
  /// Extent all tracks to the given vertex
  virtual void extendTracksToVertex(StiHit* vertex)=0;
  /// Reset the tracker
  virtual void reset()=0;
  /// Reset the tracker
  virtual void clear()=0;
  /// Get the number of number of track seed used by the seed finder
  virtual int getTrackSeedFoundCount() const=0;
  /// Get the number of track found
  virtual int getTrackFoundCount() const=0;
  /// Get the number of track found that satisfy the given filter
  virtual int getTrackFoundCount(Filter<StiTrack> * filter) const=0;
  /// Get the track filter currently used by the tracker
  virtual Filter<StiTrack> * getTrackFilter() const = 0;

  /// Get the vertex finder used by this track finder
  virtual StiVertexFinder * getVertexFinder()=0;
  /// Set the vertex finder used by this tracker
  virtual void setVertexFinder(StiVertexFinder *)=0;

  /// Depracated
  virtual Filter<StiTrack> * getGuiTrackFilter() const = 0;
  /// Depracated
  virtual Filter<StiTrack> * getGuiMcTrackFilter() const = 0;

  /// Set Tracking Mode used for Interactive Tracking
  virtual void setTrackingMode(StiFindStep m)=0;
  /// Get Tracking Mode used for Interactive Tracking
  virtual StiFindStep getTrackingMode() const = 0;

  virtual EditableParameters & getParameters()=0;
};


#endif

#ifndef StiTrackFinder_H
#define StiTrackFinder_H 1
class StiHit;
class StiTrack;
class StiVertexFinder;
class EditableParameters;
template<class Filtered>class Filter;

/// An abstract class defining  the interface to the track finder.
class StiTrackFinder 
{
public:
  /// Initialize the finder
  virtual void initialize()=0;
  /// Find all tracks of the currently loaded event
  virtual void findTracks()=0; 
  /// Find/extend the given track, in the given direction
  virtual bool find(StiTrack *track, int direction) = 0;
  /// Find the next track 
  virtual void findNextTrack()=0;
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
  /// Get the track filter currently used by the tracker
  virtual Filter<StiTrack> * getTrackFilter() const = 0;
  /// Get the vertex finder used by this track finder
  virtual StiVertexFinder * getVertexFinder()=0;
  /// Set the vertex finder used by this tracker
  virtual void setVertexFinder(StiVertexFinder *)=0;
  virtual EditableParameters & getParameters()=0;
};

#endif

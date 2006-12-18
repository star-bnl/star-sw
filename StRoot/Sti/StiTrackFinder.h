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
   StiTrackFinder(){mComb=1;}
  /// Initialize the finder
  virtual void initialize()=0;
  /// Find all tracks of the currently loaded event
  virtual void findTracks()=0; 
  /// Find/extend the given track, in the given direction
  virtual bool find(StiTrack *track, int direction, double rmin=0) = 0;
  /// Find the next track 
  virtual StiTrack * findTrack(double rMin=0)=0;
  /// Extent all tracks to the given vertex
  virtual void extendTracksToVertex(StiHit* vertex)=0;
  virtual void extendTracksToVertices(const std::vector<StiHit*> &vertices){};
  /// Reset the tracker
  virtual void reset()=0;
  virtual void unset(){;}
  /// Reset the tracker
  virtual void clear()=0;
  /// Get the track filter currently used by the tracker
  virtual Filter<StiTrack> * getTrackFilter()= 0;
  /// Get the vertex finder used by this track finder
  virtual StiVertexFinder * getVertexFinder()=0;
  /// Set the vertex finder used by this tracker
  virtual void setVertexFinder(StiVertexFinder *)=0;
  virtual EditableParameters & getParameters()=0;
  
  void setComb(int comb=1)		{mComb = comb;}
  int  useComb() const			{return mComb;}
  

protected:
  int mComb; //0=no combinatoric , no tree search


};

#endif

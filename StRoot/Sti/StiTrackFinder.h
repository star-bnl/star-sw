#ifndef StiTrackFinder_H
#define StiTrackFinder_H 1
#include <vector>
class StiHit;
class StiTrack;
class EditableParameters;
template<class Filtered>class Filter;

/// An abstract class defining  the interface to the track finder.
class StiTrackFinder 
{
public:
  StiTrackFinder(){mComb=7;}
  virtual ~StiTrackFinder(){ /* nada*/ }
  /// Initialize the finder
  virtual void initialize()=0;
  /// Find all tracks of the currently loaded event
  virtual void findTracks()=0; 
  /// Find/extend the given track, in the given direction
  virtual bool find(StiTrack *track, int direction, double rmin=0) = 0;
  /// Find the next track 
  virtual StiTrack * findTrack(double rMin=0)=0;
  virtual void extendTracksToVertices(const std::vector<StiHit*> &vertices){};
  /// Reset the tracker
  virtual void reset()=0;
  virtual void unset(){;}
  /// Reset the tracker
  virtual void clear()=0;
  /// Get the track filter currently used by the tracker
  virtual Filter<StiTrack> * getTrackFilter()= 0;
  /// Set the vertex finder used by this tracker
  void setComb(int comb=7)		{mComb = comb;}
  int  useComb() const			{return mComb;}
  virtual void FeedBack(int badGood){;};
  

protected:
  int mComb; //=silicon+4*tpc
             // silicon/tpc 0=no combinatoric , no tree search
             //             1=combinatoric , only hits count
             //             2=combinatoric , no hits also counts


};

#endif

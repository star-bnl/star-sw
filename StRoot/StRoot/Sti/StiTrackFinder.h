#ifndef StiTrackFinder_H
#define StiTrackFinder_H 1
#include <vector>
#include "Sti/Base/Named.h"
class StiHit;
class StiTrack;
class EditableParameters;
template<class Filtered>class Filter;

/// An abstract class defining  the interface to the track finder.
class StiTrackFinder: public Named
{
public:
  StiTrackFinder(){mComb=7;}
  virtual ~StiTrackFinder(){ /* nada*/ }
  /// Initialize the finder
  virtual void initialize(){;};
  /// Trigger class about new event
  virtual void startEvent(){;}; 
  /// Find all tracks of the currently loaded event
  virtual void findTracks(){;}; 
  /// Find/extend the given track, in the given direction
  virtual bool find(StiTrack *track, int direction, double rmin=0){return 0;}
  /// Find the next track 
  virtual StiTrack * findTrack(double){return 0;};
  virtual void extendTracksToVertices(const std::vector<StiHit*> &vertices){;};
  /// Reset the tracker
  virtual void reset(){;};
  virtual void unset(){;}
  /// Reset the tracker
  virtual void clear(){;};
  /// Get the track filter currently used by the tracker
  virtual Filter<StiTrack> * getTrackFilter(){return 0;};
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

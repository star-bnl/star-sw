#ifndef StiTrackFinder_H
#define StiTrackFinder_H 1

class StiHit;
class StiTrack;
class StiTrackFilter;

/*!
A purely abstract class defining  the interface to a track finder.
*/
class StiTrackFinder 
{
public:
  
  //_c-tor/d-tor__________________________________________________
  //StiTrackFinder();
  //virtual ~StiTrackFinder();
  
  //_action methods_______________________________________________
  virtual void findTracks()=0; 
  virtual void fitTracks()=0; 
  virtual void extendTracksToVertex(StiHit* vertex)=0;
  virtual void findNextTrack()=0;
  virtual void fitNextTrack()=0;
  virtual void reset()=0;
  virtual bool isValid(bool debug=false) const = 0;
  virtual int getTrackSeedFoundCount() const=0;
  virtual int getTrackFoundCount() const=0;
  virtual int getTrackFoundCount(StiTrackFilter * filter) const=0;
  virtual bool find(StiTrack *track, int direction) = 0;
  
  virtual StiTrackFilter * getTrackFilter() const = 0;
  virtual StiTrackFilter * getGuiTrackFilter() const = 0;
  virtual StiTrackFilter * getGuiMcTrackFilter() const = 0;

protected:

};


#endif

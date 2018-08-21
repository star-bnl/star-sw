/*!
  \class StxKalmanTrackContainer
  
  StxKalmanTrackContainer is a container of track based on the STL vector class.
  <p>
  StxKalmanTrackContainer is polymorphic and can hold all forms of StxKalmanTrack objects. That includes in
  particular StxKalmanTrack.

  \author M.L. Miller (Yale Software)
  \author C.A. Pruneau(Wayne State University)
*/

#ifndef StxKalmanTrackContainer_HH
#define StxKalmanTrackContainer_HH
#include "TNamed.h"
#include <vector>
using namespace std;

class StxKalmanTrack;
//________________________________________________________________________________
///Define the Less-Than operator for track ordering in the track container.
struct StxKalmanTrackLessThan {
  bool operator()(const StxKalmanTrack* lhs, const StxKalmanTrack* rhs) const;
};
//________________________________________________________________________________
class StxKalmanTrackContainer : public vector<StxKalmanTrack*>, public TNamed {
 public:
  
  StxKalmanTrackContainer(const Char_t * name, const Char_t * description);
  virtual ~StxKalmanTrackContainer();  
  void Add(StxKalmanTrack * track)  {push_back(track);}
  virtual void Clear(const Option_t* opt="") { clear();}
  void sort();
  ClassDef(StxKalmanTrackContainer,0)
};

#endif

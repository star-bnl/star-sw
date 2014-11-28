/*!
  \class StiKalmanTrackContainer
  
  StiKalmanTrackContainer is a container of track based on the STL vector class.
  <p>
  StiKalmanTrackContainer is polymorphic and can hold all forms of StiKalmanTrack objects. That includes in
  particular StiKalmanTrack.

  \author M.L. Miller (Yale Software)
  \author C.A. Pruneau(Wayne State University)
*/

#ifndef StiKalmanTrackContainer_HH
#define StiKalmanTrackContainer_HH
#include "TNamed.h"
#include <vector>
using namespace std;

class StiKalmanTrack;
//________________________________________________________________________________
///Define the Less-Than operator for track ordering in the track container.
struct StiKalmanTrackLessThan {
  bool operator()(const StiKalmanTrack* lhs, const StiKalmanTrack* rhs) const;
};
//________________________________________________________________________________
class StiKalmanTrackContainer : public vector<StiKalmanTrack*>, public TNamed {
 public:
  
  StiKalmanTrackContainer(const Char_t * name, const Char_t * description);
  virtual ~StiKalmanTrackContainer();  
  void Add(StiKalmanTrack * track)  {push_back(track);}
  virtual void Clear(const Option_t* opt="") { clear();}
  void sort();
  ClassDef(StiKalmanTrackContainer,0)
};

#endif

#ifndef StiSeedFinder_HH
#define StiSeedFinder_HH
#include <stdexcept>
#include <string>
using std::string;

#include "Sti/Base/Named.h"

class StiKalmanTrack;
class StTrack;
class Messenger;
class StiHitContainer;
class StiDetectorContainer;
template<class Factorized>class Factory;

/*!Abstract base defining a mechanism to find track seeds.
<p>
The seed finders shall be given a unique name to identify them.
<p>
The seed finders require valid pointers to a track factory, a hit container,
and a detector container. Pointers to such three objects must be passed
to the class constructor. An exception is thrown if any of the three 
pointers are null.
<p>
\author M.L.Miller, Yale, 03/01
*/
class StiSeedFinder : public Named
{
public:
  StiSeedFinder(const string& name,
		Factory<StiKalmanTrack> * trackFactory,
		StiHitContainer         * hitContainer,
		StiDetectorContainer    * detectorContainer);
  virtual ~StiSeedFinder();
  
  //Inherited interface
  virtual bool hasMore() = 0;
  virtual StiKalmanTrack* next() = 0;
  virtual void reset() =0;
  
  ///Set factory
  void setFactory(Factory<StiKalmanTrack>* val);
  ///Set hit container
  void setHitContainer(StiHitContainer*);
  StiHitContainer* getHitContainer();
  
 protected:
  Factory<StiKalmanTrack>* _trackFactory;
  StiHitContainer* _hitContainer;
  StiDetectorContainer* _detectorContainer;
  Messenger &  _messenger;
  
private:
  StiSeedFinder(); //Not implemented
};

//inlines

inline void StiSeedFinder::setFactory(Factory<StiKalmanTrack>* val)
{
  _trackFactory=val;
}

inline void StiSeedFinder::setHitContainer(StiHitContainer* val)
{
  _hitContainer=val;
}

inline StiHitContainer* StiSeedFinder::getHitContainer()
{
  return _hitContainer;
}

#endif


#ifndef StiDetectorGroup_H_INCLUDED
#define StiDetectorGroup_H_INCLUDED

#include <stdexcept>

#include "Sti/StiGenericDetectorGroup.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiToolkit.h"
template<class Event,class Detector> class StiHitLoader;

template<class Event>
class StiDetectorGroup : public StiGenericDetectorGroup
{
  public:

  /// Get a hit loader appropriate for this detector group
  virtual StiHitLoader<Event,StiDetectorBuilder> * getHitLoader();
  StiHitLoader<Event,StiDetectorBuilder> *hitLoader() {return _hitLoader;}

 protected:

  StiDetectorGroup(const string & name);
  StiDetectorGroup(const string & name,
		   StiHitLoader<Event,StiDetectorBuilder> * hitLoader,
		   StiDetectorBuilder * detectorBuilder);
  ~StiDetectorGroup();
  StiHitLoader<Event,StiDetectorBuilder> * _hitLoader;
};


template<class Event>
StiDetectorGroup<Event>::StiDetectorGroup(const string & name,
					  StiHitLoader<Event,StiDetectorBuilder> * hitLoader,
					  StiDetectorBuilder * detectorBuilder)
  :  StiGenericDetectorGroup(name,detectorBuilder),
    _hitLoader(hitLoader)
{
  // If a loader was specified, make sure it uses the selected detector builder.
  if (_hitLoader)
    {
      _hitLoader->setDetector(detectorBuilder);
    }
  initialize();
}

template<class Event>
StiDetectorGroup<Event>::StiDetectorGroup(const string & name)
  :  StiGenericDetectorGroup(name),
     _hitLoader(0)
{
  initialize();
}

template<class Event>
StiDetectorGroup<Event>::~StiDetectorGroup()
{
  delete _hitLoader;
}

/// Get a hit loader appropraite for this detector group
template<class Event>
StiHitLoader<Event,StiDetectorBuilder> * StiDetectorGroup<Event>::getHitLoader()
{
  if (_detectorBuilder==0)
    {
      string message = "StiDetectorGroup::getHitLoader() - ERROR - Hit loader requested for non active detector:";
      message += getName();
//      throw logic_error(message.c_str());
    }
  /*  if (_hitLoader==0)
    {
      string message = "StiDetectorGroup::getHitLoader() - ERROR - Hit loader == 0 for detector:";
      message += getName();
      throw logic_error(message.c_str());
      }*/
  return _hitLoader;
}
#endif


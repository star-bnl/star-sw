#ifndef StiDetectorGroup_H_INCLUDED
#define StiDetectorGroup_H_INCLUDED

#include <stdexcept>
#include "Sti/Base/Named.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiDetectorBuilder.h"
template<class Event, class McEvent,class Detector> class StiHitLoader;
class StiDedxCalculator;
class StiElossCalculator;
#include "StiGui/StiDetectorViews.h"
#include "StiGui/StiAllVisibleDetectorView.h"
#include "StiGui/StiAllInvisibleDetectorView.h"
#include "StiGui/StiActiveDetectorView.h"
#include "StiGui/StiSkeletonDetectorView.h"

template<class Event, class McEvent>
class StiDetectorGroup : public Named
{
  public:

  /// Get a hit loader appropriate for this detector group
  virtual StiHitLoader<Event,McEvent,StiDetectorBuilder> * getHitLoader();
  
  /// Get a detector builder appropriate for this detector group
  virtual StiDetectorBuilder * getDetectorBuilder();

  virtual void initialize();

  ///Get Detector Views for this group
  StiDetectorViews * getDetectorViews();

  /// Get a pid calculator appropriate for this detector group
  /// A dedx calculator is used after the track are fitted
  /// to determine the average (or appropriate measure) dedx.
  virtual StiDedxCalculator * getDedxCalculator();

  /// Get an energy loss calculator appropriate for this detector group
  /// An eloss calculator is used in the kalman propagation to determine
  /// the track energy loss.
  StiElossCalculator * getElossCalculator();
  void setGroupId(int id);
  int  getGroupId() const;

 protected:

  StiDetectorGroup(const string & name);
  StiDetectorGroup(const string & name,
		   StiHitLoader<Event,McEvent,StiDetectorBuilder> * hitLoader,
		   StiDetectorBuilder * detectorBuilder,
		   StiDedxCalculator *  dedxCalculator,
		   StiElossCalculator * elossCalculator);
  ~StiDetectorGroup();
  StiHitLoader<Event,McEvent,StiDetectorBuilder> * _hitLoader;
  StiDetectorBuilder * _detectorBuilder;
  StiDedxCalculator *  _dedxCalculator;
  StiElossCalculator * _elossCalculator; 
  StiDetectorViews   * _detectorViews;
  /// Detector group identifier.
  int _groupId;
};

template<class Event, class McEvent>
void StiDetectorGroup<Event, McEvent>::initialize()
{
  //instantiate all basic views.
  _detectorViews = new StiDetectorViews(_name+"Views",_name+" Views");
  StiDetectorView * view;
  _detectorViews->add(new StiAllVisibleDetectorView(_name+"AllVisble",_name+" All Visble",_detectorBuilder));
  _detectorViews->add(new StiAllInvisibleDetectorView(_name+"AllInvisble",_name+" All Invisble",_detectorBuilder));
  _detectorViews->add(new StiActiveDetectorView(_name+"Active",_name+" Active",_detectorBuilder));
  _detectorViews->add(view = new StiSkeletonDetectorView(_name+"Skeleton",_name+" Skeleton",_detectorBuilder));
  _detectorViews->setDefaultView(view);    
}

template<class Event, class McEvent>
StiDetectorGroup<Event, McEvent>::StiDetectorGroup(const string & name,
					  StiHitLoader<Event,McEvent,StiDetectorBuilder> * hitLoader,
					  StiDetectorBuilder * detectorBuilder,
					  StiDedxCalculator *  dedxCalculator,
					  StiElossCalculator * elossCalculator)
  :  Named(name),
     _hitLoader(hitLoader),
     _detectorBuilder(detectorBuilder),
     _dedxCalculator(dedxCalculator),
     _elossCalculator(elossCalculator),
     _detectorViews(0),
     _groupId(-1)
{
  // If a loader was specified, make sure it uses the selected detector builder.
  if (_hitLoader)
    {
      _hitLoader->setDetector(detectorBuilder);
    }
  initialize();
}

template<class Event, class McEvent>
StiDetectorGroup<Event, McEvent>::StiDetectorGroup(const string & name)
  : Named(name),
     _hitLoader(0),
     _detectorBuilder(0),
     _dedxCalculator(0),
     _elossCalculator(0),
     _detectorViews(0),
     _groupId(-1)
{
  initialize();
}

template<class Event, class McEvent>
StiDetectorGroup<Event, McEvent>::~StiDetectorGroup()
{
  delete _hitLoader;
  delete _detectorBuilder;
  delete _detectorViews;
}

/// Get a hit loader appropraite for this detector group
template<class Event, class McEvent>
StiHitLoader<Event,McEvent, StiDetectorBuilder> * StiDetectorGroup<Event, McEvent>::getHitLoader()
{
  if (_detectorBuilder==0)
    {
      string message = "StiDetectorGroup::getHitLoader() - ERROR - Hit loader requested for non active detector:";
      message += _name;
      throw logic_error(message.c_str());
    }
  /*  if (_hitLoader==0)
    {
      string message = "StiDetectorGroup::getHitLoader() - ERROR - Hit loader == 0 for detector:";
      message += _name;
      throw logic_error(message.c_str());
      }*/
  return _hitLoader;
}


/// Get a detector builder appropriate for this detector group
template<class Event, class McEvent>
StiDetectorBuilder * StiDetectorGroup<Event, McEvent>::getDetectorBuilder()
{
  if (_detectorBuilder==0)
    {
      string message = "StiDetectorGroup::getDetectorBuilder() - ERROR - builder == 0 for detector:";
      message += _name;
      throw logic_error(message.c_str());
    }
  return _detectorBuilder; 
}

/// Get a pid calculator appropriate for this detector group
/// A dedx calculator is used after the track are fitted
/// to determine the average (or appropriate measure) dedx.
template<class Event, class McEvent>
StiDedxCalculator * StiDetectorGroup<Event, McEvent>::getDedxCalculator()
{ 
  if (_dedxCalculator==0)
    {
      string message = "StiDetectorGroup::getDedxCalculator() - ERROR - dedxCalculator == 0 for detector:";
      message += _name;
      throw logic_error(message.c_str());
    }
  return _dedxCalculator; 
}


/// Get an energy loss calculator appropriate for this detector group
/// An eloss calculator is used in the kalman propagation to determine
/// the track energy loss.
template<class Event, class McEvent>
StiElossCalculator * StiDetectorGroup<Event, McEvent>::getElossCalculator()
{
  if (_elossCalculator==0)
    {
      string message = "StiDetectorGroup::getElossCalculator() - ERROR - elossCalculator == 0 for detector:";
      message += _name;
      throw logic_error(message.c_str());
    }
  return _elossCalculator; 
}

template<class Event, class McEvent>
inline void StiDetectorGroup<Event, McEvent>::setGroupId(int id)
{
  if (_detectorBuilder) _detectorBuilder->setGroupId(id);
  _groupId = id;
}

template<class Event, class McEvent>
inline int  StiDetectorGroup<Event, McEvent>::getGroupId() const
{
  return _groupId;
}

template<class Event, class McEvent>
inline StiDetectorViews * StiDetectorGroup<Event, McEvent>::getDetectorViews()
{
  return _detectorViews;
}

#endif


#ifndef EventDisplayParameters_H
#define EventDisplayParameters_H 1
#include "Sti/Base/EditableParameters.h"

class EventDisplayParameters : public EditableParameters
{
public: 
  EventDisplayParameters();
  EventDisplayParameters(const EventDisplayParameters & pars);
  ~EventDisplayParameters();
  const EventDisplayParameters & operator=(const EventDisplayParameters & p);
  void initialize();
  bool getDetectorVisible() const;
  bool getTrackVisible() const;
  bool getHitVisible() const;
  bool getMcTrackVisible() const;
  bool getMcHitVisible() const;
  void setDetectorVisible(bool detectorVisible); 
  void setTrackVisible(bool trackVisible); 
  void setHitVisible(bool hitVisible) ;
  void setMcTrackVisible(bool mcTrackVisible); 
  void setMcHitVisible(bool mcHitVisible); 
  
 protected:
  bool   _detectorVisible;
  bool   _trackVisible;
  bool   _hitVisible;
  bool   _mcTrackVisible;
  bool   _mcHitVisible;
};


inline bool EventDisplayParameters::getDetectorVisible() const { return _detectorVisible; }
inline bool EventDisplayParameters::getTrackVisible() const { return _trackVisible; }
inline bool EventDisplayParameters::getHitVisible() const { return _hitVisible; }
inline bool EventDisplayParameters::getMcTrackVisible() const { return _mcTrackVisible; }
inline bool EventDisplayParameters::getMcHitVisible() const { return _mcHitVisible; }


inline void EventDisplayParameters::setDetectorVisible(bool detectorVisible) 
{
  _detectorVisible = detectorVisible; 
}

inline void EventDisplayParameters::setTrackVisible(bool trackVisible) 
{
  _trackVisible = trackVisible; 
}

inline void EventDisplayParameters::setHitVisible(bool hitVisible) 
{ 
  _hitVisible = hitVisible; 
}

inline void EventDisplayParameters::setMcTrackVisible(bool mcTrackVisible) 
{ 
  _mcTrackVisible = mcTrackVisible; 
}

inline void EventDisplayParameters::setMcHitVisible(bool mcHitVisible) 
{
  _mcHitVisible = mcHitVisible; 
}

#endif

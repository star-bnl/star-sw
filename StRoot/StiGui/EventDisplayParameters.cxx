#include "StiGui/EventDisplayParameters.h" 
#include "Sti/Base/EditableParameter.h" 
 
EventDisplayParameters::EventDisplayParameters() 
  : EditableParameters("EventDisplayParameters","EventDisplayParameters"),
    _detectorVisible(true),
    _trackVisible(true),
    _hitVisible(true),
    _mcTrackVisible(true),
    _mcHitVisible(false)
{ 
  initialize(); 
} 
   
EventDisplayParameters::~EventDisplayParameters() 
{}   
 
 
EventDisplayParameters::EventDisplayParameters(const EventDisplayParameters & p) 
{ 
  _enabled  = p._enabled; 
  _editable = p._editable;  
  _detectorVisible = p._detectorVisible;
  _trackVisible    = p._trackVisible;
  _hitVisible      = p._hitVisible;
  _mcTrackVisible  = p._mcTrackVisible;
  _mcHitVisible    = p._mcHitVisible; 
  initialize(); 
} 
 
const EventDisplayParameters & EventDisplayParameters::operator=(const EventDisplayParameters & p) 
{ 
  clear(); 
  _enabled  = p._enabled; 
  _editable = p._editable;  
  _detectorVisible = p._detectorVisible;
  _trackVisible    = p._trackVisible;
  _hitVisible      = p._hitVisible;
  _mcTrackVisible  = p._mcTrackVisible;
  _mcHitVisible    = p._mcHitVisible;
  return *this; 
} 
 
void EventDisplayParameters::initialize() 
{ 
  add(new EditableParameter("DetectorVisible","DetectorVisible",&_detectorVisible,_detectorVisible,0));
  add(new EditableParameter("TrackVisible",   "TrackVisible",   &_trackVisible,   _trackVisible,0));
  add(new EditableParameter("HitVisible",     "HitVisible",     &_hitVisible,     _hitVisible,0));
  add(new EditableParameter("McTrackVisible", "McTrackVisible", &_mcTrackVisible, _mcTrackVisible,0));
  add(new EditableParameter("McHitVisible",   "McHitVisible",   &_mcHitVisible,   _mcHitVisible,0));
} 


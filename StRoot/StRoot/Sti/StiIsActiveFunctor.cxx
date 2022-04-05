#include "StiIsActiveFunctor.h"

StiIsActiveFunctor::StiIsActiveFunctor(bool active, bool editable)
  : _active(active),
    _editable(editable)
{} // StiIsActiveFunctor

StiIsActiveFunctor::~StiIsActiveFunctor()
{} // ~StiIsActiveFunctor

///Returns whether the object is active.
bool StiIsActiveFunctor::isActive() const
{
  return _active;
}

///Set whether the object is active.
void StiIsActiveFunctor::setIsActive(bool active)
{
  _active = active;
}

///Returns whether the object is editable.
bool StiIsActiveFunctor::isEditable() const
{
  return _editable;
}

///Set whether the object is editable
void StiIsActiveFunctor::setIsEditable(bool editable)
{
  _editable = editable;
}

/// Determines whether the object is considered active at the
/// given coordinates. Note that this base class implementation
/// is implemented as an "all active/ianactive" i.e. no partition 
/// of the object is defined.
bool StiIsActiveFunctor::operator()(double y, double z) const
{
  return _active;
}

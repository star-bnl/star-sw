#include "StxIsActiveFunctor.h"

StxIsActiveFunctor::StxIsActiveFunctor(bool active, bool editable)
  : _active(active),
    _editable(editable)
{} // StxIsActiveFunctor

StxIsActiveFunctor::~StxIsActiveFunctor()
{} // ~StxIsActiveFunctor

///Returns whether the object is active.
bool StxIsActiveFunctor::IsActive() const
{
  return _active;
}

///Set whether the object is active.
void StxIsActiveFunctor::SetIsActive(bool active)
{
  _active = active;
}

///Returns whether the object is editable.
bool StxIsActiveFunctor::isEditable() const
{
  return _editable;
}

///Set whether the object is editable
void StxIsActiveFunctor::setIsEditable(bool editable)
{
  _editable = editable;
}

/// Determines whether the object is considered active at the
/// given coordinates. Note that this base class implementation
/// is implemented as an "all active/ianactive" i.e. no partition 
/// of the object is defined.
bool StxIsActiveFunctor::operator()(double y, double z) const
{
  return _active;
}

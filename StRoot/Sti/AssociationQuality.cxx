#include <stdexcept>
#include "Sti/AssociationQuality.h" 
#include "Sti/StiTrack.h"

AssociationQuality::AssociationQuality()
  : _first(0),
    _second(0),
    _quality(0),
    _label(0)
{}

AssociationQuality::~AssociationQuality()
{}

void AssociationQuality::reset()
{
  _first = 0;
  _second = 0;
  _quality = 0;
  _label   = 0;
}

void AssociationQuality::setFirst(StiTrack *first)
{
  _first = first;
}

void AssociationQuality::setSecond(StiTrack *second)
{
  _second = second;
}

void AssociationQuality::setQuality(double quality)
{
  _quality = quality;
}

void AssociationQuality::incrementQuality()
{
  _quality++;
}

void AssociationQuality::setLabel(int label)
{
  _label = label;
}

StiTrack * AssociationQuality::getFirst() const
{
  return _first;
}

StiTrack * AssociationQuality::getSecond() const
{
  return _second;
}

int    AssociationQuality::getLabel() const
{
  return _label;
}

double AssociationQuality::getQuality() const
{
  return _quality;
}

double AssociationQuality::getDifference(int type) const
{
  return _second->getValue(type) - _first->getValue(type);
}

double AssociationQuality::getRelativeDifference(int type) const
{
  double firstValue = _first->getValue(type);
  if (firstValue!=0)
    return (_second->getValue(type)-firstValue)/firstValue;
  else
    throw runtime_error("AssociationQuality::getRelativeDifference(int type) const -E- Division by zero");
}

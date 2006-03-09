#include "StiToolkit.h"

StiToolkit * StiToolkit::_instance = 0;

void StiToolkit::setToolkit(StiToolkit * toolkit)
{
  _instance = toolkit;
}

StiToolkit * StiToolkit::instance()
{
  return _instance;
}

void StiToolkit::kill()
{
  if (_instance)
    {
      delete _instance;
      _instance = 0;
    }
}

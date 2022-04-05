#include <stdlib.h>
#include <assert.h>
#include "StiToolkit.h"

StiToolkit * StiToolkit::_instance = 0;

StiToolkit::StiToolkit()
{
  assert(!_instance);
  _instance = this;
}

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

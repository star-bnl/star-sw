#include <stdlib.h>
#include <assert.h>
#include "StxToolkit.h"

StxToolkit * StxToolkit::_instance = 0;

StxToolkit::StxToolkit()
{
  assert(!_instance);
  _instance = this;
}

void StxToolkit::SetToolkit(StxToolkit * toolkit)
{
  _instance = toolkit;
}

StxToolkit * StxToolkit::instance()
{
  return _instance;
}

void StxToolkit::Kill()
{
  if (_instance)
    {
      delete _instance;
      _instance = 0;
    }
}

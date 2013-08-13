#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "StiToolkit.h"

StiToolkit * StiToolkit::_instance = 0;

StiToolkit::StiToolkit()
{
  assert(!_instance);
  _instance = this;
  memset(mBeg,0,mEnd-mBeg+1);
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

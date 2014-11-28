#include "ConstrainedParameter.h"
#include <stdexcept>

ConstrainedParameter::ConstrainedParameter() 
  : Parameter(),
    _minimum(0.),
    _maximum(1.),
    _default(0.)
{}
 
ConstrainedParameter::ConstrainedParameter(const string & name, 
					   const string & description,
					   double value, 
					   double defaultValue, 
					   double min, 
					   double max, 
					   int    type,
					   int    key)
  : Parameter(name,description,value,type,key),
    _minimum(min),
    _maximum(max),
    _default(defaultValue)
{}

ConstrainedParameter::ConstrainedParameter(const ConstrainedParameter & parameter)
  : Parameter(parameter)
{ 
  _minimum = parameter._minimum;
  _maximum = parameter._maximum;
  _default = parameter._default;
}

ConstrainedParameter::ConstrainedParameter(const string & name, const string & description,
					   bool * value, bool defaultValue, int key)
  : Parameter(name,description,value,key),
    _minimum(0),
    _maximum(1),
    _default(defaultValue)
{
  *value = defaultValue;
}

ConstrainedParameter::ConstrainedParameter(const string & name, const string & description,
					   int *  value, int    defaultValue, int min, int max, int key)
  : Parameter(name,description,value,key),
    _minimum(min),
    _maximum(max),
    _default(defaultValue)
{
  *value = defaultValue;
}

ConstrainedParameter::ConstrainedParameter(const string & name, const string & description,
					   float* value, float  defaultValue,float min, float max, int key)
  : Parameter(name,description,value,key),
    _minimum(min),
    _maximum(max),
    _default(defaultValue)
{
  *value = defaultValue;
}

ConstrainedParameter::ConstrainedParameter(const string & name, const string & description,
					   double *  value, double defaultValue, double min, double max, int key)
  : Parameter(name,description,value,key),
    _minimum(min),
    _maximum(max),
    _default(defaultValue)
{
  *value = defaultValue;
}


ConstrainedParameter::~ConstrainedParameter()
{}


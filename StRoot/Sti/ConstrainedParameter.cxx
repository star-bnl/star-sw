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
					   int    type)
  : Parameter(name,description,type,value),
    _minimum(min),
    _maximum(max),
    _default(defaultValue)
{}

ConstrainedParameter::ConstrainedParameter(const ConstrainedParameter & parameter)
{ 
  _type    = parameter._type;
  _value   = parameter._value; 
  _minimum = parameter._minimum;
  _maximum = parameter._maximum;
  _default = parameter._default;
}

ConstrainedParameter::~ConstrainedParameter()
{}

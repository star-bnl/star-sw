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
{ 
  _key     = parameter._key;
  _type    = parameter._type;
  _value   = parameter._value; 
  _minimum = parameter._minimum;
  _maximum = parameter._maximum;
  _default = parameter._default;
}

ConstrainedParameter::~ConstrainedParameter()
{}

ConstrainedParameterFactory::ConstrainedParameterFactory(const string& newName,
							 int original,
							 int incremental, 
							 int maxInc)
  : StiObjectFactoryInterface<Parameter>(newName, 
					 original, 
					 incremental, 
					 maxInc)
{
  initialize();
}

ConstrainedParameterFactory::~ConstrainedParameterFactory()
{
  // cout <<"ConstrainedParameterFactory::~ConstrainedParameterFactory()"<<endl;
}

void* ConstrainedParameterFactory::makeNewObject() const
{
  return new ConstrainedParameter();
}

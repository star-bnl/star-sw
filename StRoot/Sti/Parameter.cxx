#include "Parameter.h"
#include <stdexcept>

const int Parameter::Boolean = 0;
const int Parameter::Integer = 1;
const int Parameter::Double  = 2;

Parameter::Parameter()
 : Named(""),
   Described(""),
   _key(0),
   _type(Double),
   _value(0.)
{ }

Parameter::Parameter(const string & name, 
		     const string & description,
		     double value,
		     int type, 
		     int key)
 : Named(name),
   Described(description),
   _key(key),
   _type(type),
   _value(value)
{ }

Parameter::Parameter(const Parameter & parameter)
{
  _name = parameter._name;
  _description = parameter._description;
  _key  = parameter._key;
  _type = parameter._type;
  _value = parameter._value;
}

Parameter::~Parameter()
{}


ParameterFactory::ParameterFactory(const string& newName,
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

ParameterFactory::~ParameterFactory()
{
  // cout <<"ParameterFactory::~ParameterFactory()"<<endl;
}

void * ParameterFactory::makeNewObject() const
{
  return new Parameter();
}

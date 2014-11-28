#include "Sti/Base/Parameter.h"
#include <stdexcept>

const int Parameter::Boolean = 0;
const int Parameter::Integer = 1;
const int Parameter::Float   = 2;
const int Parameter::Double  = 3;

Parameter::Parameter()
 : Named(""),
   Described(""),
   _key(0),
   _type(Double),
   _value(0.),
   _exValue(0)

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
   _value(value),
   _exValue(0)
{ }

Parameter::Parameter(const string & name, const string & description, bool  * value, int key)
  : Named(name),
    Described(description),
    _key(key),
    _type(Boolean),
    _value(0),
    _exValue(static_cast<bool*>(value))
{}

Parameter::Parameter(const string & name, const string & description, int   * value, int key)
  : Named(name),
    Described(description),
    _key(key),
    _type(Integer),
    _value(0),
    _exValue(static_cast<int*>(value))
{}

Parameter::Parameter(const string & name, const string & description, float * value, int key)
  : Named(name),
    Described(description),
    _key(key),
    _type(Float),
    _value(0),
    _exValue(static_cast<float*>(value))
{}

Parameter::Parameter(const string & name, const string & description, double* value, int key)
  : Named(name),
    Described(description),
    _key(key),
    _type(Double),
    _value(0),
    _exValue(static_cast<double*>(value))
{}

Parameter::Parameter(const Parameter & parameter)
{
  setName(parameter.getName());
  _description = parameter._description;
  _key  = parameter._key;
  _type = parameter._type;
  _value = parameter._value;
  _exValue = parameter._exValue;
}

Parameter::~Parameter()
{}

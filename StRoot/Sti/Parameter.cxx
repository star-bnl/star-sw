#include "Parameter.h"
#include <stdexcept>

const int Parameter::Boolean = 0;
const int Parameter::Integer = 1;
const int Parameter::Double  = 2;

Parameter::Parameter()
 : Named(""),
   Described(""),
   _type(Double),
   _value(0.)
{ }

Parameter::Parameter(const string & name, 
		     const string & description,
		     int type, double value)
 : Named(name),
   Described(description),
   _type(type),
   _value(value)
{ }

Parameter::Parameter(const Parameter & parameter)
{
  _name = parameter._name;
  _description = parameter._description;
  _type = parameter._type;
  _value = parameter._value;
}

Parameter::~Parameter()
{}


#include "Parameter.h"

const int Parameter::Boolean = 0;
const int Parameter::Integer = 1;
const int Parameter::Double  = 2;

Parameter::Parameter()
 : _type(Double),
   _value(0.)
{ }

Parameter::Parameter(int type, double value)
 : _type(type),
   _value(value)
{ }

Parameter::Parameter(const Parameter & parameter)
{
  _type = parameter._type;
  _value = parameter._value;
}

Parameter::~Parameter()
{}


#include "EditableParameter.h"

EditableParameter::EditableParameter()
 : ConstrainedParameter(),
   _increment(1.)
{ }

EditableParameter::EditableParameter(const string & name, 
				     const string & description,
				     double value, 
				     double defaultValue, 
				     double min, 
				     double max,
				     double increment,
				     int    type)
  : ConstrainedParameter(name,description,value,defaultValue,min,max,type),
    _increment(increment)
{}

EditableParameter::EditableParameter(const EditableParameter & parameter)
{
  _type      = parameter._type;
  _value     = parameter._value; 
  _minimum   = parameter._minimum;
  _maximum   = parameter._maximum;
  _default   = parameter._default;
  _increment = parameter._increment;  
}

EditableParameter::~EditableParameter()
{}


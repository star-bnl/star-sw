#include "EditableParameter.h"
#include <stdexcept>

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
				     int    type,
				     int    key)
  : ConstrainedParameter(name,description,value,defaultValue,min,max,type,key),
    _increment(increment)
{}

EditableParameter::EditableParameter(const EditableParameter & parameter)
{
  _name        = parameter._name;
  _description = parameter._description;
  _key       = parameter._key;
  _type      = parameter._type;
  _value     = parameter._value; 
  _minimum   = parameter._minimum;
  _maximum   = parameter._maximum;
  _default   = parameter._default;
  _increment = parameter._increment;  
}

EditableParameter::~EditableParameter()
{}

void EditableParameter::set(const string & name, 
			    const string & description,
			    double value, 
			    double defaultValue, 
			    double min, 
			    double max,
			    double increment,
			    int    type,
			    int    key)
{
  _name = name;
  _description = description; 
  _key         = key;
  _type        = type;
  _value       = value;
  _default     = defaultValue;
  _minimum     = min;
  _maximum     = max;
  _increment   = increment;
}


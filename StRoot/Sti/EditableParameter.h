#if !defined(EDITABLEPARAMETER_H_INCLUDED_)
#define EDITABLEPARAMETER_H_INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "ConstrainedParameter.h"

class EditableParameter : ConstrainedParameter
{
 public:

  EditableParameter();
  EditableParameter(const string & name, 
		    const string & description,
		    double value, 
		    double defaultValue, 
		    double min, 
		    double max,
		    double increment,
		    int    type);
  EditableParameter(const EditableParameter & parameter);
  virtual ~EditableParameter();
  
  const EditableParameter & operator=(const EditableParameter & parameter);
  
  double  getIncrement() const;
  void    setIncrement(double increment);
  
 protected:
  
  double  _increment;
  
};

inline const EditableParameter & EditableParameter::operator=(const EditableParameter & parameter)
{
  if (&parameter==this)
    return *this;  
  _type      = parameter._type;
  _value     = parameter._value; 
  _minimum   = parameter._minimum;
  _maximum   = parameter._maximum;
  _default   = parameter._default;
  _increment = parameter._increment;
  return *this;
}

double  EditableParameter::getIncrement() const
{
  return _increment;
}

void    EditableParameter::setIncrement(double increment)
{
  _increment = increment;
}

#endif // !defined(EDITABLEPARAMETER_H_INCLUDED_)

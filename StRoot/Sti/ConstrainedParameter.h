#if !defined(CONSTRAINEDPARAMETER_H__5B75CCD2_01CA_4993_8BD6_836465B6A0E1__INCLUDED_)
#define CONSTRAINEDPARAMETER_H__5B75CCD2_01CA_4993_8BD6_836465B6A0E1__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Parameter.h"

class ConstrainedParameter : public Parameter  
{
public:
  ConstrainedParameter();
  ConstrainedParameter(const string & name, 
		       const string & description,
		       double value, 
                       double defaultValue, 
                       double min, 
                       double max, 
                       int type=Double);
  ConstrainedParameter(const ConstrainedParameter & parameter);
  virtual ~ConstrainedParameter();
  
  const ConstrainedParameter & operator=(const ConstrainedParameter & parameter);
  
  double  getMinimum() const;
  double  getMaximum() const;
  double  getDefault() const;
  void    setMinimum(double min);
  void    setMaximum(double max);
  void    setDefault(double value);
  void    setValue(double value);
  void    set(double value, 
              double defaultValue, 
              double min, 
              double max, 
              int type=Double);

protected:

  double  _minimum;
  double  _maximum;
  double  _default;

};

inline const ConstrainedParameter & ConstrainedParameter::operator=(const ConstrainedParameter & parameter)
{
  if (&parameter==this)
    return *this;
  _type    = parameter._type;
  _value   = parameter._value; 
  _minimum = parameter._minimum;
  _maximum = parameter._maximum;
  _default = parameter._default;
  return *this;
}

inline  double  ConstrainedParameter::getMinimum() const
{
  return _minimum;
}

inline  double  ConstrainedParameter::getMaximum() const
{
  return _maximum;
}

inline  double  ConstrainedParameter::getDefault() const
{
  return _default;
}

inline  void    ConstrainedParameter::setMinimum(double min)
{
  _minimum = min;
  if (_value<_minimum)
    _value = _minimum;
  if (_default<_minimum)
    _default = _minimum;
}

inline  void    ConstrainedParameter::setMaximum(double max)
{
  _maximum = max;
  if (_value>_maximum)
    _value = _maximum;
  if (_default>_maximum)
    _default = _maximum;
}

inline  void    ConstrainedParameter::setDefault(double value)
{
  if (value<_minimum)
    _default = _minimum;
  else if (value>_maximum)
    _default = _maximum;
  else
    _default = value;
}

inline  void    ConstrainedParameter::setValue(double value)
{
  if (value<_minimum)
    _value = _minimum;
  else if (value>_maximum)
    _value = _maximum;
  else
    _value = value;
}

inline  void ConstrainedParameter::set(double value, 
				       double defaultValue, 
				       double min, 
				       double max, 
				       int type)
{
  if (type==Double || type==Integer)
    {
      if (min<max)
	{
	  _minimum = min;
	  _maximum = max;
	}
      else
	{
	  _minimum = max;
	  _maximum = min;
	}
      setDefault(defaultValue);
      setValue(value);
      _type = type;
    }
  else // Boolean
    {
      _minimum = 0;
      _maximum = 1;
      _default = (defaultValue!=0);
      _value   = (value!=0);
      _type = Boolean;
    }
}



#endif // !defined(CONSTRAINEDPARAMETER_H__5B75CCD2_01CA_4993_8BD6_836465B6A0E1__INCLUDED_)

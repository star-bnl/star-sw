#if !defined(CONSTRAINEDPARAMETER_H__5B75CCD2_01CA_4993_8BD6_836465B6A0E1__INCLUDED_)
#define CONSTRAINEDPARAMETER_H__5B75CCD2_01CA_4993_8BD6_836465B6A0E1__INCLUDED_

#include <cassert>
#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Parameter.h"

class ConstrainedParameter : public Parameter  
{
public:
  ConstrainedParameter();
  ConstrainedParameter(const string & name, const string & description,
		       double value, double defaultValue, 
                       double min, double max, int type=Double, int key=0);
  ConstrainedParameter(const string & name, const string & description,
		       bool * value, bool defaultValue, int key=0);
  ConstrainedParameter(const string & name, const string & description,
		       int *  value, int    defaultValue, int min, int max, int key=0);
  ConstrainedParameter(const string & name, const string & description,
		       float* value, float  defaultValue,float min, float max, int key=0);
  ConstrainedParameter(const string & name, const string & description,
		       double *  value, double defaultValue, double min, double max, int key=0);

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
  void    set(const string & name,const string & description,
	      double value, double defaultValue, double min, double max, int type=Double, int key=0);
  void    set(const string & name,const string & description,bool*value, bool defaultValue, int key=0);
  void    set(const string & name,const string & description,int *value, int  defaultValue, int min,  int max, int key=0);
  void    set(const string & name,const string & description,float*value, float defaultValue, float min, float max, int key=0);
  void    set(const string & name,const string & description,double*value, double defaultValue, double min, double max, int key=0);
  void    reset();

protected:

  double  _minimum;
  double  _maximum;
  double  _default;

};

inline const ConstrainedParameter & ConstrainedParameter::operator=(const ConstrainedParameter & parameter)
{
  if (&parameter==this)
    return *this;
  _key     = parameter._key;
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
    Parameter::setValue(_minimum);
  else if (value>_maximum)
		Parameter::setValue(_maximum);
  else
		Parameter::setValue(value);
}

/*! Reset the parameter value to its default value.
  \see void setDefault()
 */
inline void ConstrainedParameter::reset()
{
  _value = _default;
}

inline  void ConstrainedParameter::set(const string & name,
				       const string & description,
				       double value, 
				       double defaultValue, 
				       double min, 
				       double max, 
				       int type,
				       int key)
{
  Parameter::set(name,description,value,type,key);
  assert(min<=max);
  if (type==Double || type==Integer)
    {
      _minimum = min;
      _maximum = max;
      setDefault(defaultValue);
      setValue(value);
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

inline void ConstrainedParameter::set(const string & name,const string & description,
				      bool*value, bool defaultValue, int key)
{
  Parameter::set(name,description,value,key);
  _default = defaultValue;
  _minimum = 0;
  _maximum = 1;
}

inline void ConstrainedParameter::set(const string & name,const string & description,
				      int *value, int  defaultValue, int min,  int max, int key)
{ 
  Parameter::set(name,description,value,key);
  _default = defaultValue;
  _minimum = min;
  _maximum = max;
}

inline void ConstrainedParameter::set(const string & name,const string & description,
				      float*value, float defaultValue, float min, float max, int key)
{
  Parameter::set(name,description,value,key);
  _default = defaultValue;
  _minimum = min;
  _maximum = max;
}

inline void ConstrainedParameter::set(const string & name,const string & description,
				      double*value, double defaultValue, double min, double max, int key)
{ 
  Parameter::set(name,description,value,key);
  _default = defaultValue;
  _minimum = min;
  _maximum = max;
}
     
#endif // !defined(CONSTRAINEDPARAMETER_H__5B75CCD2_01CA_4993_8BD6_836465B6A0E1__INCLUDED_)
     

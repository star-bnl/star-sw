#if !defined(PARAMETER_H_INCLUDED_)
#define PARAMETER_H_INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Named.h"
#include "Described.h"
#include "Stiostream.h"
#include <stdexcept>
using namespace std;

/*! Class defining a mutable and generic parameter.
 <p>
 A parameter has a value, a name, and can also be given a short description. The parameter 
may be of type Boolean, Integer, or Double. An integer key may be optionally specified to 
provide a unique identifier. This class is a base class for ConstrainedParameter and
EditableParameter classes.
\see ConstrainedParameter
\see EditableParameter 
 */
class Parameter : public Named, public Described
{
 public:

  static const int Boolean;
  static const int Integer;
  static const int Float;
  static const int Double;

  Parameter();
  Parameter(const string & name, const string & description, double value, int type, int key);
  Parameter(const string & name, const string & description, bool  * value, int key);
  Parameter(const string & name, const string & description, int   * value, int key);
  Parameter(const string & name, const string & description, float * value, int key);
  Parameter(const string & name, const string & description, double* value, int key);
  Parameter(const Parameter & parameter);
  virtual ~Parameter();
  
  const Parameter & operator=(const Parameter & parameter);
  
  int    getKey() const;
  int    getType() const;
  bool   getBoolValue() const;
  int    getIntValue() const;
  float  getFloatValue() const;
  double getDoubleValue() const;
  
  void   setKey(int key);
  void   setValue(bool value);
  void   setValue(int  value);
  void   setValue(float  value);
  void   setValue(double  value);
  void   set(const string & name,const string & description, double value,int type=Double,int key=0);
  void   set(const string & name,const string & description, bool  * value,int key=0);
  void   set(const string & name,const string & description, int   * value,int key=0);
  void   set(const string & name,const string & description, float * value,int key=0);
  void   set(const string & name,const string & description, double* value,int key=0);

 protected:
  
  int     _key;
  int     _type;
  double  _value;
  void *  _exValue;
  
};


inline const Parameter & Parameter::operator=(const Parameter & parameter)
{
  if (&parameter==this)
    return *this;
  _key    = parameter._key;
  _type   = parameter._type;
  _value  = parameter._value;
  return *this;
}

inline int     Parameter::getKey() const
{
  return _key;
}

inline int     Parameter::getType() const
{
  return _type;
}

inline bool  Parameter::getBoolValue() const
{
  if (_exValue)
    return *static_cast<bool*>(_exValue);
  else
    return _value>0;
}

inline int  Parameter::getIntValue() const
{ 
  if (_exValue)
    return *static_cast<int*>(_exValue); 
  else
    return int(_value);
}

inline float  Parameter::getFloatValue() const
{  
  if (_exValue)
    return *static_cast<float*>(_exValue);
  else
    return float(_value);
}

inline double  Parameter::getDoubleValue() const
{ 
  if (_exValue)
    return *static_cast<double*>(_exValue);
  else
    return _value;
}

inline void    Parameter::setKey(int key)
{
  _key = key;
}

inline void    Parameter::setValue(bool value)
{
  //cout << " Parameter::setValue(bool value) value:"<<value<<endl;
  if (_exValue)
    {
      switch (_type)
	{
	case 0: *static_cast<bool*>(_exValue)   = value;  break;
	case 1: *static_cast<int*>(_exValue)    = (int) value;  break;
	case 2: *static_cast<float*>(_exValue)  = (float)value;  break;
	case 3: *static_cast<double*>(_exValue) = (double)value;  break;
	}
    }
  else
    _value = value;
}

inline void    Parameter::setValue(int value)
{ 
  //	cout << " Parameter::setValue(int value) value:"<<value<<endl;
  if (_exValue)
    {
      switch (_type)
	{
	case 0: *static_cast<bool*>(_exValue)   = value>0;  break;
	case 1: *static_cast<int*>(_exValue)    = value;  break;
	case 2: *static_cast<float*>(_exValue)  = (float)value;  break;
	case 3: *static_cast<double*>(_exValue) = (double)value;  break;
	}
    }
  else
    _value = value;
}

inline void    Parameter::setValue(float value)
{
  //cout << " Parameter::setValue(float value) value:"<<value<<endl;
  if (_exValue)
    {
      switch (_type)
	{
	case 0: *static_cast<bool*>(_exValue)   = value>0;  break;
	case 1: *static_cast<int*>(_exValue)    = (int) value;  break;
	case 2: *static_cast<float*>(_exValue)  = value;  break;
	case 3: *static_cast<double*>(_exValue) = (double)value;  break;
	}
    }
  else
    _value = value;
}


inline void    Parameter::setValue(double value)
{
  //cout << " Parameter::setValue(double value) value:"<<value<<endl;
  if (_exValue)
    {
      switch (_type)
	{
	case 0: *static_cast<bool*>(_exValue)   = value>0;  break;
	case 1: *static_cast<int*>(_exValue)    = (int) value;  break;
	case 2: *static_cast<float*>(_exValue)  = (float)value;  break;
	case 3: *static_cast<double*>(_exValue) = value;  break;
	}
    }
  else
    _value = value;
}

inline  void Parameter::set(const string & name, 
			    const string & description,
			    double value,
			    int type,
			    int key)
{  
  setName(name);
  _description = description;
  _type        = type;
  _key         = key;
  _value       = value;
  _exValue     = 0;
}

inline void   Parameter::set(const string & name,const string & description, bool  * value,int key)
{
  setName(name);
  _description = description;
  _type        = Boolean;
  _key         = key;
  _exValue     = value;
}

inline void   Parameter::set(const string & name,const string & description, int   * value,int key)
{ 
  setName(name);
  _description = description;
  _type        = Integer;
  _key         = key;
  _value       = 0;
  _exValue     = value;
}

inline void   Parameter::set(const string & name,const string & description, float * value,int key)
{ 
  setName(name);
  _description = description;
  _type        = Float;
  _key         = key;
  _value       = 0;
  _exValue     = value;
}


inline void   Parameter::set(const string & name,const string & description, double* value,int key)
{ 
  setName(name);
  _description = description;
  _type        = Double;
  _key         = key;
  _value       = 0;
  _exValue     = value;
}

#endif // !defined(PARAMETER_H_INCLUDED_)

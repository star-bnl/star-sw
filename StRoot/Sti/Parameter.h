#if !defined(PARAMETER_H_INCLUDED_)
#define PARAMETER_H_INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Named.h"
#include "Described.h"

class Parameter : public Named, public Described
{
 public:

  static const int Boolean;
  static const int Integer;
  static const int Double;

  Parameter();
  Parameter(const string & name, 
	    const string & description,
            double value,
	    int type,
	    int key);
  Parameter(const Parameter & parameter);
  virtual ~Parameter();
  
  const Parameter & operator=(const Parameter & parameter);
  
  int     getKey() const;
  int     getType() const;
  double  getValue() const;
  
  void    setKey(int key);
  void    setType(int type);
  virtual void setValue(double value);
  virtual void set(const string & name, 
		   const string & description,
		   double value,
		   int type=Double,
		   int key=0);

 protected:
  
  int     _key;
  int     _type;
  double  _value;
  
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

inline double  Parameter::getValue() const
{
  return _value;
}

inline void    Parameter::setKey(int key)
{
  _key = key;
}

inline void    Parameter::setType(int type)
{
  _type = type;
}

inline void    Parameter::setValue(double value)
{
  _value = value;
}

inline  void Parameter::set(const string & name, 
			    const string & description,
			    double value,
			    int type,
			    int key)
{  
  _name        = name;
  _description = description;
  _type        = type;
  _key         = key;
  _value       = value;
}

#endif // !defined(PARAMETER_H_INCLUDED_)

#if !defined(PARAMETER_H_INCLUDED_)
#define PARAMETER_H_INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

//#include "Named.h"
//#include "Described.h"
//: public Named, public Described
class Parameter 
{
 public:

  static const int Boolean;
  static const int Integer;
  static const int Double;

  Parameter();
  Parameter(int type,
            double value);
  Parameter(const Parameter & parameter);
  virtual ~Parameter();
  
  const Parameter & operator=(const Parameter & parameter);
  
  int     getType() const;
  double  getValue() const;
  
  void    setType(int type);
  virtual void    setValue(double value);
  
 protected:
  
  int     _type;
  double  _value;
  
};


inline const Parameter & Parameter::operator=(const Parameter & parameter)
{
  if (&parameter==this)
    return *this;
  _type = parameter._type;
  _value = parameter._value;
  return *this;
}

inline int     Parameter::getType() const
{
  return _type;
}

inline double  Parameter::getValue() const
{
  return _value;
}

inline void    Parameter::setType(int type)
{
  _type = type;
}

inline void    Parameter::setValue(double value)
{
  _value = value;
}

#endif // !defined(PARAMETER_H_INCLUDED_)

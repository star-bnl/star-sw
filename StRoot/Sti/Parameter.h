#if !defined(PARAMETER_H_INCLUDED_)
#define PARAMETER_H_INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Named.h"
#include "Described.h"
#include "StiObjectFactoryInterface.h"

class Parameter : public Named, public Described
{
 public:

  static const int Boolean;
  static const int Integer;
  static const int Double;

  Parameter();
  Parameter(const string & name, 
	    const string & description,
	    int type,
            double value);
  Parameter(const Parameter & parameter);
  virtual ~Parameter();
  
  const Parameter & operator=(const Parameter & parameter);
  
  int     getType() const;
  double  getValue() const;
  
  void    setType(int type);
  virtual void setValue(double value);
  virtual void set(const string & name, 
		   const string & description,
		   double value,
		   int type);

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

inline  void Parameter::set(const string & name, 
			    const string & description,
			    double value,
			    int type)
{  
  _name        = name;
  _description = description;
  _type        = type;
  _value       = value;
}



/*! Parameter factory
 */
class ParameterFactory : public StiObjectFactoryInterface<Parameter>
{
public:
    ///This is the only constructor available.
    ParameterFactory(const string& newName, 
		      int original=-1, int 
		      incremental=-1, 
		      int maxInc=-1);
    ///Default destructor.
    virtual ~ParameterFactory();
    
protected:
    ///Return a pointer to a new Parameter object on the heap.
    virtual void * makeNewObject() const;

private:
    ParameterFactory(); //Not implemented
};

#endif // !defined(PARAMETER_H_INCLUDED_)

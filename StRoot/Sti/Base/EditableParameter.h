#if !defined(EDITABLEPARAMETER_H_INCLUDED_)
#define EDITABLEPARAMETER_H_INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "ConstrainedParameter.h"

class EditableParameter : public ConstrainedParameter
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
		    int    type,
		    int    key);
  EditableParameter(const string & name, 
		    const string & description,
		    bool*  value, 
		    bool  defaultValue, 
		    int   key);
  EditableParameter(const string & name, 
		    const string & description,
		    int*   value, 
		    int   defaultValue, 
		    int   min, 
		    int   max,
		    int   increment,
		    int   key);
  EditableParameter(const string & name, 
		    const string & description,
		    float* value, 
		    float defaultValue, 
		    float min, 
		    float max,
		    float increment,
		    int    key);
  EditableParameter(const string & name, 
		    const string & description,
		    double* value, 
		    double defaultValue, 
		    double min, 
		    double max,
		    double increment,
		    int    key);
  EditableParameter(const EditableParameter & parameter);
  virtual ~EditableParameter();
  
  const EditableParameter & operator=(const EditableParameter & parameter);
  
  double  getIncrement() const;
  void    setIncrement(double increment);
  EditableParameter* set(const string & name, const string & description,
			 double value, double defaultValue, double min, double max, double increment, int type,int    key);
  EditableParameter* set(const string & name,const string & description,bool*value, bool defaultValue, int key=0);
  EditableParameter* set(const string & name,const string & description,int *value, int  defaultValue, int min,  int max, int increment,int key=0);
  EditableParameter* set(const string & name,const string & description,float*value, float defaultValue, float min, float max, float increment,int key=0);
  EditableParameter* set(const string & name,const string & description,double*value, double defaultValue, double min, double max, double increment,int key=0);
  void    reset();
  void    unset(){;}
  friend ostream& operator<<(ostream& os, const EditableParameter&par);
  
 protected:
  
  double  _increment;
  
};

inline const EditableParameter & EditableParameter::operator=(const EditableParameter & parameter)
{
  if (&parameter==this)
    return *this;  
  _key      = parameter._key;
  _type      = parameter._type;
  _value     = parameter._value; 
  _minimum   = parameter._minimum;
  _maximum   = parameter._maximum;
  _default   = parameter._default;
  _increment = parameter._increment;
  return *this;
}

inline double  EditableParameter::getIncrement() const
{
  return _increment;
}

inline void    EditableParameter::setIncrement(double increment)
{
  _increment = increment;
}

inline void    EditableParameter::reset()
{
  _value = _default;
}

inline EditableParameter* EditableParameter::set(const string & name,const string & description, double value, 
			    double defaultValue, double min, double max,double increment,int type,int    key)
{
  ConstrainedParameter::set(name,description,value,defaultValue,min,max,type,key);
  _increment   = increment;
  return this;
}

inline EditableParameter* EditableParameter::set(const string & name,const string & description,
		bool*value, bool defaultValue, int key)
{  
  ConstrainedParameter::set(name,description,value,defaultValue,key);
  _increment   = 1;
  *value = defaultValue;
  return this;
}

inline EditableParameter* EditableParameter::set(const string & name,const string & description,
		int *value, int  defaultValue, int min,  int max, int increment,int key)
{
  ConstrainedParameter::set(name,description,value,defaultValue,min,max,key);
  _increment   = increment;
  *value = defaultValue;
  return this;
}

inline EditableParameter* EditableParameter::set(const string & name,const string & description,
				   float*value, float defaultValue, float min, float max, float increment,int key)
{
  ConstrainedParameter::set(name,description,value,defaultValue,min,max,key);
  _increment   = increment;
  *value = defaultValue;
  return this;
}

inline EditableParameter * EditableParameter::set(const string & name,const string & description,
				   double*value, double defaultValue, double min, double max, double increment,int key)
{
  ConstrainedParameter::set(name,description,value,defaultValue,min,max,key);
  _increment   = increment;
  *value = defaultValue;
  return this;
}

#endif // !defined(EDITABLEPARAMETER_H_INCLUDED_)

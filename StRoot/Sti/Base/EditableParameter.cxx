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

EditableParameter::EditableParameter(const string & name, 
				     const string & description,
				     bool * value, 
				     bool  defaultValue, 
				     int   key)
  : ConstrainedParameter(name,description,value,defaultValue,key),
    _increment(1)
{
  
}

EditableParameter::EditableParameter(const string & name, 
				     const string & description,
				     int * value, 
				     int   defaultValue, 
				     int   min, 
				     int   max,
				     int   increment,
				     int   key)
  : ConstrainedParameter(name,description,value,defaultValue,min,max,key),
    _increment(increment)
{
}

EditableParameter::EditableParameter(const string & name, 
				     const string & description,
				     float*value, 
				     float defaultValue, 
				     float min, 
				     float max,
				     float increment,
				     int    key)
  : ConstrainedParameter(name,description,value,defaultValue,min,max,key),
    _increment(increment)
{}

EditableParameter::EditableParameter(const string & name, 
				     const string & description,
				     double*value, 
				     double defaultValue, 
				     double min, 
				     double max,
				     double increment,
				     int    key)
  : ConstrainedParameter(name,description,value,defaultValue,min,max,key),
    _increment(increment)
{}


EditableParameter::EditableParameter(const EditableParameter & parameter)
  :  ConstrainedParameter(parameter),
     _increment(parameter._increment)  
{}

EditableParameter::~EditableParameter()
{}


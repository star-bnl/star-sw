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


ostream& operator<<(ostream& os, const EditableParameter&par)
{
	os <<par.getName()<<"\t";
	switch (par.getType())
		{
		case 0: os << "(BOOL)=" << par.getBoolValue();   break;
		case 1: os << "(INT) =" << par.getIntValue(); break;
		case 2: os << "(FLT) =" << par.getFloatValue(); break;
		case 3: os << "(DBL) =" << par.getDoubleValue(); break;
		}
	os << "\tMIN :"<<par.getMinimum()
		 << "\tMAX :"<<par.getMaximum()
		 << "\tINC :"<<par.getIncrement()
		 << "\tKey :"<<par.getKey()
		 << endl;
	return os;
}

#include "EditableParameters.h"
#include "EditableParameter.h"
#include "StiToolkit.h"
#include <stdexcept>

EditableParameters::EditableParameters()
  : Parameters(),
    Subject()
{}

EditableParameters::EditableParameters(const string & name, const string & description)
  : Parameters(name, description),
    Subject()
{}

EditableParameters::EditableParameters(const EditableParameters & parameter)
  : Parameters(parameter.getName(), parameter.getDescription()),
    Subject()
{}

EditableParameters::~EditableParameters()
{}

const EditableParameters & EditableParameters::operator=(const EditableParameters & parameter)
{
  this->Parameters::operator=(parameter);
  return *this;
}

void EditableParameters::add(const string & name, 
			     const string & description,
			     double value, 
			     double defaultValue, 
			     double min, 
			     double max,
			     double increment,
			     int    type,
			     int    key)
{
  cout << "EditableParameters::add(name,...)" << endl;
  Parameter * parameter = StiToolkit::instance()->getParameterFactory()->getObject();

  if (parameter)
    cout << "EditableParameters::add(name,...) - INFO - parameter OK" << endl;
  else
    {
      cout << "EditableParameters::add(name,...) - INFO - parameter NOK" << endl;
      return;
    }
  EditableParameter * editableParameter = static_cast<EditableParameter*>(parameter);
  if (editableParameter)
    {
      cout << "EditableParameters::add(name,...) - INFO - editableParameter OK" << endl;
      editableParameter->set(name,description,value,defaultValue,min,max,increment,type,key);
      this->Parameters::add(editableParameter);
    }
  else
    {
      cout << "EditableParameters::add(name,...) - INFO - editablearameter NOK" << endl;
      return;
      // throw runtime_error("EditableParameters::add() - ERROR - static_cast return null pointer");
    }
}

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
			     int    type)
{
  Parameter * parameter = StiToolkit::instance()->getParameterFactory()->getObject();
  EditableParameter * editableParameter = static_cast<EditableParameter*>(parameter);
  if (editableParameter)
    {
      editableParameter->set(name,description,value,defaultValue,min,max,increment,type);
      this->Parameters::add(editableParameter);
    }
  else
    throw runtime_error("EditableParameters::add() - ERROR - static_cast return null pointer");
}

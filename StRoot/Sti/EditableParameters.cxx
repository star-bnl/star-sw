#include <stdexcept>
#include "Sti/Base/Factory.h"
#include "Sti/Base/EditableParameter.h"
#include "Sti/EditableParameters.h"
#include "Sti/StiToolkit.h"

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
  //cout << "EditableParameters::add(name,...)" << endl;
  Parameter * parameter = StiToolkit::instance()->getParameterFactory()->getInstance();
  EditableParameter * editableParameter = static_cast<EditableParameter*>(parameter);
  if (editableParameter)
    {
      editableParameter->set(name,description,value,defaultValue,min,max,increment,type,key);
      this->Parameters::add(editableParameter);
    }
  else
		throw runtime_error("EditableParameters::add() - ERROR - static_cast return null pointer");
}

void EditableParameters::setDefaults()
{
  ParameterIterator iter;
  for (iter=begin();iter!=end();iter++)
    {
      EditableParameter * ep = static_cast<EditableParameter *>(*iter);
      if (ep)
	ep->reset();
    }
}


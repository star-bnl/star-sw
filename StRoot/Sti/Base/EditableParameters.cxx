#include <stdexcept>
#include "Sti/Base/Factory.h"
#include "Sti/Base/EditableParameter.h"
#include "Sti/Base/EditableParameters.h"

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
  //this->Parameters::operator=(parameter);
  return *this;
}

///Reset the parameter elements held by this container to their default value.
///Non editable parameters are simply skipped.
void EditableParameters::setDefaults()
{
  cout << "EditableParameters::setDefaults() -I- Started" << endl;
  ParameterIterator iter;
  for (iter=begin();iter!=end();iter++)
    {
      EditableParameter * ep = static_cast<EditableParameter *>(*iter);
      if (ep)
        ep->reset();
    }
  cout << *this;
  cout << "EditableParameters::setDefaults() -I- Started" << endl;
}


ostream& operator<<(ostream& os, const EditableParameters&pars)
{
  ParameterConstIterator iter;
  for (iter=pars.begin();iter!=pars.end();iter++)
    {
      EditableParameter * ep = static_cast<EditableParameter *>(*iter);
      if (ep) os << *ep;
    }
	return os;
}

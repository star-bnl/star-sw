#include "Parameters.h"
#include "Parameter.h"
#include "StiToolkit.h"
#include "Factory.h"
#include <stdexcept>

//Factory<Parameter> * Parameters::_factory = 0;

Parameters::Parameters()
  : Named("noName"),
    Described("noDesc")
{}

Parameters::Parameters(const string & name, const string & description)
  : Named(name),
    Described(description)
{}

Parameters::Parameters(const Parameters & parameter)
{
  ParameterConstIterator iter;
  for (iter=parameter.begin();
       iter!=parameter.end();
       iter++)
    {
      parameterVector.push_back(*iter);
    }
}

Parameters::~Parameters()
{
}

const Parameters & Parameters::operator=(const Parameters & parameter)
{
  ParameterConstIterator iter;
  for (iter=parameter.begin();
       iter!=parameter.end();
       iter++)
    {
      parameterVector.push_back(*iter);
    } 
  return *this;
}

void Parameters::add(const string & name, 
		     const string & description,
		     double value,
		     int type,
		     int key)
{
  Parameter * par = StiToolkit::instance()->getParameterFactory()->getInstance();
  par->set(name,description,value,type,key);
  add(par);
}

void Parameters::add(Parameter * parameter)
{
  parameterVector.push_back(parameter);
}

ParameterIterator Parameters::begin() 
{
  return parameterVector.begin();
}

ParameterIterator Parameters::end() 
{
  return parameterVector.end();
}

ParameterConstIterator Parameters::begin() const
{
  return parameterVector.begin();
}

ParameterConstIterator Parameters::end() const
{
  return parameterVector.end();
}

Parameter * Parameters::getParameter(const string name)
{
  ParameterIterator iter;
  for (iter=parameterVector.begin();
       iter!=parameterVector.end();
       iter++)
    {
      if ((*iter)->isName(name))
	return *iter;
    } 
  throw runtime_error("Parameters::getParameter(name) - ERROR - Requested name not found");
}

/*
void Parameters::setFactory(Factory<Parameter> * factory)
{
  _factory = factory;
}

Factory<Parameter> * Parameters::getFactory()
{
  return _factory;
}
*/

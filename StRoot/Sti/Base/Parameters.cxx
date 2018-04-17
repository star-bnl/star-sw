#include <cassert>
#include "Sti/Base/Parameter.h"
#include "Sti/Base/Factory.h"
#include "Sti/Base/Parameters.h"
#include <stdexcept>

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
	/*
  ParameterConstIterator iter;
  for (iter=parameter.begin();
       iter!=parameter.end();
       iter++)
    {
      parameterVector.push_back(*iter);
			}*/
}

Parameters::~Parameters()
{
}

const Parameters & Parameters::operator=(const Parameters & parameter)
{
  return *this;
}

Parameter * Parameters::add(Parameter * parameter)
{
  parameterVector.push_back(parameter);
  return parameter;
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
  assert(0);
}



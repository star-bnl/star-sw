#if !defined(PARAMETERS_H_INCLUDED_)
#define PARAMETERS_H_INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <vector>
using namespace std;

#include "Parameter.h"

typedef vector<Parameter*> ParameterVector;
typedef ParameterVector::iterator ParameterIterator;
typedef ParameterVector::const_iterator ParameterConstIterator;

class Parameters  : public Named, public Described
{
 public:

  Parameters();
  Parameters(const string & name, const string & description);
  Parameters(const Parameters & parameter);
  virtual ~Parameters();
  
  const Parameters & operator=(const Parameters & parameter);
  virtual void add(const string & name, 
		   const string & description,
		   double value,
		   int    type,
		   int    key);
  virtual void add(Parameter * parameter);
  Parameter * getParameter(const string name);
  ParameterIterator begin();
  ParameterIterator end();
  ParameterConstIterator begin() const;
  ParameterConstIterator end() const;
  virtual void initialize()=0;

 protected:
  
  ParameterVector parameterVector;
};


#endif // !defined(PARAMETERS_H_INCLUDED_)

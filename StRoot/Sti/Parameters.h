#if !defined(PARAMETERS_H_INCLUDED_)
#define PARAMETERS_H_INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <vector>
using namespace std;

#include "Parameter.h"
#include "Factory.h"

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
  bool isEnabled();
  bool isEditable();
  void setEnabled(bool value);
  void setEditable(bool value);

  //static void setFactory(Factory<Parameter> * factory);
  //static Factory<Parameter> * getFactory();

 protected:
  
  //static Factory<Parameter> * _factory;
  ParameterVector parameterVector; 
  bool _enabled;
  bool _editable;
};


/*! Get the "enabled" state of this object.
<p>
The "enabled" state dictates whether this object is available for use.
*/
inline bool Parameters::isEnabled()
{
	return _enabled;
}

/*! Get the "editable" state of this object.
<p>
The "editable" state dictates whether this object can be edited by user code, or via a GUI interface.
*/
inline bool Parameters::isEditable()
{
	return _editable;
}

/*! Set the "enabled" state of this object to the given value.
<p>
The "enabled" state dictates whether this object is available for use.
*/
inline void Parameters::setEnabled(bool value)
{
	_enabled = value;
}

/*! Set the "editable" state of this object.
<p>
The "editable" state dictates whether this object can be edited by user code, or via a GUI interface.
*/
inline void Parameters::setEditable(bool value)
{
	_editable = value;
}



#endif // !defined(PARAMETERS_H_INCLUDED_)

#if !defined(EDITABLEPARAMETERS_H_INCLUDED_)
#define EDITABLEPARAMETERS_H_INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
#include "Parameters.h"
#include "SubjectObserver.h"

class EditableParameters : public Parameters, public Subject
{
 public:

  EditableParameters();
  EditableParameters(const string & name, const string & description);
  EditableParameters(const EditableParameters & parameter);
  virtual ~EditableParameters();
  
  const EditableParameters & operator=(const EditableParameters & parameter);
  
  virtual void setDefaults()=0;

  virtual void add(const string & name, 
		   const string & description,
		   double value, 
		   double defaultValue, 
		   double min, 
		   double max,
		   double increment,
		   int    type);
};

#endif  // !defined(EDITABLEPARAMETERS_H_INCLUDED_)

#if !defined(EDITABLEPARAMETERS_H_INCLUDED_)
#define EDITABLEPARAMETERS_H_INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
#include "Sti/Base/Parameters.h"
#include "Sti/Base/SubjectObserver.h"

class EditableParameters : public Parameters, public Subject
{
 public:
  EditableParameters();
  EditableParameters(const string & name, const string & description);
  EditableParameters(const EditableParameters & parameter);
  virtual ~EditableParameters();
  const EditableParameters & operator=(const EditableParameters & parameter);
  virtual void setDefaults(); 
  friend ostream& operator<<(ostream& os, const EditableParameters&pars);
};

#endif  // !defined(EDITABLEPARAMETERS_H_INCLUDED_)

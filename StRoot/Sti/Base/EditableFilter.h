#ifndef StiRootSimpleTrackFilter_H_INCLUDED
#define StiRootSimpleTrackFilter_H_INCLUDED

/*! Pure virtual class defining an editable filter. */

#include "Sti/Base/Filter.h"
#include "Sti/Base/EditableParameters.h"

template <class Filtered>
class EditableFilter : public Filter<Filtered>, public EditableParameters
{
 public:
  
  EditableFilter();
  EditableFilter(const string & name, const string & description);
  virtual ~EditableFilter();
};

template <class Filtered>
EditableFilter<Filtered>::EditableFilter()
  :  Filter<Filtered>(),
     EditableParameters()
{}

template <class Filtered>
EditableFilter<Filtered>::EditableFilter(const string & name, const string & description)
  :  Filter<Filtered>(),
     EditableParameters(name,description)
{}

template <class Filtered>
EditableFilter<Filtered>::~EditableFilter()
{}

#endif

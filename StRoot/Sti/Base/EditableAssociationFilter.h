#ifndef EditableAssociationFilter_H_INCLUDED
#define EditableAssociationFilter_H_INCLUDED

/*! Pure virtual class defining an editable filter. */

#include "Sti/Base/AssociationFilter.h"
#include "Sti/Base/EditableParameters.h"

template <class Filtered>
class EditableAssociationFilter : public AssociationFilter<Filtered>, public EditableParameters
{
 public:
  
  EditableAssociationFilter();
  EditableAssociationFilter(const string & name, const string & description);
  virtual ~EditableAssociationFilter();
};

template <class Filtered>
EditableAssociationFilter<Filtered>::EditableAssociationFilter()
  :  AssociationFilter<Filtered>(),
     EditableParameters()
{}

template <class Filtered>
EditableAssociationFilter<Filtered>::EditableAssociationFilter(const string & name, const string & description)
  :  AssociationFilter<Filtered>(),
     EditableParameters(name,description)
{}

template <class Filtered>
EditableAssociationFilter<Filtered>::~EditableAssociationFilter()
{}

#endif

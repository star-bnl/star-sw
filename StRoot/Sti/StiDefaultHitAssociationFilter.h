#ifndef StiDefaultHitAssociationFilter_H_INCLUDED
#define StiDefaultHitAssociationFilter_H_INCLUDED
#include "Sti/Base/EditableAssociationFilter.h"
///\class StiDefaultHitAssociationFilter
///Class implementing an EditableAssociationFilter for the filtering of StiHit
///associations.
///Filtering is accomplished on the basis of the distance separating two hits
///in local cordinates.
class StiDefaultHitAssociationFilter : public EditableAssociationFilter<StiHit>
{
 public:
  
  StiDefaultHitAssociationFilter();
  StiDefaultHitAssociationFilter(const string & name, const string & description);
  virtual ~StiDefaultHitAssociationFilter();  
  bool accept(const StiHit * h1,const StiHit * h2) const;
  virtual void initialize();

 protected:
  double _maxDistance;
};
#endif

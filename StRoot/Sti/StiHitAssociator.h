#ifndef StiHitAssociator_H_INCLUDED
#define StiHitAssociator_H_INCLUDED
#include "StiHit.h"
#include "StiHitContainer.h"
template<class Filtered> class AssociationFilter;
typedef  map<StiHit*,StiHit*, AssociationFilter<StiHit> >  HitToHitMapType;

/*!
  Concrete class implementing a hit associator
*/
class StiHitAssociator 
{
 public:
  StiHitAssociator();
  virtual ~StiHitAssociator();
  virtual bool associate(StiHitContainer * evaluatedContainer,
			 StiHitContainer * referenceContainer,
			 AssociationFilter<StiHit> * associationFilter);
  virtual void reset();
  StiHit * getEvaluatedHit(StiHit * referenceHit);
  StiHit * getReferenceHit(StiHit * evaluatedHit);

  int getMatchedCount() const;
  int getUnmatchedReferenceCount() const;
  int getUnmatchedCount() const;
  HitToHitMapType::iterator    beginReference();
  HitToHitMapType::iterator    endReference();
  HitToHitMapType::iterator    beginEvaluated();
  HitToHitMapType::iterator    endEvaluated();

  HitToHitMapType::const_iterator    beginReference() const;
  HitToHitMapType::const_iterator    endReference() const;
  HitToHitMapType::const_iterator    beginEvaluated() const;
  HitToHitMapType::const_iterator    endEvaluated() const;

 protected:
  int _unmatchedEvaluatedCount;
  int _unmatchedReferenceCount;
  HitToHitMapType * _evalToRefMap;
  HitToHitMapType * _refToEvalMap;
};

#endif



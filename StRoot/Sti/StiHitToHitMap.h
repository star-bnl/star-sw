#ifndef StiHitToHitMap_H_INCLUDED
#define StiHitToHitMap_H_INCLUDED
#include "StiHitContainer.h"
#include "AssociationFilter.h"
typedef map<StiHit*,StiHit*> HitToHitMap;

class StiHitToHitMap : public HitToHitMap
{
 public:

  StiHitToHitMap();
  virtual ~StiHitToHitMap();
  void build(StiHitContainer * firstContainer,
	     StiHitContainer * secondContainer,
	     AssociationFilter<StiHit> * associationFilter);
  
};


#endif

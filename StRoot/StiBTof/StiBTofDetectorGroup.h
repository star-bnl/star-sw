#ifndef StiBTofDetectorGroup_H_INCLUDED
#define StiBTofDetectorGroup_H_INCLUDED

#include "Sti/StiDetectorGroup.h"
class StEvent;
class StiBTofDetectorGroup : public StiDetectorGroup<StEvent>
{
  public:
    StiBTofDetectorGroup(Bool_t active);
    ~StiBTofDetectorGroup() {}
    ClassDef(StiBTofDetectorGroup,0)
};

#endif


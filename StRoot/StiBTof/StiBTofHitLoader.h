#ifndef StiBTofHitLoader_H
#define StiBTofHitLoader_H
#include "Sti/StiHitLoader.h"

class StEvent;
class StiDetectorBuilder;
class StTpcHit;
class StiBTofHitLoader : public StiHitLoader<StEvent,StiDetectorBuilder> {
 public:
  StiBTofHitLoader() : StiHitLoader<StEvent,StiDetectorBuilder>("BTofHitLoader") {}
  StiBTofHitLoader(StiHitContainer * hitContainer, Factory<StiHit> * hitFactory, StiDetectorBuilder * detector)
    : StiHitLoader<StEvent,StiDetectorBuilder>("BTofHitLoader",hitContainer,hitFactory,detector) {}
  virtual ~StiBTofHitLoader() {}
  virtual void loadHits(StEvent* source,
                        Filter<StiTrack> * trackFilter,
                        Filter<StiHit> * hitFilter);
  ClassDef(StiBTofHitLoader,0)
};
#endif

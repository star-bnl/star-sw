#ifndef StiSsdHitLoader_H
#define StiSsdHitLoader_H


template<class Factorized>class Factory;
class StiHit;
class StiHitContainer;
class StEvent;
class StiGeometryTransform;

/*! \class StiSsdHitLoader
  StiSsdHitLoader is a concrete class implementing the StiHitLoader abstract
  interface. It is used to load hits from Star StEvent into the StiHitContainer
  for Sti tracking. StEvent hits from the Ssd are converted using the 
  StiGeometryTransform class.
  <p>
  This class is essentially morphed from the class StiHitFiller 
  originally written by Mike Miller.

  \author Claude A Pruneau (Wayne) and M.L. Miller (Yale Software)
 */
class StiSsdHitLoader : public StiHitLoader<StEvent,StiGeometryTransform>
{
public:

    StiSsdHitLoader();
    StiSsdHitLoader(StiHitContainer* hitContainer,
      Factory<StiHit>*hitFactory,
      Transform*transform);
    virtual ~StiSsdHitLoader();
    virtual void loadHits(StEvent* source);
};

StiSsdHitLoader::StiSsdHitLoader()
  : StiHitLoader<StEvent,StiGeometryTransform>()
{}
    
StiSsdHitLoader::StiSsdHitLoader(StiHitContainer* hitContainer,
      Factory<StiHit>*hitFactory,
      StiGeometryTransform*transform)
  : StiHitLoader<StEvent,StiGeometryTransform>(hitContainer,hitFactory,transform)
{}

StiSsdHitLoader::~StiSsdHitLoader()
{}

void StiSsdHitLoader::loadHits(StEvent* source)
{
}


#endif

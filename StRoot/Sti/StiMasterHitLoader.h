#ifndef StiMasterHitLoader_H
#define StiMasterHitLoader_H

#include <stdexcept>
#include <vector>
#include "StiHitLoader.h"

/*! \class StiMasterHitLoader
  StiMasterHitLoader is an implementation of the abstract interface 
  StiMasterHitLoader designed to enable hit load for a variety of containers
  sequentially. The sources are assumed to be of same type e.g. StEvent
  but of various sources e.g. Tpc, Svt, etc. StiMasterHitLoader is
  actually acting as a broker: it sequentially invokes the actual
  loaders that are registered with it.
  <p>
  Actual loaders must be implemented in class deriving from 
  StiMasterHitLoader. They are registered at startup time with this 
  broker using the "addLoader" method.
  <p>
  Note that this class is templated in the same way the base class 
  StiMasterHitLoader is so as to enable hit loading from potentially
  diverse sources.
  
  \author Claude A Pruneau (Wayne)
 */
template<class Source,class Transform>
class StiMasterHitLoader : public StiHitLoader<Source,Transform>
{
public:

    StiMasterHitLoader();
    StiMasterHitLoader(StiHitContainer* hitContainer,
      Factory<StiHit>*hitFactory,
      Transform*transform);
    virtual ~StiMasterHitLoader();
    void addLoader(StiHitLoader<Source,Transform>*loader);
    void loadHits(Source *source);
    void setHitContainer(StiHitContainer* hitContainer);
    void setHitFactory(Factory<StiHit>*hitFactory);
    void setGeometryTransform(Transform*transform);

protected:
    typedef vector<StiHitLoader<Source,Transform>*>  HitLoaderVector;
    typedef HitLoaderVector::iterator HitLoaderIter;
    typedef HitLoaderVector::const_iterator HitLoaderConstIter;
    Transform       * _transfrom;
    StiHitContainer * _hitContainer;
    Factory<StiHit> * _hitFactory;
    HitLoaderVector _hitLoaders;
};

template<class Source,class Transform>
StiMasterHitLoader<Source,Transform>::StiMasterHitLoader()
  : StiHitLoader<Source,Transform>()
{}
    
template<class Source,class Transform>
StiMasterHitLoader<Source,Transform>::StiMasterHitLoader(StiHitContainer* hitContainer,
      Factory<StiHit>*hitFactory,
      Transform*transform)
  : StiHitLoader<Source,Transform>(hitContainer,hitFactory,transform)
{}

template<class Source,class Transform>
StiMasterHitLoader<Source,Transform>::~StiMasterHitLoader()
{}

template<class Source,class Transform>   

void StiMasterHitLoader<Source,Transform>::addLoader(StiHitLoader<Source,Transform>*loader)
{
   _hitLoaders.push_back(loader); 
}

template<class Source,class Transform>
void StiMasterHitLoader<Source,Transform>::loadHits(Source *source)
{
  HitLoaderConstIter iter;
  for (iter=_hitLoaders.begin();iter!=_hitLoaders.end();iter++)
    (*iter)->loadHits(source);
}

template<class Source,class Transform>
void StiMasterHitLoader<Source,Transform>::setHitContainer(StiHitContainer* hitContainer)
{
  HitLoaderIter iter;
  for (iter=_hitLoaders.begin();iter!=_hitLoaders.end();iter++)
    (*iter)->setHitContainer(hitContainer);
}
    
template<class Source,class Transform>
void StiMasterHitLoader<Source,Transform>::setHitFactory(Factory<StiHit>*hitFactory)
{
  HitLoaderIter iter;
  for (iter=_hitLoaders.begin();iter!=_hitLoaders.end();iter++)
    (*iter)->setHitFactory(hitFactory);
}

template<class Source,class Transform>
void StiMasterHitLoader<Source,Transform>::setGeometryTransform(Transform*transform)
{
  HitLoaderIter iter;
  for (iter=_hitLoaders.begin();iter!=_hitLoaders.end();iter++)
    (*iter)->setGeometryTransform(transform);
}

#endif

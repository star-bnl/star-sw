#ifndef StiMasterHitLoader_H
#define StiMasterHitLoader_H

#include <stdexcept>
#include <vector>
#include "StiHitLoader.h"

/*! \class StiMasterHitLoader
  StiMasterHitLoader is an implementation of the abstract interface 
  StiHitLoader designed to enable hit load for a variety of containers
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
  StiHitLoader is so as to enable hit loading from potentially
  diverse sources.
  
  \author Claude A Pruneau (Wayne)
 */
template<class Source,class Detector>
class StiMasterHitLoader : public StiHitLoader<Source,Detector>
{
public:

    StiMasterHitLoader();
    StiMasterHitLoader(const string& name,StiHitContainer* hitContainer,
		       Factory<StiHit>*hitFactory,
		       Detector*transform);
    virtual ~StiMasterHitLoader();
    void addLoader(StiHitLoader<Source,Detector>*loader);
    void loadHits(Source *source);
    void setHitContainer(StiHitContainer* hitContainer);
    void setHitFactory(Factory<StiHit>*hitFactory);
    virtual void setDetector(Detector*detector);
    
protected:
    typedef vector<StiHitLoader<Source,Detector>*>  HitLoaderVector;
    typedef HitLoaderVector::iterator HitLoaderIter;
    typedef HitLoaderVector::const_iterator HitLoaderConstIter;
    Detector       * _transfrom;
    StiHitContainer * _hitContainer;
    Factory<StiHit> * _hitFactory;
    HitLoaderVector _hitLoaders;
};

template<class Source,class Detector>
StiMasterHitLoader<Source,Detector>::StiMasterHitLoader()
  : StiHitLoader<Source,Detector>("MasterHitLoader",0,0,0)
{}

template<class Source,class Detector>
StiMasterHitLoader<Source,Detector>::StiMasterHitLoader(const string& name,
							StiHitContainer* hitContainer,
							Factory<StiHit>*hitFactory,
							Detector*transform)
  : StiHitLoader<Source,Detector>(name,hitContainer,hitFactory,transform)
{}

template<class Source,class Detector>
StiMasterHitLoader<Source,Detector>::~StiMasterHitLoader()
{}

template<class Source,class Detector>   

void StiMasterHitLoader<Source,Detector>::addLoader(StiHitLoader<Source,Detector>*loader)
{
   _hitLoaders.push_back(loader); 
}

template<class Source,class Detector>
void StiMasterHitLoader<Source,Detector>::loadHits(Source *source)
{
  HitLoaderConstIter iter;
  for (iter=_hitLoaders.begin();iter!=_hitLoaders.end();iter++)
    (*iter)->loadHits(source);
}

template<class Source,class Detector>
void StiMasterHitLoader<Source,Detector>::setHitContainer(StiHitContainer* hitContainer)
{
  HitLoaderIter iter;
  for (iter=_hitLoaders.begin();iter!=_hitLoaders.end();iter++)
    (*iter)->setHitContainer(hitContainer);
}
    
template<class Source,class Detector>
void StiMasterHitLoader<Source,Detector>::setHitFactory(Factory<StiHit>*hitFactory)
{
  HitLoaderIter iter;
  for (iter=_hitLoaders.begin();iter!=_hitLoaders.end();iter++)
    (*iter)->setHitFactory(hitFactory);
}

template<class Source,class Detector>
void StiMasterHitLoader<Source,Detector>::setDetector(Detector*transform)
{
  throw runtime_error("StiMasterHitLoader<Source,Detector>::setDetector(Detector*) - This call is Forbiden in StiMasterHitLoader");
}

#endif

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
template<class Source1, class Source2,class Detector>
class StiMasterHitLoader : public StiHitLoader<Source1, Source2,Detector>,
                           public vector< StiHitLoader<Source1, Source2,Detector> *>
{
public:

    StiMasterHitLoader();
    StiMasterHitLoader(const string& name,
		       StiHitContainer* hitContainer,
		       StiHitContainer* mcHitContainer,
		       Factory<StiHit>*hitFactory,
		       Detector*transform);
    virtual ~StiMasterHitLoader();
    void addLoader(StiHitLoader<Source1, Source2,Detector>*loader); 
    void loadEvent(Source1 *source1, 
		   Source2 *source2,
		   Filter<StiTrack> * trackFilter, 
		   Filter<StiHit>   * hitFilter);
    void setHitContainer(StiHitContainer* hitContainer);
    void setMcHitContainer(StiHitContainer* hitContainer);
    void setHitFactory(Factory<StiHit>*hitFactory);
    virtual void setDetector(Detector*detector);
    virtual void setUseMcAsRec(bool value);
protected:
    typedef StiHitLoader<Source1,Source2,Detector>* HitLoaderKey;
    typedef vector< HitLoaderKey >  HitLoaderVector;
    typedef HitLoaderVector::iterator HitLoaderIter;
    typedef HitLoaderVector::const_iterator HitLoaderConstIter;
    //HitLoaderVector _hitLoaders;
};

template<class Source1, class Source2,class Detector>
StiMasterHitLoader<Source1, Source2,Detector>::StiMasterHitLoader()
  : StiHitLoader<Source1, Source2,Detector>("MasterHitLoader",0,0,0)
{}

template<class Source1, class Source2,class Detector>
StiMasterHitLoader<Source1, Source2,Detector>::StiMasterHitLoader(const string& name,
							StiHitContainer* hitContainer,
							StiHitContainer* mcHitContainer,
							Factory<StiHit>*hitFactory,
							Detector*transform)
  : StiHitLoader<Source1, Source2,Detector>(name,hitContainer,mcHitContainer,hitFactory,transform)
{}

template<class Source1, class Source2,class Detector>
StiMasterHitLoader<Source1, Source2,Detector>::~StiMasterHitLoader()
{}

template<class Source1, class Source2,class Detector>   

void StiMasterHitLoader<Source1, Source2,Detector>::addLoader(StiHitLoader<Source1, Source2,Detector>*loader)
{
  push_back(loader); 
}

template<class Source1, class Source2,class Detector>
void StiMasterHitLoader<Source1, Source2,Detector>::loadEvent(Source1 *source1, 
							      Source2 * source2,
							      Filter<StiTrack> * trackFilter, 
							      Filter<StiHit>   * hitFilter)
{
  if(!_hitContainer)
    throw runtime_error("StiMasterHitLoader::loadEvent( ) -F- _hitContainer==0");
  _hitContainer->clear();
  if (source2)
    {
      if(!_mcHitContainer)
	throw runtime_error("StiMasterHitLoader::loadEvent( ) -F- _hitContainer==0");
      _mcHitContainer->clear();
    }
  HitLoaderConstIter iter;
  for (iter=begin();iter!=end();iter++)
    (*iter)->loadEvent(source1,source2,trackFilter, hitFilter); 
  _hitContainer->sortHits();
  _hitContainer->reset();//declare all hits as unused...
  if (source2)
    {
      _mcHitContainer->sortHits(); 
      _mcHitContainer->reset();
    }
}

template<class Source1, class Source2,class Detector>
void StiMasterHitLoader<Source1, Source2,Detector>::setHitContainer(StiHitContainer* hitContainer)
{
  _hitContainer = hitContainer;
  HitLoaderIter iter;
  for (iter=begin();iter!=end();iter++)
    (*iter)->setHitContainer(hitContainer);
}
    
template<class Source1, class Source2,class Detector>
void StiMasterHitLoader<Source1, Source2,Detector>::setMcHitContainer(StiHitContainer* mcHitContainer)
{
  _mcHitContainer = mcHitContainer;
  HitLoaderIter iter;
  for (iter=begin();iter!=end();iter++)
    (*iter)->setMcHitContainer(mcHitContainer);
}
    
template<class Source1, class Source2,class Detector>
void StiMasterHitLoader<Source1, Source2,Detector>::setHitFactory(Factory<StiHit>*hitFactory)
{
  HitLoaderIter iter;
  for (iter=begin();iter!=end();iter++)
    (*iter)->setHitFactory(hitFactory);
}

template<class Source1, class Source2,class Detector>
void StiMasterHitLoader<Source1, Source2,Detector>::setDetector(Detector*transform)
{
  throw runtime_error("StiMasterHitLoader<Source1, Source2,Detector>::setDetector(Detector*) - This call is Forbiden in StiMasterHitLoader");
}


template<class Source1, class Source2,class Detector>
void StiMasterHitLoader<Source1, Source2,Detector>::setUseMcAsRec(bool value)
{
  _useMcAsRec = value; 
  HitLoaderIter iter;
  for (iter=begin();iter!=end();iter++)
    (*iter)->setUseMcAsRec(value);
}

#endif

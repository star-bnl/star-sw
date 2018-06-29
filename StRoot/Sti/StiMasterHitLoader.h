#ifndef StiMasterHitLoader_H
#define StiMasterHitLoader_H

#include <cassert>
#include <stdexcept>
#include <vector>
#include "StiHitLoader.h"
#include "Sti/StiTrackContainer.h"
#include "StEvent.h"
#include "StEventTypes.h"

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
template<class Source1, class Detector>
class StiMasterHitLoader : public StiHitLoader<Source1, Detector>,
public vector< StiHitLoader<Source1, Detector> *>
{
public:

  StiMasterHitLoader();
  StiMasterHitLoader(const string& name,
                     StiHitContainer* hitContainer,
                     Factory<StiHit>*hitFactory,
                     Detector*transform);
  virtual ~StiMasterHitLoader();
  void addLoader(StiHitLoader<Source1, Detector>*loader);
  void loadEvent(Source1 *source1,
                 Filter<StiTrack> * trackFilter,
                 Filter<StiHit>   * hitFilter);
  void setHitContainer(StiHitContainer* hitContainer);
  void setHitFactory(Factory<StiHit>*hitFactory);
  virtual void setDetector(Detector*detector);
protected:
    typedef StiHitLoader<Source1,Detector>* HitLoaderKey;
  typedef vector< HitLoaderKey >  HitLoaderVector;
  typedef typename HitLoaderVector::iterator HitLoaderIter;
  typedef typename HitLoaderVector::const_iterator HitLoaderConstIter;
  //HitLoaderVector _hitLoaders;
};

template<class Source1, class Detector>
StiMasterHitLoader<Source1,Detector>::StiMasterHitLoader()
: StiHitLoader<Source1, Detector>("MasterHitLoader",0,0,0)
{}

template<class Source1,class Detector>
StiMasterHitLoader<Source1,Detector>::StiMasterHitLoader(const string& name,
                                                                  StiHitContainer* hitContainer,
                                                                  Factory<StiHit>*hitFactory,
                                                                  Detector*transform)
: StiHitLoader<Source1, Detector>(name,hitContainer,hitFactory,transform)
{}

template<class Source1,class Detector>
StiMasterHitLoader<Source1, Detector>::~StiMasterHitLoader()
{}

template<class Source1,class Detector>
void StiMasterHitLoader<Source1, Detector>::addLoader(StiHitLoader<Source1, Detector>*loader)
{
  this->push_back(loader);
}

template<class Source1, class Detector>
void StiMasterHitLoader<Source1, Detector>::loadEvent(Source1 *source1,
                                                              Filter<StiTrack> * trackFilter,
                                                              Filter<StiHit>   * hitFilter)
{
  assert(this->_hitContainer);
  this->_hitContainer->clear();
  HitLoaderConstIter iter;
  for (iter=this->begin();iter!=this->end();iter++)	{
    unsigned nHitsBeforeLoad = this->_hitContainer->size();
    (*iter)->loadHits(source1,trackFilter, hitFilter);
    LOG_INFO << "StiMasterHitLoader::loadEvent() - " 
    << (*iter)->getName() << " loaded "
    << this->_hitContainer->size() - nHitsBeforeLoad
    << " hits" << endm;
  }
  this->_hitContainer->sortHits();
  this->_hitContainer->reset();//declare all hits as unused...
}

template<class Source1, class Detector>
void StiMasterHitLoader<Source1, Detector>::setHitContainer(StiHitContainer* hitContainer)
{
  this->_hitContainer = hitContainer;
  HitLoaderIter iter;
  for (iter=this->begin();iter!=this->end();iter++)
    (*iter)->setHitContainer(hitContainer);
}


template<class Source1, class Detector>
void StiMasterHitLoader<Source1,Detector>::setHitFactory(Factory<StiHit>*hitFactory)
{
  HitLoaderIter iter;
  for (iter=this->begin();iter!=this->end();iter++)
    (*iter)->setHitFactory(hitFactory);
}

template<class Source1,class Detector>
void StiMasterHitLoader<Source1,Detector>::setDetector(Detector*transform)
{
  cout << "StiMasterHitLoader<Source1,Detector>::setDetector(Detector*) - This call is Forbiden in StiMasterHitLoader" << endl;
  assert(0);
}


#endif



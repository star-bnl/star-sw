#ifndef StiMasterHitLoader_H
#define StiMasterHitLoader_H

#include <stdexcept>
#include <vector>
#include "StiHitLoader.h"
#include "StMcTrack.hh"
#include "StMcTpcHit.hh"
#include "Sti/StiMcTrack.h"
#include "Sti/StiTrackContainer.h"
#include "StEvent.h"
#include "StMcEvent.hh"
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
  void loadAllMcHits(StMcEvent* source,
                  bool useMcAsRec,
                  Filter<StiTrack> * trackFilter,
                  Filter<StiHit> * hitFilter);
  void setHitContainer(StiHitContainer* hitContainer);
  void setMcHitContainer(StiHitContainer* hitContainer);
  void setHitFactory(Factory<StiHit>*hitFactory);
  virtual void setDetector(Detector*detector);
  virtual void setUseMcAsRec(bool value);
protected:
    typedef StiHitLoader<Source1,Source2,Detector>* HitLoaderKey;
  typedef vector< HitLoaderKey >  HitLoaderVector;
  typedef typename HitLoaderVector::iterator HitLoaderIter;
  typedef typename HitLoaderVector::const_iterator HitLoaderConstIter;
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
    (*iter)->loadHits(source1,trackFilter, hitFilter);
  _hitContainer->sortHits();
  _hitContainer->reset();//declare all hits as unused...
    if (source2)
      {
      loadAllMcHits(source2,_useMcAsRec,trackFilter,hitFilter);
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

template<class Source1, class Source2,class Detector>
void StiMasterHitLoader<Source1, Source2,Detector>::loadAllMcHits(StMcEvent* source,
                                                               bool useMcAsRec,
                                                               Filter<StiTrack> * trackFilter,
                                                               Filter<StiHit> * hitFilter)
{
  cout << "StiMasterHitLoader::loadMcHits(StEvent*) -I- Started" << endl;
  if(!_mcHitContainer)    throw runtime_error("StiMasterHitLoader::loadMcHits(StEvent*) -F- _mcHitContainer==0");
  if(!_mcTrackFactory)    throw runtime_error("StiMasterHitLoader::loadMcHits() -F- _mcTrackFactory==0");
  if (!_mcTrackContainer) throw runtime_error("StiMasterHitLoader::loadMcHitss() -F- _mcTrackContainer==0");
  if(!_hitFactory)        throw runtime_error("StiMasterHitLoader::loadMcHits(StMcEvent*) -F- _hitFactory==0");
  cout << "StiMasterHitLoader::loadMcHits() -I- Loading"<<endl;
  StSPtrVecMcTrack & stMcTracks = source->tracks();
  StiMcTrack * stiMcTrack;
  StMcTrack  * stMcTrack;
  int nPlusTracks  = 0;
  int nMinusTracks = 0;
  for ( StMcTrackConstIterator iter=stMcTracks.begin();iter!=stMcTracks.end();++iter)
    {
    stMcTrack = *iter;
    stiMcTrack = _mcTrackFactory->getInstance();
    stiMcTrack->reset();
    stiMcTrack->setStMcTrack( stMcTrack );
    if (!trackFilter || trackFilter->filter( stiMcTrack) )
      {
      _mcTrackContainer->add( stiMcTrack);
      for (HitLoaderConstIter iLoader=begin();iLoader!=end();++iLoader)
        (*iLoader)->loadMcHits(source,useMcAsRec,trackFilter, hitFilter, *stMcTrack, *stiMcTrack);
      if (stiMcTrack->getCharge()>0)
        ++nPlusTracks;
      else
        ++nMinusTracks;
      }
    }
  cout << "StiMasterHitLoader::loadMcHits() -I- Event Loaded"<< endl
    << "============================================================================="<< endl
    << "      hitContainer size = " << _hitContainer->size()<<endl
    << "  mc  hitContainer size = " << _mcHitContainer->size()<<endl
    << " mc track Container size= " << _mcTrackContainer->size()<<endl
    << "               nTracks  = " << nPlusTracks+nMinusTracks << endl
    << "           nPlusTracks  = " << nPlusTracks << endl
    << "          nMinusTracks  = " << nMinusTracks << endl;
  cout << "StiMasterHitLoader::loadMcHits(StEvent*) -I- Done" << endl;
}

#endif



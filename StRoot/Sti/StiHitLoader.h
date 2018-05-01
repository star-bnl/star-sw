#ifndef StiHitLoader_H
#define StiHitLoader_H

#include "Riostream.h"

#include "Sti/Base/Factory.h"
#include "Sti/Base/Named.h"
#include "Sti/Base/Filter.h"
#include "Sti/StiToolkit.h"
#include "Sti/StiHitContainer.h"
class StiHit;
class StiTrack;
class StiHitContainer;
class StiTrackContainer;

/*!
  \class StiHitLoader
  StiHitLoader is an abstract interface defining a mechanism to load
  hits in Sti containers from an external source or package, "Source". The external
  source is templated in this base class so the load mechanism could in principle
  be exported to any experimental environment. External hits are converted
  to StiHit with a templated transform function "Transform"
  <p>
  This base class holds pointers to the destination container, the
  factory used to get instance of StiHit, and the tranformation utility 
  to effect the transformation between the external hits and the StiHit
  objects used in this package.
  <p>
  This class is essentially morphed from the class StiHitFiller 
  originally written by Mike Miller.
  
  \author Claude A Pruneau (Wayne) and M.L. Miller (Yale Software)
*/

template <class Source1, class Detector>
class StiHitLoader : public Named
{
 public:
  
  StiHitLoader(const string & name);
  StiHitLoader(const string & name,
	       StiHitContainer* hitContainer,
	       Factory<StiHit>*hitFactory,
	       Detector * detector);
  virtual ~StiHitLoader();
  virtual void loadEvent(Source1 *source1, 
			 Filter<StiTrack> * trackFilter, 
			 Filter<StiHit>   * hitFilter);
  virtual void loadHits(Source1 * source,	
			Filter<StiTrack> * trackFilter, 
			Filter<StiHit>   * hitFilter); 
  virtual void setHitContainer(StiHitContainer* hitContainer);
  virtual void setHitFactory(Factory<StiHit>*hitFactory);
  virtual void setDetector(Detector*detector);
          void setMaxTimes(int nMaxTimes){_hitContainer->setMaxTimes(nMaxTimes);}
  virtual Detector* getDetector(); 
 protected:
  StiHitContainer     * _hitContainer;
  StiTrackContainer   * _trackContainer;
  Factory<StiHit>     * _hitFactory;
  Factory<StiKalmanTrack> * _trackFactory;
  Detector            * _detector;
};

template<class Source1, class Detector>
StiHitLoader<Source1, Detector>::StiHitLoader(const string & name)
  :  Named(name),
     _hitContainer(StiToolkit::instance()->getHitContainer()),
     _trackContainer(StiToolkit::instance()->getTrackContainer()),
     _hitFactory(StiToolkit::instance()->getHitFactory()),
     _trackFactory(StiToolkit::instance()->getTrackFactory()),
     _detector(0)
{}
    
template<class Source1, class Detector>
StiHitLoader<Source1, Detector>::StiHitLoader(const string & name,
					    StiHitContainer* hitContainer,
					    Factory<StiHit>*hitFactory,
					    Detector * detector)
  :  Named(name),
     _hitContainer(hitContainer),
     _trackContainer(StiToolkit::instance()->getTrackContainer()),
     _hitFactory(hitFactory),
     _trackFactory(StiToolkit::instance()->getTrackFactory()),
     _detector(detector)
{}

template<class Source1, class Detector>StiHitLoader<Source1, Detector>::~StiHitLoader(){}

template<class Source1, class Detector>
void StiHitLoader<Source1, Detector>::loadEvent(Source1 *source1, 
							Filter<StiTrack> * trackFilter, 
							Filter<StiHit> * hitFilter)
{
}

template<class Source1, class Detector>
void StiHitLoader<Source1,Detector>::setHitContainer(StiHitContainer* hitContainer)
{
  _hitContainer = hitContainer;
}
 
    
template<class Source1, class Detector>
void StiHitLoader<Source1,Detector>::setHitFactory(Factory<StiHit>*hitFactory)
{
  _hitFactory = hitFactory;
}

template<class Source1,class Detector>
void StiHitLoader<Source1,Detector>::setDetector(Detector *detector)
{
  _detector = detector;
}

template<class Source1,class Detector>
Detector* StiHitLoader<Source1,Detector>::getDetector()
{
  return _detector;
}


template<class Source1,class Detector>
void StiHitLoader<Source1,Detector>::loadHits(Source1 * source,
						       Filter<StiTrack> * trackFilter, 
						       Filter<StiHit> * hitFilter)
{
}

#endif


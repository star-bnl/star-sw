#ifndef StiHitLoader_H
#define StiHitLoader_H

#include "Sti/Base/Factory.h"
#include "Sti/Base/Messenger.h"
#include "Sti/Base/Named.h"
#include "Sti/StiDetectorFinder.h"
#include "Sti/StiToolkit.h"
class StiHit;
class StiHitContainer;

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

template <class Source, class Detector>
class StiHitLoader : public Named
{
 public:
  
  StiHitLoader(const string & name);
  StiHitLoader(const string & name,
	       StiHitContainer* hitContainer,
	       Factory<StiHit>*hitFactory,
	       Detector * detector);
  virtual ~StiHitLoader();
  virtual void loadHits(Source *source)=0;
  virtual void setHitContainer(StiHitContainer* hitContainer);
  virtual void setHitFactory(Factory<StiHit>*hitFactory);
  virtual void setDetector(Detector*detector);
  virtual Detector* getDetector();
 protected:
  StiHitContainer   * _hitContainer;
  Factory<StiHit>   * _hitFactory;
  Detector          * _detector;
  StiDetectorFinder * _detectorFinder;
  Messenger &         _messenger;
  
};

template<class Source, class Detector>
StiHitLoader<Source,Detector>::StiHitLoader(const string & name)
  :  Named(name),
     _hitContainer(StiToolkit::instance()->getHitContainer()),
     _hitFactory(StiToolkit::instance()->getHitFactory()),
     _detector(0),
     _detectorFinder(StiDetectorFinder::instance()),
     _messenger(*(Messenger::instance(MessageType::kHitMessage)))
{}
    
template<class Source, class Detector>
StiHitLoader<Source,Detector>::StiHitLoader(const string & name,
					    StiHitContainer* hitContainer,
					    Factory<StiHit>*hitFactory,
					    Detector * detector)
  :  Named(name),
     _hitContainer(hitContainer),
     _hitFactory(hitFactory),
     _detector(detector),
     _detectorFinder(StiDetectorFinder::instance()),
     _messenger( *(Messenger::instance(MessageType::kHitMessage)) )
{}

template<class Source, class Detector>
StiHitLoader<Source,Detector>::~StiHitLoader()
{}

template<class Source, class Detector>
void StiHitLoader<Source,Detector>::setHitContainer(StiHitContainer* hitContainer)
{
  _hitContainer = hitContainer;
}
    
template<class Source, class Detector>
void StiHitLoader<Source,Detector>::setHitFactory(Factory<StiHit>*hitFactory)
{
  _hitFactory = hitFactory;
}

template<class Source, class Detector>
void StiHitLoader<Source,Detector>::setDetector(Detector *detector)
{
  _detector = detector;
}

template<class Source, class Detector>
Detector* StiHitLoader<Source,Detector>::getDetector()
{
  return _detector;
}

#endif

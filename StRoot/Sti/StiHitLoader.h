#ifndef StiHitLoader_H
#define StiHitLoader_H

#include "Messenger.h"
#include "StiDetectorFinder.h"

template<class Factorized>class Factory;
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
;
template <class Source, class Transform>
class StiHitLoader
{
 public:
  
  StiHitLoader();
  StiHitLoader(StiHitContainer* hitContainer,
	       Factory<StiHit>*hitFactory,
	       Transform*transform);
  virtual ~StiHitLoader();
  virtual void loadHits(Source *source)=0;
  virtual void setHitContainer(StiHitContainer* hitContainer);
  virtual void setHitFactory(Factory<StiHit>*hitFactory);
  virtual void setGeometryTransform(Transform*transform);
  
 protected:
  Transform         * _transform;
  StiHitContainer   * _hitContainer;
  Factory<StiHit>   * _hitFactory;
  StiDetectorFinder * _detectorFinder;
  Messenger &         _messenger;
  
};

template<class Source,class Transform>
StiHitLoader<Source,Transform>::StiHitLoader()
  :  _transform(0),
     _hitContainer(0),
     _hitFactory(0),
     _detectorFinder(StiDetectorFinder::instance()),
     _messenger(*(Messenger::instance(MessageType::kHitMessage)))
{}
    
template<class Source,class Transform>
StiHitLoader<Source,Transform>::StiHitLoader(StiHitContainer* hitContainer,
					     Factory<StiHit>*hitFactory,
					     Transform*transform)
  :  _transform(transform),
     _hitContainer(hitContainer),
     _hitFactory(hitFactory),
     _detectorFinder(StiDetectorFinder::instance()),
     _messenger( *(Messenger::instance(MessageType::kHitMessage)) )
{}

template<class Source,class Transform>
StiHitLoader<Source,Transform>::~StiHitLoader()
{}

template<class Source,class Transform>
void StiHitLoader<Source,Transform>::setHitContainer(StiHitContainer* hitContainer)
{
  _hitContainer = hitContainer;
}
    
template<class Source,class Transform>
void StiHitLoader<Source,Transform>::setHitFactory(Factory<StiHit>*hitFactory)
{
  _hitFactory = hitFactory;
}

template<class Source,class Transform>
void StiHitLoader<Source,Transform>::setGeometryTransform(Transform*transform)
{
  _transform = transform;
}



#endif

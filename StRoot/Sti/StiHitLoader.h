#ifndef StiHitLoader_H
#define StiHitLoader_H

#include "Sti/Base/Factory.h"
#include "Sti/Base/Messenger.h"
#include "Sti/Base/Named.h"
#include "Sti/Base/Filter.h"
#include "Sti/StiDetectorFinder.h"
#include "Sti/StiToolkit.h"
class StiHit;
class StiTrack;
class StiMcTrack;
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

template <class Source1, class Source2, class Detector>
class StiHitLoader : public Named
{
 public:
  
  StiHitLoader(const string & name);
  StiHitLoader(const string & name,
	       StiHitContainer* hitContainer,
	       StiHitContainer* mcHitContainer,
	       Factory<StiHit>*hitFactory,
	       Detector * detector);
  virtual ~StiHitLoader();
  virtual void loadEvent(Source1 *source1, 
			 Source2 *source2,	
			 Filter<StiTrack> * trackFilter, 
			 Filter<StiHit>   * hitFilter);
  virtual void loadHits(Source1 * source,	
			Filter<StiTrack> * trackFilter, 
			Filter<StiHit>   * hitFilter); 
  virtual void loadMcHits(Source2 * source,
			  bool useMcAsRec,	
			  Filter<StiTrack> * trackFilter, 
			  Filter<StiHit>   * hitFilter);
  virtual void setHitContainer(StiHitContainer* hitContainer);
  virtual void setMcHitContainer(StiHitContainer* hitContainer);
  virtual void setHitFactory(Factory<StiHit>*hitFactory);
  virtual void setDetector(Detector*detector);
  virtual Detector* getDetector(); 
  virtual void setUseMcAsRec(bool value);
  virtual bool useMcAsRec() const;
 protected:
  StiHitContainer     * _hitContainer;
  StiHitContainer     * _mcHitContainer;
  StiTrackContainer   * _trackContainer;
  StiTrackContainer   * _mcTrackContainer;
  Factory<StiHit>     * _hitFactory;
  Factory<StiKalmanTrack> * _trackFactory;
  Factory<StiMcTrack> * _mcTrackFactory;
  Detector            * _detector;
  StiDetectorFinder   * _detectorFinder;
  Messenger &         _messenger;
  bool                _useMcAsRec;
};

template<class Source1, class Source2, class Detector>
StiHitLoader<Source1, Source2,Detector>::StiHitLoader(const string & name)
  :  Named(name),
     _hitContainer(StiToolkit::instance()->getHitContainer()),
     _mcHitContainer(StiToolkit::instance()->getMcHitContainer()),
     _trackContainer(StiToolkit::instance()->getTrackContainer()),
     _mcTrackContainer(StiToolkit::instance()->getMcTrackContainer()),
     _hitFactory(StiToolkit::instance()->getHitFactory()),
     _trackFactory(StiToolkit::instance()->getTrackFactory()),
     _mcTrackFactory(StiToolkit::instance()->getMcTrackFactory()),
     _detector(0),
     _detectorFinder(StiDetectorFinder::instance()),
     _messenger(*(Messenger::instance(MessageType::kHitMessage)))
{}
    
template<class Source1, class Source2, class Detector>
StiHitLoader<Source1, Source2,Detector>::StiHitLoader(const string & name,
					    StiHitContainer* hitContainer,
					    StiHitContainer* mcHitContainer,
					    Factory<StiHit>*hitFactory,
					    Detector * detector)
  :  Named(name),
     _hitContainer(hitContainer),
     _mcHitContainer(mcHitContainer),
     _trackContainer(StiToolkit::instance()->getTrackContainer()),
     _mcTrackContainer(StiToolkit::instance()->getMcTrackContainer()),
     _hitFactory(hitFactory),
     _trackFactory(StiToolkit::instance()->getTrackFactory()),
     _mcTrackFactory(StiToolkit::instance()->getMcTrackFactory()),
     _detector(detector),
     _detectorFinder(StiDetectorFinder::instance()),
     _messenger( *(Messenger::instance(MessageType::kHitMessage)) )
{}

template<class Source1, class Source2, class Detector>
StiHitLoader<Source1, Source2,Detector>::~StiHitLoader()
{}

template<class Source1, class Source2, class Detector>
void StiHitLoader<Source1, Source2,Detector>::loadEvent(Source1 *source1, 
							Source2 * source2,
							Filter<StiTrack> * trackFilter, 
							Filter<StiHit> * hitFilter)
{
  cout << "Loader "<<_name<<" loading event"<<endl;
  if (source1 && !_useMcAsRec)
    loadHits(source1,trackFilter,hitFilter);
  if (source2)
    loadMcHits(source2,_useMcAsRec,trackFilter,hitFilter);
}

template<class Source1, class Source2, class Detector>
void StiHitLoader<Source1, Source2,Detector>::setHitContainer(StiHitContainer* hitContainer)
{
  _hitContainer = hitContainer;
}
 
template<class Source1, class Source2, class Detector>
void StiHitLoader<Source1, Source2,Detector>::setMcHitContainer(StiHitContainer* mcHitContainer)
{
  _mcHitContainer = mcHitContainer;
}
    
template<class Source1, class Source2, class Detector>
void StiHitLoader<Source1, Source2,Detector>::setHitFactory(Factory<StiHit>*hitFactory)
{
  _hitFactory = hitFactory;
}

template<class Source1, class Source2, class Detector>
void StiHitLoader<Source1, Source2,Detector>::setDetector(Detector *detector)
{
  _detector = detector;
}

template<class Source1, class Source2, class Detector>
Detector* StiHitLoader<Source1, Source2,Detector>::getDetector()
{
  return _detector;
}

template<class Source1, class Source2, class Detector>
void StiHitLoader<Source1, Source2,Detector>::setUseMcAsRec(bool value)
{
  _useMcAsRec = value;
}

template<class Source1, class Source2, class Detector>
bool StiHitLoader<Source1, Source2,Detector>::useMcAsRec() const
{
  return _useMcAsRec;
}

template<class Source1, class Source2, class Detector>
void StiHitLoader<Source1, Source2,Detector>::loadHits(Source1 * source,
						       Filter<StiTrack> * trackFilter, 
						       Filter<StiHit> * hitFilter)
{
  cout << "StiHitLoader<Source1, Source2,Detector>::loadHits(Source1 * source) -E- Called for "<<_name<<endl;
}
template<class Source1, class Source2, class Detector>
void StiHitLoader<Source1, Source2,Detector>::loadMcHits(Source2 * source,
							 bool useMcAsRec,
							 Filter<StiTrack> * trackFilter, 
							 Filter<StiHit> * hitFilter)
{
  cout << "StiHitLoader<Source1, Source2,Detector>::loadMcHits(Source1 * source) -E- Called for "<<_name<<endl;
}

#endif


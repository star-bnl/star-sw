#ifndef StiTrackFinder_H
#define StiTrackFinder_H 1

#include "StiConstants.h"

#include "StiObjectFactoryInterface.h"

class StiSeedFinder;
class StiTrackFilter;
class StiDetectorContainer;
class StiHitContainer;
class StiTrackContainer;
class StMagUtilities;
class StiDynamicTrackFilter;

class StiTrackFinder 
{
public:
    
    //_c-tor/d-tor__________________________________________________
    StiTrackFinder();
    virtual ~StiTrackFinder();
    
    //_action methods_______________________________________________
    virtual void findTracks()=0; //Do tracking
    virtual void reset()=0; //Full internal reset
    virtual bool isValid(bool debug=false) const = 0; //Check if everything is kosher
    
    //_accessor methods_____________________________________________
    void setTrackSeedFinder(StiSeedFinder * finder);
    //void setTrackFilter(StiTrackFilter * filter);
    void setTrackFilter(StiDynamicTrackFilter * filter);
    void setDetectorContainer(StiDetectorContainer* detcontainer);
    void setHitContainer(StiHitContainer * hitContainer);
    void setTrackContainer(StiTrackContainer * newTrackContainer);
    void setTrackNodeFactory(StiObjectFactoryInterface<StiKalmanTrackNode> * factory);
    void setMagneticField(StMagUtilities * magField);
    
    virtual void setTrackFiltering(bool option);
    virtual void setElossCalculated(bool option);
    virtual void setMCSCalculated(bool option);
    
    StiSeedFinder             * getTrackSeedFinder()    const;
    //StiTrackFilter            * getTrackFilter()        const;
    StiDynamicTrackFilter     * getTrackFilter()        const;
    StiDetectorContainer      * getDetectorContainer()  const;
    StiHitContainer           * getHitContainer()       const;
    StiTrackContainer         * getTrackContainer()     const;
    StiObjectFactoryInterface<StiKalmanTrackNode> * getTrackNodeFactory()   const;
    StMagUtilities            * getMagneticField()      const;
    
    
    bool isTrackFiltering()   const;
    bool isElossCalculated()  const;
    bool isMCSCalculated()    const;
    
protected:

    //Objects owned by this class
    //StiTrackFilter            * trackFilter;
    
    //Objects not owned by this class
    StiDynamicTrackFilter            * trackFilter;
    StiSeedFinder             * trackSeedFinder;
    StiObjectFactoryInterface<StiKalmanTrackNode> * trackNodeFactory;
    StMagUtilities            * magField;
    StiDetectorContainer      * detectorContainer;
    StiHitContainer           * hitContainer;
    StiTrackContainer         * trackContainer;
    
    bool trackFiltering;
    bool elossCalculated;
    bool mcsCalculated;
};


inline  bool StiTrackFinder::isTrackFiltering()   const  
{
  return trackFiltering;
}

inline  bool StiTrackFinder::isElossCalculated()    const
{
  return elossCalculated;
}

inline  bool StiTrackFinder::isMCSCalculated()   const   
{
  return mcsCalculated;
}

inline  void StiTrackFinder::setTrackFiltering(bool option) 
{
  trackFiltering  = option;
}

inline  void StiTrackFinder::setElossCalculated(bool option) 
{
  elossCalculated = option; 
}

inline  void StiTrackFinder::setMCSCalculated(bool option)   
{
  mcsCalculated   = option; 
}

inline StiObjectFactoryInterface<StiKalmanTrackNode>* StiTrackFinder::getTrackNodeFactory() const
{
  return  trackNodeFactory;
}

inline StiHitContainer * StiTrackFinder::getHitContainer() const 
{
  //----------------------------------------------------------------- 
  // Return the hit container used by this finder
  //-----------------------------------------------------------------   
  return hitContainer;
}

inline StiTrackContainer * StiTrackFinder::getTrackContainer() const
{
  //----------------------------------------------------------------- 
  // Return the track container used by this finder
  //-----------------------------------------------------------------   
  return trackContainer;
}

inline StiDetectorContainer * StiTrackFinder::getDetectorContainer() const
{
  //----------------------------------------------------------------- 
  // Returns the detector geometry used by this finder
  //-----------------------------------------------------------------   
  return detectorContainer;
}


inline StiSeedFinder * StiTrackFinder::getTrackSeedFinder() const
{
  return trackSeedFinder;
}

//inline StiTrackFilter     * StiTrackFinder::getTrackFilter() const
inline StiDynamicTrackFilter     * StiTrackFinder::getTrackFilter() const
{
  return trackFilter;
}


inline StMagUtilities * StiTrackFinder::getMagneticField() const
{
  //----------------------------------------------------------------- 
  // Returns the descriptor of the magnetic field used by this 
  // tracker.
  //----------------------------------------------------------------- 
  return magField;
}


#endif

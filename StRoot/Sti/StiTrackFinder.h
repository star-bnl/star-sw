#ifndef StiTrackFinder_H
#define StiTrackFinder_H 1

#include "StiFactoryTypedefs.h"
#include "StiConstants.h"

class StiSeedFinder;
class StiTrackFilter;
class StiDetectorContainer;
class StiHitContainer;
class StiTrackContainer;
class StMagUtilities;

class StiTrackFinder 
{
 public:

  //_c-tor/d-tor__________________________________________________
  StiTrackFinder();
  virtual ~StiTrackFinder();
  
  //_action methods_______________________________________________
  virtual void findTracks();


  //_accessor methods_____________________________________________
  void setTrackSeedFinder(StiSeedFinder * finder);
  void setTrackFilter(StiTrackFilter * filter);
  void setGeometryContainer(StiDetectorContainer* geometry);
  void setHitContainer(StiHitContainer * hitContainer);
  void setTrackContainer(StiTrackContainer * newTrackContainer);
  void setTrackNodeFactory(StiTrackNodeFactory * factory);
  void setMagneticField(StMagUtilities * magField);

  void setTrackFiltering(bool option);
  void setElossCalculated(bool option);
  void setMCSCalculated(bool option);

  StiSeedFinder             * getTrackSeedFinder()    const;
  StiTrackFilter            * getTrackFilter()        const;
  StiDetectorContainer      * getGeometryContainer()  const;
  StiHitContainer           * getHitContainer()       const;
  StiTrackContainer         * getTrackContainer()     const;
  StiTrackNodeFactory       * getTrackNodeFactory()   const;
  StMagUtilities            * getMagneticField()      const;


  bool isTrackFiltering()   const;
  bool isElossCalculated()  const;
  bool isMCSCalculated()    const;
  int  getAnalyzedTrackSeedCount()  const;
  int  getFoundTrackCount()         const;
  int  getAcceptedTrackCount()      const;
  int  getStatus()                  const;

 protected:

  StiSeedFinder             * trackSeedFinder;
  StiTrackFilter            * trackFilter;
  StiDetectorContainer      * geometryContainer;
  StiHitContainer           * hitContainer;
  StiTrackContainer         * trackContainer;
  StiTrackNodeFactory       * trackNodeFactory;
  StMagUtilities            * magField;

  int analyzedTrackSeeds;
  int acceptedTracks;
  int foundTracks;

  bool trackFiltering;
  bool elossCalculated;
  bool mcsCalculated;
  int  status;
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

inline StiTrackNodeFactory * StiTrackFinder::getTrackNodeFactory() const
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

inline StiDetectorContainer * StiTrackFinder::getGeometryContainer() const
{
  //----------------------------------------------------------------- 
  // Returns the detector geometry used by this finder
  //-----------------------------------------------------------------   
  return geometryContainer;
}


inline StiSeedFinder * StiTrackFinder::getTrackSeedFinder() const
{
  return trackSeedFinder;
}

inline StiTrackFilter     * StiTrackFinder::getTrackFilter() const
{
  return trackFilter;
}

inline int StiTrackFinder::getAnalyzedTrackSeedCount() const
{
  //----------------------------------------------------------------- 
  // Returns the number of track seeds analyzed by this finder since
  // last reset.
  //----------------------------------------------------------------- 
  return analyzedTrackSeeds;
}

inline int StiTrackFinder::getFoundTrackCount() const
{
  //----------------------------------------------------------------- 
  // Returns the number of found tracks by this finder since
  // last reset.
  //----------------------------------------------------------------- 
  return foundTracks;
}

inline int StiTrackFinder::getAcceptedTrackCount() const
{
  //----------------------------------------------------------------- 
  // Returns the number of found and accepted tracks by this finder 
  // since last reset.
  //----------------------------------------------------------------- 
  return acceptedTracks;
}

inline int StiTrackFinder::getStatus() const
{
  //----------------------------------------------------------------- 
  // Returns status of this finder
  // See top of this file for the meaning associated with each
  // status state.
  //----------------------------------------------------------------- 
  return status;
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

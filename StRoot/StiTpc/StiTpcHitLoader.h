#ifndef StiTpcHitLoader_H
#define StiTpcHitLoader_H
#include "Sti/StiHitLoader.h"

class StEvent;
class StiDetectorBuilder;
class StTpcHit;

/*! \class StiTpcHitLoader
  StiTpcHitLoader is a concrete class implementing the StiHitLoader abstract
  interface. It is used to load hits from Star StEvent into the StiHitContainer
  for Sti tracking. StEvent hits from the TPC are converted using the 
  StiTpcDetectorBuilder methods.
  <p>
  This class is substantially morphed from the class StiHitFiller 
  originally written by Mike Miller.
  \author Claude A Pruneau (Wayne) 
 */
class StiTpcHitLoader : public StiHitLoader<StEvent,StiDetectorBuilder>
{
 public:
  StiTpcHitLoader();
  StiTpcHitLoader(StiHitContainer * hitContainer,
		  Factory<StiHit> * hitFactory,
		  StiDetectorBuilder * detector);
  virtual ~StiTpcHitLoader() {}
  virtual void loadHits(StEvent* source,
			Filter<StiTrack> * trackFilter, 
			Filter<StiHit> * hitFilter);
  void         setMinRow(UInt_t r= 1) 		{_minRow    = r;}
  void         setMinSector(UInt_t r= 1) 	{_minSector = r;}
  void         setMaxSector(UInt_t r=24) 	{_maxSector = r;}
//			Distribute hit to near detectors
   int         giveOut(const StiDetector *stiDetector,int stiSector
                            ,int stiRow,StiHit *stiHit);
   int         iTPCvers();
  UInt_t       minRow() 	{return _minRow;}
  UInt_t       maxRow() 	{return _maxRow;}
  UInt_t       minSector() 	{return _minSector;}
  UInt_t       maxSector() 	{return _maxSector;}
     int       toStiSect(int s); 
     int       nextSideSect(int s); 
     int       nextStiSect(int sect,int add); 
  
 protected:
  UInt_t         _minRow;
  UInt_t         _maxRow;
  UInt_t         _minSector;
  UInt_t         _maxSector;
     int	 _iTPC;	// 0=no iTPC, 1 = only sector 20 is iTPC, 2=Full iTPC
};

#endif

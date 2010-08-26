// Hey Emacs this is really -*-c++-*- ! 
// \class  EEmcMCData
// \author Piotr A. Zolnierczuk             
// \date   Aug 26, 2002
#ifndef EEmcMCData_h
#define EEmcMCData_h
/*********************************************************************
 * Description:
 * STAR Endcap Electromagnetic Calorimeter Monte Carlo Data
 *********************************************************************/
#include "TObject.h"


class   StMaker;

class   St_g2t_emc_hit;
class   St_g2t_event;

class  EEeventDst;
class  StMcEventMaker;

const   Float_t kEEmcDefaultEnergyThreshold = 0.0005; // 0.5 MeV
const   Int_t   kEEmcDefaultMCHitSize       = 0x1000; // 4k hitow


enum EEmcVolId {
  // for Tower
  kEEmcTowerHalfId =  100000,
  kEEmcTowerPhiId  =    1000,
  kEEmcTowerEtaId  =      10,
  kEEmcTowerDepId  =       1,
  
  // for SMDs
  kEEmcSmdHalfId   = 1000000,
  kEEmcSmdPhiId    =   10000,
  kEEmcSmdPlaneId  =    1000,
  kEEmcSmdStripId  =        1
};


struct EEmcMCHitTower { 
  UChar_t  ssec;  // endcap subsector  1:5 (A-E)
  UChar_t  eta;   // endcap eta        1:12
};


struct EEmcMCHit {
  UChar_t  detector;  // endcap detector part  (prs,tower,post,smdu,smdv)
  UChar_t  sector;    // endcap phi sector 1:12
  union {
    EEmcMCHitTower tower;
    UShort_t strip;   // smd's  strip numbers
  };
  Float_t  de;        // energy loss in the element (GeV)
  int track_p;  // parent track for this hit- if aplicable
};

  

class   EEmcMCData : public TObject {
public:
  enum MCDepth {          // FIXME LATER: depths encoded in g2t data
    kUnknownDepth    = 0,
    kPreShower1Depth = 1,
    kPreShower2Depth = 2,
    kTower1Depth     = 3,
    kTower2Depth     = 4,
    kPostShowerDepth = 5
  };


  enum MCDetectorId {
    kEEmcMCUnknownId    = 0,
    kEEmcMCTowerId      = 1,
    kEEmcMCPreShower1Id = 2,
    kEEmcMCPreShower2Id = 3,
    kEEmcMCSmdUStripId  = 4,
    kEEmcMCSmdVStripId  = 5,
    kEEmcMCPostShowerId = 6
  };


  EEmcMCData();                              // default constructor
  EEmcMCData(const EEmcMCData& );                 // copy constructor
  virtual ~EEmcMCData();                     // the destructor

  Int_t      readEventFromChain(const StMaker *mk); // reads g2t event from chain
  
  Int_t      getSize() const      { return mSize;    };
  Int_t      getLastHit() const   { return mLastHit; }; 
  Int_t      getEventID() const   { return mEventID; }; 

  Float_t    getEnergyThreshold() const { return mEthr; };
  void       setEnergyThreshold(Float_t e) { mEthr = e; };

  Int_t      getHitArray(EEmcMCHit *h, Int_t size) const;
  Int_t      setHitArray(EEmcMCHit *h, Int_t size);

  void       print() const;                                      // diagnostic print
  void unpackGeantHits(St_g2t_emc_hit* g2t_tile, St_g2t_emc_hit* g2t_smd);
  const struct EEmcMCHit * getGeantHits(int &nHit) const { nHit=mLastHit; return mHit;}

  // obsolete functions
  Int_t    read     (void *d, int s);   // reads  in  s bytes of hits 
  Int_t    write    (void *d, int s);   // writes out s bytes of hits 
  Int_t    write    (EEeventDst *);        // export hits to TTree

protected:
  Int_t             mEventID;  // WARN: not added in to binary I/O, JB
  struct EEmcMCHit *mHit ;     // array to hold hit data
  Int_t             mSize;     // size of the above array
  Int_t             mLastHit;  // last hit index
  Float_t           mEthr;     // energy threshold
  
private:
  //Float_t          *mDE; // temporary array
  Int_t             expandMemory();
  
  ClassDef(EEmcMCData,2) // Endcap Emc event
};

#endif

/*
 * $Log: EEmcMCData.h,v $
 * Revision 1.7  2010/08/26 22:48:54  ogrebeny
 * Improved constness
 *
 * Revision 1.5  2005/06/03 19:19:48  balewski
 * for embedding, GEANT unpcker was split on 2 parts
 *
 * Revision 1.4  2003/09/02 17:57:56  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2003/02/20 21:27:06  zolnie
 * added simple geometry class
 *
 * Revision 1.2  2003/02/20 20:13:20  balewski
 * fixxy
 * xy
 *
 * Revision 1.1  2003/02/20 05:14:07  balewski
 * reorganization
 *
 * Revision 1.1  2003/01/28 23:16:07  balewski
 * start
 *
 * Revision 1.9  2002/11/13 21:53:52  zolnie
 * restored "private" DetectorID in EEmcMCData
 *
 * Revision 1.8  2002/11/11 21:22:48  balewski
 * EEMC added to StEvent
 *
 * Revision 1.7  2002/10/03 15:52:25  zolnie
 * updates reflecting changes in *.g files
 *
 * Revision 1.6  2002/10/01 06:03:15  balewski
 * added smd & pre2 to TTree, tof removed
 *
 * Revision 1.5  2002/09/30 21:58:27  zolnie
 * do we understand Oleg? (the depth problem)
 *
 * Revision 1.4  2002/09/30 20:15:55  zolnie
 * Oleg's geometry updates
 *
 * Revision 1.3  2002/09/24 22:47:35  zolnie
 * major rewrite: SMD incorporated, use constants rather hard coded numbers
 * 	introducing exceptions (rather assert)
 *
 * Revision 1.2  2002/09/20 15:49:05  balewski
 * add event ID
 *
 * Revision 1.1.1.1  2002/09/19 18:58:54  zolnie
 * Imported sources
 *
 *********************************************************************/

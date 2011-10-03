// $Id: StMCTrackerSD.h,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $
//
// Geant4 ExampleN02 adapted to Virtual Monte Carlo 
//
// Id: ExN02TrackerSD.hh,v 1.6 2002/01/09 17:24:09 ranjard Exp 
// GEANT4 tag Name: geant4-04-00-patch-02 
//
// by Ivana Hrivnacova, 21.4.2002

#ifndef STMC_TRACKER_SD_H
#define STMC_TRACKER_SD_H

#include <TNamed.h>
#include <TClonesArray.h>

class StMCTrackerHit;

class StMCTrackerSD : public TNamed
{
  public:
    StMCTrackerSD(const char* name);
    StMCTrackerSD();
    virtual ~StMCTrackerSD();

    // methods
    void   Initialize();
    Bool_t  ProcessHits();
    void   EndOfEvent();
    void   Register();
    void   Print(const Option_t* opt=0) const;
    
    // set methods
    void SetVerboseLevel(Int_t level) { fVerboseLevel = level; }

  private:
    // methods
    StMCTrackerHit* AddHit();
  
    // data members
    TClonesArray*  fTrackerCollection;
    Int_t          fSensitiveVolumeID;
    Int_t          fVerboseLevel;
   
  ClassDef(StMCTrackerSD,1) //StMCTrackerSD 

};

#endif //STMC_TRACKER_SD_H


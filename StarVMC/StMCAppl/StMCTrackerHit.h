// $Id: StMCTrackerHit.h,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $
//
// Geant4 ExampleN02 adapted to Virtual Monte Carlo 
//
// Id: StMCTrackerHit.hh,v 1.6 2002/01/09 17:24:09 ranjard Exp 
// GEANT4 tag Name: geant4-04-00-patch-02 
//
// by Ivana Hrivnacova, 21.4.2002

#ifndef STMC_TRACKER_HIT_H
#define STMC_TRACKER_HIT_H

#include <TObject.h>
#include <TVector3.h>

class StMCTrackerHit : public TObject
{
  public:
    StMCTrackerHit();
    virtual ~StMCTrackerHit();

    // methods
    //void Draw();
    void Print(const Option_t* opt = 0) const;

    // set methods
    void SetTrackID  (Int_t track)  { fTrackID = track; };
    void SetChamberNb(Int_t chamb)  { fChamberNb = chamb; };  
    void SetEdep     (Double_t de)  { fEdep = de; };
    void SetPos      (TVector3 xyz) { fPos = xyz; };
      
    // get methods
    Int_t GetTrackID()   { return fTrackID; };
    Int_t GetChamberNb() { return fChamberNb; };
    Double_t GetEdep()   { return fEdep; };      
    TVector3 GetPos()    { return fPos; };
      
  private:
    Int_t      fTrackID;
    Int_t      fChamberNb;
    Double_t   fEdep;
    TVector3   fPos;
    
  ClassDef(StMCTrackerHit,1) //StMCTrackerHit  
};

#endif //STMC_TRACKER_HIT_H



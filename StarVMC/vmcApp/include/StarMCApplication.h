// $Id: StarMCApplication.h,v 1.1 2004/07/12 20:35:59 potekhin Exp $

#ifndef STARMCAPPLICATION_H
#define STARMCAPPLICATION_H

#include <TVirtualMCApplication.h>

#include "StarDetectorConstruction.h"
#include "StarModule.h"
#include "StarHit.h"
#include "StarRootManager.h"

class TGeoVolume;
class StarStack;
class StarGenerator;

class StarMCApplication : public TVirtualMCApplication
{
  public:
    StarMCApplication(const char* name,  const char *title, 
                      FileMode fileMode = kWrite);
    StarMCApplication();
    virtual ~StarMCApplication();
  
    // static access method
    static StarMCApplication* Instance(); 

    // methods
    void InitMC(void);
    void RunMC(Int_t nofEvents);
    void FinishRun();
    void ReadEvent(Int_t i);
 
    virtual void ConstructGeometry();
    virtual void InitGeometry();
    virtual void InspectGeometry(TGeoVolume* v);
    virtual void GeneratePrimaries();
    virtual void BeginEvent();
    virtual void BeginPrimary();
    virtual void PreTrack();
    virtual void Stepping();
    virtual void PostTrack();
    virtual void FinishPrimary();
    virtual void FinishEvent();
    virtual void Field(const Double_t* x, Double_t* b) const;

    
    virtual void setFileBased();

    virtual  StarGenerator*          Generator() const {return  _generator;}
    virtual  void SetGenerator  (StarGenerator                  *generator);
    virtual  void ResetGenerator(StarGenerator                  *generator);

    TObjArray*   Modules() const {return _modules;}
    TObjArray*   Hits()    const {return _hits;}

    virtual void AddModule(StarModule* m_);
    virtual void AddHit   (StarHit*    h_);

    virtual void PrintHits(void);

  private:
    // methods
    void RegisterStack();
  
    // attributes
    TObjArray*               _modules;
    TObjArray*               _hits;


    StarGenerator*           _generator;         //  Generator used in the MC
    StarStack*               _stack;
    StarDetectorConstruction _DetectorConstruction;
    Double_t*                fFieldB;
    StarRootManager          fRootManager;
    int                      fileBased;

    ClassDef(StarMCApplication,1)  //Interface to MonteCarlo application
};

// inline functions

inline StarMCApplication* StarMCApplication::Instance()
{ return (StarMCApplication*)(TVirtualMCApplication::Instance()); }

#endif //STARMCAPPLICATION_H


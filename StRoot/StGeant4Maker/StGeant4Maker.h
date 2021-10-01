#ifndef __StGeant4Maker_h__
#define __StGeant4Maker_h__
#include <StMaker.h>

#include "StChain.h"

#include "TVirtualMC.h"
#include "TVirtualMCApplication.h"
#include "TVirtualMagField.h"
#include "StarMagField.h"
#include <StSensitiveDetector.h> 



class StGeant4Maker;
class StMCParticleStack;

class AgMLExtension;

class TGeoNode;
class TGeoVolume;

class TG4RunConfiguration;

class St_g2t_track; 

class StEvtHddr;

//______________________________________________________________________________________
class StarMagFieldAdaptor : public TVirtualMagField {
public:
  void Field( const double *x, double *B )
  { 
    StarMagField::Instance()->BField(x,B); 
  }
};
//______________________________________________________________________________________
class StarVMCApplication : public TVirtualMCApplication {
public:
  StarVMCApplication( const char *name = "starsim", const char *title="STAR VMC simulation", double zmax=DBL_MAX, double rmax=DBL_MAX );
 ~StarVMCApplication(){ /* nada */ };

  /// Geometry construction is the responsability of the STAR chain
  virtual void ConstructGeometry();
  virtual void ConstructSensitiveDetectors();

  /// Misalignment of geometry.  Default false.
  virtual bool MisalignGeometry(){ return false; }

  /// Define parameters for optical processes (optional)
  virtual void ConstructOpGeometry() { LOG_INFO << "VMC APP ConstructOpGeometry()" << endm; }
  
  /// TODO: Initialize geometry. (Usually used to define sensitive volumes IDs)
  virtual void InitGeometry();

  /// Add user defined particles (optional)
  virtual void AddParticles() {LOG_INFO << "VMC APP AddParticles()" << endm; };

  /// Add user defined ions (optional)
  virtual void AddIons() {}

  /// Primary generation is responsability of the STAR framework
  virtual void GeneratePrimaries();

  /// Define actions at the beginning of the event
  virtual void BeginEvent();

  /// Define actions at the beginning of the primary track
  virtual void BeginPrimary();

  /// Define actions at the beginning of each track
  virtual void PreTrack();

   /// Define action at each step
  virtual void Stepping();

  /// Define actions at the end of each track
  virtual void PostTrack();

  /// Define actions at the end of the primary track
  virtual void FinishPrimary();

  /// Define actions at the end of the event
  virtual void FinishEvent();

  /// Define maximum radius for tracking (optional)
  virtual double TrackingRmax() const;

  /// Define maximum z for tracking (optional)
  virtual double TrackingZmax() const;

  /// Calculate user field \a b at point \a x
  virtual void Field(const double* x, double* b) const { StarMagField::Instance()->BField(x,b); }

  /// Define action at each step for Geane
  virtual void GeaneStepping() {;}    

private:
protected:

  double mZmax;
  double mRmax;

  ClassDef(StarVMCApplication, 1);

};
//______________________________________________________________________________________
class StGeant4Maker : public StMaker {

public:

  StGeant4Maker( const char* nm="geant4star" );
  ~StGeant4Maker(){ /* nada */ };

  /// Initialize maker
  int Init();
  int  InitRun( int run );
  int  InitGeom();
  int  InitHits();

  /// Perform runtime initialization of per-medium track propagation cuts, processes, etc...
  int  ConfigureGeometry();

  /// Process one event
  int Make();  

  void BeginEvent();
  void BeginPrimary();
  void PreTrack();
  void PostTrack();
  void FinishPrimary();
  void FinishEvent();
  
  // Executed on each tracking step
  void Stepping();

  /// Push primary event out for tracking
  void PushPrimaries();

  /// Clear for the next event
  void Clear( const Option_t* opts="" );

private:
protected:

  StarVMCApplication *mVmcApplication;
  TString mGeomPath; // Search path for geometry construction
  StarMagField *mStarField;
  StMCParticleStack *mMCStack;      // Monte Carlo stack
  StarMagFieldAdaptor *mMagfield;
  TG4RunConfiguration* mRunConfig;
  //  StVMCStepManager* mStepping;

  void UpdateHistory();

  TGeoNode*   mCurrentNode;
  TGeoNode*   mPreviousNode;
  TGeoVolume* mCurrentVolume;
  TGeoVolume* mPreviousVolume;

  int         mCurrentTrackingRegion;
  int         mPreviousTrackingRegion;

  AgMLExtension* acurr;
  AgMLExtension* aprev;

  StEvtHddr* mEventHeader;

  int regionTransition( int, int );

  /// @param T specifies the type of the table   
  /// @param F specifies the functor class which retrieves the hits from geant  
  template<typename T, typename F>
  int AddHits( std::string name, std::vector<std::string> volumes, std::string gname, F sd2table ) {

    int nhits = 0;
    StSensitiveDetector* sd = 0;
    for ( auto v : volumes ) {
      sd = dynamic_cast<StSensitiveDetector*>(TVirtualMC::GetMC()->GetSensitiveDetector( v.c_str() )); 
      if ( 0==sd ) { LOG_INFO << "no SD for " << v << endm; continue; } 
      nhits += sd->numberOfHits(); 
      break;
    }
    if ( 0==sd ) return 0;

    LOG_INFO << name << " adding number of hits = " << nhits << endm;
    auto* table     = new T( gname.c_str(), nhits );
    auto* g2t_track = (St_g2t_track*)FindByName("g2t_track"); 

    // Copy data from the sensitive detector to the table
    sd2table( sd, table, g2t_track ); 
                                    
    AddData( table ); 

    // Clear the sensitive detector
    sd->Clear();

    return nhits;
  };

  ClassDef(StGeant4Maker,1);

public:

  virtual const char *GetCVS() const
  {
    static const char cvs[]="Tag $Name:$ $Id:$ built __DATE__ __TIME__ "; 
    return cvs;
  }

};
//______________________________________________________________________________________
#endif

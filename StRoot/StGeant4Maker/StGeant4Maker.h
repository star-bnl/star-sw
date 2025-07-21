#ifndef __StGeant4Maker_h__
#define __StGeant4Maker_h__
#include <StMaker.h>

#include "StChain.h"

#include "TVirtualMC.h"
#include "TVirtualMCApplication.h"
#include "TVirtualMagField.h"
#include "StarMagField.h"
#include <StSensitiveDetector.h> 
#include <map>
#include <string>
#include "StMCParticleStack.h"

#include <functional>

class StGeant4Maker;

class AgMLExtension;

class TGeoNode;
class TGeoVolume;

class TG4RunConfiguration;

class St_g2t_track; 

class StEvtHddr;

//______________________________________________________________________________________
/**
 * @class StarMagFieldAdaptor
 * @brief An adaptor class to interface STAR's magnetic field with the VMC.
 *
 * This class inherits from TVirtualMagField and implements the `Field` method
 * by calling the singleton `StarMagField::Instance()->BField()`. This allows
 * the VMC framework to use the standard STAR magnetic field map.
 */
class StarMagFieldAdaptor : public TVirtualMagField {
public:
  void Field( const double *x, double *B )
  { 
    StarMagField::Instance()->BField(x,B); 
  }
};
//______________________________________________________________________________________
/**
 * @class StarVMCApplication
 * @brief The main user application for the Virtual Monte Carlo (VMC) simulation.
 *
 * This class orchestrates the simulation process by implementing the hooks
 * provided by the TVirtualMCApplication interface. It manages the construction
 * of geometry and sensitive detectors, generation of primary particles, and
 * defines user actions for various stages of event and track processing.
 * Most of the heavy lifting is delegated to the StGeant4Maker instance.
 */
class StarVMCApplication : public TVirtualMCApplication {
public:
  StarVMCApplication( const char *name = "starsim", const char *title="STAR VMC simulation", double zmax=DBL_MAX, double rmax=DBL_MAX, std::string engine="geant4", StMCParticleStack* stack = 0 );
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

  bool   mMulti;

  StMCParticleStack* mMCStack;

  ClassDef(StarVMCApplication, 0);

};
//______________________________________________________________________________________
/**
 * @class StGeant4Maker
 * @brief The primary STAR maker for running Geant4-based simulations.
 *
 * This class serves as the main entry point and controller for the Geant4
 * simulation within the STAR software framework. It handles initialization
 * of the geometry, physics engines (Geant4, Geant3, or a mix), and sensitive
 * detectors. It processes events by calling the VMC, manages the particle
 * stack, and fills the output g2t tables with simulation results (hits and
 * truth information).
 */
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

  /// Final end of run actions
  int  Finish();

  void SetDefaultEngine( int e ) { mDefaultEngine = e; }
  
  /// Sets the MC engine for a given module
  void SetEngineForModule( const char* module_, const int engine );

  StMCParticleStack* stack(){ return mMCStack; }

  void AddUserPostSteppingAction( std::function<void()> f ) { mPostSteppingActions.push_back(f); }

  /// Registers a command line option 
  template<typename T> 
  void AddOption( const char* opt, T val, const char* help ){
    SetAttr( opt, val );
    mCmdOptions.push_back( { opt, help } );
  }

  void PrintOptions( const char* opts = 0 );

  
private:
protected:

  struct CmdOption {
    std::string name;
    std::string help;
  };
  std::vector<CmdOption> mCmdOptions;
  
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

  std::map<std::string, double> mHitSum;

  /// @param T specifies the type of the table   
  /// @param F specifies the functor class which retrieves the hits from geant  
  template<typename T, typename F>
  int AddHits( std::string name, std::vector<std::string> volumes, std::string gname, F sd2table ) {

    // Need to move table creation inside of the loop.

    // Count total number of hits in the SDs for this table
    int nhits = 0;
    StSensitiveDetector* sd = 0;
    std::vector<StSensitiveDetector*> sds;
    for ( auto v : volumes ) {
      sd = dynamic_cast<StSensitiveDetector*>(TVirtualMC::GetMC()->GetSensitiveDetector( v.c_str() )); 
      if ( 0==sd ) { /*LOG_INFO << "no SD for " << v << endm;*/ continue; } 
      nhits += sd->numberOfHits(); 
      sds.push_back(sd);
    }
    
    // Create new table (possibly empty)
    auto* table     = new T( gname.c_str(), nhits );
    // Get the track table
    auto* g2t_track = (St_g2t_track*)FindByName("g2t_track"); 



    LOG_INFO << name << " adding number of hits = " << nhits << endm;
    if ( nhits > 0 ) {

      // Copy data from the sensitive detectors to the table
      for ( auto* sd_ : sds ) {
	sd2table( sd_, table, g2t_track ); 
      }

      // Register the table with the maker
      AddData( table ); 
      double sum = 0.0;

      // Accumulate total energy deposition for QA later
      for ( auto hit : (*table) ) {
	sum += hit.de;
      }
      mHitSum[ name ] += sum;
    
    }

    // Clear the sensitive detectors
    for ( auto sd_ : sds) {
      sd_->Clear();
    }

    return nhits;
  };

  int mDefaultEngine;

  std::vector< std::function<void()> > mPostSteppingActions;

  ClassDef(StGeant4Maker,0);

public:

  virtual const char *GetCVS() const
  {
    static const char cvs[]="Tag $Name:$ $Id:$ built __DATE__ __TIME__ "; 
    return cvs;
  }

};
//______________________________________________________________________________________
#endif

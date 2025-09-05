#ifndef __StGeant4Application_h__
#define __StGeant4Application_h__

#include "TVirtualMCApplication.h"

/**
 * @class StGeant4Application
 * @brief An empty user Monte-Carlo application class.
 *
 * This class inherits from TVirtualMCApplication but its implementation is empty.
 * All functionality is provided by the StGeant4Maker. This class is required by
 * the TVirtualMC interface.
 */
class StGeant4Application : public TVirtualMCApplication {
public:
  StGeant4Application( const char* name="g4application", const char* title="Geant4 Application" );
  virtual ~StGeant4Application(){ /* nada */ };
  
  static StGeant4Application* Instance();

  void InitMC ( const char* macro );
  void RunMC  ( const int   nevents );

  virtual void ConstructGeometry();
  virtual void ConstructOpGeometry();
  virtual void InitGeometry();
  virtual void GeneratePrimaries();
  virtual void BeginEvent();
  virtual void BeginPrimary();
  virtual void PreTrack();
  virtual void Stepping();
  virtual void PostTrack();
  virtual void FinishPrimary();
  virtual void FinishEvent();

  void  SetVerboseLevel(int verboseLevel);

 private:
    // data members
    int                     fEventNo;         ///< Event counter
    int                    fGammaCounter;    ///< Optical photons counter
    TMCVerbose                fVerbose;         ///< VMC verbose helper
  //    Ex03MCStack*              fStack;           ///< VMC stack
    TVirtualMagField*         fMagField;        ///< The magnetic field 
  //    Ex06DetectorConstruction* fDetConstruction; ///< Dector construction
  //    Ex06PrimaryGenerator*     fPrimaryGenerator;///< Primary generator
  //    Bool_t                    fOldGeometry;     ///< Option for geometry definition

};

#endif

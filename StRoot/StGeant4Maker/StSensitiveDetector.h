#ifndef __StSensitiveDetector_h__
#define __StSensitiveDetector_h__

#include <TVirtualMCSensitiveDetector.h>
#include <TGeoVolume.h>
#include <iostream>
#include <vector>
#include <map>
#include <TString.h>

class TVirtualMCStack;

class AgMLExtension;
class StHitCollection;

/**
 * @class DetectorHit
 * @brief A base class for storing information about a simulated hit.
 *
 * This class provides the fundamental data members for a hit in a sensitive
 * detector, including a unique ID, the truth track ID, the geometry path
 * (`volu`, `copy`), a reduced volume ID (`numbv`), a final unique volume ID,
 * and the number of Geant4 steps contributing to the hit.
 */
class DetectorHit {
public:

  DetectorHit();
  virtual ~DetectorHit(){ /* nada */ };

  static const unsigned int maxdepth = 15;

  unsigned int id;              /// unique ID assigned to hit
  unsigned int idtruth;         /// unique ID of the track
           int volu[maxdepth];  /// volume numbers specifying path to hit
           int copy[maxdepth];  /// copy numbers specifying path to the hit
           int numbv[maxdepth]; /// "Reduced" numbering
           int volId;           /// Unique volume ID

  TString path;

  unsigned int nsteps; /// number of steps inside volume 

  std::vector<double> user; /// user specified hits

};

/**
 * @class TrackerHit
 * @brief A class for storing detailed information about a hit in a tracking detector.
 *
 * This class extends `DetectorHit` with information specific to trackers,
 * such as the particle's position and momentum at the entry and exit points
 * of the sensitive volume, the total energy deposited (`de`), the step length
 * (`ds`), and the total track length.
 */
class TrackerHit : public DetectorHit {
public:

  TrackerHit();
  virtual ~TrackerHit(){ /* nada */ }

  double position_in[4];        /// x,y,z,t position where track entered volume
  double position_out[4];       /// x,y,z,t position where track exited volume

  double momentum_in[4];
  double momentum_out[4];

  double de;
  double ds;

  double length; /// total track length to this point
  double lgam;   /// log10( Ekin / mass )

};

/**
 * @class CalorimeterHit
 * @brief A class for storing information about a hit in a calorimeter.
 *
 * This class extends `DetectorHit` for calorimeters, storing the entry
 * position of the track and the total energy deposited (`de`).
 */
class CalorimeterHit : public DetectorHit {
public:

  CalorimeterHit(); 
  virtual ~CalorimeterHit(){ /* nada */ }

  double position_in[4];        /// x,y,z,t position where track entered volume
  double de;

};

/**
 * @class StSensitiveDetector
 * @brief A VMC-compatible sensitive detector for STAR.
 *
 * This class implements the TVirtualMCSensitiveDetector interface. It is
 * associated with one or more TGeoVolumes. When a particle traverses one of
 * these volumes, this class's `ProcessHits` method is called. It creates
 * and manages a corresponding `StHitCollection` to store the resulting hits.
 */
class StSensitiveDetector : public TVirtualMCSensitiveDetector {
public:
  StSensitiveDetector( const char* sdname, const char* title );
  virtual ~StSensitiveDetector(){ /* nada */ }

  void Initialize();
  void ProcessHits();
  void EndOfEvent();
  void Clear(Option_t* o="");

  void addVolume(TGeoVolume *v);
  int  numberOfVolumes(){ return mVolumes.size(); }

  int numberOfHits();//{ return int( mCollection->size() ); }

  StHitCollection* hits(){ return mCollection; } 

  enum class DetectorType { kUninitialized, kCalorimeter, kTracker };

  DetectorType detectorType();

  void SetUserStack( TVirtualMCStack* stack );

private:
protected:

  std::vector<TGeoVolume*>   mVolumes;

  AgMLExtension* mAgMLInfo;

  StHitCollection* mCollection;
  TVirtualMCStack* mUserStack;

};

#endif

#ifndef __AgmlExtension_h__
#define __AgmlExtension_h__

#include <TGeoExtension.h>
#include <TString.h>
#include <TMath.h>
#include <vector>
#include <map>

// Volume ID functor (for sensitive volumes)
class AgMLVolumeId {
public:
  virtual int id( int* numbv ) const { return 0; }
};

// User-defined hit scoring functor (for sensitive volumes)
class AgMLScoring {
public:
  virtual float hit() const { return 0; }
};

class AgMLExtension : public TGeoRCExtension {

public:

  AgMLExtension();
  virtual ~AgMLExtension() { delete mVolumeId; mVolumeId = nullptr; }

  void SetModuleName( const char* name ){ mModuleName = name; }
  void SetFamilyName( const char* name ){ mFamilyName = name; }
  void SetVolumeName( const char* name ){ mVolumeName = name; }
  void SetSensitive( bool flag ) { mSensitive = flag; }
  void SetTracking( short track ) { 
    if ( mVolumeName == "CAVE" ) {
      mTracking = 2;
    }
    else { 
      mTracking = track; 
    };
  }

  void SetBranchings( int b ) { mBranchings=b; }  

  void SetVolumeIdentifier( AgMLVolumeId* identifier ){ delete mVolumeId; mVolumeId = identifier; }
  void AddHitScoring( AgMLScoring* sc ){ mHitScoring.push_back( sc ); }


  TString GetModuleName(){ return mModuleName; }
  TString GetFamilyName(){ return mFamilyName; }
  TString GetVolumeName(){ return mVolumeName; }

  int GetVolumeId( int* numbv ){ return mVolumeId ? mVolumeId->id( numbv ) : 0; }

  bool GetSensitive() { return mSensitive; }
  short GetTracking() { return mTracking; }
  int GetBranchings(){ return mBranchings; }

  std::vector<AgMLScoring*> GetUserHits(){ return mHitScoring; }

  void AddCut( TString cut, double value ){ mGstpar[cut] = value; }
  std::map<TString,double>& GetCuts() { return mGstpar; }
  
 
private:
protected:

  TString mModuleName; // name of the module
  TString mFamilyName; // name of the family of volumes
  TString mVolumeName; // name of the volume

  bool    mSensitive;  // volume sensitivity
  short   mTracking;   // 0=blackhole, 1=calorimeter, 2=tracking region
  int     mBranchings; // number of branchings (placed family members)

  AgMLVolumeId* mVolumeId; // Functor to calculate volume ID given reduced numbering scheme

  std::vector<AgMLScoring*> mHitScoring; // Vector of functors for hit scoring
  std::map<TString, double> mGstpar;     // GSTPAR tracking cuts for this volume

  ClassDef(AgMLExtension,0);

};

#endif

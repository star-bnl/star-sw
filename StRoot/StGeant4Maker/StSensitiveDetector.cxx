#include <StSensitiveDetector.h>

#include <TGeoManager.h>
#include <TGeoNavigator.h>
#include <TVirtualMC.h>

#include <StMCParticleStack.h>
#include <StMessMgr.h>
#include <StHitCollection.h>

#include <StarVMC/StarAgmlLib/AgMLExtension.h>

TVirtualMC*    mc        = 0;
TGeoNavigator* navigator = 0;

#include <StMessMgr.h>
#include <TString.h>

#include <TMath.h> 
//____________________________________________________________________________________________
DetectorHit::DetectorHit() : id(0), idtruth(0), volu{0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0}, copy{0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0}, numbv{0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0}, path(), nsteps(0), user()
{ 
  /* nada */ 
}
TrackerHit::TrackerHit() : DetectorHit(), 
                         position_in{0,0,0,0}, position_out{0,0,0,0}, 
                         momentum_in{0,0,0,0}, momentum_out{0,0,0,0}, 
                         de(0), ds(0), length(0), lgam(0) 
{
   /*nada*/
} 
CalorimeterHit::CalorimeterHit() : DetectorHit(), position_in{0,0,0,0}, de(0) { /* nada */ } 

//____________________________________________________________________________________________
StSensitiveDetector::StSensitiveDetector( const char* name, const char* title ) : TVirtualMCSensitiveDetector(name,title), mVolumes(), mAgMLInfo(0), mCollection(0) {
  LOG_INFO << "SD created for " << name << " " << title << endm;
} 
//____________________________________________________________________________________________
void StSensitiveDetector::Initialize(){ 
  navigator = gGeoManager->GetCurrentNavigator();
  mc = TVirtualMC::GetMC();
}
//____________________________________________________________________________________________
void StSensitiveDetector::addVolume( TGeoVolume* volume ) {
  if ( 0 == mVolumes.size() ) {
    mAgMLInfo = dynamic_cast<AgMLExtension*>( volume->GetUserExtension() );   

    TString cname  = mAgMLInfo->GetFamilyName(); cname += "_hits";
    TString ctitle = mAgMLInfo->GetModuleName(); ctitle += " "; ctitle += cname;

    if ( DetectorType::kTracker == detectorType() ) {
      mCollection = new StTrackerHitCollection(cname.Data(),ctitle.Data());
      LOG_INFO << "Setting tracker collection" << endm;
    }
    if ( DetectorType::kCalorimeter == detectorType() ) {
      mCollection = new StCalorimeterHitCollection(cname.Data(),ctitle.Data());
      LOG_INFO << "Setting calorimeter collection" << endm;
    }

  }

  mVolumes.push_back( volume );  

};
//____________________________________________________________________________________________
void StSensitiveDetector::ProcessHits(){ 

  // Is this a charged particle?  If not, skip it...
  if ( 0 == mc->TrackCharge() ) return;

  // The actual hit processing occurs in the collection. 
  mCollection->ProcessHits();

  return;
 
}
//____________________________________________________________________________________________
void StSensitiveDetector::EndOfEvent(){ 
  
  mCollection->EndOfEvent();

}
//____________________________________________________________________________________________
void StSensitiveDetector::Clear() {
  mCollection->Clear();
}
//____________________________________________________________________________________________
StSensitiveDetector::DetectorType StSensitiveDetector::detectorType() {

  // For the time being, just flag on the name of the module to decide tracker vs calorimeter  
  if ( 0 == mAgMLInfo ) {
    return DetectorType::kUninitialized;
  }

  TString module = mAgMLInfo->GetModuleName();
  module.ToLower();

  if ( module.Contains("cal") ) {
    return DetectorType::kCalorimeter;
  }
  else {
    return DetectorType::kTracker;    
  }

}
//____________________________________________________________________________________________
int StSensitiveDetector::numberOfHits(){ return int( mCollection->numberOfHits() ); }
//____________________________________________________________________________________________


#include "AgMLExtension.h"
#include <StMessMgr.h>

AgMLExtension::AgMLExtension() : TGeoRCExtension(), 
				 mModuleName("none"), 
				 mFamilyName("none"), 
				 mVolumeName("none"), 
				 mSensitive(0), 
				 mTracking(0),
				 mBranchings(0),
				 mVolumeId( new AgMLVolumeId ),
				 mHitScoring(),
				 mGstpar(),
				 mEngine(-1)
{
  Grab();
}


void AgMLExtension::AddCut( TString cut, double value ){ mGstpar[cut] = value; }

void AgMLExtension::Print( Option_t* opts ) const {

//static const char* en[] = { "default", "geant3", "geant4" };

  LOG_INFO << mModuleName.Data() << ":"
	   << mFamilyName.Data() << ":"
	   << mVolumeName.Data() << " this="
	   << this << " sens="
	   << mSensitive << " tracking="
	   << mTracking << " nbranch="
	   << mBranchings << " nuser="
    //	   << mHitScoring.size() << " engine="
    //    	   << (mEngine>=0 && mEngine < 2) ? en[mEngine] : "invalid!? " 
	   << endm;

}

std::map<TString, AgMLExtension*> AgMLExtension::mExtensionMap;


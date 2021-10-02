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
				 mGstpar()
{
  Grab();
}

void AgMLExtension::Print( const char* opts ) {

  LOG_INFO << mModuleName.Data() << ":"
	   << mFamilyName.Data() << ":"
	   << mVolumeName.Data() << " this="
	   << this << " sens="
	   << mSensitive << " tracking="
	   << mTracking << " nbranch="
	   << mBranchings << " nuser="
	   << mHitScoring.size() << " "
	   << endm;

}

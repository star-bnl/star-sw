#include "AgMLExtension.h"

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

#ifndef __GeometryUtils_h__
#define __GeometryUtils_h__

#include <TGeoNode.h>
#include <TGeoVolume.h>
#include <StarVMC/StarAgmlLib/AgMLExtension.h>

AgMLExtension* getExtension( TGeoNode*   n );
AgMLExtension* getExtension( TGeoVolume* v );

#endif

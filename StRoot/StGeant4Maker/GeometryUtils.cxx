#include <GeometryUtils.h>

AgMLExtension* getExtension( TGeoNode* n ) {

  //
  // Extension classes may be attached to the node or to the volume
  //
  AgMLExtension* agmlext = dynamic_cast<AgMLExtension*>( n->GetUserExtension() );
  if ( 0==agmlext ) agmlext = getExtension ( n->GetVolume() );

  // 
  // If the extension is still missing, inherit from the parent volume
  //
  if ( 0==agmlext ) agmlext = getExtension( n->GetMotherVolume() );


  //
  // If it is still missing, its the user's problem now
  //
  return agmlext;

}

AgMLExtension* getExtension( TGeoVolume* v ) {
  return AgMLExtension::get(v);
}

//
//  Usage:
//  root loadAgML.C GeometryUtils.C DumpAlignment.C 
//
//

#if 0
const char* paths[] = {
  "/HALL_1/CAVE_1/TpcRefSys_1/TPCE_1",
  "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1",
  "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1",
  "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1/PXSI_100",
  "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1/PXSI_200",
  "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1/PXSI_300",
  "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/PXMO_1/PXSI_400",
  "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1",
  "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1/IBSS_36",
  "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1/IBSS_72",
  "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1/IBSS_108",
  "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/IBMO_1/IBSS_144",
  "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/SFMO_1",
  "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/SFMO_1/SFSW_80",
  "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/SFMO_1/SFSW_160",
  "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/SFMO_1/SFSW_240",
  "/HALL_1/CAVE_1/TpcRefSys_1/IDSM_1/SFMO_1/SFSW_320",
  "/HALL_1"
};
void DumpAlignment()
{

  for ( unsigned int i=0; i < sizeof(paths)/sizeof(char*); i++ )
    {
       DumpAlignment( paths[i] ); 
    }
}
#endif

void DumpAlignment( TString path, TString match ); 
void DumpAlignment( TString path ); 
void DumpAlignment()
{

gGeoManager->SetTopVolume( gGeoManager->FindVolumeFast("TpcRefSys") ); 

     TGeoIterator iter( gGeoManager->GetTopVolume() );
     TGeoNode* node = 0;
     while(( node = iter.Next() )) {
         TString path;  iter.GetPath( path );
// cout << path.Data() << endl; 
         DumpAlignment( path, "PXSI_(\\d+)$" );
         DumpAlignment( path, "IBSS_(\\d+)$" );
         DumpAlignment( path, "SFSW_(\\d+)$" ); 

     } 
}

void DumpAlignment( TString path, TString match )
{

   TPMERegexp regEx( match );
       if ( regEx.Match( path ) ) {
            DumpAlignment( path ); 
       } 

}

void DumpAlignment( TString path )
{

      bool refsys = gGeoManager->FindVolumeFast("TpcRefSys"); 
      if ( 0==refsys ) path.ReplaceAll("TpcRefSys_1/","");
      std::cout << path.Data() << std::endl;
      Cd( path );
      if ( _matrix ) { 
         _matrix -> Print(); 
      }
      else { std::cout << "IDENTITY" << std::endl; } 
}

//____________________________________________________________________
TGeoVolume    *_volume    = 0;
TGeoNode      *_node      = 0;
TGeoHMatrix   *_matrix    = 0;
TGeoIterator  *_iter      = 0;
TGeoNavigator *_navigator = 0;
//____________________________________________________________________
void GeometryUtils() { 
  _navigator = gGeoManager->AddNavigator();
}
//____________________________________________________________________
void setVolume(const char* name = "CAVE" ){ 
  _volume = gGeoManager->FindVolumeFast(name); 
  _node   = 0;
  _matrix = 0;
}
//____________________________________________________________________
void CdNode(const char* name)  { 
  _node   = _volume->GetNode(name); 
  _matrix = new TGeoHMatrix( *_node->GetMatrix() );
  _volume = _node->GetVolume();
}
//____________________________________________________________________
bool Cd( const char* path )
{
  // Change to the specified path
  bool ret = _navigator -> cd(path);
  // If successful, set the node, the volume, and the GLOBAL transformation
  // for the requested node.  Otherwise, invalidate these
  if ( ret ) {
    _matrix = _navigator->GetCurrentMatrix();
    _node   = _navigator->GetCurrentNode();
    _volume = _node->GetVolume();
  }
  else {
    _matrix = 0; _node = 0; _volume = 0;
  }
  return ret;
}
//____________________________________________________________________
bool CheckVolumeForOverlapsAt( const char* path, const int ntries=100000 )
{
  if ( 0 == gRandom ) gRandom = new TRandom3(0);

  if ( 0 == Cd( path ) ) return false;
  TGeoBBox* _shape =(TGeoBBox*) _volume->GetShape();
  // for now, assume this is a bbox... because these are all gonna be 
  // active sensors in the HFT.  Future revision ought to do this right
  double dx = _shape->GetDX(); // half width in X
  double dy = _shape->GetDY(); // half width in Y
  double dz = _shape->GetDZ(); // half width in Z

  double total   = 0;
  double success = 0;
  for ( int i = 0; i < ntries; i++ )
    {
      total++;  // now sure how it could fail but think about this...

      // local coordinates on (in) the shape
      double lx = 2.0 * dx * ( gRandom->Rndm() - 0.5 );
      double ly = 2.0 * dy * ( gRandom->Rndm() - 0.5 );
      double lz = 2.0 * dz * ( gRandom->Rndm() - 0.5 );
      double local[3]; local[0] = lx; local[1] = ly; local[2] = lz;

      // global coordinate system
      double global[3]; global[0]=0; global[1]=0; global[2] = 0;
      _matrix->LocalToMaster( local, global );

      // now ask where am i?
      TGeoNode* mynode = gGeoManager->FindNode( global[0], global[1], global[2] );

      if ( mynode == _node ) success += 1;

      Cd(path); // and reset path
      
    }

  cout << Form("[%s] total   = ",path) << total << endl;
  cout << Form("[%s] success = ",path) << success << endl;
  cout << Form("[%s] rate    = ",path) << double(success) / double(total) << endl;
  

  
}
//____________________________________________________________________
// bool GlobalMatrix(const char* path, const char* top="CAVE" )
// {
//   if ( 0==_iter ) _iter = new TGeoIterator( gGeoManager->FindVolumeFast(top) );
//   // Brute force it...
//   TString mypath = "$";
//   while (1) 
//     {
//       _iter->GetPath( mypath );
//       _matrix = new TGeoHMatrix( *_iter -> GetCurrentMatrix() );
//       //      _node   = _iter->GetNode(1);
//       //      _volume = _node->GetVolume();
//       if ( mypath == path ) {
// 	return true;
//       }
            
//     }

//   return false;

// }


//____________________________________________________________________
void applyTransparency(const Char_t *TOP, Int_t value)
{


  TGeoIterator next( gGeoManager->FindVolumeFast(TOP) );
  TGeoNode *node = 0;
  
  while ( (node=(TGeoNode*)next()) )
    {
      TGeoVolume *volume = node->GetVolume();
      if ( volume )
	volume->SetTransparency(value);
      
    }
}

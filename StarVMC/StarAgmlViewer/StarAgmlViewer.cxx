#include "StarAgmlViewer.h"

#include "TGeoManager.h"
#include "TGeoNode.h"
   #include "TEveGeoNode.h"
#include "TGeoVolume.h"
#include "TEveViewer.h"
#include "TGLViewer.h"

StarAgmlViewer::StarAgmlViewer( const Char_t *name ) : StMaker(name)
{

  SetAttr("TopVolume", "HALL");
  SetAttr("TopNode",   "CAVE_1");
  
  SetAttr("TopVolumeTrans", 90 );
  SetAttr("DaughterVolumeTrans", 90);

}

Int_t StarAgmlViewer::Init()
{
  // Load the geometry
  LoadGeometry();

  // Create event viewer manager
  TEveManager::Create();  

  // Store pointer to gEve (and stop using gEve... bad bad bad globals...)
  mEve = gEve;
  
  mEve -> 
    GetDefaultGLViewer() ->
       SetCurrentCamera(TGLViewer::kCameraPerspXOZ);

  // Add default visualization elements
  addGlobalElement
    ( 
     SAttr("TopVolume"), 
     SAttr("TopNode") 
      );

  return StMaker::Init();
}

Bool_t StarAgmlViewer::addGlobalElement( const Char_t *topv,
					 const Char_t *topn )
{
  // Get the top volume and node for visualization
  TGeoVolume *topVolume = gGeoManager->FindVolumeFast( topv );
  TGeoNode   *topNode   = topVolume->FindNode( topn );

  // Apply transparency
  //  applyTransparency(topv,1);
  
  TEveGeoTopNode *top = new TEveGeoTopNode( gGeoManager, topNode );
  gEve -> AddGlobalElement(top);


  
  return true;
   
}

Bool_t StarAgmlViewer::LoadGeometry()
{

  std::map<Int_t,Int_t> cmap;
  Int_t i = kBlue;
  cmap[ 0] = kBlue;
  cmap[ 1] = kBlue - 1;
  cmap[ 2] = kBlue + 1;
  cmap[ 3] = kBlue - 2;
  cmap[ 4] = kBlue + 2;
  cmap[ 5] = kBlue - 3;
  cmap[ 6] = kBlue + 3;
  cmap[ 7] = kBlue - 4;
  cmap[ 8] = kBlue + 4;
  cmap[ 9] = kBlue - 10;
  cmap[10] = kBlue - 5;
  cmap[11] = kBlue - 9;
  cmap[12] = kBlue - 6;
  cmap[13] = kBlue - 8;
  cmap[14] = kBlue - 7;
  cmap[15] = kBlue;
  

  // Remap colors
  TGeoVolume *top = gGeoManager->FindVolumeFast("HALL");
  TGeoIterator next( top ); 
  TGeoNode *node = 0;
  while ( (node=(TGeoNode*)next()) )
    {
      TGeoVolume *volume = node->GetVolume();
      volume->SetFillColor( cmap[ next.GetLevel() ] );
      volume->SetLineColor( cmap[ next.GetLevel() ] );
    }

  return true;
}

void StarAgmlViewer::applyTransparency(const Char_t *TOP, Int_t mode )
{

  // Apply transparency to top volume
  TGeoVolume *top = gGeoManager->FindVolumeFast(TOP);
  Int_t value = IAttr("TopVolumeTrans");

  if ( !top ) return;
  top->SetTransparency(value);
  if ( mode<0 ) return;
 
  value = IAttr("DaughterVolumeTrans");

  TGeoIterator next( top );
  next.SetType(mode);

  TGeoNode *node = 0;

  while ( (node=(TGeoNode*)next()) )
    {
      TGeoVolume *volume = node->GetVolume();
      if ( volume )
        volume->SetTransparency(value);

    }
}

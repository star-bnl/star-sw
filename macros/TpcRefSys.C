/*
  .L .sl64_gcc447/obj/StarDb/AgiGeometry/Geometry.y2007h.C
  .L .sl64_gcc447/obj/StarDb/AgiGeometry/Geometry.y2013_2x.C
  CreateTable()
  .x TpcRefSys.C+
*/
#ifndef __CINT__
#include "Riostream.h"
#include "TGeoManager.h"
#include "TGeoMatrix.h"
#include "TGeoNode.h"
#include "TGeoVolume.h"
#include "TObjArray.h"
#endif
void TpcRefSys() {
  // Fix TpcRedSys
  TGeoVolumeAssembly *TpcRefSys = (TGeoVolumeAssembly *) gGeoManager->FindVolumeFast("TpcRefSys");
  if (! TpcRefSys) {
    cout << "Create and Fill TpcRefSys" << endl;
    TpcRefSys = new TGeoVolumeAssembly("TpcRefSys");
    TGeoVolume *CAVE = gGeoManager->FindVolumeFast("CAVE");
    if (! CAVE) return;
    cout << "Found CAVE" << endl;
    Int_t         NV = 13;
    const Char_t *VNames[13] = {"SVTT","SCON","SFMO","TPCE","FTPC","FTMO","PXMO","IGMO","YPXM","FGMO",
				"IBMO","BTOF","IDSM"};
    TObjArray *nodes = CAVE->GetNodes();
    Int_t nd = CAVE->GetNdaughters();
    for (Int_t i = nd - 1; i >= 0 ; i--) {
      TGeoNode *node = (TGeoNode *) nodes->UncheckedAt(i);
      TGeoVolume *vol = node->GetVolume();
      TString Name(vol->GetName());
      //      cout << "node " << node->GetName() << " vol " << Name.Data() << endl;
      for (Int_t j = 0; j < NV; j++) {
	if (Name == VNames[j]) {
	  cout << "Move " << vol->GetName() << " from CAVE to TpcRefSys" << endl;
// 	  if (node->IsOverlapping()) 
// 	    TpcRefSys->AddNodeOverlap(vol, node->GetNumber(), node->GetMatrix(), "");
// 	  else 
	    TpcRefSys->AddNode(vol, node->GetNumber(), node->GetMatrix());
	  CAVE->RemoveNode(node);
	  break;
	}
      }
    }
    CAVE->AddNode(TpcRefSys,1);
    TpcRefSys->PrintNodes();
  }
}

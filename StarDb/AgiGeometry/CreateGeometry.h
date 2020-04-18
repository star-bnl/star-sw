#ifndef __CreateGeometry__
#define __CreateGeometry__
#if 1
#if !defined( __CINT__) && !defined(__CLING__)
#include "Riostream.h"
#include "TGeoManager.h"
#include "TGeoMatrix.h"
#include "TDataSet.h"
#include "TObjectSet.h"
#include "TInterpreter.h"
#include "TSystem.h"
#include "TError.h"
#include "TGeoNode.h"
#include "TGeoVolume.h"
#include "TObjArray.h"
#include "Tenv.h"
#endif
//#include "Rotations+"
//________________________________________________________________________________
void Geometry() {
}
//________________________________________________________________________________
TGeoMaterial *GetMat(const char *matname) {
  TGeoMaterial *mat = gGeoManager->GetMaterial(matname);
  if (! mat) cout << "GetMat: cannot find material " << matname << endl;
  return mat;
}
//________________________________________________________________________________
TGeoMedium *GetMed(const char *medname) {
  TGeoMedium *med = gGeoManager->GetMedium(medname);
  if (! med)  cout << "GetMed: cannot find media " << medname << endl;
  return med;
}
//________________________________________________________________________________
TGeoRotation *GetRot(const char *rotname) {
  TObjArray *listOfMatrices =  gGeoManager->GetListOfMatrices();
  TGeoRotation *rot = (TGeoRotation *) listOfMatrices->FindObject(rotname);
  if (! rot)  cout << "GetRot: cannot find rotation " << rotname << endl;
  return rot;
}
#endif
#include "Material.h"
#include "Media.h"
//________________________________________________________________________________
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
    //      cout << "node " << node->GetName() << " vol " << Name.Data() << endl;
    for (Int_t j = 0; j < NV; j++) {
      TObjArray *nodes = CAVE->GetNodes();
      Int_t nd = CAVE->GetNdaughters();
      for (Int_t i = nd - 1; i >= 0 ; i--) {
	TGeoNode *node = (TGeoNode *) nodes->UncheckedAt(i);
	TGeoVolume *vol = node->GetVolume();
	if (! vol) continue;
	TString Name(vol->GetName());
	if (Name == VNames[j]) {
	  cout << "Move " << vol->GetName() << " from CAVE to TpcRefSys" << endl;
	  TpcRefSys->AddNode(vol, node->GetNumber(), node->GetMatrix());
	  CAVE->RemoveNode(node);
	  break;
	}
      }
    }
    CAVE->AddNode(TpcRefSys,1);
    //    TpcRefSys->PrintNodes();
  }
}

//________________________________________________________________________________
TDataSet *CreateGeometry(const Char_t *name, TEnv *configGeom) {
  TDataSet *geometry = 0;
  TObjectSet *geom = 0;
  if (gGeoManager) {
    cout << "VMC geometry " << gGeoManager->GetName() << " has beed created. Ignore request for " 
	 << name << " ! " << endl;
    return geom;
  }
  gSystem->Load("libRotations");
  TString path(".:");
  TString STAR(gSystem->Getenv("STAR"));
#if 0
  TString STAR_HOST_SYS(gSystem->Getenv("STAR_HOST_SYS"));
  TString OBJ;
  if (gSystem->Getenv("NODEBUG")) OBJ = "OBJ";
  else                            OBJ = "obj";
#endif
  path += "./StarDb/AgiGeometry:";
  path += STAR + "/StarDb/AgiGeometry";
#if 0
  path += "." + STAR_HOST_SYS + "/" + OBJ + "/StarDb/AgiGeometry:";
  path += STAR + "/." + STAR_HOST_SYS + "/" + OBJ + "/StarDb/AgiGeometry:";
#endif
  TString geomF(name); geomF += ".h";
  Char_t *file = gSystem->Which(path,geomF,kReadPermission);
  if (! file) Fatal("CreateGeometry","File %s has not found in path %s",geomF.Data(),path.Data());
  else        Warning("CreateGeometry","File %s has been found as %s",geomF.Data(),file);
  TString command = ".L "; command += file;
  gInterpreter->ProcessLine(command);
  TString cmd(name); cmd += "()";
  gInterpreter->Calc(cmd);
  command.ReplaceAll(".L ",".U "); 
  gInterpreter->ProcessLine(command);
  if (gGeoManager) {
    geometry = new TDataSet("Geometry");
    geom = new TObjectSet("GeoManager",gGeoManager,kFALSE);
    geom->SetTitle(name);
    geometry->Add(geom);
    TObjectSet *confgeom = new TObjectSet("configGeom",configGeom,kTRUE);
    geometry->Add(confgeom);
  }
#if 0
  // Fix TpcRedSys
  TpcRefSys();
#endif
  gGeoManager->CloseGeometry();
  return  geometry;
}
#endif /* ! __CreateGeometry__ */

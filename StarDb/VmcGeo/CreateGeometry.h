// $Id: CreateGeometry.h,v 1.5 2011/09/26 14:34:42 fisyak Exp $
// $Log: CreateGeometry.h,v $
// Revision 1.5  2011/09/26 14:34:42  fisyak
// use VmcGeo instead of VmcGeometry
//
// Revision 1.4  2009/01/14 16:31:50  fisyak
// Freeze conversion from mortran to Cint for SVT
//
// Revision 1.3  2008/09/03 20:44:46  fisyak
// replace tpc geometry with translate one from mortran, clean ups
//
#ifndef __CreateGeometry__
#define __CreateGeometry__
#ifndef __CINT__
#include "Riostream.h"
#include "TGeoManager.h"
#include "TGeoMatrix.h"
#include "TDataSet.h"
#include "TObjectSet.h"
#include "TInterpreter.h"
#include "TSystem.h"
#include "TError.h"
#endif
TDataSet *CreateGeometry(const Char_t *name="y2005", const Char_t *vers = 0) {
  TObjectSet *geom = 0;
  if (gGeoManager) {
    cout << "VMC geometry " << gGeoManager->GetName() << " has beed created. Ignore request for " 
	 << name << " ! " << endl;
    return geom;
  }
  TString cmd(name);
  if (vers) {
    gInterpreter->ProcessLine(Form("#define %s 1",vers));
    cmd += "(\""; cmd += vers; cmd += "\")";
  }
  else {
    cmd += "()";
  }
  static const Char_t *path  = ".:./StarDb/VmcGeo:$STAR/StarDb/VmcGeo";
  TString geomF(name); geomF += ".h";
  Char_t *file = gSystem->Which(path,geomF,kReadPermission);
  if (! file) Fatal("CreateGeometry","File %s has not found in path %s",geomF.Data(),path);
  else        Warning("CreateGeometry","File %s has been found as %s",geomF.Data(),file);
  TString command = ".L "; command += file;
  gInterpreter->ProcessLine(command);
  gInterpreter->Calc(cmd);
  command.ReplaceAll(".L ",".U "); 
  gInterpreter->ProcessLine(command);
  if (gGeoManager) {
    geom = new TObjectSet("Geometry",gGeoManager,kFALSE);
    geom->SetTitle(name);
  }
  return (TDataSet *) geom;
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

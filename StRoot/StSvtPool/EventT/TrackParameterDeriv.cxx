#include "TMath.h"
#include "TrackParameterDeriv.h"
//________________________________________________________________________________
Double_t TrackParameterDeriv::Fcn(Double_t  add) {
  if (! fHelixI || ! fMatrix) return 0;
  const Double_t *xyz = fHelixI->GetXYZ();
  const Double_t *dir = fHelixI->GetDir();
  Double_t XyzDirRho[6] = {xyz[0], xyz[1], xyz[2], 
			   dir[2]/TMath::Sqrt(dir[0]*dir[0]+dir[1]*dir[1]), TMath::ATan2(dir[1],dir[0]), 
			   fHelixI->GetRho()};
  XyzDirRho[fJ] += add;
  Double_t CosL = 1./TMath::Sqrt(1. + XyzDirRho[3]*XyzDirRho[3]);
  Double_t Dir[3] = {CosL*TMath::Cos(XyzDirRho[4]),
		     CosL*TMath::Sin(XyzDirRho[4]),
		     CosL*XyzDirRho[3]};
  THelixTrack vHelixI(XyzDirRho,Dir,XyzDirRho[5]);
  const Double_t *tr = fMatrix->GetTranslation();
  const Double_t *rot = fMatrix->GetRotationMatrix();
  static const Double_t stepMX = 1.e2;
  Double_t RefSurface[4] = {-(tr[0]*rot[2]+tr[1]*rot[5]+tr[2]*rot[8]),
			    rot[2],      rot[5],      rot[8]};
  Double_t step = vHelixI.Step(stepMX, RefSurface, 4, fGlobalXYZ, fGlobalDir);
  if (step > stepMX) {fFail = 1; return 0;}
  fMatrix->MasterToLocal(fGlobalXYZ,fLocalXYZ);
  fMatrix->MasterToLocalVect(fGlobalDir,fLocalDir);
  return fLocalXYZ[fI];
}

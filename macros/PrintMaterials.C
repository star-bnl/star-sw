void PrintMaterials() {
  if (! gGeoManager) return;
  TList *matList = gGeoManager->GetListOfMaterials();
  if (! matList) return;
  TListIter next(matList);
  TGeoMaterial *mat = 0;
  while ((mat = (TGeoMaterial*) next())) {
    mat->Print();
  }
}

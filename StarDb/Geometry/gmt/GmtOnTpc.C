TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_Survey") || !gROOT->GetClass("TGeoRotation")) return 0;
  Survey_st row = {0, 1,0,0, 0,1,0, 0,0,1, 0,0,0, 1e-5,1e-5,1e-4,4e-3,4e-3,4e-3,"Pass 5"};
  //   
  /* A.Lebedev 07/28/15
    GMT modules installed
    West   mid of sectors 2 and 5
    East    mid of sectors 17 and 22
    So it is a continuous  2 lines Phi = 210 and 300.
    I don't know exact Z position
    Best regards,
    Alexie
  */
  double inch = 2.54;
  double R = 85.606; // inches => cm
  double deltaphi = 5./R; // crude radian conversion
  //                sector = 5/19                    sector = 2/22
  // sector               5      5       19       19       2      2       22       22
  // module               0      1        2        3       4      5        6        7
  Double_t   z[8] = {77.768, 2.729, -77.768,  -2.729, 77.768, 2.729, -77.768,  -2.729};
  Int_t    phi[8] = {   300,   300,     300,     300,     30,    30,      30,      30};
			      
  Int_t noModules = 8;// 
  St_Survey *tableSet = new St_Survey("GmtOnTpc",noModules);
  TString Rot;
  TGeoRotation *rotm = 0;
  for (Int_t m = 0; m < noModules; m++) {
    row.Id = m;
    TGeoRotation *rotm = new TGeoRotation(Rot);
    //    rotm->RotateZ(-phi[m]); // Pass 6
    rotm->RotateZ(phi[m]); // Pass 5, 7
    Double_t *rotaion = rotm->GetRotationMatrix();
    memcpy(&row.r00, rotm->GetRotationMatrix(), 9*sizeof(Double_t));
    Double_t xyz[3] = {R*inch, 0, z[m]*inch};
    rotm->LocalToMaster(xyz, &row.t0);
    tableSet->AddAt(&row.Id);
    delete rotm;
  }
  return (TDataSet *) tableSet;
}

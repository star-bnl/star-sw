void ePICMARCO() {
  const Char_t *FileName = "MARCO_v.6.4.1.1.3_1.7T_Magnetic_Field_Map_2022_11_14_rad_coords_cm_T.txt";
  TString fName(FileName);
  fName.ReplaceAll(".txt",".root");
  TFile *fOut = new TFile(fName,"recreate");
  //                         R                  Z  //400000
  TH2F *fBr = new TH2F("br","br",  500, -1.0, 999.0,  800, -801, 799);
  TH2F *fBz = new TH2F("bz", "bz", 500, -1.0, 999.0,  800, -801, 799);
  FILE *fp = fopen(FileName,"r");
  Int_t npoints = 0;
  Float_t R,Z,BR,BZ;
  char line[121];
  while (fgets(&line[0],120,fp)) {
    Int_t nc = sscanf(&line[0],"%f %f %f %f",&R, &Z, &BR, &BZ);
    //    printf("%i %s",nc, line);
    if (nc != 4) continue;
    //    printf("R = %f, Z = %f, BR = %f, BZ = %f\n", R, Z, BR, BZ);
    fBr->Fill(R, Z, BR);
    fBz->Fill(R, Z, BZ); 
    npoints++;
  }
  fclose(fp);
  printf("Read %i points\n", npoints);
  fOut->Write();
}

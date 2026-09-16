void gen_bsmd_cells() {
  
  gROOT->Macro("LoadLogger.C");
  gROOT->Macro("loadMuDst.C");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StMcEvent"); 
  gSystem->Load("StMcEventMaker"); 
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcSimulatorMaker");

  StEmcGeom* geoE = StEmcGeom::instance("bsmde");
  StEmcGeom* geoP = StEmcGeom::instance("bsmdp");

  ofstream out("bsmd_cells.dat");
  out.precision(7);
  out.setf(ios::fixed);

  Float_t x, y, z;
  TVector3 v;

  cout << "Generating BSMD Map" << endl;

  for (int rileft = 1; rileft <= 2; rileft++) { 
    for (int phi_local = 60; phi_local >= 1; phi_local--) { 
        
      int m = (rileft == 1) ? (61 - phi_local) : (phi_local + 60);

      int phi_enc = (rileft == 1) ? phi_local : (phi_local + 60);

      for (int layer = 1; layer <= 2; layer++) {
        for (int strip = 1; strip <= 75; strip++) {

        int e = (layer == 1) ? strip : (strip + 75);
        int real_eta_bin = (e - 1) / 15 + 1;
            
          int temp_vid = 100000000  * rileft 
                        + 1000000   * real_eta_bin 
                        + 1000      * phi_enc 
                        + 100       * layer 
                        + strip;

          if (geoE->getXYZfromGeant(temp_vid, x, y, z) == 0) {
            v.SetXYZ(x, y, z);
            int final_vid = 100000000 * rileft 
                          + 1000000   * real_eta_bin 
                          + 1000      * phi_enc 
                          + 100       * layer 
                          + strip;

            int s = 1;
            int softId;
            geoE->getId(m, e, s, softId);

            out << final_vid << " " << softId << " " << m << " " << e << " " << s << " " << 3 << " " 
                << v.Eta() << " " << v.Phi() << endl;
          }
        }
      }
    }
  }
  for (int rileft = 1; rileft <= 2; rileft++) { 
    for (int phi_local = 60; phi_local >= 1; phi_local--) { 
      
      int m = (rileft == 1) ? (61 - phi_local) : (phi_local + 60);
      
      int phi_enc = (rileft == 1) ? phi_local : (phi_local + 60);
      
      for (int eta_bin = 1; eta_bin <= 10; eta_bin++) {
        for (int strip = 1; strip <= 15; strip++) {
            
            int vid = 100000000 * rileft 
                    + 1000000   * eta_bin 
                    + 1000      * phi_enc 
                    + 100       * 3 
                    + strip;

            if (geoP->getXYZfromGeant(vid, x, y, z) == 0) {
              v.SetXYZ(x, y, z);

              int e = eta_bin;
              int s = strip;
              int softId;
              geoP->getId(m, e, s, softId);

              out << vid << " " << softId << " " << m << " " << e << " " << s << " " << 4 << " " 
                  << v.Eta() << " " << v.Phi() << endl;
            }
          }
        }
      }
    }

  out.close();
  cout << "Done. bsmd_cells.dat generated." << endl;
  
}
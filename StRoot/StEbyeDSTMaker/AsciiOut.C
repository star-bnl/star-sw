{
  #include<fstream.h>
  
  //
  // A macro for extracting data (from STAR EbyE Ntuples) 
  //  and writing to ascii files for offline (laptop) 
  //  analysis of pt fluctuations
  //
  // by Jeff Reid
  //

  // open files for output
  fstream plusOut0("qm01.mb0c2.+",ios::out);
  fstream minusOut0("qm01.mb0c2.-",ios::out);

  fstream plusOut1("qm01.mb1c2.+",ios::out);
  fstream minusOut1("qm01.mb1c2.-",ios::out);

  fstream plusOut2("qm01.mb2c2.+",ios::out);
  fstream minusOut2("qm01.mb2c2.-",ios::out);

  fstream plusOut3("qm01.mb3c2.+",ios::out);
  fstream minusOut3("qm01.mb3c2.-",ios::out);

  fstream plusOut4("qm01.mb4c2.+",ios::out);
  fstream minusOut4("qm01.mb4c2.-",ios::out);

  fstream plusOut5("qm01.mb5c2.+",ios::out);
  fstream minusOut5("qm01.mb5c2.-",ios::out);

  fstream plusOut6("qm01.mb6c2.+",ios::out);
  fstream minusOut6("qm01.mb6c2.-",ios::out);

  fstream plusOut7("qm01.mb7c2.+",ios::out);
  fstream minusOut7("qm01.mb7c2.-",ios::out);

  // load the Ebye Ntuple files
  TChain *chain = new TChain("ntuple");

  // open data file
  chain->Add("C:/root/data/star/minbias/minbias.root");

  Float_t Np,Nm;
  Float_t pp,pm;
  Float_t p2p,p2m;
  Float_t Nc,No;

  chain->SetBranchAddress("Np",&Np);
  chain->SetBranchAddress("Nm",&Nm);
  chain->SetBranchAddress("pp",&pp);
  chain->SetBranchAddress("pm",&pm);
  chain->SetBranchAddress("p2p",&p2p);
  chain->SetBranchAddress("p2m",&p2m);

  chain->SetBranchAddress("No",&No);
  chain->SetBranchAddress("Nc",&Nc);

  Int_t nentries,i;
  Int_t count[20];
  Float_t Nbar[20];  // multiplicity for centrality definition
  Float_t Nmine[20]; // actual multiplicity of data (after cuts)

  for (i = 0 ; i < 8 ; i++) {count[i] = 0; Nbar[i] = 0; Nmine[i] = 0;}

  nentries = Int_t(chain->GetEntries());
  for (i = 0 ; i < nentries ; i++) {
    chain->GetEntry(i);

    No = Nc;

    if ((Np > 0) && (Nm > 0)) {

      // Write data to 8 files with equal numbers of events in each centrality bin
      if (No < 5) {
        plusOut0 << Np << " " << pp << " " << p2p << endl;
        minusOut0 << Nm << " " << pm << " " << p2m << endl;
        Nbar[0] += Nc;
        Nmine[0] += Np+Nm;
        count[0]++; 
      } else if (No < 12) {
        plusOut1 << Np << " " << pp << " " << p2p << endl;
        minusOut1 << Nm << " " << pm << " " << p2m << endl;
        Nbar[1] += Nc;
        Nmine[1] += Np+Nm;
        count[1]++;
      } else if (No < 25) {
        plusOut2 << Np << " " << pp << " " << p2p << endl;
        minusOut2 << Nm << " " << pm << " " << p2m << endl;
        Nbar[2] += Nc;
        Nmine[2] += Np+Nm;
        count[2]++;
      } else if (No < 45) {
        plusOut3 << Np << " " << pp << " " << p2p << endl;
        minusOut3 << Nm << " " << pm << " " << p2m << endl;
        Nbar[3] += Nc;
        Nmine[3] += Np+Nm;
        count[3]++;
      } else if (No < 75) {
        plusOut4 << Np << " " << pp << " " << p2p << endl;
        minusOut4 << Nm << " " << pm << " " << p2m << endl;
        Nbar[4] += Nc;
        Nmine[4] += Np+Nm;
        count[4]++;
      } else if (No < 116) {
        plusOut5 << Np << " " << pp << " " << p2p << endl;
        minusOut5 << Nm << " " << pm << " " << p2m << endl;
        Nbar[5] += Nc;
        Nmine[5] += Np+Nm;
        count[5]++;
      } else if (No < 172) {
        plusOut6 << Np << " " << pp << " " << p2p << endl;
        minusOut6 << Nm << " " << pm << " " << p2m << endl;
        Nbar[6] += Nc;
        Nmine[6] += Np+Nm;
        count[6]++;
      } else {       
        plusOut7 << Np << " " << pp << " " << p2p << endl;
        minusOut7 << Nm << " " << pm << " " << p2m << endl;
        Nbar[7] += Nc;
        Nmine[7] += Np+Nm;
        count[7]++;
        //printf("%i \n",i);
      }

    }
  }

  // double-check bin centrality definitions  
  for (i = 0 ; i < 8 ; i++) {
    Nbar[i] /= count[i];
    Nmine[i] /= count[i];
    printf("%f=count[%i] %f=N(cent)[%i] %f=N(actual)[%i]\n",
           ((float)count[i]/(float)nentries),i,Nbar[i],i,Nmine[i],i);
  }

}


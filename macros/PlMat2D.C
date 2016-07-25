TGeant3 *geant3 = 0;

struct Gpart_t {
  Int_t  No;
  Char_t *Part;
  Int_t   Options;
  Float_t Mass;    
  Float_t Charge;
  Float_t LifeTime;
};
Gpart_t Gpart[50] = {
  {    1,"GAMMA",                      1,       0.0000E+00,     0.,   0.10000E+16},
  {    2,"POSITRON",                   2,       0.5110E-03,     1.,   0.10000E+16},
  {    3,"ELECTRON",                   2,       0.5110E-03,    -1.,   0.10000E+16},
  {    4,"NEUTRINO",                   3,       0.0000E+00,     0.,   0.10000E+16},
  {    5,"MUONP",                      5,       0.1057E+00,     1.,   0.21970E-05},
  {    6,"MUONN",                      5,       0.1057E+00,    -1.,   0.21970E-05},
  {    7,"PION0",                      3,       0.1350E+00.     0.,   0.84000E-16},
  {    8,"PIONP",                      4,       0.1396E+00,     1.,   0.26030E-07},
  {    9,"PIONN",                      4,       0.1396E+00,    -1.,   0.26030E-07},
  {   10,"KAON0LONG",                  3,       0.4977E+00,     0.,   0.51700E-07},
  {   11,"KAONP",                      4,       0.4937E+00,     1.,   0.12370E-07},
  {   12,"KAONN",                      4,       0.4937E+00,    -1.,   0.12370E-07},
  {   13,"NEUTRON",                    3,       0.9396E+00,     0.,   0.88700E+03},
  {   14,"PROTON",                     4,       0.9383E+00,     1.,   0.10000E+16},
  {   15,"ANTIPROTON",                 4,       0.9383E+00,    -1.,   0.10000E+16},
  {   16,"KAON0SHORT",                 3,       0.4977E+00,     0.,   0.89260E-10},
  {   17,"ETA",                        3,       0.5475E+00,     0.,   0.54850E-18},
  {   18,"LAMBDA",                     3,       0.1116E+01,     0.,   0.26320E-09},
  {   19,"SIGMAP",                     4,       0.1189E+01,     1.,   0.79900E-10},
  {   20,"SIGMA0",                     3,       0.1193E+01,     0.,   0.74000E-19},
  {   21,"SIGMAN",                     4,       0.1197E+01,    -1.,   0.14790E-09},
  {   22,"XI0",                        3,       0.1315E+01,     0.,   0.29000E-09},
  {   23,"XIN",                        4,       0.1321E+01,    -1.,   0.16390E-09},
  {   24,"OMEGAN",                     4,       0.1672E+01,    -1.,   0.82200E-10},
  {   25,"ANTINEUTRON",                3,       0.9396E+00,     0.,   0.88700E+03},
  {   26,"ANTILAMBDA",                 3,       0.1116E+01,     0.,   0.26320E-09},
  {   27,"ANTISIGMAN",                 4,       0.1189E+01,    -1.,   0.79900E-10},
  {   28,"ANTISIGMA0",                 3,       0.1193E+01,     0.,   0.74000E-19},
  {   29,"ANTISIGMAP",                 4,       0.1197E+01,     1.,   0.14790E-09},
  {   30,"ANTIXI0",                    3,       0.1315E+01,     0.,   0.29000E-09},
  {   31,"ANTIXIP",                    4,       0.1321E+01,     1.,   0.16390E-09},
  {   32,"ANTIOMEGAP",                 4,       0.1672E+01,     1.,   0.82200E-10},
  {   33,"TAUP",                       4,       0.1777E+01,     1.,   0.29560E-12},
  {   34,"TAUN",                       4,       0.1777E+01,    -1.,   0.29560E-12},
  {   35,"DP",                         4,       0.1869E+01,     1.,   0.10620E-11},
  {   36,"DN",                         4,       0.1869E+01,    -1.,   0.10620E-11},
  {   37,"D0",                         3,       0.1865E+01,     0.,   0.42800E-12},
  {   38,"ANTID0",                     3,       0.1865E+01,     0.,   0.42800E-12},
  {   39,"DSP",                        4,       0.1969E+01,     1.,   0.43600E-12},
  {   40,"DSN",                        4,       0.1969E+01,    -1.,   0.43600E-12},
  {   41,"LAMBDACP",                   4,       0.2285E+01,     1.,   0.17900E-12},
  {   42,"WP",                         4,       0.8022E+02,     1.,   0.31600E-24},
  {   43,"WN",                         4,       0.8022E+02,    -1.,   0.26400E-24},
  {   44,"Z0",                         3,       0.9119E+02,     0.,   0.77400E-25},
  {   45,"DEUTERON",                   8,       0.1876E+01,     1.,   0.10000E+16},
  {   46,"TRITON",                     8,       0.2809E+01,     1.,   0.10000E+16},
  {   47,"ALPHA",                      8,       0.3727E+01,     2.,   0.10000E+16},
  {   48,"gianottino",                 6,       0.0000E+00,     0.,   0.10000E+16},
  {   49,"HE3",                        8,       0.2809E+01,     2.,   0.10000E+16},
  {   50,"Cerenkov",                   7,       0.0000E+00,     0.,   0.10000E+16}
};
void PlMat2) {
  if (! gMC) {
    gROOT->LoadMacro("bfc.C");
    bfc(-1,"geant NoDefault",0,0,0);
    geant = (St_geant_Maker *) chain->GetMaker("geant");
    geant->Do("gdebug 2");
    geant->Do("detp geometry year2001");
    //    geant->Do("loss 2");
    //    geant->Do("physi");
    geant->Do("gclose all");
  }
  TGeant3 *geant3 = (TGeant3 *) gMC;
  Int_t imat=41;
  char name[80]= "                                                "; 
  Float_t a, z, dens, radl, absl, ubuf[1], nbuf = 0;
  geant3->Gfmate(imat, name, a, z, dens, radl, absl, ubuf, nbuf);
  printf("%s: a:%f z:%f dens:%f radl:%f absl:%f\n",name,a, z, dens, radl, absl,nbuf);
  Int_t parl[] = {2,3,14};//,6,8,9,11,12,14,15,45,46,49};
  //  Int_t parl[] = {14};//,6,8,9,11,12,14,15,45,46,49};
  const Int_t Npart = sizeof(parl)/sizeof(Int_t);
  TH1F *hists[10];
  Int_t kdim = 1;
  Float_t t[1];
  Float_t val[1];
  Float_t pcut[5]  = {0,0,0,0,0};
  Int_t ixst;
  for (int j=0; j<Npart; j++) {
    Int_t Np = parl[j] - 1;
    Int_t part = Gpart[Np].No;
    TString Name(Gpart[Np].Part);
    TString Title("dE/dx for ");
    Title += Name;
    Title += " (keV/cm) versus log10(bata*gamma) in ";
    Title += name;
    Int_t nbins = 51;
    //    hists[j] = new TH1F(Name.Data(),Title.Data(),nbins,-0.025,4.025);
    hists[j] = new TH1F(Name.Data(),Title.Data(),nbins,-1.05,4.05);
    hists[j]->SetMinimum(2.e-3);
    hists[j]->SetLineColor(j+1);
#if 1
    printf("{// %s",Name.Data());
#endif
    for (int i=1; i<=nbins; i++) {
      Double_t bgL = hists[j]->GetBinCenter(i);
      //      Double_t bgL = hists[j]->GetBinLowEdge(i);
      Double_t bg  = pow(10.,bgL);
      Double_t bgLe = TMath::Log(bg);
      Double_t gamma = sqrt(1.+bg*bg);
      Double_t b2inv = 1. + 1./(bg*bg);
      t[0]    = Gpart[Np].Mass*(gamma - 1.);
//       t[0] = pow(10.,bgL);
//       Double_t gamma = t[0]/Gpart[Np].Mass + 1;
//       Double_t b2inv = 1 + gamma*gamma;
//       Double_t bg    = TMath::Sqrt(gamma*gamma - 1);
      geant3->Gftmat(imat,part, "LOSS",kdim,t,val,pcut,ixst);
      //      if (!ixst) continue;
      Double_t val0 = 1.e3*val[0];
      if (part == 49) val0 /= 4;
//       val0 /= b2inv;
//       val0 /= (bgL + 2.);
      Double_t val2 = val0; //TMath::Log(val0);
      hists[j]->SetBinContent(i,val2);
#if 0
      //      hists[j]->SetBinError(i,0.01*TMath::Abs(val0));
      printf("%s part:%i mass:%f bgL:%f bg:%f gamma:%f T:%f val:%f\n",
	     Name.Data(),part,Gpart[Np].Mass,bgL,bg,gamma,t[0],val0);
#endif
#if 1
      if (i%5 == 1) printf("\n");
      if (i != nbins) printf(" %10.6f,",val0);
      else            printf(" %10.6f ",val0);
#endif
    }
    printf("};\n");
    if (!j) hists[j]->Draw();
    hists[j]->Draw("samec");
  }
}

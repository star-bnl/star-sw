// void BadDriftRegions(const Char_t *macro = 
// 		     "/afs/rhic.bnl.gov/star/packages/.DEV2/StarDb/Calibrations/svt/svtHybridDriftVelocity.20070524.000208.C",
// 		     const Char_t *badF = "/star/data07/calib/fisyak/SvtSsdAlignment/RF.Bad.Drift",
// 		     Int_t date = 20070524, Int_t time = 209) {
void BadDriftRegions(const Char_t *macro = 
		     "/afs/rhic.bnl.gov/star/packages/.DEV2/StarDb/Calibrations/svt/svtHybridDriftVelocity.20070321.000207.C",
		     const Char_t *badF = "/star/data07/calib/fisyak/SvtSsdAlignment/FF.Bad.Drift",
		     Int_t date = 20070321, Int_t time = 209) {
  gSystem->Load("libStDb_Tables.so");
  gROOT->LoadMacro(macro);
  St_svtHybridDriftVelocity *svtHybridDriftVelocity = (St_svtHybridDriftVelocity *) CreateTable();  
  Int_t NN = svtHybridDriftVelocity->GetNRows();
  FILE *fp = fopen(badF,"r");
  if (! fp) return;
  Int_t Barrel, Ladder, Wafer, Hybrid;
  Float_t xmin, xmax;
  Char_t line[120];
  while (fgets(&line[0],120,fp)) {
    cout << line;
    TString T(line);
    Int_t iok = 0;
    if (T.Contains("BAD")) iok = 1;
    else if (T.Contains("DEAD")) iok = 2;
    else if (T.Contains("GOOD")) iok = 3;
    xmin = xmax = -99;
    if (! iok) {
      sscanf(&line[0],"| B%1dL%02dW%1dH%1d| [%f,%f]",&Barrel,&Ladder,&Wafer,&Hybrid,&xmin,&xmax);
      if (Hybrid == 1) {
	xmin += 0.1;
	xmax += 0.1;
	Float_t temp = - xmin;
	xmin = - xmax;
	xmax = temp;
      } else {
	xmin -= 0.1;
	xmax -= 0.1;
      }
      Int_t K = 100*(xmin + 0.005);
      xmin = K/100.;
      K = 100*(xmax + 0.005);
      xmax = K/100.;
    } else {
      sscanf(&line[0],"| B%1dL%02dW%1dH%1d|",&Barrel,&Ladder,&Wafer,&Hybrid);
    }
    cout << "B"<< Barrel << "L" << Ladder << "W" << Wafer << "H" << Hybrid;
    cout << " xmin " << xmin << " xmax " << xmax;
    if (! iok )     cout << " O.K";
    if (  iok == 1) cout << " BAD";
    if (  iok == 2) cout << " DEAD";
    cout << endl;
    svtHybridDriftVelocity_st *row = svtHybridDriftVelocity->GetTable();
    for (Int_t i = 0; i < NN; i++) {
      if (row[i].barrel == Barrel &&
	  row[i].ladder == Ladder && 
	  row[i].wafer  == Wafer  &&
	  row[i].hybrid == Hybrid) {
	svtHybridDriftVelocity->Print(i,1);
	if (iok == 1) row[i].npar = -1;
	if (iok == 2) row[i].npar = -2;
	if (iok == 0) {
	  row[i].npar += 100;
	  row[i].dtmin  = xmin;
	  row[i].dtmax  = xmax;
	} else {
	  if (iok == 3) {
	    row[i].dtmin  = 0;
	    row[i].dtmax  = 1;
	  } else {
	    row[i].dtmin  = 0;
	    row[i].dtmax  = 0;
	  }
	}
	svtHybridDriftVelocity->Print(i,1);
	goto ENDL;
      }
    }
  ENDL:
    continue;
  }
  fclose(fp);
  
#if 1
  Char_t Out[132];
  sprintf(Out,"%s.%8i.%06i.C",svtHybridDriftVelocity->GetName(),date,time);
  ofstream out;
  out.open(Out); 
  cout << "Create " << Out << endl;
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_svtHybridDriftVelocity\")) return 0;" << endl;
  out << "  svtHybridDriftVelocity_st row[" << NN << "] = {//Clean up" << endl; 
  for (Int_t i = 0; i < NN; i++) {
    out << Form("{%2i,%1i,%4i,%4i,%3i,%7i",row[i].type,row[i].status,row[i].idx,row[i].nrows,row[i].npar,row[i].Id);
    out << Form(",%1i,%2i,%1i,%1i",row[i].barrel,row[i].ladder,row[i].wafer,row[i].hybrid);
    out << Form(",%6.3f,%5.3f,%7.3f,%6.3f",row[i].tmin,row[i].dtmin,row[i].tmax,row[i].dtmax);
    Double_t *v = &row[i].v0;
    for (Int_t j = 0; j < 10; j++) {
      if (v[j]) out << Form(",%8.5f",v[j]);
      else      out << ", 0.00000";
    }
    if (i < NN - 1)  out << "},";
    else             out << "}";
    out << Form("// B%iL%02iW%iH%i",row[i].barrel,row[i].ladder,row[i].wafer,row[i].hybrid) << endl;
  } 
  out << "  };" << endl;
  out << "  St_svtHybridDriftVelocity *tableSet = new St_svtHybridDriftVelocity(\"" << svtHybridDriftVelocity->GetName() << "\"," << NN << ");" << endl; 
  out << "  for (Int_t i = 0; i < " << NN << "; i++) tableSet->AddAt(&row[i].type, i);" << endl; 
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close(); 
#endif
}


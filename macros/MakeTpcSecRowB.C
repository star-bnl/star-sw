//#define __RECOVER__
//________________________________________________________________________________
void MakeTpcSecRowB(TH1 *hist, TH1 *histSigma = 0,
		    Int_t d = 20070321, Int_t t = 58, 
		    const Char_t *TableName = "TpcSecRowB",
		    const Char_t *TpcRowQ = 0){
  if (gClassTable->GetID("TTable") < 0) gSystem->Load("libTable");
  if (gClassTable->GetID("St_TpcSecRowCor") < 0) gSystem->Load("libStDb_Tables");
  St_TpcSecRowCor *secrowold = (St_TpcSecRowCor *) gDirectory->Get(TableName);
  Int_t update = 0;
  if (secrowold) {
    cout << "Update existing " << TableName << endl; 
    update = 1;
  }
  else cout << "Create new " << TableName << endl; 
  St_TpcSecRowCor *secrow = new St_TpcSecRowCor(TableName,24);
  TpcSecRowCor_st *gainold = 0;
  if (secrowold) gainold = secrowold->GetTable();
  Int_t NbinsX = hist->GetNbinsX(); cout << "NbinsX = " << NbinsX << endl;
  Int_t NbinsY = hist->GetNbinsY(); cout << "NbinsY = " << NbinsY << endl;
  TpcSecRowCor_st row;
  for (Int_t i=1; i<=NbinsX; i++) {
    memset(&row,0,secrow->GetRowSize());
    for (Int_t j=1; j<= NbinsY; j++) {
      //!!!!!  skip sector 24 row 14-21 for Run X only
      //      Bool_t badSector = (i==24) && ( j >=14 && j <= 21);
      Bool_t badSector = kFALSE; // <<<<<<
      //!!!!!  skip sector/row = 5 /38-40, for Run XV only
      //      Bool_t badSector = ((i == 5) && (j >= 38 && j <= 40)) || ( i == 3 && j == 7);
      Double_t dev = hist->GetBinContent(i,j) - hist->GetBinContent(0,j);
      Double_t err  = TMath::Sqrt(TMath::Power(hist->GetBinError(i,j),2) + TMath::Power(hist->GetBinError(0,j),2));
      Double_t devD = 1e3;
      if (err > 0  && TMath::Abs(dev) < 1) devD =  dev/err;
      if (gainold)  {
	if (histSigma && histSigma->GetBinContent(i,j) > 0.1) {
	  if (TMath::Abs((gainold+i-1)->GainScale[j-1]) < 1e-7) (gainold+i-1)->GainScale[j-1] = 1.;
	}
	if (!  badSector) {
	//	if (devD < 20 && ! badSector) {
	  row.GainScale[j-1] = (gainold+i-1)->GainScale[j-1]*TMath::Exp(-dev);
	  cout << "i/j\t" << i << "/" << j 
	       << "\tgain old/new\t" << (gainold+i-1)->GainScale[j-1] << "/" 
	       << row.GainScale[j-1] << "\tdevD =" << devD << endl;
	} else {	
#ifdef __RECOVER__
	  row.GainScale[j-1] = 1; // <<<<<<<
	  cout << "Reset Sector \t" << i << "\tRow\t" << j;
#else
	  row.GainScale[j-1] = 0;
	  cout << "Skip Sector \t" << i << "\tRow\t" << j;
#endif
	  row.GainRms[j-1]   = 0;
	  cout << "\tdev " << dev << "\tdevD " << devD
	       << "\twith correction\t" << hist->GetBinContent(i,j) << "\tError\t" << err << endl;
	  continue;
	}
      }
      else  {
	if ( hist->GetBinError(i,j) < 1.e-7) {
#ifndef __RECOVER__
	  cout << "i/j\t" << i << "/" << j << " is empty. Skipped !" << endl;
#else
	  cout << "i/j\t" << i << "/" << j << " is empty. Reset to 1 !" << endl;
	  row.GainScale[j-1] = 1;
#endif
	} else {
	  row.GainScale[j-1] = TMath::Exp(-dev);
	}
      }
      if (histSigma) 
      row.GainRms[j-1]   = histSigma->GetBinContent(i,j);
    }
    secrow->AddAt(&row);
  }
  secrow->Print(0,24);
  //  TDatime  time(20010701,120000);
  TDatime  time(d,t);
  TString filename(Form("%s.%08d.%06d",TableName,time.GetDate(),time.GetTime()));
  //  sprintf(filename,"./StarDb/Calibrations/tpc/TpcSecRowTest.%08d.%06d.C",time.GetDate(),time.GetTime());
  //  sprintf(filename,"TpcSecRow.%08d.%06d.root",time.GetDate(),time.GetTime());
  printf("Create %s\n",filename.Data());
#if 0
  TString dirname = gSystem->DirName(filename);
  if (gSystem->OpenDirectory(dirname.Data())==0) { 
    if (gSystem->mkdir(dirname.Data())) {
      cout << "Directory " << dirname << " creation failed" << endl;
      cout << "Putting " << TableName << ".C in current directory" << endl;
      filename += ".C";
    }
  }
  ofstream *out = new ofstream(filename.Data());
  secrow->SavePrimitive(*out,"");
  delete out;
#else
  filename += ".root";
  TFile *f = new TFile(filename.Data(),"recreate");
  secrow->Write();
  delete f;
#endif
  tpcCorrection_st rowq;
  tpcCorrection_st *Row = &rowq;
  memset (Row, 0, sizeof(tpcCorrection_st));
  if (TpcRowQ) {
    gROOT->LoadMacro(TpcRowQ);
    St_tpcCorrection *tableSet = (St_tpcCorrection *) CreateTable();
    Row = tableSet->GetTable();
  } 
  TString fOut =  Form("TpcRowQ.%8i.%06i.C",time.GetDate(),time.GetTime());
  ofstream out;
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_tpcCorrection\")) return 0;" << endl;
  out << "  Double_t rowsGain[45] = {" << endl;
  TString line("    ");
  for (Int_t j=1; j<= NbinsY; j++) {
    Double_t dev = hist->GetBinContent(0,j);
    if (TpcRowQ) {
      cout << "Old value : " << (Row+j-1)->a[0] << "\tCorrection " << dev;
      dev += (Row+j-1)->a[0];
      cout << "\tFinal value: " << dev << endl;
    } 
    line += Form("%10.5f",dev);
    if (j != NbinsY) line += ",";
    if ( j%5 == 0) {out << line << endl; cout << "Final\t" << line << endl; line = "    ";}
  }
  out << "  };" << endl;
  out << "  St_tpcCorrection *tableSet = new St_tpcCorrection(\"TpcRowQ\",45);" << endl;
  out << "  tpcCorrection_st row;" << endl;
  out << "  memset(&row,0,tableSet->GetRowSize());" << endl;
  out << "  row.nrows = 45;" << endl;
  out << "  row.npar  =  1;" << endl;
  out << "  for (Int_t i = 0; i < 45; i++) {" << endl;
  out << "    row.idx  = i + 1;" << endl; 
  out << "    row.a[0] = rowsGain[i];" << endl;
  out << "    tableSet->AddAt(&row);" << endl;
  out << "  }" << endl;
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close(); 
}
//________________________________________________________________________________
void MakeTpcSecRowB(Int_t d=20060210,Int_t t= 80000, const Char_t *TableName = "TpcSecRowB", const Char_t *TpcRowQ = 0){
  TH1 *mu = 0, *sigma = 0;
  TFile *calib = 0;
  TSeqCollection   *files = gROOT->GetListOfFiles();
  if (! files) return;
  TIter next(files);
  TFile *f = 0;
  while ((f = (TFile *) next())) {
    TString name(gSystem->BaseName(f->GetName()));
    if (name.BeginsWith(TableName)) {
      calib = f;
      cout << "Found file " << f->GetName() << endl;
      continue;
    }
    mu = (TH1 *) f->Get("mu");
    if (mu) {
      cout << "found histogram " << mu->GetName() << " in file " << f->GetName()  << endl;
    }
    sigma = (TH1 *) f->Get("sigma");
    if (sigma) {
      cout << "found histogram " << sigma->GetName() << " in file " << f->GetName()  << endl;
    }
  }
  if (mu) {
    if (calib) calib->cd();
    MakeTpcSecRowB(mu,sigma,d,t,TableName,TpcRowQ);
  } else {
    if (! mu) cout << "Histogram was not found" << endl;
    if (! calib) cout << "file with " << TableName << " was not found" << endl;
  }
}

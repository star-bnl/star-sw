/*
  ClearH1(mu);
  MakeTimeTable(mu,79460,79756);
*/
ofstream out;
void MakeTimeTable(TH1 *mu = 0, Int_t bin1 = -1, Int_t bin2 = 0) {
  if (! mu) return;
  TDatime t; t.Set(mu->GetXaxis()->GetBinLowEdge(bin1)+788936400); t.Print();
  TString fOut =  Form("tpcTimeDependence.%8i.%06i.C",t.GetDate(),t.GetTime());
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() { " << endl;
  out << "// ------  Test whether this table share library was loaded ------ " << endl;
  out << "  if (!gROOT->GetClass(\"St_tpcCorrection\")) return 0; " << endl;
  out << "  tpcCorrection_st row;" << endl;
  out << "  Int_t nrows = 1; "<< endl;
  out << "  St_tpcCorrection *tableSet = new St_tpcCorrection(\"tpcTimeDependence\",nrows); "<< endl;
  out << "  memset(&row,0,tableSet->GetRowSize()); // " << endl;
  out << "  row.idx        = 1;    //" << endl;
  out << "  row.nrows      = nrows;//" << endl;
  Double_t min = mu->GetXaxis()->GetBinLowEdge(bin1);
  Double_t max = mu->GetXaxis()->GetBinUpEdge(bin2);
  mu->Fit("pol1","er","",min,max);
  TF1 *pol1 = (TF1 *) mu->GetListOfFunctions()->FindObject("pol1");

  cout << "/* bin1 = " << bin1 << ", bin2 = " <<bin2 << "*/" << endl;
  cout << "  row.min   = " << Form("%13.7g;",min) << endl;
  cout << "  row.max   = " << Form("%13.7g;",max) << endl;
  cout << "  row.npar  =             2;" << endl;
  cout << "  row.a[0]  = " << Form("%13.7g;",pol1->GetParameter(0)) << endl;;
  cout << "  row.a[1]  = " << Form("%13.7g;",pol1->GetParameter(1)) << endl;;
  out << "/* bin1 = " << bin1 << ", bin2 = " <<bin2 << "*/" << endl;
  out << "  row.min   = " << Form("%13.7g;",min) << endl;
  out << "  row.max   = " << Form("%13.7g;",max) << endl;
  out << "  row.npar  =             2;" << endl;
  out << "  row.a[0]  = " << Form("%13.7g;",pol1->GetParameter(0)) << endl;;
  out << "  row.a[1]  = " << Form("%13.7g;",pol1->GetParameter(1)) << endl;;
  out << "  tableSet->AddAt(&row);" << endl;
  out << "  // ----------------- end of code ---------------" << endl;
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
}

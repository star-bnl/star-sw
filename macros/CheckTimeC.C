void CheckTimeC() {
  TList *files = gROOT->GetListOfFiles();
  TIter next(files);
  while ( (f = (TFile *) next()) ) { 
    cout << f->GetName() << "\t";
    TH2D *TimeC = (TH2D *) f->Get("TimeC");
    if (TimeC) cout << "TimeC has \t" <<TimeC->GetEntries() << " entries, mean \t" << TimeC->ProjectionY()->GetMean() <<endl;
    else       cout << "TimeC is missing" << endl;
  }
}

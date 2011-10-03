// macro for the profile plots
void plotProfile(int plotNumber = 1) {
  plotNumber--;
  if (plotNumber < 1) {
  }
  else {
    cout << "$$ bad plot number" << endl;
    exit();
  }
  char *baseName[] = {"Flow_prof_Cos_Pair","Flow_Res_Pair"};
  const int nSubEvents = 4;
  int columns = nSubEvents/2;
  int rows = 2;
  int pads = rows*columns;
  cout << "pads= " << pads << endl;

  TCanvas *c = new TCanvas("c1","c1",650,800);
  c->Divide(columns,rows);
  for (int j = 0; j < rows; j++) {
    int baseNumber = j;
    cout << "base name= " << baseName[baseNumber] << endl;
    for (int k = 0; k < columns; k++) {
      char CountColumns[2];
      sprintf(CountColumns,"%d",k);
      int i = j*columns + k;
      TString *histName = new TString(baseName[baseNumber]);
      histName->Append(*CountColumns + 1);
      cout << "row= " << j << " col= " << CountColumns <<
	" pad= " << i+1 << "\t" << histName->Data() << endl;
      TH1 *pad= (TH1*)chain->Maker("FlowAnalysis")->GetHistList()->
      	FindObject(histName->Data());
      c->cd(i+1);
      if (pad) pad->Draw();
      delete histName;
    }
  }
}

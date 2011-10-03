// macro for sub-event and full event plots
void plot1D(int baseNumber = 1) {
  baseNumber--;
  const int nHarmonics = 5;
  const int nSubEvents = 4;
  char *baseName[] = {"Flow_Psi_Pair","Flow_MeanPt_Pair","Flow_q_Pair",
		      "Flow_Mult_Pair","Flow_Psi_Sub"};
  if (baseNumber < 4) {
    int columns = nSubEvents/2;
  }
  else if (baseNumber < 5){
    int columns = nSubEvents;
  }
  else {
    cout << "$$ bad base number" << endl;
    exit();
  }
  cout << "base name= " << baseName[baseNumber] << endl;
  int rows = nHarmonics;
  int pads = rows*columns;
  cout << "pads= " << pads << endl;

  TCanvas *c = new TCanvas("c1","c1",650,800);
  c->Divide(columns,rows);
  for (int j = 0; j < rows; j++) {
    char CountRows[2];
    sprintf(CountRows,"%d",j);
    for (int k = 0; k < columns; k++) {
      char CountColumns[2];
      sprintf(CountColumns,"%d",k);
      int i = j*columns + k;
      TString *histName = new TString(baseName[baseNumber]);
      histName->Append(*CountColumns + 1);
      histName->Append("_Har");
      histName->Append(*CountRows + 1);
      cout << "row= " << CountRows << " col= " << CountColumns <<
	" pad= " << i+1 << "\t" << histName->Data() << endl;
      TH1 *pad= (TH1*)chain->Maker("FlowAnalysis")->GetHistList()->
      	FindObject(histName->Data());
      c->cd(i+1);
      if (pad) pad->Draw();
      delete histName;
    }
  }
}

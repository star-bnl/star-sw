void tpcAdc() {
  gSystem->Load("libTable.so");
  gSystem->Load("St_Tables.so");
  gROOT->LoadMacro("$STAR/StarDb/Calibrations/tpc/TpcAdcCorrection.20010701.120000.C");
  St_tpcCorrection *corr = (St_tpcCorrection *) CreateTable();
  tpcCorrection_st *row[2];
  row[0] = corr->GetTable() + corr->GetNRows() - 2;
  row[1] = row[0]+1;
  Char_t *OI[2] = {"Outer","Inner"};
  TF1 *fun[2];
  TString opt("");
//   TF1 *one = new TF1("one","1.6716e*x",0,20);
//   one->SetLineColor(2);
//   one->Draw();
//   opt = "same";
  TF1 *sline = new TF1("sline","1.6716e*x",0,20);
  sline->SetLineColor(2);
  sline->Draw();
  opt = "same";
  for (int i = 0; i < 2; i++) {
    //    fun[i] = new TF1(OI[i],Form("pol%i/(1.6716e*x)-1.",row[i]->npar),row[i]->min,row[i]->max);
    fun[i] = new TF1(OI[i],Form("pol%i",row[i]->npar),0,row[i]->max);
    for (int k = 0; k < row[i]->npar; k++) 
      fun[i]->SetParameter(k,row[i]->a[k]); 
    fun[i]->Draw(opt.Data());
    opt = "same";
  }
}

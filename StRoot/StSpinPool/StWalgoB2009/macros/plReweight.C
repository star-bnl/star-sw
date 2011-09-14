//determine vertex z reweighting for embedded simulation samples
const int maxFiles=5;

void plReweight(){

  gStyle->SetOptStat(00000);
  gStyle->SetOptDate(0);

  char* iPath="./outEmb/";
  char* fileName[maxFiles]={"Wplus","Wminus","Wtau","Zany","Ze+e-Interf"};
  TH1F* h[maxFiles];
  for(int i=0; i<maxFiles; i++){
    TString file=iPath; file+=fileName[i];
    h[i]=plReweightX(file,fileName[i]);
  }

  //write file with ratio histo to use in analysis code
  outf=new TFile("zVertReweight.root","recreate");
  for(int j=0;j<maxFiles;j++)
    h[j]->Write();
}


TH1F* plReweightX(TString file, char* name){

  //load embedding sample file
  file+=".wana.hist.root";
  fd=new TFile(file);
  
  //load data file
  fdata=new TFile("/star/data01/pwg/stevens4/wAnalysis/xSecPaper/sl11b/data/run9setABCD.wana.hist.root");

  //get histos
  TH1F* simZ=fd->Get("muZv"); simZ->Rebin();
  TH1F* dataZ=fdata->Get("muZv"); dataZ->Rebin();

  assert(simZ); assert(dataZ);
  
  //take ratio of data/embedding and scale to 1 at z=0
  TH1F* ratio=dataZ->Clone();
  ratio->Divide(simZ);
  ratio->Scale(1./ratio->GetBinContent(24));
  ratio->SetName(name);
  //ratio->Draw();
  //cout<<ratio->Integral(24,25)<<endl;

  //scale embedded sample with ratio histo (like in analysis)
  TH1F* test= simZ->Clone();
  for(int i=1; i<=test->GetNbinsX(); i++){
    float z=test->GetBinCenter(i);
    test->SetBinContent(i,test->GetBinContent(i)*ratio->GetBinContent(ratio->FindBin(z)));
  }
  //check that re-weighted sample agrees with data
  TCanvas *c1=new TCanvas("aa","bb",600,400);
  dataZ->Draw(); dataZ->SetTitle("; Z_{vertex} (cm)");
  test->SetLineColor(2);
  test->DrawNormalized("same",dataZ->Integral());
  simZ->SetLineColor(4);
  simZ->DrawNormalized("same",dataZ->Integral());
  //gPad->SetLogy();

  TLegend *leg = new TLegend(0.6,0.65,0.9,0.9);
  leg->SetFillColor(0);
  leg->AddEntry(dataZ," Data Z_{vertex}","l");
  leg->AddEntry(simZ," Thrown MC Z_{vertex}","l");
  leg->AddEntry(test," Re-weighted MC Z_{vertex}","l");
  leg->Draw("same");
  c1->Print(Form("plots/vertReweight/%s.png",name));
  c1->Print(Form("plots/vertReweight/%s.eps",name));

  return ratio;
  
  //look at histos before weighting
  dataZ->Draw();
  simZ->SetLineColor(2);
  simZ->DrawNormalized("same",dataZ->GetEntries());
  dataZ->SetMinimum(1);
  gPad->SetLogy();

}

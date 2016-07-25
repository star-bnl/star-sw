const Int_t NHYPS = 5;
const Char_t *Names[] = {"p",
			 "K",
			 "pi",
			 "e",
			 "d"};
//________________________________________________________________________________
void PlotBX(const Int_t hyp=4, const Int_t iX = 8, const Int_t iY1 = 4, const Int_t iY2=15) {
#if 0
  TString tfname("zbgx");
  tfname += Names[hyp];
  tfname += "hist223.root";
  TFile *ff = new TFile(tfname.Data());
  if (!ff ) return;
#endif
  TString name(Form("%s_z-plot%i",Names[hyp],iX));
  TCanvas *c1 = new TCanvas(name.Data(),name.Data(),10,10,800,900);
  c1->Divide(3,4);
  Int_t i = 1;
  TText *t = new TText();
  for (Int_t iY=iY1; iY<=iY2; iY++) {
    c1->cd(i++);
    TString hName(Form("%s_%i_%i",Names[hyp],iX,iY)); 
    TH1 *hist = (TH1 *) gDirectory->Get(hName.Data());
    if (hist) {
      //      c1->DrawFrame(2.,0,5.,2.5);
      TAxis *ax = hist->GetXaxis();
      Double_t x = ax->GetBinLowEdge(ax->GetFirst()+2);
      Double_t y = hist->GetMaximum(); cout << "x/y" << x << "/" << y << endl;
      t->DrawText(x,y,hist->GetTitle()); 
      hist->SetXTitle("z");
      hist->SetYTitle("#phi(z)");
      hist->SetStats(); hist->Draw("");}
    else {cout << " Did not find histogram " << hName.Data() << endl;}
  }
  c1->Update();
}
//________________________________________________________________________________
void PlotBY(const Int_t hyp=4, const Int_t iX1 = 1, const Int_t iX2 = 6, const Int_t iY = 8) {
#if 0
  TString tfname("zbgx");
  tfname += Names[hyp];
  tfname += "hist223.root";
  TFile *ff = new TFile(tfname.Data());
  if (!ff ) return;
#endif
  TString name(Form("%s_z-plot_%i_%i",Names[hyp],iX1,iY));
  TCanvas *c1 = new TCanvas(name.Data(),name.Data(),10,10,800,900);
  gStyle->SetStatY(1.1);
  c1->Divide(2,3);
  Int_t i = 1;
  TText *t = new TText();
  for (Int_t iX=iX1; iX<=iX2; iX++) {
    c1->cd(i++);
    TString hName(Form("%s_%i_%i",Names[hyp],iX,iY)); 
    TH1 *hist = (TH1 *) gDirectory->Get(hName.Data());
    if (hist) {
      //      c1->DrawFrame(2.,0,5.,2.5);
      TAxis *ax = hist->GetXaxis();
      Double_t x = ax->GetBinLowEdge(ax->GetFirst()+2);
      Double_t y = hist->GetMaximum(); //cout << "x/y =" << x << "/" << y << endl;
      t->DrawText(x,y,hist->GetTitle()); 
      hist->SetXTitle("z");
      hist->SetYTitle("#phi(z)");
      hist->SetStats(); hist->Draw("");}
    else {cout << " Did not find histogram " << hName.Data() << endl;}
  }
  c1->Update();
}

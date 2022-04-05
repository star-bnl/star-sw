// This is a simple macro to test the fitting function and result interactively. The input data is a non-tree root file from pi0 tree. The default name here is 'allruns.hist.root', but users may change it by your preference.
// This macro will basically generate a fitted plot with multiple parts: raw histogram, overall fit, background fit, pi0 after background subtraction and residual, etc.
// Author: Weihong He

const int mx=1;
const int mt=1;
TFile *fd[mx];
TCanvas *c1=0;

FitPtMass() {

  float ptmin=7.0,ptmax=8.0;
  float fit_x1=0.07,fit_x2=0.4;
  const int nend=int(fit_x1/0.01);
  const int nstart=int(fit_x2/0.01);
  cout<<"nend="<<nend<<" nstart="<<nstart<<endl;
  int minbin,maxbin;
  TH1F*  h1= new TH1F("hMassAny","diphoton invariant mass",120,0.,1.2 );
  TH1F*  h2= new TH1F("hMassAny","diphoton invariant mass",120,0.,1.2 );
  TH1F*  hResidual= new TH1F("hMassAny","diphoton invariant mass",120,0.,1.2 );
  FILE* fout=fopen("mass.txt","w");assert(fout);
  fprintf(fout,"Bin# Entry\n");
  char *PlotName[mx]={"hPTAny"};
  char *fName[mt]={"allruns.hist"};
  TString inPath="";
  c1=new TCanvas("tmp","tmp",700,600);
  gStyle->SetPalette(1,0); 
  int i;
  for(i=0;i<mt;i++) {
    TString   hFile=inPath+fName[i];
    hFile+=".root";
    fd0=new TFile(hFile); assert(fd0->IsOpen());
    fd[i]=fd0;
  }
  c1->Divide(1,1);
  c1->cd(1);
  int i;
  for(i=0;i<mx;i++) 
    { 
      
      //gStyle->SetPalette(1,0);
      // .... inp eve
      TString hname=PlotName[i];
      printf("=%s=\n",hname.Data());
      h0=(TH2F*)fd[0]->Get(hname); assert(h0);
      minbin=h0->GetYaxis()->FindBin(ptmin);
      maxbin=h0->GetYaxis()->FindBin(ptmax)-1;
      h1->Add(h0->ProjectionX("htemp",minbin,maxbin,""));
      int ent=h1->Integral();
      cout<<"entry="<<ent<<endl;
      h1->SetEntries(ent);
      int ccount=0;
      float sum=0;
      float tpt=ptmin;
      for(int i=minbin;i<=maxbin;i++){
	ccount+=h0->ProjectionX("",i,i,"")->Integral();
	sum+=(h0->ProjectionX("",i,i,"")->Integral())*tpt;
	tpt+=(ptmax-ptmin)/(maxbin+1-minbin);
      }
      float ptmean=sum/ccount;
      cout<<"raw="<<int(h1->Integral(11,19))<<" ptmean="<<ptmean<<endl;
      h1->Draw();
      h1->SetMinimum(-ent/100);
      TF1* f1=new TF1("ffit","exp([0]+[1]*x)+[2]*exp(-0.5*((x-[3])/([4]*(1.0+6.11*(x-[3]))))**2)",fit_x1,fit_x2);
      //TF1* f1=new TF1("ffit","exp([0]+[1]*x+[2]/x)+[3]*exp(-0.5*((x-[4])/([5]*(1.0+6.11*(x-[4]))))**2)",fit_x1,fit_x2);
      f1->SetParameters(9.0,-8.0,10000,0.135,0.02);
      //f1->FixParameter(1,-2.854);
      //f1->FixParameter(2,0.002);
      //f1->FixParameter(4,0.1366);
      //f1->FixParameter(5,0.02676);
      h1->Fit(f1,"R+","",fit_x1,fit_x2);
      int nx=h1->GetNbinsX();
      
      float sumx=0,sum2;
      for(int ix=1;ix<=nx;ix++)
	{
	  fprintf(fout,"%d %d\n",ix,h1->GetBinContent(ix));
	  sumx+=h1->GetBinContent(ix);
	}

      //cout<<"sumx="<<sumx<<endl;
      //TF1* f2=new TF1("fit2","expo(0)",0.,1.2);
      TF1* f2=new TF1("fit2","exp([0]+[1]*x+[2]/x)",0.,1.2);
      //TF1* f2=new TF1("fit2","exp(5.7-6.4*x)",0.,1.2);
      h2->Add(h1);
      hResidual->Add(h1);
      hResidual->Add(f1,-1);
      for (int ix= 1; ix <= nend; ix++) {
	hResidual->SetBinContent(ix, 0);
      }  
      for (int ix= nstart; ix <= nx; ix++) {
	hResidual->SetBinContent(ix, 0);
      }
      hResidual->SetLineColor(11);
      hResidual->Draw("same");
      f2->SetParameters(f1->GetParameter(0),f1->GetParameter(1));
      h2->Add(f2,-1);
      for (int ix= 1; ix <= 8; ix++) {
	h2->SetBinContent(ix, 0);
      }  
      //for(int ix=9;ix<=19;ix++){
      //sum2+=h2->GetBinContent(ix);
      //}
      h2->SetLineColor(kBlue);
      TF1* f3=new TF1("f3","exp([0]+[1]*x+[2]/x)",fit_x1,fit_x2);
      f3->SetParameters(f1->GetParameter(0),f1->GetParameter(1));
      cout<<"p0="<<f1->GetParameter(0)<<endl;
      //h2->Fit("gaus","","",0.42,0.66);
      int Realpi=h2->Integral(11,19);
      int backpi=h1->Integral(11,19)-h2->Integral(11,19);
      int totpi=h1->Integral(11,19);
      cout<<"total pi="<<totpi<<" Real Pi0 integral="<<Realpi<<" backpi="<<backpi<<endl;
      h2->Draw("same");
      f3->SetLineColor(6);
      f3->Draw("same");
      //h2->Draw("error");
      l1=new TLine(0.135,0.,0.135,7000);
      l1->SetLineColor(kRed);
      l1->Draw();
      l2=new TLine(0.18,0.,0.18,7000);
      l2->SetLineColor(kGreen);
      l2->Draw();
      l3=new TLine(0.1,0.,0.1,7000);
      l3->SetLineColor(kGreen);
      l3->Draw();
      //h2->SetStats(kFALSE);
      c1->Print("hMassAny.gif");
      
    }
  return;
  
}


void SaveEff   ( char * infilename1 ="temp1.root", 
		 char * histogram1  = "acc1", 
		 char * histogram2  = "acc2",
		 char * outfilename = "ratio.root")
  
{
  TLine *line = new TLine(0.0,1.0,0.20,1.0);
  gStyle->SetOptStat(00000);
  gStyle->SetTitleColor(kWhite);
  gStyle->SetStatColor(kWhite);
  TFile *f1 = new TFile(infilename1);

  TFile outfile(outfilename,"recreate");
  if(! outfile.IsOpen())
   {
    cerr<<"Can not open "<<outfilename<<endl;
    return;
   }

//******************************************************************** 
  outfile.cd();

  TCanvas *c1 = new TCanvas ("ratioc","ratioc", 0, 0, 900, 600);  
  TH1F *numerator = (TH1F*)f1->Get(histogram2);
  TH1F *denominator = (TH1F*)f1->Get(histogram1);
  TH1F *ratio = new TH1F("ratio","ratio",denominator->GetNbinsX(),0.0,5.);
  
  cout <<"Operating on hists:"<<endl
       <<numerator->GetName()<<" and "<<denominator->GetName()<<endl;

  numerator->Sumw2();
  denominator->Sumw2();

  c1->SetFillColor(kWhite);
  c1->SetBorderMode(0);

  ratio->SetTitle("\\pi^{+}");
 ratio->Divide(numerator,denominator,1.,1.,"b");
 ratio->SetMaximum(1.0);
 ratio->SetMinimum(0.0);
 ratio->SetXTitle("pt");
 ratio->SetYTitle("ratio");
 ratio->Draw("E");

 ratio->Write();


 ratio->Draw();
 TString outpsfilename=outfilename;
 outpsfilename.ReplaceAll("root","ps");
 c1->SaveAs(outpsfilename);

	     
	     
 outfile.Close();


}

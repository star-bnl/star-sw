
void getAvgEff()
{
  gStyle->SetPalette(1);
  gStyle->SetOptStat(0);
  // TFile f("rootOut/mrgSignalShapes.root");
  TFile f("signalShapes.root");
 TH2D* h=(TH2D*)f.Get("radioDiskEffLoose_2");
 TH2D* hEff=(TH2D*)f.Get("allCountsLooseDisk_3");//counting different for this histo
 TH2D* hNonEff=(TH2D*)f.Get("radioDiskNonEffLoose_2");

 Double_t max=h->GetXaxis()->GetXmax();
 Double_t min=h->GetXaxis()->GetXmin();

 // cout <<"max: " << max << " min: " << min << " numBins: "<< h->GetNbinsX() <<endl;

 TH2D OverallEff("overallEff","overallEff",h->GetNbinsX(),min,max,h->GetNbinsY(),min,max);
 OverallEff.GetXaxis()->SetTitle("x [cm]");
 OverallEff.GetYaxis()->SetTitle("y [cm]");
Double_t eff=0;
Int_t count=0;

 Double_t overallErr=0;
for(Int_t i=1;i<h->GetNbinsX()+1;i++)
  {
    for(Int_t j=1;j<h->GetNbinsY()+1;j++)
      {
	///do you only want to use Quad B?

	if(i<(h->GetNbinsX()-1)/2|| j>(h->GetNbinsY()-1)/2)
	  continue;
	//Int_t gBin=h->GetBin(i,j);
	//	Double_t xpos=h->GetXaxis()->GetBinCenter(gBin);
	//	Double_t ypos=h->GetYaxis()->GetBinCenter(gBin);
	//	if(xpos<0 || ypos >0)
	//	  continue;


	    Int_t numEff=hEff->GetBinContent(i,j);
	    Int_t numNonEff=hNonEff->GetBinContent(i,j);
	    Int_t numCounts=numEff+numNonEff;
	    Double_t efficiency=0;
	    if(numCounts>0)
	      efficiency=(Double_t)numEff/(Double_t)numCounts;

	    OverallEff.SetBinContent(i,j,efficiency);

	if(efficiency>0.05)
	  {
	    //
	    Double_t relErr=sqrt((1/(Double_t)(numEff+numNonEff)+1/(Double_t)numEff));
	    Double_t err=relErr*efficiency;
	    eff+=efficiency/(err*err);
	    overallErr+=(1/(err*err));
	    count++;
	  }
      }
  }
 TCanvas c;
 char buffer[100];
 sprintf(buffer,"Average Efficiency is %f +- %f",eff/overallErr,sqrt(1/overallErr));
 TLatex t1(-30,0,buffer);
 OverallEff.Draw("colz");
 t1.Draw();
 c.SaveAs("overallEff.png");
 c.SaveAs("overallEff.C");
//cout <<"avg eff: " << eff/count <<endl;
 cout <<"avg eff: " << eff/overallErr <<" +- "<< sqrt(1/overallErr)<<endl;
}

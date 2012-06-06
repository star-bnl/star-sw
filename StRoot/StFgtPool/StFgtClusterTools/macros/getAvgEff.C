
void getAvgEff(Char_t* signalFile="signalShapes.root", Bool_t onlyQuadB=true, Int_t diskNr=3)
{
  gStyle->SetPalette(1);
  gStyle->SetOptStat(0);
   TFile f(signalFile);
   //TFile f("signalShapes.root");
 char buffer[100];
 sprintf(buffer,"radioDiskEffLoose_%d",diskNr-1);
 TH2D* h=(TH2D*)f.Get(buffer);
 sprintf(buffer,"allCountsLooseDisk_%d",diskNr);//counting different for this histo
 TH2D* hEff=(TH2D*)f.Get(buffer);
 sprintf(buffer,"radioDiskNonEffLoose_%d",diskNr-1);
 TH2D* hNonEff=(TH2D*)f.Get(buffer);

 Double_t max=h->GetXaxis()->GetXmax();
 Double_t min=h->GetXaxis()->GetXmin();

 // cout <<"max: " << max << " min: " << min << " numBins: "<< h->GetNbinsX() <<endl;

 TH2D OverallEff("overallEff","overallEff",h->GetNbinsX(),min,max,h->GetNbinsY(),min,max);
 OverallEff.GetXaxis()->SetTitle("x [cm]");
 OverallEff.GetYaxis()->SetTitle("y [cm]");
Double_t eff=0;
Int_t count=0;


 Int_t sumEff=0;
 Int_t sumNonEff=0;

 Double_t overallErr=0;
for(Int_t i=1;i<h->GetNbinsX()+1;i++)
  {
    for(Int_t j=1;j<h->GetNbinsY()+1;j++)
      {
	///do you only want to use Quad B?
	if(onlyQuadB)
	  {
	    if(i<(h->GetNbinsX()-1)/2|| j>(h->GetNbinsY()-1)/2)
	      continue;
	  }
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
	      {

	      efficiency=(Double_t)numEff/(Double_t)numCounts;
	      }
	    if(numEff>0)
	      {
		sumEff+=numEff;
		sumNonEff+=numNonEff;
	      }

	    OverallEff.SetBinContent(i,j,efficiency);

	if(numCounts>0)
	  {
	    //
	    Double_t relErr=999;
	    Double_t err=999;
	    if(numEff>0)
	      {
		relErr=sqrt((1/(Double_t)(numEff+numNonEff)+1/(Double_t)numEff));
		err=relErr*efficiency;

	      }
	    else
	      {
		err=1/sqrt(numNonEff);
	      }
	    eff+=efficiency/(err*err);
	    overallErr+=(1/(err*err));

	    count++;
	  }
      }
  }
 TCanvas c;
 Double_t avgEff=sumEff/(Double_t)(sumEff+sumNonEff);
 //binomial error, beware of 0
 Double_t errOnEffNum=sqrt(avgEff*(1-avgEff)*(sumEff+sumNonEff));
 Double_t altErr= ((Double_t)1/(Double_t)(sumEff+sumNonEff))*sqrt(sumEff*(Double_t)(1-sumEff/(sumEff+sumNonEff)));
 sprintf(buffer,"Average Efficiency is %f +- %f",avgEff,altErr);
 TLatex t1(-30,0,buffer);
 OverallEff.Draw("colz");
 t1.Draw();
 c.SaveAs("overallEff.png");
 c.SaveAs("overallEff.C");
//cout <<"avg eff: " << eff/count <<endl;

 cout <<"Hits found: " << sumEff <<" Hits not found: " << sumNonEff<< " efficiency: " <<avgEff<<" +- " << altErr<<endl;
}

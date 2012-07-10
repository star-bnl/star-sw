
void getCluAvgEff(Char_t* signalFile="signalShapes.root", Bool_t onlyQuadB=true, Int_t diskNr=3)
{
  gStyle->SetPalette(1);
  gStyle->SetOptStat(0);
   TFile f(signalFile);
   //TFile f("signalShapes.root");
 char buffer[100];

 sprintf(buffer,"allClusterCountsDisk_%d",diskNr);//counting different for this histo
 TH2D* hEff=(TH2D*)f.Get(buffer);
 sprintf(buffer,"radioDiskNonEff_%d",diskNr-1);
 TH2D* hNonEff=(TH2D*)f.Get(buffer);

 Double_t max=hEff->GetXaxis()->GetXmax();
 Double_t min=hEff->GetXaxis()->GetXmin();
 Int_t minCounts=10;
 // cout <<"max: " << max << " min: " << min << " numBins: "<< h->GetNbinsX() <<endl;

 TH2D OverallEff("overallEff","overallEff",hEff->GetNbinsX(),min,max,hEff->GetNbinsY(),min,max);
 TH2D OverallCounts("overallCounts","overallCounts",hEff->GetNbinsX(),min,max,hEff->GetNbinsY(),min,max);
 TH2D OverallFound("overallFound","overallFound",hEff->GetNbinsX(),min,max,hEff->GetNbinsY(),min,max);
 OverallEff.GetXaxis()->SetTitle("x [cm]");
 OverallEff.GetYaxis()->SetTitle("y [cm]");
 OverallCounts.GetXaxis()->SetTitle("x [cm]");
 OverallCounts.GetYaxis()->SetTitle("y [cm]");
 OverallFound.GetXaxis()->SetTitle("x [cm]");
 OverallFound.GetYaxis()->SetTitle("y [cm]");



Double_t eff=0;
Int_t count=0;

 Int_t sumEff=0;
 Int_t sumNonEff=0;

 Double_t overallErr=0;
for(Int_t i=1;i<hEff->GetNbinsX()+1;i++)
  {
    for(Int_t j=1;j<hEff->GetNbinsY()+1;j++)
      {
	///do you only want to use Quad B?
	if(onlyQuadB)
	  {
	    if(i<(hEff->GetNbinsX()-1)/2|| j>(hEff->GetNbinsY()-1)/2)
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
	    if(numCounts>minCounts)
	      {
		efficiency=(Double_t)numEff/(Double_t)numCounts;
	      }
	    if(numEff>0)
	      {
		sumEff+=numEff;
		sumNonEff+=numNonEff;
	      }
	    OverallEff.SetBinContent(i,j,efficiency);
	    OverallCounts.SetBinContent(i,j,numCounts);
	    OverallFound.SetBinContent(i,j,numEff);
	if(numCounts>minCounts)
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
 TCanvas c("effs","effs",1,1,800,800);
 Double_t avgEff=sumEff/(Double_t)(sumEff+sumNonEff);
 //binomial error, beware of 0
 Double_t errOnEffNum=sqrt(avgEff*(1-avgEff)*(sumEff+sumNonEff));
 Double_t altErr= ((Double_t)1/(Double_t)(sumEff+sumNonEff))*sqrt(sumEff*(Double_t)(1-sumEff/(sumEff+sumNonEff)));
 sprintf(buffer,"Average Efficiency is %f +- %f",avgEff,altErr);
 //15 degrees
 Float_t rotationRadians=-0.261799388;
 Float_t rotationRadians2=-1.83259571;
 Float_t rotation=-15;
 Float_t innerR=11.5;
 Float_t outerR=38;
 TLatex t1(-30,0,buffer);
 TArc outerA(0,0,outerR,90+rotation,-90+rotation);
 TArc innerA(0,0,innerR,90+rotation,-90+rotation);
 // cout <<"cos rot: " << cos(rotation) <<" sin: " << sin(rotation) <<endl;
 TLine l1(innerR*cos(rotationRadians),innerR*sin(rotationRadians),outerR*cos(rotationRadians),outerR*sin(rotationRadians));
 TLine l2(innerR*cos(rotationRadians2),innerR*sin(rotationRadians2),outerR*cos(rotationRadians2),outerR*sin(rotationRadians2));
 l1.SetLineWidth(3);
 l2.SetLineWidth(3);
 outerA.SetLineWidth(3);
 innerA.SetLineWidth(3);
 OverallEff.Draw("colz");
  outerA.Draw();
  innerA.Draw();
 
 OverallEff.Draw("same colz");
 t1.Draw();
 l1.Draw();
 l2.Draw();
 c.SaveAs("overallEff.png");




 //counts

 TCanvas cC("counts","counts",1,1,800,800);
 OverallCounts.Draw("colz");
 outerA.Draw();
 innerA.Draw();
 
 OverallCounts.Draw("same colz");
 l1.Draw();
 l2.Draw();
 cC.SaveAs("overallCounts.png");


 TCanvas cf("found","found",1,1,800,800);
 OverallFound.Draw("colz");
 outerA.Draw();
 innerA.Draw();
 OverallFound.Draw("same colz");
 l1.Draw();
 l2.Draw();
 cf.SaveAs("overallFound.png");


 // c.SaveAs("overallEff.C");
//cout <<"avg eff: " << eff/count <<endl;
 cout <<"Hits found: " << sumEff <<" Hits not found: " << sumNonEff<< " efficiency: " <<avgEff<<" +- " << altErr<<endl;

}

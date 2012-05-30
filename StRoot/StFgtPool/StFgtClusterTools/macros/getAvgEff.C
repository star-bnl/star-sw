
void getAvgEff()
{
TFile f("signalShapes.root");
 TH2D* h=(TH2D*)f.Get("radioDiskEffLoose_2");
 TH2D* hEff=(TH2D*)f.Get("allCountsLooseDisk_3");//counting different for this histo
 TH2D* hNonEff=(TH2D*)f.Get("radioDiskNonEffLoose_2");

Double_t eff=0;
Int_t count=0;

 Double_t overallErr=0;
for(Int_t i=1;i<h->GetNbinsX()-1;i++)
  {
    for(Int_t j=1;j<h->GetNbinsY()-1;j++)
      {
	Double_t efficiency=h->GetBinContent(i,j);
	if(efficiency>0.05)
	  {
	    Int_t numEff=hEff->GetBinContent(i,j);
	    Int_t numNonEff=hNonEff->GetBinContent(i,j);
	    Int_t numCounts=numEff+numNonEff;
	    //
	    Double_t relErr=sqrt((1/(Double_t)(numEff+numNonEff)+1/(Double_t)numEff));
	    Double_t err=relErr*efficiency;
	    eff+=efficiency/(err*err);
	    overallErr+=(1/(err*err));
	    count++;
	  }
      }
  }
//cout <<"avg eff: " << eff/count <<endl;
cout <<"avg eff: " << eff/overallErr <<endl;
}


void getAvgEff()
{
TFile f("signalShapes.root");
 TH2D* h=(TH2D*)f.Get("radioDiskEffLoose_2");
 TH2D* hEff=(TH2D*)f.Get("allCountsLooseDisk_3");//counting different for this histo
 TH2D* hNonEff=(TH2D*)f.Get("radioDiskNonEffLoose_2");

Double_t eff=0;
Int_t count=0;

 Double_t overallCounts=0;
for(Int_t i=1;i<h->GetNbinsX()-1;i++)
  {
    for(Int_t j=1;j<h->GetNbinsY()-1;j++)
      {
	if(h->GetBinContent(i,j)>0.05)
	  {
	    Int_t numCounts=hEff->GetBinContent(i,j)+hNonEff->GetBinContent(i,j);
	    //
	    eff+=(h->GetBinContent(i,j))*numCounts;
	    overallCounts+=numCounts;
	    count++;
	  }
      }
  }
//cout <<"avg eff: " << eff/count <<endl;
cout <<"avg eff: " << eff/overallCounts <<endl;
}

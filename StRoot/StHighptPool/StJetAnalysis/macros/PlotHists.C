#include <iostream>

void PlotHists(const char* infile, const char* outfile)
{
    TFile* ifile = new TFile(infile,"READ");
    TH1* pp = plusPlus;
    TH1* mm = minusMinus;
    TH1* pm = plusMinus;

    TFile* ofile = new TFile(outfile,"RECREATE");
    
    TH1* likeSign = new TH1D("likeSign","Like Sign Bgd",32, 6.,14.);
    TH1* subtracted = new TH1D("subtracted","Background Subtracted",32, 6., 14.);
    TH1* seff = new TH1D("seff","S_{effective}= #frac{Signal}{(2*Background/Signal)+1}",32, 6., 14.);
    
    for (int i=1; i<=pp->GetNbinsX(); ++i) {
	double m=pp->GetBinCenter(i);
	double ppval = pp->GetBinContent(i);
	double pperror = pp->GetBinError(i);
	double mmval = mm->GetBinContent(i);
	double mmerror = mm->GetBinError(i);
	/*
	  if (ppval==0. || mmval==0.) {
	  cout <<"Error, gonna divide by zero"<<endl;
	  }
	*/
	double val = 2.*sqrt(ppval*mmval);
	double valerror = sqrt(val*( (pperror/ppval)*(pperror/ppval) + (mmerror/mmval)*(mmerror/mmval) ) );
	
	likeSign->SetBinContent(i, val);
	likeSign->SetBinError(i, valerror);

	double pmval = pm->GetBinContent(i);
	double pmerror = pm->GetBinError(i);
	double sub = pmval-val;
	double suberror = sqrt(pmerror*pmerror + valerror*valerror);
	subtracted->SetBinContent(i, sub);
	subtracted->SetBinError(i, suberror);

	//S_eff = S/((2B/S)+1). The latter is *the* variable to optimize.
	double s_eff = sub/((2.*val/sub)+1);
	double s_efferror = sqrt((suberror/sub)*(suberror/sub) + (valerror/val)*(valerror/val) );
	seff->SetBinContent(i, s_eff);
	seff->SetBinError(i, s_efferror);
	//cout <<i<<" pm: "<<ppval<<" mm: "<<mmval<<" ls: "<<val<<" pm: "<<pmval<<" seff: "<<s_eff<<endl;
    }
    //ofile->cd();
    pp->Write();
    mm->Write();
    pm->Write();
    ofile->Write();
    ofile->Close();
}

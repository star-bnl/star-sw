#include <iostream>

void PlotSpectra(const char* tag, int nDraws=1000000000)
{
    cout <<"PlotSpectra(const char*)"<<endl;
    
    cout <<"Make Chain with tag:\t"<<tag<<endl;
    TChain* chain = new TChain("mTree");
    chain->Add(tag);
    int n=chain->GetEntries();
    cout <<"Number of events:\t"<<n<<endl;

    bool plotMomentum=false;
    bool plotFitPoint=false;
    bool plotEta=false;
    bool plotHPlusMinusRatio=true;
    
    chain->SetLineWidth(5.);
    
    cout <<"Plot some things"<<endl;

    if (plotHPlusMinusRatio) {
	
	TCanvas* c1 = new TCanvas();

	TH1* pplus = new TH1D("pplus","Momentum of Positives",70, 3., 10.);
	TH1* pminus = new TH1D("pminus","Momentum of Negatives",70, 3., 10.);
	TH1* ptplus = new TH1D("ptplus","Pt of Positives",70, 3., 10.);
	TH1* ptminus = new TH1D("ptminus","Pt of Negatives",70, 3., 10.);

	TH1* pratio = new TH1D("pratio","#frac{N_{H+}}{N_{H-}} vs. Momentum",70, 3., 10.);
	TH1* ptratio = new TH1D("ptratio","#frac{N_{H+}}{N_{H-}} vs. Pt",70, 3., 10.);
	
	chain->Draw("sqrt(mTracks.mP.mX1*mTracks.mP.mX1+mTracks.mP.mX2*mTracks.mP.mX2+mTracks.mP.mX3*mTracks.mP.mX3)>>pplus","abs(mEta)<1.0 && mTracks.mHelix.mQ==1 && mTracks.mNHitsFit>22","same",nDraws);
	chain->Draw("sqrt(mTracks.mP.mX1*mTracks.mP.mX1+mTracks.mP.mX2*mTracks.mP.mX2+mTracks.mP.mX3*mTracks.mP.mX3)>>pminus","abs(mEta)<1.0 && mTracks.mHelix.mQ==-1 && mTracks.mNHitsFit>22","same",nDraws);

	chain->Draw("mTracks.mPt>>ptplus","abs(mEta)<1.0 && mTracks.mHelix.mQ==1 && mTracks.mNHitsFit>22","same",nDraws);
	chain->Draw("mTracks.mPt>>ptminus","abs(mEta)<1.0 && mTracks.mHelix.mQ==-1 && mTracks.mNHitsFit>22","same",nDraws);

	pplus->SetLineWidth(2);
	pminus->SetLineWidth(2);
	ptplus->SetLineWidth(2);
	ptminus->SetLineWidth(2);
	pratio->SetLineWidth(2);
	ptratio->SetLineWidth(2);
	
	c1->SetLogy();
	pplus->SetXTitle("P (GeV)");
	pplus->SetTitle("Momentum: black==positive, red==negative");
	pplus->Draw("pe");
	pminus->SetLineColor(2);
	pminus->Draw("pesame");

	TCanvas* c2 = new TCanvas();
	c2->SetLogy();
	
	ptplus->SetXTitle("Pt (GeV)");
	ptplus->SetTitle("Pt: black==positive, red==negative");
	ptplus->Draw("pe");
	ptminus->SetLineColor(2);
	ptminus->Draw("pesame");

	//Loop on the histograms by hand to set the errors
	for (int i=1; i<=pplus->GetNbinsX(); ++i) {
	    
	    double pval = pplus->GetBinContent(i);
	    double mval = pminus->GetBinContent(i);
	    if (pval!=0. && mval!=0.) {
		double perror = pplus->GetBinError(i);
		double merror = pminus->GetBinError(i);
		double val = pval/mval;
		double valerror = sqrt(val*( (perror/pval)*(perror/pval) + (merror/mval)*(merror/mval) ) );
		
		pratio->SetBinContent(i, val);
		pratio->SetBinError(i, valerror);
	    }

	    double ptval = ptplus->GetBinContent(i);
	    double mtval = ptminus->GetBinContent(i);
	    if (ptval!=0. && mtval!=0.) {
		double pterror = ptplus->GetBinError(i);
		double mterror = ptminus->GetBinError(i);
		double tval = ptval/mtval;
		double tvalerror = sqrt(tval*( (pterror/ptval)*(pterror/ptval)
					       + (mterror/mtval)*(mterror/mtval) ) );
		
		ptratio->SetBinContent(i, tval);
		ptratio->SetBinError(i, tvalerror);
	    }
	}

	TH2* pzone = new TH2D("pzone","#frac{N_{H+}}{N_{H-}} vs. Momentum",1, 3., 10., 1, 0., 2.);
	TH2* ptzone = new TH2D("ptzone","#frac{N_{H+}}{N_{H-}} vs. Pt",1, 3., 10., 1, 0., 2.);
    
	pzone->SetXTitle("p (GeV");
	ptzone->SetXTitle("pt (GeV");
	
	TCanvas* c3 = new TCanvas();
	c3->SetGridx();
	c3->SetGridy();
	pratio->SetXTitle("P (GeV)");
	pzone->Draw();
	pratio->Draw("pesame");

	TCanvas* c4 = new TCanvas();
	c4->SetGridx();
	c4->SetGridy();
	ptratio->SetXTitle("Pt (GeV)");
	ptzone->Draw();
	ptratio->Draw("pesame");
    }
    
    if (plotEta) {
	TCanvas* c1 = new TCanvas();
	chain->SetLineColor(1);
	chain->Draw("mTracks.mEta","mPt<20 && abs(mEta)<1.0 && mTracks.mHelix.mQ==1","",nDraws);
	htemp->SetXTitle("Eta");
	htemp->SetTitle("Eta of candidate tracks: black=pos, red=neg");
	chain->SetLineColor(2);
	chain->Draw("mTracks.mEta","mPt<20 && abs(mEta)<1.0 && mTracks.mHelix.mQ==-1","same",nDraws);
    }
    
    if (plotFitPoint) {
	TCanvas* c1 = new TCanvas();
	chain->SetLineColor(1);
	chain->Draw("mTracks.mNHitsFit","mPt<20 && abs(mEta)<1.0 && mTracks.mHelix.mQ==1","",nDraws);
	htemp->SetXTitle("N_{Fit Points}");
	htemp->SetTitle("Fit Points of candidate tracks: black=pos, red=neg");
	chain->SetLineColor(2);
	chain->Draw("mTracks.mNHitsFit","mPt<20 && abs(mEta)<1.0 && mTracks.mHelix.mQ==-1","same",nDraws);
    }

    if (plotMomentum) {
	TCanvas* c1 = new TCanvas();
	chain->SetLineColor(1);
	/*
	chain->Draw("sqrt(mTracks.mP.mX1*mTracks.mP.mX1+mTracks.mP.mX2*mTracks.mP.mX2+mTracks.mP.mX3*mTracks.mP.mX3)","mPt<20 && abs(mEta)<1.0 && mTracks.mHelix.mQ==1","",nDraws);
	cout <<htemp->GetEntries()<<endl;
	htemp->SetXTitle("p (GeV)");
	htemp->SetTitle("Momentum of candidate tracks: black=pos, red=neg");
	*/
	chain->SetLineColor(2);
	chain->Draw("sqrt(mTracks.mP.mX1*mTracks.mP.mX1+mTracks.mP.mX2*mTracks.mP.mX2+mTracks.mP.mX3*mTracks.mP.mX3)","mPt<20 && abs(mEta)<1.0 && mTracks.mHelix.mQ==-1","same",nDraws);
	cout <<htemp->GetEntries()<<endl;
    }
    
    cout <<"done"<<endl;    
}

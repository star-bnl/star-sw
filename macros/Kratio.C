TH1F *sum = 0, *sumP = 0, *sumN = 0, *ratio = 0;
void Kratio(Int_t mult1=1, Int_t mult2=9,
	    Int_t   iy1=6, Int_t iy2=9, // |eta| < 0.6
	    const Char_t *set = "4_14",
	    const Char_t *parN = "K"
	    ) 
{
  //  TString pTN = "p=T" +  Form("
  //  TH1F *pT = 
  Char_t *Charge[2] = {"P","N"};
  TH1F *pT = 0, *pTCut = 0,  *KpT = 0;
  for (int m = mult1; m <= mult2; m++) {
    for (int iy = iy1; iy <= iy2; iy++) {
      for (int is = 0; is < 2; is++) {
	TString pTn = "pT"; //pT09_4_14P_08 pT01_4_14_P_05
	pTn += Form("%02i",m); 
	pTn += "_"; 
	pTn += set; 
	pTn += Charge[is]; 
	pTn += "_"; 
	pTn += Form("%02i",iy);
	pT = (TH1F *)gROOT->FindObject(pTn.Data());
	if (pT) cout << "Found \t" << pT->GetName() << "\t" << pT->GetTitle() << endl;
	else   {cout << pTn.Data() << " is not found" << endl; continue;}
	TString pTCutn(pTn);
	pTCutn += "Cut";
	pTCut = (TH1F *)gROOT->FindObject(pTCutn.Data());
	if (pTCut) cout << "Found \t" << pTCut->GetName() << "\t" << pTCut->GetTitle() << endl;
	else      {cout << pTCutn.Data() << " is not found" << endl; continue;}
	TString KpTn(parN);
	KpTn += pTn;
	KpT = (TH1F *)gROOT->FindObject(KpTn.Data());
	if (KpT) cout << "Found \t" << KpT->GetName() << "\t" << KpT->GetTitle() << endl;
	else    {cout << KpTn.Data() << " is not found" << endl; continue;}
	if (is == 0 && sumP == 0) {
	  sumP = new TH1F(*pT);
	  sumP->Reset();
	  sumP->SetName("sumP");
	  sumP->SetTitle("Positive sum");
	}
	if (is == 1 && sumN == 0) {
	  sumN = new TH1F(*pT);
	  sumN->Reset();
	  sumN->SetName("sumN");
	  sumN->SetTitle("Negative sum");
	}
	if (is == 0) sum = sumP;
	else         sum = sumN;
	if (ratio == 0) {
	  ratio = new TH1F(*pT);
	  ratio->Reset();
	  ratio->SetName("ratio");
	  TString title("Ratio of ");
	  title += parN;
	  title += "^{+} / ";
	  title += parN;
	  Char_t line[80];
	  sprintf(line,"- for %5.2f < #eta < %5.2f and %i #leq Centrality #leq %i",
		  0.2*(iy1-8),0.2*(iy2-7),mult1,mult2);
	  title += line;
// 	  Double_t etamax = 0.2*(iy2-7);
// 	  title += Form("%5.2f",etamax);
	  ratio->SetTitle(title.Data());
	  ratio->SetXTitle("p_{T} (GeV/c)");
	  ratio->SetYTitle("Ratio");
	  ratio->SetStats(0);
	  ratio->SetMarkerStyle(20);
	}
	Int_t nx = pT->GetNbinsX();
	for (int ix=1; ix <= nx; ix++) {
	  Double_t x      = pT->GetBinCenter(ix);
	  Double_t CpT    = pT->GetBinContent(ix);
	  Double_t EpT    = pT->GetBinError(ix);
	  Double_t CpTCut = pTCut->GetBinContent(ix);
	  Double_t EpTCut = pTCut->GetBinError(ix);
	  Double_t CKpT   = KpT->GetBinContent(ix);
	  Double_t EKpT   = KpT->GetBinError(ix);
	  Double_t Csum   = sum->GetBinContent(ix);
	  Double_t Esum   = sum->GetBinError(ix);
	  Double_t cont   = Csum + CpT*CKpT;
	  Double_t EK     = EpT*CKpT; EK *= EK;
	  Double_t EP     = CpT*EKpT; EP *= EP;
	  Double_t err    = Esum*Esum + EK + EP;
	  err = TMath::Sqrt(err);
	  sum->SetBinContent(ix,cont);
	  sum->SetBinError(ix,err);
	}
      }
    }
  }
  Int_t nx = ratio->GetNbinsX();
  for (int i=1; i<=nx; i++) {
    Double_t r = sumP->GetBinContent(i)/sumN->GetBinContent(i);
    Double_t eP = sumP->GetBinError(i)/sumP->GetBinContent(i);
    Double_t eN = sumN->GetBinError(i)/sumN->GetBinContent(i);
    eP *= eP;  
    eN *= eN;
    Double_t er = r*TMath::Sqrt(eP + eN);
    ratio->SetBinContent(i,r);
    ratio->SetBinError(i,er);
  }
  TCanvas *c1 = new TCanvas("c1","ratio");
  c1->SetLogx(1);
  TF1 *pol = new TF1("pol","pol0",0.2,0.8);
  ratio->SetMaximum(1.4);
  ratio->SetMinimum(0.8);
  ratio->Fit("pol","r");
}

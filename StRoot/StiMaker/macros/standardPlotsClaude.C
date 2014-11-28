/*
 *
 * $Id: standardPlotsClaude.C,v 1.2 2002/09/05 21:27:18 pruneau Exp $
 *  A. Rose, WSU
 *  
 *
 * $Log: standardPlotsClaude.C,v $
 * Revision 1.2  2002/09/05 21:27:18  pruneau
 * Fixed problem with StiRootSimpleTrackFilter::makeNewObject
 *
 * Revision 1.1  2002/07/11 18:21:31  pruneau
 * new plots added
 *
 * Revision 1.4  2002/07/08 14:32:48  pruneau
 * added efficiency plots
 *
 * Revision 1.3  2002/07/02 18:58:52  andrewar
 * fixed bug with Pt plot
 *
 * Revision 1.2  2002/06/26 14:27:54  andrewar
 * Added cut function and track efficiency hists.
 *
 * Revision 1.1  2002/06/12 20:23:35  andrewar
 * Initial commit. Methods file for standard evaluation plot package.
 * Few plots are defined; plots will be added as cuts and characteristics
 * of the tracker are explored.
 *
 *
 *
 *
 *
 */

#define standardPlots_cxx
#include "standardPlots.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TFile.h"
#include "TF1.h"
#include "TCanvas.h"
#include <iostream.h>

#define MAX_TRACKS 7000

void run()
{
	TChain * c = new TChain("StMiniMcTree");
	standardPlots t(c);
	//t.makeTrackEffPlots("/star/u/pruneau/ittf5/","trackEff.root");
	t.makeTrackEffPlots("/star/u/pruneau/ittf5/","rcf0183_20_300evts.minimc.root");
}

void standardPlots::Loop()
{
//   In a ROOT session, you can do:
//      Root > .L standardPlots.C
//      Root > standardPlots t
//      Root > t.GetEntry(12); // Fill t data members with entry number 12
//      Root > t.Show();       // Show values of entry 12
//      Root > t.Show(16);     // Read and show values of entry 16
//      Root > t.Loop();       // Loop on all entries
//

//     This is the loop skeleton
//       To read only selected branches, Insert statements like:
// METHOD1:
//    fChain->SetBranchStatus("*",0);  // disable all branches
//    fChain->SetBranchStatus("branchname",1);  // activate branchname
// METHOD2: replace line
//    fChain->GetEntry(i);  // read all branches
//by  b_branchname->GetEntry(i); //read only this branch
   if (fChain == 0) return;
   nentries = Int_t(fChain->GetEntries());
   nbytes = 0;
   nb = 0;
   for (Int_t jentry=0; jentry<nentries;jentry++) {
      ientry = LoadTree(jentry); //in case of a TChain, ientry is the entry number in the current file
      nb = fChain->GetEntry(jentry);   nbytes += nb;
   }
}

void standardPlots::makeTrackEffPlots(const TString & path,const TString &fileName)
{
   if (fChain == 0) return;


	 TFile outputFile(path+"/"+fileName,"RECREATE");
	 long j=0;
	 
	 TString name;
	 int    ptBins = 16;
	 double ptMin = 0.;
	 double ptMax = 4.;
	 int    pzBins = 20;
	 double pzMin = -4.;
	 double pzMax = 4.;
	 int    etaBins = 15;
	 double etaMin = -1.5;
	 double etaMax =  1.5;
	 // Book histograms 0:inclusive 1/2/3: centrality bins
	 TString ptString = "pt";
	 TString dptString = "Dpt";
	 TString dpt2String = "Dpt2";
	 TString pzString = "pz";
	 TString dpzString = "Dpz";
	 TString dpz2String = "Dpz2";
	 TString etaString = "eta";
	 TString detaString = "Deta";
	 TString deta2String = "Deta2";

	 double ptBinSizes[] = {0.,0.2,0.4,0.6,0.8,1.0,1.5,2.,3.,4.,5.};

	 TString hitEffString     = "hitEff";
	 TString hitEffMultString = "hitEffMult";
	 TString hitEffPtString   = "hitEffPt";
	 TString hitEffEtaString  = "hitEffEta";
	 name = hitEffMultString+j;
	 hitEffMult = new TProfile(name,name, 25, 0, MAX_TRACKS);

	 for (j=0;j<4;j++)
		 {
			 name = ptString+"Mc"+j;
			 hMcPt[j] = new TH1D(name,name, ptBins, ptMin, ptMax);
			 name = ptString+j;
			 hPt[j]   = new TH1D(name,name, ptBins, ptMin, ptMax);
			 name = ptString+"R"+j;
			 hPtR[j]  = new TH1D(name,name, ptBins, ptMin, ptMax);
			 name = etaString+"Mc"+j;
			 hMcEta[j] = new TH1D(name,name, etaBins, etaMin, etaMax);
			 name = etaString+j;
			 hEta[j]   = new TH1D(name,name, etaBins, etaMin, etaMax);
			 name = etaString+"R"+j;
			 hEtaR[j]  = new TH1D(name,name, etaBins, etaMin, etaMax);

			 name = dptString+"_"+j;
			 pDPt[j]   = new TProfile(name,name, 10, ptBinSizes, "");
			 name = dpzString+"_"+j;
			 pDPz[j]   = new TProfile(name,name, pzBins, pzMin, pzMax);
			 name = detaString+"_"+j;
			 pDEta[j]  = new TProfile(name,name, etaBins, etaMin, etaMax);
			 name = dpt2String+"_"+j;
			 pDPt2[j]  = new TProfile(name,name, 10, ptBinSizes, "");
			 name = dpz2String+"_"+j;
			 pDPz2[j]  = new TProfile(name,name, pzBins, pzMin, pzMax);
			 name = deta2String+"_"+j;
			 pDEta2[j] = new TProfile(name,name, etaBins, etaMin, etaMax);

			 name = dptString+"R_"+j;
			 pDPtR[j]   = new TH1D(name,name, 10, ptBinSizes);
			 name = dpzString+"R_"+j;
			 pDPzR[j]   = new TH1D(name,name, pzBins, pzMin, pzMax);
			 name = detaString+"R_"+j;
			 pDEtaR[j]  = new TH1D(name,name, etaBins, etaMin, etaMax);
			 name = dpt2String+"R_"+j;
			 pDPt2R[j]  = new TH1D(name,name, 10, ptBinSizes);
			 name = dpz2String+"R_"+j;
			 pDPz2R[j]  = new TH1D(name,name, pzBins, pzMin, pzMax);
			 name = deta2String+"R_"+j;
			 pDEta2R[j] = new TH1D(name,name, etaBins, etaMin, etaMax);

			 name = hitEffString+j;
			 hitEff[j] = new TH1D(name,name, 25, 0.,1.); 
			 name = hitEffPtString+j;
			 hitEffPt[j]   = new TProfile(name,name, 25, 0.,5.); 
			 name = hitEffEtaString+j;
			 hitEffEta[j]  = new TProfile(name,name, 25, -1.5,1.5); 
		 }

	 nentries = Int_t(fChain->GetEntries());
	 nbytes = 0;
	 nb = 0;

	 double pt=0,mcPt=0,pz=0,mcPz=0,eta=0,mcEta=0,dpt=0,dpz=0,deta=0,hitRatio=0;
	 double nCommon;
	 int    nHits=0;
	 int    gId=0;
	 double q=0;
	 int iTrack=0;
	 int iC=0;
	 
	 // Event Loop
	 for (Int_t jentry=0; jentry<nentries;jentry++) 
		 {
			 //in case of a TChain, ientry is the entry number in the current file
			 ientry = LoadTree(jentry); 
			 nb = fChain->GetEntry(jentry);   nbytes += nb;
			 //if (!Cut(ientry)) continue;
			 
			 if (mMcMult<2000)
				 iC=1;
			 else if (mMcMult<5000)
				 iC=2;
			 else
				 iC=3;
			 
			 for(Int_t nMatched = 0; nMatched<mMatchedPairs_;nMatched++)
				 {
					 pt      = mMatchedPairs_mPtMc[nMatched];
					 eta     = mMatchedPairs_mEtaMc[nMatched];
					 nCommon = mMatchedPairs_mNCommonHit[nMatched];
					 nHits   = mMatchedPairs_mNHitMc[nMatched];
					 gId     = mMatchedPairs_mGeantId[nMatched];
					 q       = mMatchedPairs_mChargeMc[nMatched];
					 hitRatio = nCommon/nHits;
					 hitEffPt[0]->Fill(pt,hitRatio);
					 hitEffEta[0]->Fill(eta,hitRatio);
					 if (pt>0.2 && fabs(eta)<=0.5 && gId>3 && fabs(q)>0 && nHits>25) 
						 {
							 hitEff[0]->Fill(hitRatio);
							 hitEff[iC]->Fill(hitRatio);
							 hitEffMult->Fill(mNMcTrack, hitRatio);
						 }							
					 if (fabs(eta)<=0.5 && gId>3 && fabs(q)>0 && nHits>25) 
						 {
							 hitEffPt[0]->Fill(pt,hitRatio); 
							 hitEffPt[iC]->Fill(pt,hitRatio); 
						 }
					 if (pt>0.2 && gId>3 && fabs(q)>0 && nHits>25) 
						 {
							 hitEffEta[0]->Fill(eta,hitRatio); 
							 hitEffEta[iC]->Fill(eta,hitRatio); 
						 }
				 }
			 
			 //make Pt and Eta spectra of MC tracks
			 for(iTrack=0;iTrack<mMcTracks_;iTrack++)
				 {
					 //if(!mcTrackCut(ientry,iTrack)) continue;  //next track if mcTrackCut doesn't pass
					 pt    = mMcTracks_mPtMc[iTrack];
					 eta   = mMcTracks_mEtaMc[iTrack];
					 nHits = mMcTracks_mNHitMc[iTrack];
					 gId   = mMcTracks_mGeantId[iTrack];
					 q     = mMcTracks_mChargeMc[iTrack];
					 
					 if (fabs(eta)<=0.5 && gId>3 && fabs(q)>0 && nHits>25)
						 {
							 hMcPt[0]->Fill(pt);
							 hMcPt[iC]->Fill(pt);
						 }
					 if (pt>0.2 && gId>3 && fabs(q)>0 && nHits>25)  
						 {
							 hMcEta[0]->Fill(eta); 
							 hMcEta[iC]->Fill(eta); 
						 }
				 }
			 
			 //make Pt and Eta spectra of matched tracks
			 for(iTrack=0;iTrack<mMatchedPairs_;iTrack++)
				 {
					 //if (!trackCut(ientry,iTrack)) continue;  //next track if trackCut 
					 
					 mcPt  = mMatchedPairs_mPtMc[iTrack];
					 mcPz  = mMatchedPairs_mPzMc[iTrack];
					 mcEta = mMatchedPairs_mEtaMc[iTrack];
					 pt    = mMatchedPairs_mPtPr[iTrack];
					 pz    = mMatchedPairs_mPzPr[iTrack];
					 eta   = mMatchedPairs_mEtaPr[iTrack];
					 nHits = mMatchedPairs_mNHitMc[iTrack];
					 gId   = mMatchedPairs_mGeantId[iTrack];
					 q     = mMatchedPairs_mChargeMc[iTrack];
					 
					 if (fabs(eta)<=0.5 && gId>3 && fabs(q)>0 && nHits>25)   
						 {
							 hPt[0]->Fill(mcPt);
							 hPt[iC]->Fill(mcPt);
							 dpt = pt-mcPt;
							 pDPt[0]->Fill(mcPt,dpt);
							 pDPt[iC]->Fill(mcPt,dpt);
							 pDPt2[0]->Fill(mcPt,dpt*dpt);
							 pDPt2[iC]->Fill(mcPt,dpt*dpt);
						 }
					 if (pt>0.2 && gId>3 && fabs(q)>0 && nHits>25) 
						 {
							 hEta[0]->Fill(mcEta); 
							 hEta[iC]->Fill(mcEta); 
							 dpz = pz-mcPz;
							 pDPz[0]->Fill(mcPz,dpz);
							 pDPz[iC]->Fill(mcPz,dpz);
							 pDPz2[0]->Fill(mcPz,dpz*dpz);
							 pDPz2[iC]->Fill(mcPz,dpz*dpz);
							 deta = eta-mcEta;
							 pDEta[0]->Fill(mcEta,deta);
							 pDEta[iC]->Fill(mcEta,deta);
							 pDEta2[0]->Fill(mcEta,deta*deta);
							 pDEta2[iC]->Fill(mcEta,deta*deta);
						 }
				 }
		 }
	 // Save
	 hitEffMult->Write();
	 for (j=0;j<4;j++)
		 {
			 
			 hitEff[j]->Write();     
			 hitEffPt[j]->Write();  
			 hitEffEta[j]->Write(); 
			 
			 hPtR[j]->Divide(hPt[j],hMcPt[j]);
			 hEtaR[j]->Divide(hEta[j],hMcEta[j]);
			 
			 hPt[j]->Write();
			 hMcPt[j]->Write();
			 hPtR[j]->Write();
			 hEta[j]->Write();
			 hMcEta[j]->Write();
			 hEtaR[j]->Write();
			 
			 pDPt[j]->Write(); 
			 pDPz[j]->Write();  
			 pDEta[j]->Write(); 
			 pDPt2[j]->Write(); 
			 pDPz2[j]->Write(); 
			 pDEta2[j]->Write();

			 calculateRel(pDPt[j],pDPt2[j],pDPtR[j], pDPt2R[j]);
			 calculateRel(pDPz[j],pDPz2[j],pDPzR[j], pDPz2R[j]);
			 calculateRel(pDEta[j],pDEta2[j],pDEtaR[j], pDEta2R[j]);

			 pDPtR[j]->Write();
			 pDPzR[j]->Write(); 
			 pDEtaR[j]->Write();
			 pDPt2R[j]->Write();
			 pDPz2R[j]->Write();
			 pDEta2R[j]->Write();
			 
		 }
	 print(path,fileName);
	 outputFile.Close();	 
}

void standardPlots::calculateRel(const TProfile *p1, const TProfile *p2, 
									TH1D *h1, TH1D *h2)
{
	double x=0, mean=0, eMean=0, relMean=0, eRelMean=0, var=0,eVar=0,s=0,eS=0;
	int n = p1->GetNbinsX();
	for (int k=1;k<=n;k++)
		{
			mean  = p1->GetBinContent(k);
			eMean = p1->GetBinError(k);
			x = fabs(p1->GetBinCenter(k));
			if (x==0) continue;
			relMean = mean/x; // fractional bias
			eRelMean = eMean/x;
			h1->SetBinContent(k,relMean);
			h1->SetBinError(k,eRelMean);
			
			var  = p2->GetBinContent(k);
			eVar = p2->GetBinError(k);
			s = sqrt(var)/x;
			if (var==0.)
				eS=0;
			else
				eS = s*eVar/var;
			h2->SetBinContent(k,s);
			h2->SetBinError(k,eS);
		}
}

void standardPlots::print(const TString & path,const TString &fileName)
{
	TCanvas * c = new TCanvas();
	hMcPt[0]->Draw();
	hPt[0]->Draw("SAME");
	c->Print(path+"nTrackVsPt.gif");
	cout<<"set the options"<<endl;
	int done;
	cin >> done;
	for (int j=0;j<4;j++)
		{
			hPt[j]->SetLineColor(j+1);
			hMcPt[j]->SetLineColor(j+1);
			hPtR[j]->SetLineColor(j+1);
			hEta[j]->SetLineColor(j+1);
			hMcEta[j]->SetLineColor(j+1);
			hEtaR[j]->SetLineColor(j+1);
			pDPt[j]->SetLineColor(1+j);
			pDPt2[j]->SetLineColor(1+j);
			pDPtR[j]->SetLineColor(1+j);
			pDPt2R[j]->SetLineColor(1+j);
			pDPz[j]->SetLineColor(1+j);
			pDPz2[j]->SetLineColor(1+j);
			pDPzR[j]->SetLineColor(1+j);
			pDPz2R[j]->SetLineColor(1+j);
			pDEta[j]->SetLineColor(1+j);
			pDEta2[j]->SetLineColor(1+j);
			pDEtaR[j]->SetLineColor(1+j);
			pDEta2R[j]->SetLineColor(1+j);
			
			hPtR[j]->SetMinimum(0.);
			hPtR[j]->SetMaximum(1.1);
			hEtaR[j]->SetMinimum(0.);
			hEtaR[j]->SetMaximum(1.1);
		}
	
	hPtR[0]->Draw();
	hPtR[1]->Draw("SAME");
	hPtR[2]->Draw("SAME");
	hPtR[3]->Draw("SAME");
	c->Print(path+"trackEffVsPt.gif");
	
	hMcEta[0]->Draw();
	hEta[0]->Draw("SAME");
	c->Print(path+"nTrackVsEta.gif");
	
	hEtaR[0]->Draw();
	hEtaR[1]->Draw("SAME");
	hEtaR[2]->Draw("SAME");
	hEtaR[3]->Draw("SAME");
	c->Print(path+"trackEffVsEta.gif");
	
	
	pDPt[0]->SetMinimum(-1.);
	pDPt[0]->SetMaximum(1.);
	pDPt[0]->Draw();
	pDPt[1]->Draw("SAME");
	pDPt[2]->Draw("SAME");
	pDPt[3]->Draw("SAME");
	c->Print(path+"dPtVsPt.gif");
	
	pDPt2[0]->SetMinimum(-1.);
	pDPt2[0]->SetMaximum(1.);
	pDPt2[0]->Draw();
	pDPt2[1]->Draw("SAME");
	pDPt2[2]->Draw("SAME");
	pDPt2[3]->Draw("SAME");
	c->Print(path+"dPt2VsPt.gif");
	
	pDPtR[0]->SetMinimum(-0.3);
	pDPtR[0]->SetMaximum(0.3);
	pDPtR[0]->Draw();
	pDPtR[1]->Draw("SAME");
	pDPtR[2]->Draw("SAME");
	pDPtR[3]->Draw("SAME");
	c->Print(path+"dPtRelVsPt.gif");
	
	pDPt2R[3]->SetMinimum(0.);
	pDPt2R[3]->SetMaximum(0.3);
	//pDPt2R[0]->Draw();
	//pDPt2R[1]->Draw("SAME");
	//pDPt2R[2]->Draw("SAME");
	pDPt2R[3]->Draw();
	c->Print(path+"dPt2RelVsPt.gif");
	
	pDPz[0]->SetMinimum(-0.2);
	pDPz[0]->SetMaximum(0.2);
	pDPz[0]->Draw();
	pDPz[1]->Draw("SAME");
	pDPz[2]->Draw("SAME");
	pDPz[3]->Draw("SAME");
	c->Print(path+"dPzVsPz.gif");
	
	pDPz2[0]->SetMinimum(0.);
	pDPz2[0]->SetMaximum(0.5);
	pDPz2[0]->Draw();
	//pDPz2[1]->Draw("SAME");
	//pDPz2[2]->Draw("SAME");
	//pDPz2[3]->Draw("SAME");
	c->Print(path+"dPz2VsPz.gif");

	pDPzR[0]->SetMinimum(-0.2);
	pDPzR[0]->SetMaximum(0.2);
	pDPzR[0]->Draw();
	pDPzR[1]->Draw("SAME");
	pDPzR[2]->Draw("SAME");
	pDPzR[3]->Draw("SAME");
	c->Print(path+"dPzRelVsPz.gif");

	pDPz2R[0]->SetMinimum(0.);
	pDPz2R[0]->SetMaximum(0.5);	
	pDPz2R[0]->Draw();
	//pDPz2R[1]->Draw("SAME");
	//pDPz2R[2]->Draw("SAME");
	//pDPz2R[3]->Draw("SAME");
	c->Print(path+"dPz2RelVsPz.gif");
	
	pDEta[0]->SetMinimum(-0.5);
	pDEta[0]->SetMaximum(0.5);
	pDEta[0]->Draw();
	pDEta[1]->Draw("SAME");
	pDEta[2]->Draw("SAME");
	pDEta[3]->Draw("SAME");
	c->Print(path+"dEtaVsEta.gif");
	
	pDEta2[0]->SetMinimum(0.);
	pDEta2[0]->SetMaximum(0.5);
	pDEta2[0]->Draw();
	pDEta2[1]->Draw("SAME");
	pDEta2[2]->Draw("SAME");
	pDEta2[3]->Draw("SAME");
	c->Print(path+"dEta2VsEta.gif");

	pDEtaR[0]->SetMinimum(-0.5);
	pDEtaR[0]->SetMaximum(0.5);
	pDEtaR[0]->Draw();
	pDEtaR[1]->Draw("SAME");
	pDEtaR[2]->Draw("SAME");
	pDEtaR[3]->Draw("SAME");
	c->Print(path+"dEtaRelVsEta.gif");
	
	pDEta2R[0]->SetMinimum(0.);
	pDEta2R[0]->SetMaximum(0.5);
	pDEta2R[0]->Draw();
	//pDEta2R[1]->Draw("SAME");
	//pDEta2R[2]->Draw("SAME");
	//pDEta2R[3]->Draw("SAME");
	c->Print(path+"dEta2RelVsEta.gif");
}

void standardPlots::makeMomentumPlots()
{
  cout <<"standardPlots::makeMomentumPlots | Creating Standard Momentum Plots"<<endl;
   if (fChain == 0) return;

   nentries = int(fChain->GetEntries());
   nbytes = 0;
   nb = 0;

   TH3D* resolpteta = new TH3D("resolpteta","3d dpt/pt vs pt vs eta",20,-1,1,20,0,2,50,-.5,.5);
   TH3D* resolpeta = new TH3D("resolpeta","3d dp/p vs p vs eta",20,-1,1,20,0,2,50,-.5,.5);
   resolpeta->SetXTitle("eta");
   resolpeta->SetYTitle("p");
   resolpeta->SetZTitle("#delta p/p");

  cout <<"standardPlots::makeMomentumPlots +-+Looping over events:"<<endl;
  cout <<"standardPlots::makeMomentumPlots   +-There are "<<nentries<<" events."<<endl;
   
   for (int jentry=0; jentry<nentries;jentry++)
   {
      ientry = LoadTree(jentry); //in case of a TChain, ientry
                                       //is the entry number in the
                                       //current file
      nb = fChain->GetEntry(jentry);
      nbytes += nb;
      // if (Cut(ientry) < 0) continue;

      //now loop over all Matched tracks in event
      for(int tMatched=0;tMatched<mMatchedPairs_;tMatched++)
	{
	  if(mMatchedPairs_mFitPts[tMatched]>=24 && mMatchedPairs_mDcaXYGl[tMatched]<1)
	    {
	      float dptp = (mMatchedPairs_mPtMc[tMatched]-mMatchedPairs_mPtPr[tMatched])
		             /mMatchedPairs_mPtPr[tMatched];
	      resolpteta->Fill(mMatchedPairs_mEtaPr[tMatched],mMatchedPairs_mPtMc[tMatched],dptp);
	      dptp = (sqrt(mMatchedPairs_mPtMc[tMatched]*mMatchedPairs_mPtMc[tMatched]
			  +mMatchedPairs_mPzMc[tMatched]*mMatchedPairs_mPzMc[tMatched])
	             -sqrt(mMatchedPairs_mPtPr[tMatched]*mMatchedPairs_mPtPr[tMatched]
			   +mMatchedPairs_mPzPr[tMatched]*mMatchedPairs_mPzPr[tMatched]))
		     /sqrt(mMatchedPairs_mPtMc[tMatched]*mMatchedPairs_mPtMc[tMatched]
			    +mMatchedPairs_mPzMc[tMatched]*mMatchedPairs_mPzMc[tMatched]);
      	      resolpeta->Fill(mMatchedPairs_mEtaPr[tMatched],
			      sqrt(mMatchedPairs_mPtMc[tMatched]*mMatchedPairs_mPtMc[tMatched]
			           +mMatchedPairs_mPzMc[tMatched]*mMatchedPairs_mPzMc[tMatched]),
			      dptp);
	    }//end cut 
      	}//end loop over tracks
   }//end loop over events
   cout <<"standardPlots::makeMomentumPlots   +-Done."<<endl;

   //-----------Slice and fit 3D hists-----------

    cout <<"standardPlots::makeMomentumPlots +-Slicing 3D Hists:"<<endl;
   
    //Transverse Momentum
    TString projTit("resolslice");
    char* extraTitle = "ptYY";
    TF1* gaus2 = new TF1("gaus2","gaus(0)+gaus(3)",-.25,.25);
    gaus2->SetParameter(0,500);
    gaus2->SetParameter(1,0);
    gaus2->SetParameter(2,.08);
    gaus2->SetParameter(3,100);
    gaus2->SetParameter(4,0);
    gaus2->SetParameter(5,.04);
    gaus2->SetParName(0,"area");
    gaus2->SetParName(1,"mean");
    gaus2->SetParName(2,"width");
    gaus2->SetParName(3,"area2");
    gaus2->SetParName(4,"mean2");
    gaus2->SetParName(5,"width2");


    TH1D* resolutionPtAtEtaZero = new TH1D("resolutionPtAtEtaZero","RMS (Pt_rec - Pt_mc)  vs Pt (eta=0)",20,0,2);
    TH1D* energylossPtAtEtaZero = new TH1D("energylossPtAtEtaZero","energy loss vs Pt (|eta| < 0.1)",20,0,2);
    for (int i=2;i<=20;++i)
    {
	sprintf(extraTitle,"pt%02i",i);
	TString currentTit = projTit + extraTitle;

	TH1D*  resolslice = resolpteta->ProjectionZ(currentTit.Data(),10,11,i,i,"e");

	resolslice->Fit("gaus");
// 	//etaResol->ProjectionZ("etaresolPt",11,11,i,i)->Fit("gaus");
	//canvas->Modified();
 	//canvas->Update();
 	energylossPtAtEtaZero->SetBinContent(i,resolslice->GetMean());
 	energylossPtAtEtaZero->SetBinError(i,gaus2->GetParError(1));
 	resolutionPtAtEtaZero->SetBinContent(i,gaus2->GetParameter(2));
 	resolutionPtAtEtaZero->SetBinError(i,gaus2->GetParError(2));
    }
    energylossPtAtEtaZero->SetMarkerStyle(20);
    energylossPtAtEtaZero->SetMarkerColor(4);
    energylossPtAtEtaZero->SetXTitle("pt (GeV/c)");
    resolutionPtAtEtaZero->SetMarkerStyle(20);
    resolutionPtAtEtaZero->SetMarkerColor(4);
    resolutionPtAtEtaZero->SetXTitle("pt (GeV/c)");
    resolutionPtAtEtaZero->Draw();

    TH1D* resolutionPAtEtaZero = new TH1D("resolutionPAtEtaZero","RMS (P_rec - P_mc)  vs P (eta=0)",20,0,2);
    TH1D* energylossPAtEtaZero = new TH1D("energylossPAtEtaZero","energy loss vs P (|eta| < 0.1)",20,0,2);
    for (int i=2;i<=20;++i)
    {
	sprintf(extraTitle,"pt%02i",i);
	TString currentTit = projTit + extraTitle;

	TH1D*  resolslice = resolpeta->ProjectionZ(currentTit.Data(),10,11,i,i,"e");

	resolslice->Fit("gaus");
 	energylossPAtEtaZero->SetBinContent(i,resolslice->GetMean());
 	energylossPAtEtaZero->SetBinError(i,gaus2->GetParError(1));
 	resolutionPAtEtaZero->SetBinContent(i,gaus2->GetParameter(2));
 	resolutionPAtEtaZero->SetBinError(i,gaus2->GetParError(2));
    }
    energylossPAtEtaZero->SetMarkerStyle(20);
    energylossPAtEtaZero->SetMarkerColor(4);
    energylossPAtEtaZero->SetXTitle("pt (GeV/c)");
    resolutionPAtEtaZero->SetMarkerStyle(20);
    resolutionPAtEtaZero->SetMarkerColor(4);
    resolutionPAtEtaZero->SetXTitle("pt (GeV/c)");
    resolutionPAtEtaZero->Draw();

    //make momentum resolution plot

    TProfile *myMomResPlot=new TProfile("myMomResPlot","dp/p vs p.",20,0,4);
    TProfile *mySigmaMomResPlot=new TProfile("mySigmaMomResPlot","RMS(dp/p) vs p.",20,0,4);
    float totalM=0; float totalMmc=0;
    nbytes = 0;
    nb = 0;
   for (Int_t jentry=0; jentry<nentries;jentry++) {
      ientry = LoadTree(jentry); //in case of a TChain, ientry is the entry number in the current file
      nb = fChain->GetEntry(jentry);   nbytes += nb;
      for(int iTrack=0; iTrack<mMatchedPairs_; iTrack++)
	{
	  if(!trackCut(ientry,iTrack)) continue;
	     totalM=mMatchedPairs_mPtPr[iTrack]*mMatchedPairs_mPtPr[iTrack]
	       +mMatchedPairs_mPzPr[iTrack]*mMatchedPairs_mPzPr[iTrack];
	     totalM=sqrt(totalM);
	  
	     totalMmc=mMatchedPairs_mPtMc[iTrack]*mMatchedPairs_mPtMc[iTrack]
	       +mMatchedPairs_mPzMc[iTrack]*mMatchedPairs_mPzMc[iTrack];
	     totalMmc=sqrt(totalMmc);
	     myMomResPlot->Fill(totalMmc, (totalMmc-totalM)/totalMmc);
      	}
	  
   }
   for(int iBin=0; iBin<20;iBin++)
     {
       float mom = 4.*((float)(iBin+1))/20.-.1;
       mySigmaMomResPlot->Fill(mom,myMomResPlot->GetBinError(iBin));
     }
    return;
}
  
void standardPlots::makeFitPointsPlots()
{
  
  TH1D* fitPointsPlot=new TH1D("fitPointsPlot","Number of Fit Points / Number Possible", 50, 0,1);
  TH1D* fitPointsUsed=new TH1D("fitPointsUsed","Number of Fit Points Used", 50, 0, 50);
  TH1D* fitPointsPossible=new TH1D("fitPointsPossible","Number of Fit Points Possible", 50, 0,50);


  
   if (fChain == 0) return;

   nentries = Int_t(fChain->GetEntries());

   nbytes = 0;
   nb = 0;
   for (Int_t jentry=0; jentry<nentries;jentry++) {
		 ientry = LoadTree(jentry); //in case of a TChain, ientry is the entry number in the current file
      nb = fChain->GetEntry(jentry);
      nbytes += nb;
      // if (Cut(ientry) < 0) continue;

      for(Int_t nMatched = 0; nMatched<mMatchedPairs_;nMatched++)
	{
	  if(mMatchedPairs_mFitPts[nMatched]>=24 && mMatchedPairs_mDcaXYGl[nMatched]<1
	     && mMatchedPairs_mPtMc[nMatched] >.4)
	    {
	      fitPointsPlot->Fill(mMatchedPairs_mFitPts[nMatched]/mMatchedPairs_mNPossible[nMatched]);
	      fitPointsUsed->Fill(mMatchedPairs_mFitPts[nMatched]);
	      fitPointsPossible->Fill(mMatchedPairs_mNPossible[nMatched]);
	    }
	}

   }
   fitPointsPlot->Draw();


   fitPointsUsed->SetMarkerColor(3);
   fitPointsUsed->SetLineColor(3);
   fitPointsPossible->SetMarkerColor(4);
   fitPointsPossible->SetLineColor(4);
   fitPointsPossible->Draw();
   fitPointsUsed->Draw("same");
}


/*
 *
 * $Id: standardPlots.C,v 1.6 2002/08/26 22:17:15 andrewar Exp $
 *  A. Rose, WSU
 *  
  *
 * $Log: standardPlots.C,v $
 * Revision 1.6  2002/08/26 22:17:15  andrewar
 * Added multi-file input functionality (see doRun() for examples of use). Added
 * cut string to provided easy equivalence of cut function in on-the-fly hists.
 *
 * Revision 1.5  2002/08/19 19:33:20  pruneau
 * eliminated cout when unnecessary, made helix member of the EventFiller
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
#define MAX_TRACKS 7000

#include <iostream.h>

#include "TSystem.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TF1.h"
#include "standardPlots.h"

void doRun(char* outfile)
{
  standardPlots old(gSystem, "/star/data22/ITTF/EvalData/MCNtuple","*tpt.minimc.root");
  standardPlots closed(gSystem);
  standardPlots open(gSystem, "/star/data22/ITTF/EvalData/TempHist/Window20","*evts.minimc.root");

  
  old.SetTrackCutNHit(1,55);
  closed.SetTrackCutNHit(1,55);
  open.SetTrackCutNHit(1,55);
  old.SetTrackCutNAHit(1,55);
  closed.SetTrackCutNAHit(1,55);
  open.SetTrackCutNAHit(1,55);

      //do lowest mult first
      int lowMult=0, highMult=2000;
    old.SetEventCutMult(lowMult,highMult);
    closed.SetEventCutMult(lowMult,highMult);
    open.SetEventCutMult(lowMult,highMult);

    old.makeTrackEffPlots(100);
    closed.makeTrackEffPlots(100);
    open.makeTrackEffPlots(100);

    TH1D *oldLowMultLowHit=new TH1D("oldLowMultLowHit","",100,0,5);
    oldLowMultLowHit->Divide(old.primaryTrackPt,old.mcTrackEffPt);
    TH1D *closedLowMultLowHit=new TH1D("closedLowMultLowHit","",100,0,5);
    closedLowMultLowHit->Divide(closed.primaryTrackPt,closed.mcTrackEffPt);
    TH1D *openLowMultLowHit=new TH1D("openLowMultLowHit","",100,0,5);
    openLowMultLowHit->Divide(open.primaryTrackPt,open.mcTrackEffPt);

    lowMult=200;
    highMult=4000;
    old.SetEventCutMult(lowMult,highMult);
    closed.SetEventCutMult(lowMult,highMult);
    open.SetEventCutMult(lowMult,highMult);

    old.makeTrackEffPlots(100);
    closed.makeTrackEffPlots(100);
    open.makeTrackEffPlots(100);

    TH1D *oldMedMultLowHit=new TH1D("oldMedMultLowHit","",100,0,5);
    oldMedMultLowHit->Divide(old.primaryTrackPt,old.mcTrackEffPt);
    TH1D *closedMedMultLowHit=new TH1D("closedMedMultLowHit","",100,0,5);
    closedMedMultLowHit->Divide(closed.primaryTrackPt,closed.mcTrackEffPt);
    TH1D *openMedMultLowHit=new TH1D("openMedMultLowHit","",100,0,5);
    openMedMultLowHit->Divide(open.primaryTrackPt,open.mcTrackEffPt);

    lowMult=4000;
    highMult=6000;
    old.SetEventCutMult(lowMult,highMult);
    closed.SetEventCutMult(lowMult,highMult);
    open.SetEventCutMult(lowMult,highMult);

    old.makeTrackEffPlots(100);
    closed.makeTrackEffPlots(100);
    open.makeTrackEffPlots(100);

    TH1D *oldHiMultLowHit=new TH1D("oldHiMultLowHit","",100,0,5);
    oldHiMultLowHit->Divide(old.primaryTrackPt,old.mcTrackEffPt);
    TH1D *closedHiMultLowHit=new TH1D("closedHiMultLowHit","",100,0,5);
    closedHiMultLowHit->Divide(closed.primaryTrackPt,closed.mcTrackEffPt);
    TH1D *openHiMultLowHit=new TH1D("openHiMultLowHit","",100,0,5);
    openHiMultLowHit->Divide(open.primaryTrackPt,open.mcTrackEffPt);

    



    TFile *oFile = new TFile(outfile,"RECREATE");
    oldHiMultLowHit->Write();
    closedHiMultLowHit->Write();
    openHiMultLowHit->Write();
    oldMedMultLowHit->Write();
    closedMedMultLowHit->Write();
    openMedMultLowHit->Write();
    oldLowMultLowHit->Write();
    closedLowMultLowHit->Write();
    openLowMultLowHit->Write();
    
    oFile->Close();


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
      Int_t ientry = LoadTree(jentry); //in case of a TChain, ientry is the entry number in the current file
      if(!ientry%100) cout <<"Local file entry "<<ientry<<endl;
      nb = fChain->GetEntry(jentry);   nbytes += nb;
   }
}

void standardPlots::makeTrackEffPlots(int nEvents)
{

   if (fChain == 0) return;

   showCuts();

   //Hists are now initialized in InitHists()

   nentries = Int_t(fChain->GetEntries());
   nbytes = 0;
   nb = 0;
   for (Int_t jentry=0; jentry<nentries;jentry++) {
      int ientry=LoadTree(jentry); //in case of a TChain, ientry is the entry number in the current file
      nb = fChain->GetEntry(jentry);   nbytes += nb;

      if (!Cut(ientry)) continue;

      //do some global event characteristic stuff
      float mPrimaryTrackEff = (float)mMatchedPairs_/(float)mMcTracks_;
      primaryTrackEffMult->Fill((float)mMcTracks_,mPrimaryTrackEff);

      
      double pt;
      double eta;
      int    nHits;
      int    gId;
      double q;
      int iTrack;

      //make Pt and Eta spectra of MC tracks
      for(iTrack=0;iTrack<mMcTracks_;iTrack++)
	{


	  if(!mcTrackCut(ientry,iTrack)) continue;  //next track if mcTrackCut doesn't pass
	  pt    = mMcTracks_mPtMc[iTrack];
	  eta   = mMcTracks_mEtaMc[iTrack];
	  nHits = mMcTracks_mNHitMc[iTrack];
	  gId   = mMcTracks_mGeantId[iTrack];
	  q     = mMcTracks_mChargeMc[iTrack];

	  mcTrackEffEta->Fill(eta);
	  mcTrackEffPt->Fill(pt);
	}

      //make Pt and Eta spectra of matched tracks
      for(iTrack=0;iTrack<mMatchedPairs_;iTrack++)
	{
	  if (!trackCut(ientry,iTrack)) continue;  //next track if !trackCut 
	  pt    = mMatchedPairs_mPtMc[iTrack];
	  eta   = mMatchedPairs_mEtaMc[iTrack];
	  nHits = mMatchedPairs_mNHitMc[iTrack];
	  gId   = mMatchedPairs_mGeantId[iTrack];
	  q     = mMatchedPairs_mChargeMc[iTrack];

	  primaryTrackEta->Fill(eta);
	  primaryTrackPt->Fill(pt);
	}


   }//end loop over events
 
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
      int ientry = LoadTree(jentry); //in case of a TChain, ientry
                                       //is the entry number in the
                                       //current file
      nb = fChain->GetEntry(jentry);
      nbytes += nb;
      if (Cut(ientry) < 0) continue;

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
    return;
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
      Int_t ientry = LoadTree(jentry); //in case of a TChain, ientry is the entry number in the current file
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
  

  
   if (fChain == 0) return;

   nentries = Int_t(fChain->GetEntries());

   nbytes = 0;
   nb = 0;
   cout <<"Making Plots..."<<endl;
   for (Int_t jentry=0; jentry<nentries;jentry++) {
      Int_t ientry = LoadTree(jentry); //in case of a TChain, ientry is the entry number in the current file
      nb = fChain->GetEntry(jentry);
      nbytes += nb;
      if (Cut(ientry) < 0) continue;

      for(Int_t iTrack = 0; iTrack<mMatchedPairs_;iTrack++)
	{
	  if(trackCut(ientry, iTrack))
	    {
	      fitPointsPlot->Fill((float)mMatchedPairs_mFitPts[iTrack]/(float)mMatchedPairs_mNPossible[iTrack]);
	      fitPointsUsed->Fill((float)mMatchedPairs_mFitPts[iTrack]);
	      fitPointsPossible->Fill(mMatchedPairs_mNPossible[iTrack]);
	      fitPointsPt->Fill(mMatchedPairs_mPtMc[iTrack],
				(float)mMatchedPairs_mFitPts[iTrack]); 
	      fitPointsEta->Fill(mMatchedPairs_mEtaMc[iTrack],
				(float)mMatchedPairs_mFitPts[iTrack]); 
	    }
	}

   }

}

void standardPlots::makeHitEffPlots()
{
  cout <<"standardPlots::hitPlots | Plotting Hit Efficiencies"<<endl;

   if (fChain == 0) return;

   nentries = Int_t(fChain->GetEntries());
   nbytes = 0;
   nb = 0;
   for (Int_t jentry=0; jentry<nentries;jentry++)
   {
      Int_t ientry = LoadTree(jentry); //in case of a TChain, ientry is the entry number in the current file
      nb = fChain->GetEntry(jentry);
      nbytes += nb;
      if (Cut(ientry) < 0) continue;

      for(Int_t nMatched = 0; nMatched<mMatchedPairs_;nMatched++)
	{

	  if(!trackCut(ientry, nMatched)) continue;

	  float hitEffRatio=(float)mMatchedPairs_mNCommonHit[nMatched]
	                    /(float)mMatchedPairs_mNHitMc[nMatched];
	  hitEffMult->Fill(mNMcTrack, hitEffRatio);
	  //cout <<mNMcTrack <<" "<<hitEffRatio<<endl;
	  hitEffPt->Fill(mMatchedPairs_mPtMc[nMatched],
			   hitEffRatio);
	  hitEffEta->Fill(mMatchedPairs_mEtaMc[nMatched],
			   hitEffRatio);

	  hitN->Fill((float)mMatchedPairs_mNCommonHit[nMatched]);

	  lastHit->Fill(mMatchedPairs_mLastFitPadrow[nMatched]);
	  firstHit->Fill(mMatchedPairs_mFirstFitPadrow[nMatched]);
	  lastHitPt->Fill(mMatchedPairs_mPtMc[nMatched],mMatchedPairs_mLastFitPadrow[nMatched]);
	  firstHitPt->Fill(mMatchedPairs_mPtMc[nMatched],mMatchedPairs_mFirstFitPadrow[nMatched]);
	  lastHitEta->Fill(mMatchedPairs_mEtaMc[nMatched],mMatchedPairs_mLastFitPadrow[nMatched]);
	  firstHitEta->Fill(mMatchedPairs_mEtaMc[nMatched],mMatchedPairs_mFirstFitPadrow[nMatched]);
	}


   }//end loop over events
   hitEffMult->Draw();
}

void standardPlots::makeGeantIdPlots()
{
   if (fChain == 0) return;

   nentries = Int_t(fChain->GetEntries());

   nbytes = 0;
   nb = 0;
   cout <<"Making Plots..."<<endl;
   for (Int_t jentry=0; jentry<nentries;jentry++) {
      Int_t ientry = LoadTree(jentry); //in case of a TChain, ientry is the entry number in the current file
      nb = fChain->GetEntry(jentry);
      nbytes += nb;
      if (Cut(ientry) < 0) continue;

      for(Int_t iTrack = 0; iTrack<mMatchedPairs_;iTrack++)
	{
	  if(trackCut(ientry, iTrack))
	    {
	      mGeantIdPlot->Fill(mMatchedPairs_mGeantId[iTrack]);
	    }
	}

   }
}

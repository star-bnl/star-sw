/*
 *
 * $Id: standardPlots.C,v 1.3 2002/07/02 18:58:52 andrewar Exp $
 *  A. Rose, WSU
 *  
 *
 * $Log: standardPlots.C,v $
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

#define MAX_TRACKS 7000
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


   Int_t nentries = Int_t(fChain->GetEntries());

   Int_t nbytes = 0, nb = 0;
   for (Int_t jentry=0; jentry<nentries;jentry++) {
      Int_t ientry = LoadTree(jentry); //in case of a TChain, ientry is the entry number in the current file
      nb = fChain->GetEntry(jentry);   nbytes += nb;

   }
}

void standardPlots::makeTrackEffPlots()
{

   if (fChain == 0) return;

   TProfile* primaryTrackEff=new TProfile("primaryTrackEff","Sti PrimaryTracks vs. Mc PrimaryTracks",100,0,MAX_TRACKS);
   TProfile* primaryTrackMult=new TProfile("primaryTrackEffMult","PrimaryTrack Finding Efficiency vs. McMult",100,0,MAX_TRACKS);
   TProfile* globalTrackEff=new TProfile("globalTrackEff","Sti GlobalTracks vs. Mc GlobalTracks",100,0,MAX_TRACKS);
   TProfile* globalTrackEffMult=new TProfile("globalTrackEffMult","GlobalTrack Finding Efficiency vs. McMult",100,0,MAX_TRACKS);


   TH1D* primaryTrackPt=new TH1D("primaryTrackPt","PrimaryTrack Pt",100,0,5);
   TH1D* primaryTrackEta=new TH1D("primaryTrackEta","PrimaryTrack Eta",100,-1,1);
   TH1D* mcTrackEffPt=new TH1D("mcTrackEffPt","McTrack Pt",100,0,5);
   TH1D* mcTrackEffEta=new TH1D("mcTrackEffEta","McTrack Eta",100,-1,1);
   TProfile* globalTrackEffPt=new TProfile("globalTrackEffPt","GlobalTrack Finding Efficiency vs. McPt",100,0,5);
   TProfile* globalTrackEffEta=new TProfile("globalTrackEffEta","GlobalTrack Finding Efficiency vs. McEta",100,-1,1);

   Int_t nentries = Int_t(fChain->GetEntries());

   Int_t nbytes = 0, nb = 0;
   for (Int_t jentry=0; jentry<nentries;jentry++) {
      Int_t ientry = LoadTree(jentry); //in case of a TChain, ientry is the entry number in the current file
      nb = fChain->GetEntry(jentry);   nbytes += nb;
      if (!Cut(ientry)) continue;
      //cout <<"mMatchedPairs: "<<mMatchedPairs_<<endl
      //   <<"mMcTracks: "<<mMcTracks_<<endl
      //;
      float mPrimaryTrackEff = (float)mMatchedPairs_/(float)mMcTracks_;
      primaryTrackEff->Fill((float)mMcTracks_,((float)mMatchedPairs_));
      primaryTrackEffMult->Fill((float)mMcTracks_,mPrimaryTrackEff);

      //make Pt and Eta spectra of matched tracks
      for(int iTrack=0;iTrack<mMatchedPairs_;iTrack++)
	{
	  if(!trackCut(ientry,iTrack)) continue;  //next track if trackCut 
	                                          //doesn't pass
	  primaryTrackEta->Fill(mMatchedPairs_mEtaPr[iTrack]);
	  primaryTrackPt->Fill(mMatchedPairs_mEtaPr[iTrack]);
	}

      //make Pt and Eta spectra of MC tracks
      for(int iTrack=0;iTrack<mMcTracks_;iTrack++)
	{
	  if(!mcTrackCut(ientry,iTrack)) continue;  //next track if mcTrackCut 
	                                          //doesn't pass
	  mcTrackEffEta->Fill(mMcTracks_mEtaMc[iTrack]);
	  mcTrackEffPt->Fill(mMcTracks_mPtMc[iTrack]);
	}

      //float mGlobalTrackEff = 0.;
      //for(int iTrack=0; iTrack<mMatchedPairs_;iTrack++)
      //{
	  //if(//global)
	  //   globalTrackEff->Fill((float)mMcTracks_,((float)mMatchedPairs_));
	  //   globalTrackEffMult->Fill((float)mMcTracks_,mGlobalTrackEff);
      //}

      //fill global hists?
      globalTrackEff->Fill((float)mMcTracks_,((float)mMatchedPairs_));
      globalTrackEffMult->Fill((float)mMcTracks_,mPrimaryTrackEff);
   }
   
   cout <<"\tDisplaying Mc Track and Sti Track Eta Distribution:"<<endl;
   mcTrackEffEta->Draw();
   primaryTrackEta->SetMarkerColor(4);
   primaryTrackEta->SetLineColor(4);
   primaryTrackEta->Draw("same");
   cout <<"\tDisplaying Mc Track and Sti Track Pt Distribution:"<<endl;
   mcTrackEffPt->Draw();
   primaryTrackPt->SetMarkerColor(4);
   primaryTrackPt->SetLineColor(4);
   primaryTrackPt->Draw("same");


   cout <<"\tDisplaying Track Efficiency vs. Multiplicity:"<<endl;
   primaryTrackEffMult->Draw();


}


void standardPlots::makeMomentumPlots()
{
  cout <<"standardPlots::makeMomentumPlots | Creating Standard Momentum Plots"<<endl;
   if (fChain == 0) return;

   int nentries = int(fChain->GetEntries());

   int nbytes = 0, nb = 0;

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
 	energylossPtAtEtaZero->SetBinError(i,gaus->GetParError(1));
 	resolutionPtAtEtaZero->SetBinContent(i,gaus->GetParameter(2));
 	resolutionPtAtEtaZero->SetBinError(i,gaus->GetParError(2));
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
 	energylossPAtEtaZero->SetBinError(i,gaus->GetParError(1));
 	resolutionPAtEtaZero->SetBinContent(i,gaus->GetParameter(2));
 	resolutionPAtEtaZero->SetBinError(i,gaus->GetParError(2));
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
   Int_t nbytes = 0, nb = 0;
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
  
  TH1D* fitPointsPlot=new TH1D("fitPointsPlot","Number of Fit Points / Number Possible", 50, 0,1);
  TH1D* fitPointsUsed=new TH1D("fitPointsUsed","Number of Fit Points Used", 50, 0, 50);
  TH1D* fitPointsPossible=new TH1D("fitPointsPossible","Number of Fit Points Possible", 50, 0,50);


  
   if (fChain == 0) return;

   Int_t nentries = Int_t(fChain->GetEntries());

   Int_t nbytes = 0, nb = 0;
   for (Int_t jentry=0; jentry<nentries;jentry++) {
      Int_t ientry = LoadTree(jentry); //in case of a TChain, ientry is the entry number in the current file
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

void standardPlots::makeHitEffPlots()
{
  cout <<"standardPlots::hitPlots | Plotting Hit Efficiencies"<<endl;
  
  TProfile *hitEffMult=new TProfile("hitEffMult","Hit Shared / N Hits Possible", 50, 0, MAX_TRACKS);
  TProfile *hitEffPt=new TProfile("hitEffPt","Hit Shared / N Hits Possible", 50, 0, 5);
  TProfile *hitEffEta=new TProfile("hitEffEta","Hits Shared with MC Track / N Hits Possible", 50, -1.5,1.5);

   if (fChain == 0) return;

   Int_t nentries = Int_t(fChain->GetEntries());

   Int_t nbytes = 0, nb = 0;
   for (Int_t jentry=0; jentry<nentries;jentry++)
   {
      Int_t ientry = LoadTree(jentry); //in case of a TChain, ientry is the entry number in the current file
      nb = fChain->GetEntry(jentry);
      nbytes += nb;
      // if (Cut(ientry) < 0) continue;

      for(Int_t nMatched = 0; nMatched<mMatchedPairs_;nMatched++)
	{
	  float hitEffRatio=(float)mMatchedPairs_mNCommonHit[nMatched]
	                    /(float)mMatchedPairs_mNHitMc[nMatched];
	  hitEffMult->Fill(mNMcTrack, hitEffRatio);
	  //cout <<mNMcTrack <<" "<<hitEffRatio<<endl;
	  hitEffPt->Fill(mMatchedPairs_mPtMc[nMatched],
			   hitEffRatio);
	  hitEffEta->Fill(mMatchedPairs_mEtaMc[nMatched],
			   hitEffRatio);
	}
   }//end loop over events
   hitEffMult->Draw();
}



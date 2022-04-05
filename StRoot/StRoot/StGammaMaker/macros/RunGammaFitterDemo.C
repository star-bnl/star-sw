///
/// \author Pibero Djawotho <pibero@indiana.edu>
/// \author Indiana University
/// \date 7 July 2007
///

void RunGammaFitterDemo(const char* filelist = "/star/u/sdhamija/StGammaMaker/StRoot/StGammaMaker/macros/photon_9_11.list")
{
  // Load shared libraries
  gROOT->Macro("$STAR/StRoot/StGammaMaker/macros/loadGammaLibs.C");

  // Create TChain from filelist
  TChain* chain = new TChain("gammas");
  ifstream in(filelist);
  string line;
  while (getline(in, line)) chain->AddFile(line.c_str());
  in.close();

  // Create gamma event buffer
  StGammaEvent* event = new StGammaEvent;
  chain->SetBranchAddress("GammaEvent", &event);

  // Create gamma fitter histograms
  TH2* hResidualYield = new TH2F("hResidualYield", "#gamma events from pp#rightarrow#gamma+jet+X;Fit Residual Sum (R_{0,u}+R_{0,v}) [MeV];Fitted Peak Integral [MeV]", 40, 0, 200, 40, 0, 600);
  TH1* hVertexZ = new TH1F("hVertexZ", ";z_{vertex} [cm]", 100, -200, 200);
  TH2* hVertexXY = new TH2F("hVertexXY", ";x_{vertex} [cm];y_{vertex} [cm]", 40, -0.2, 0.2, 40, -0.45, -0.2);
  TH1* hClusterEnergy = new TH1F("hClusterEnergy", ";Cluster E [GeV]", 100, 0, 60);
  TH1* hClusterPt = new TH1F("hClusterPt", ";Cluster p_{T} [GeV]", 100, 0, 20);
  TH2* hClusterXY = new TH2F("hClusterXY", ";Cluster x [cm];Cluster y [cm]", 40, -250, 250, 40, -250, 250);
  TH2* hClusterEtaPhi = new TH2F("hClusterEtaPhi", ";Cluster #eta;Cluster #phi [radians]", 40, 1, 2, 40, -TMath::Pi(), TMath::Pi());
  TH2* hSmdPointXY = new TH2F("hSmdPointXY", ";SMD x [cm]; SMD y [cm]", 40, -250, 250, 40, -250, 250);
  TH1* hYieldU = new TH1F("hYieldU", ";Fitted Peak Integral [MeV]", 100, 0, 400);
  TH1* hYieldV = new TH1F("hYieldV", ";Fitted Peak Integral [MeV]", 100, 0, 400);
  TH1* hResidualU = new TH1F("hResidualU", ";Fit Residual [MeV]", 100, -40, 120);
  TH1* hResidualV = new TH1F("hResidualV", ";Fit Residual [MeV]", 100, -40, 120);
  TH1* hChi2PerNdfU = new TH1F("hChi2PerNdfU", ";#chi^{2}/ndf", 100, 0, 10);
  TH1* hChi2PerNdfV = new TH1F("hChi2PerNdfV", ";#chi^{2}/ndf", 100, 0, 10);
  TH1* hMeanU = new TH1F("hMeanU", ";Mean #mu", 100, 0, 288);
  TH1* hMeanV = new TH1F("hMeanV", ";Mean #mu", 100, 0, 288);
  TH1* hSigmaU = new TH1F("hSigmaU", ";RMS #sigma", 100, 0, 10);
  TH1* hSigmaV = new TH1F("hSigmaV", ";RMS #sigma", 100, 0, 10);
  TH1* hNhitsU = new TH1F("hNhitsU", ";Number of SMD hits", 50, 0, 50);
  TH1* hNhitsV = new TH1F("hNhitsV", ";Number of SMD hits", 50, 0, 50);

  // Function for gamma/pi0 separation
  TF1* fResidualCut = new TF1("fResidualCut", "pol2", 0, 200);
  fResidualCut->SetParameters(100, 0, 0.05);

  // Get number of entries in gamma tree
  int nevents = chain->GetEntries();
  cout << "nevents = " << nevents << endl;

  // Event loop
  int ncandidates = 0;
  for (int iEvent = 0; iEvent < nevents; ++iEvent) {
    chain->GetEvent(iEvent);
    if (iEvent % 1000 == 0) cout << "iEvent = " << iEvent << endl;
    // Skip events without vertex
    if (event->vertex() == TVector3(0,0,0)) continue;
    // Candidate loop
    for (int i = 0; i < event->numberOfCandidates(); ++i) {
      StGammaCandidate* candidate = event->candidate(i);
      assert(candidate);
      // EEMC candidates only
      if (candidate->detectorId() == StGammaCandidate::kEEmc) {
	// Fit SMD u & v plane
	StGammaFitterResult fits[2];
	StGammaFitterResult& u = fits[0];
	StGammaFitterResult& v = fits[1];
	StGammaFitter::instance()->fit(candidate, fits, 0);
	StGammaFitter::instance()->fit(candidate, fits, 1);
	int sector, subsector, etabin;
	EEmcGeomSimple::Instance().getTower(candidate->position(), sector, subsector, etabin);
	TVector3& position = EEmcSmdGeom::instance()->getIntersection(sector, u.mean, v.mean);
	if (position.z() != -999) {
	  float residual = u.residual+v.residual;
	  float yield = u.yield+v.yield;
	  if (residual > 0 && yield > 0) {
	    ++ncandidates;
	    // Fill histograms
	    hResidualYield->Fill(residual, yield);
	    hVertexZ->Fill(event->vertex().z());
	    hVertexXY->Fill(event->vertex().x(), event->vertex().y());
	    hClusterEnergy->Fill(candidate->energy());
	    hClusterPt->Fill(candidate->momentum().Pt());
	    hClusterXY->Fill(candidate->position().x(), candidate->position().y());
	    hClusterEtaPhi->Fill(candidate->position().Eta(), candidate->position().Phi());
	    hSmdPointXY->Fill(position.x(), position.y());
	    hYieldU->Fill(u.yield);
	    hYieldV->Fill(v.yield);
	    hResidualU->Fill(u.residual);
	    hResidualV->Fill(v.residual);
	    if (u.ndf > 0) hChi2PerNdfU->Fill(u.chiSquare / u.ndf);
	    if (v.ndf > 0) hChi2PerNdfV->Fill(v.chiSquare / v.ndf);
	    hMeanU->Fill(u.mean);
	    hMeanV->Fill(v.mean);
	    hSigmaU->Fill(u.rms);
	    hSigmaV->Fill(v.rms);
	    hNhitsU->Fill(candidate->numberOfSmdu());
	    hNhitsV->Fill(candidate->numberOfSmdv());
	  }
	}
      }
    } // End candidate loop
  } // End event loop

  cout << "ncandidates = " << ncandidates << endl;

  // Create canvas
  TCanvas* c1 = new TCanvas("c1", "c1", 1200, 800);
  c1->Divide(3, 2);
  gStyle->SetPalette(1);
  gStyle->SetOptStat(11);

  // Draw histograms
  c1->cd(1);
  hResidualYield->Draw("colz");
  fResidualCut->SetLineColor(kRed);
  fResidualCut->Draw("same");
  gPad->Print("hResidualYield.png");

  c1->cd(2);
  hVertexZ->Draw();
  hVertexZ->Fit("gaus");
  hVertexZ->GetFunction("gaus")->SetLineColor(kRed);
  gPad->Print("hVertexZ.png");

  c1->cd(3);
  hVertexXY->Draw("colz");
  gPad->Print("hVertexXY.png");

  c1->cd(4);
  hClusterEnergy->Draw();
  gPad->Print("hClusterEnergy.png");

  c1->cd(5);
  hClusterPt->Draw();
  gPad->Print("hClusterPt.png");

  c1->cd(6);
  hClusterXY->Draw("colz");
  gPad->Print("hClusterXY.png");

  c1->cd(1);
  hClusterEtaPhi->Draw("colz");
  gPad->Print("hClusterEtaPhi.png");

  c1->cd(2);
  hSmdPointXY->Draw("colz");
  gPad->Print("hSmdPointXY.png");

  c1->cd(3);
  hYieldU->SetLineColor(kRed);
  hYieldV->SetLineColor(kBlue);
  hYieldU->SetLineWidth(2);
  hYieldV->SetLineWidth(2);
  hYieldU->Draw();
  hYieldV->Draw("same");
  TLegend* leg1 = new TLegend(0.60, 0.60, 0.85, 0.80);
  leg1->AddEntry(hYieldU, "SMD-u", "L");
  leg1->AddEntry(hYieldV, "SMD-v", "L");
  leg1->Draw();
  gPad->Print("hYield.png");

  c1->cd(4);
  hResidualU->SetLineColor(kRed);
  hResidualV->SetLineColor(kBlue);
  hResidualU->SetLineWidth(2);
  hResidualV->SetLineWidth(2);
  hResidualU->Draw();
  hResidualV->Draw("same");
  TLegend* leg2 = new TLegend(0.60, 0.60, 0.85, 0.80);
  leg2->AddEntry(hResidualU, "SMD-u", "L");
  leg2->AddEntry(hResidualV, "SMD-v", "L");
  leg2->Draw();
  gPad->Print("hResidual.png");

  c1->cd(5);
  hChi2PerNdfU->SetLineColor(kRed);
  hChi2PerNdfV->SetLineColor(kBlue);
  hChi2PerNdfU->SetLineWidth(2);
  hChi2PerNdfV->SetLineWidth(2);
  hChi2PerNdfU->Draw();
  hChi2PerNdfV->Draw("same");
  TLegend* leg3 = new TLegend(0.60, 0.60, 0.85, 0.80);
  leg3->AddEntry(hChi2PerNdfU, "SMD-u", "L");
  leg3->AddEntry(hChi2PerNdfV, "SMD-v", "L");
  leg3->Draw();
  gPad->Print("hChi2PerNdf.png");

  c1->cd(6);
  hMeanU->SetLineColor(kRed);
  hMeanV->SetLineColor(kBlue);
  hMeanU->SetLineWidth(2);
  hMeanV->SetLineWidth(2);
  hMeanU->Draw();
  hMeanV->Draw("same");
  TLegend* leg4 = new TLegend(0.60, 0.60, 0.85, 0.80);
  leg4->AddEntry(hMeanU, "SMD-u", "L");
  leg4->AddEntry(hMeanV, "SMD-v", "L");
  leg4->Draw();
  gPad->Print("hMean.png");

  c1->cd(1);
  hSigmaU->SetLineColor(kRed);
  hSigmaV->SetLineColor(kBlue);
  hSigmaU->SetLineWidth(2);
  hSigmaV->SetLineWidth(2);
  hSigmaU->Draw();
  hSigmaV->Draw("same");
  TLegend* leg5 = new TLegend(0.60, 0.60, 0.85, 0.80);
  leg5->AddEntry(hSigmaU, "SMD-u", "L");
  leg5->AddEntry(hSigmaV, "SMD-v", "L");
  leg5->Draw();
  gPad->Print("hSigma.png");

  c1->cd(2);
  hNhitsU->SetLineColor(kRed);
  hNhitsV->SetLineColor(kBlue);
  hNhitsU->SetLineWidth(2);
  hNhitsV->SetLineWidth(2);
  hNhitsU->Draw();
  hNhitsV->Draw("same");
  TLegend* leg6 = new TLegend(0.60, 0.60, 0.85, 0.80);
  leg6->AddEntry(hNhitsU, "SMD-u", "L");
  leg6->AddEntry(hNhitsV, "SMD-v", "L");
  leg6->Draw();
  gPad->Print("hNhits.png");
}

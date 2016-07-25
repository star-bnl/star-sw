void Occup(const Char_t *files= "/star/data09/calib/fisyak/Pass109/TpcOnly/020/Event*.root",const Char_t *tag = "Tb") {
  Int_t NB = 4;
  Int_t NL[4] = { 8, 12, 16, 20};
  Int_t NW[4] = { 4,  6,  7, 16};
  static Double_t Du[2] = {3.000, 3.65};
  //  static Double_t Dv[2] = {6.305, 4.35};
  static Double_t Dv[2] = {3.000, 2.00};
#if 0
  TTree *T = (TTree *) gDirectory->Get("T");
  T->Draw("fEvtHdr.fEvtNum>>NoEvents");
  TH1 * NoEvents = (TH1 *) gDirectory->Get("NoEvents");
#endif
  TString fOutFileName(gSystem->DirName(files));
  fOutFileName += tag; fOutFileName += ".root";
  TFile *fOut = new TFile(fOutFileName,"recreate");
  TString Name, Title;
  TH1D* primZ = new TH1D("PrimZ","Distribution of primary Z",200,-200,200);
  TH1D* primZC = new TH1D("PrimZC","Distribution of primary Z after cut",200,-200,200);
  TH1D* hitPerTrackH = new TH1D("hitPerTrack","Hit per track",200,0,100);
  TH2F *hist[4][7];
  for (Int_t Barrel = 1; Barrel <= NB; Barrel++) {
    Name = Form("B%i",Barrel);
    Title = Form("tracks*hits wafer versus ladder for Barrel %i",Barrel);
    hist[Barrel-1][0] = new TH2F(Name,Title,NL[Barrel-1],1,NL[Barrel-1]+1,NW[Barrel-1],1,NW[Barrel-1]+1);
    Name = Form("B%iH",Barrel);
    Title = Form("hits wafer versus ladder for Barrel %i",Barrel);
    hist[Barrel-1][1] = new TH2F(Name,Title,NL[Barrel-1],1,NL[Barrel-1]+1,NW[Barrel-1],1,NW[Barrel-1]+1);
    Name = Form("B%iT",Barrel);
    Title = Form("tracks wafer versus ladder for Barrel %i",Barrel);
    hist[Barrel-1][2] = new TH2F(Name,Title,NL[Barrel-1],1,NL[Barrel-1]+1,NW[Barrel-1],1,NW[Barrel-1]+1);
    Name = Form("B%iHT",Barrel);
    Title = Form("no. hits versus no.tracks per wafer for Barrel %i",Barrel);
    hist[Barrel-1][3] = new TH2F(Name,Title,100,0,100,100,0,100);
    Name = Form("B%iTempHit",Barrel);
    Title = Form("tracks*hits wafer versus ladder for Barrel %i per event",Barrel);
    hist[Barrel-1][4] = new TH2F(Name,Title,NL[Barrel-1],1,NL[Barrel-1]+1,NW[Barrel-1],1,NW[Barrel-1]+1);
    Name = Form("B%iTempTrack",Barrel);
    Title = Form("tracks wafer versus ladder for Barrel %i per event",Barrel);
    hist[Barrel-1][5] = new TH2F(Name,Title,NL[Barrel-1],1,NL[Barrel-1]+1,NW[Barrel-1],1,NW[Barrel-1]+1);
    Name = Form("B%iHpT",Barrel);
    Title = Form("hits per tracks wafer versus ladder for Barrel %i per event",Barrel);
    hist[Barrel-1][6] = new TH2F(Name,Title,NL[Barrel-1],1,NL[Barrel-1]+1,NW[Barrel-1],1,NW[Barrel-1]+1);
  }
  TDirIter Dir(files);
  Char_t *file = 0;
  Int_t NFiles = 0;
  TTreeIter iter("T");
  while ((file = (Char_t *) Dir.NextFile())) {
    iter.AddFile(file); 
  }
  const UInt_t          &fNPTracks = iter("fNPTracks");
  const UInt_t          &fNtrack   = iter("fNtrack");
  const UInt_t          &fNhit     = iter("fNhit");
  const UInt_t          &fFlag     = iter("fFlag");
  const Float_t       *&fVertex   = iter("fVertex[3]");
  
  const Int_t           &fEvtHdr_fEvtNum = iter("fEvtHdr.fEvtNum");
  const Int_t           &fEvtHdr_fRun    = iter("fEvtHdr.fRun");
  const Int_t           &fEvtHdr_fDate   = iter("fEvtHdr.fDate");
  const Float_t        &fEvtHdr_fField  = iter("fEvtHdr.fField");
  const Int_t          *&Id     = iter("fHits.Id");
  const Int_t          *&sector = iter("fHits.sector");
  const Int_t          *&barrel = iter("fHits.barrel");
  const Int_t          *&layer  = iter("fHits.layer");
  const Int_t          *&ladder = iter("fHits.ladder");
  const Int_t          *&wafer  = iter("fHits.wafer");
  const Int_t          *&NoHitPerTrack = iter("fHits.NoHitPerTrack");
  const Float_t     *&u      = iter("fHits.u");
  const Float_t     *&v      = iter("fHits.v");
  const Float_t     *&uP     = iter("fHits.uP");
  const Float_t     *&vP     = iter("fHits.vP");
  const UInt_t     **&IdHit  = iter("fTracks.fIdHitT[1000]");
  const UInt_t      *&Nsp    = iter("fTracks.fNsp");
  Int_t NoEvts = 0;
  while (iter.Next()) {
    //    cout << "next event" << endl;
#if 0

    for (Int_t trk = 0; trk < fNPTracks; trk++) {
      for (Int_t hit = 0; hit < Nsp[trk]; hit++) {
	Int_t k = IdHit[trk][hit]-1;
	cout << Id[k] << "\t" << sector[k] << "\t" << barrel[k] << "\t" << layer[k] << "\t" << ladder[k] << "\t" << wafer[k] << "\t" << NoHitPerTrack[k] << endl;
      }
    }
#endif
    Int_t IdOld = 0;
    Int_t NoTracks = 0;
    Int_t NoTackHits = 0;
    Double_t Z = fVertex[2];
    primZ->Fill(Z);
    if (TMath::Abs(Z) > 10) continue;
    primZC->Fill(Z);
    NoEvts++;
    for (Int_t k = 0; k < fNhit; k++) {
      //      cout << Id[k] << "\t" << sector[k] << "\t" << barrel[k] << "\t" << layer[k] << "\t" << ladder[k] << "\t" << wafer[k] << "\t" << NoHitPerTrack[k] << endl;
      Int_t b = barrel[k]-1;
      if (b < 0) continue;
      Int_t l = 0;
      if (barrel[k] == 4) l = 1;
      if (TMath::Abs(uP[k]) > Du[l] || TMath::Abs(vP[k]) > Dv[l]) continue; 
      //if (TMath::Abs(u[k]-uP[k]) > 1 || TMath::Abs(v[k]-vP[k]) > 1) continue;
      Double_t lad = ladder[k]+0.5;
      Double_t waf = wafer[k] + 0.5;
      hist[b][0]->Fill( lad, waf);
      hist[b][4]->Fill( lad, waf);
      if (NoHitPerTrack[k] == 1) hist[b][5]->Fill( lad, waf);
    }
    for (Int_t b = 0; b <NB; b++) {
      Int_t nx = hist[b][4]->GetNbinsX();
      Int_t ny = hist[b][4]->GetNbinsY();
      for (int i=1;i<=nx;i++){
	for (int j=1;j<=ny;j++){
	  Int_t bin = hist[b][4]->GetBin(i,j,0);
	  Double_t trackhit = hist[b][4]->GetBinContent(bin);
	  Double_t track = hist[b][5]->GetBinContent(bin);
	  if (track <= 0) continue;
	  Double_t hit = trackhit/track;
	  if (hit < 1) {
	    cout << "i " << i << " j " << j << " trackhit " << trackhit << " track " << track << " hit " << hit << endl;
	    continue;
	  }
	  hitPerTrackH->Fill(hit/track);
	  if (hit/track > 10) continue;
	  hist[b][1]->Fill(i+0.5,j+0.5,hit);
	  hist[b][2]->Fill(i+0.5,j+0.5,track);
	  hist[b][3]->Fill(track,hit);
	  hist[b][6]->Fill(i+0.5,j+0.5,hit/track);
	}
      }
      hist[b][4]->Reset();
      hist[b][5]->Reset();
    }
  }
  fOut->Write();
  //________________________________________________________________________________
  NoEvts =  PrimZC->GetEntries();
  for (Int_t b = 1; b <= 4; b++) {
    Name = Form("B%i",b);
    //    TCanvas *c1 = new TCanvas(Name,Name);
    TH2 *h2 = (TH2* )gDirectory->Get(Form("B%i",b));
    Name += "H";
    TH2 *h2h = (TH2* )gDirectory->Get(Form("B%iH",b)); if (! h2h)  {cout << "Cannot find histogram " << Form("B%iH",b) << endl; return;}
    TH2 *h2t = (TH2* )gDirectory->Get(Form("B%iT",b)); if (! h2t)  {cout << "Cannot find histogram " << Form("B%iT",b) << endl; return;}
    Double_t s = 2*Du[0]*2*Dv[0];
    if (b == 4) s = 2*Du[1]*2*Dv[1];
    Double_t scale = 1./s;//NoEvts/s;
//     h2->Scale(scale);
//     h2->Draw("text");
    Int_t nx = h2->GetNbinsX();
    Int_t ny = h2->GetNbinsY();
    Double_t occup = 0;
    Int_t entries = 0;
    Double_t hitPerTrack = 0;
    Double_t trackPerWafer = 0;
    Double_t hitPerWafer = 0;
    Double_t hitTrackPerWafer = 0;
    for (int i=1;i<=nx;i++){
      for (int j=1;j<=ny;j++){
	Int_t bin = h2->GetBin(i,j,0);
	Double_t cont = h2h->GetBinContent(bin);
	Double_t track = h2t->GetBinContent(bin);
	if (cont > 0 && track> 0) {
#if 0
	  cout << "b = " << b << "\tladder = " << i << "\twafer = " << j;
	  cout << "\tcont = " << cont << "\ttrak = " << track << "\tscale = " << scale;
	  cout << "\t" << cont/track*scale << endl;
#endif
	  hitTrackPerWafer += h2->GetBinContent(bin);
	  hitTrackPerWafer += track;
	  hitPerWafer += cont;
	  hitPerTrack += cont/track;
	  occup += cont*scale;
	  trackPerWafer += track;
	  entries++;
	}
      }
    }
    if (entries > 0) {
      occup /= entries*NoEvts;
      hitPerTrack /= entries;
      hitPerWafer /= entries*NoEvts;
      trackPerWafer /= entries*NoEvts;
      hitTrackPerWafer /= entries*NoEvts;
      cout << "B " << b << "\tNo.Alive Wafers = " << entries 
	   << "\tTracks/Wafer " << trackPerWafer << "\tHit/Wafer = " << hitPerWafer
	   << "\tHitTrackComb/Wafer =" << hitTrackPerWafer
	   << "\tHit/Track = " << hitPerTrack << "\tOccupancy = " << Form("%9.3f",occup) << " [hit/cm**2]" << endl; 
    }
  }
  
}

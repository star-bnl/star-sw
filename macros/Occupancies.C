void Occupancies() {
  Int_t NL[4] = { 8, 12, 16, 20};
  Int_t NW[4] = { 4,  6,  7, 16};
  static Double_t Du[2] = {3.000, 3.65};
  static Double_t Dv[2] = {6.305, 4.35};
  TTree *T = (TTree *) gDirectory->Get("T");
  T->Draw("fEvtHdr.fEvtNum>>NoEvents");
  TH1 * NoEvents = (TH1 *) gDirectory->Get("NoEvents");
  Double_t NoEvts =  NoEvents->GetEntries();
  TString Plot;
  TString Cut;
  TString Name;
  for (Int_t barrel = 1; barrel <= 4; barrel++) {
    Name = Form("B%i",barrel);
    TCanvas *c1 = new TCanvas(Name,Name);
    Plot = "wafer:ladder >> "; Plot += Name;
    Plot += Form("(%i,1,%i,%i,1,%i)",NL[barrel-1],NL[barrel-1]+1,NW[barrel-1],NW[barrel-1]+1);
    Cut = Form("barrel == %i",barrel);
    T->Draw(Plot,Cut,"colz");
    TH2 *h2 = (TH2* )gDirectory->Get(Name);
    Name += "H";
    Plot = "wafer:ladder >> "; Plot += Name;
    Plot += Form("(%i,1,%i,%i,1,%i)",NL[barrel-1],NL[barrel-1]+1,NW[barrel-1],NW[barrel-1]+1);
    Cut += "&&fHits.NoHitPerTrack==1";
    T->Draw(Plot,Cut,"colz");
    TH2 *h2h = (TH2* )gDirectory->Get(Name);
    if (! h2h) {cout << "Cannot find histogram " << Name.Data() << endl; return;}
    Double_t s = Du[0]*Dv[0];
    if (barrel == 4) s = Du[1]*Dv[1];
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
	Double_t cont = h2->GetBinContent(bin);
	Double_t track = h2h->GetBinContent(bin);
	if (cont > 0 && track> 0) {
#if 0
	  cout << "barrel = " << barrel << "\tladder = " << i << "\twafer = " << j;
	  cout << "\tcont = " << cont << "\ttrak = " << track << "\tscale = " << scale;
	  cout << "\t" << cont/track*scale << endl;
#endif
	  hitTrackPerWafer += cont/NoEvts;
	  hitPerWafer += cont/(track/NoEvts);
	  hitPerTrack += cont/track/(track/NoEvts);
	  occup += cont/track*scale; entries++;
	  trackPerWafer += track;
	}
      }
    }
    if (entries > 0) {
      occup /= entries;
      hitPerTrack /= entries;
      hitPerWafer /= entries*NoEvts;
      trackPerWafer /= entries*NoEvts;
      hitTrackPerWafer /= entries;
      cout << "Barrel " << barrel << "\tNo.Alive Wafers = " << entries 
	   << "\tTracks/Wafer " << trackPerWafer << "\tHit/Wafer = " << hitPerWafer
	   << "\tHitTrackComb/Wafer =" << hitTrackPerWafer
	   << "\tHit/Track = " << hitPerTrack << "\tOccupancy = " << Form("%9.3f",occup) << " [hit/cm**2]" << endl; 
    }
  }
}

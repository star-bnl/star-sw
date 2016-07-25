// to run it use 
// root.exe 'bfc.C(0,"in,StEvent,nodefault","/star/data07/calib/fisyak/SvtSsdAlignment/Pass106/TpcOnly/020/st_physics_5082018_raw_2010010.event.root")' 'OccupEv.C(200)'
#define NEWEVENT
#define kUndefinedMethodIdentifier           0
#define kTruncatedMeanIdentifier             1
#define kEnsembleTruncatedMeanIdentifier     2
#define kLikelihoodFitIdentifier             3
#define kWeightedTruncatedMeanIdentifier     4
#define kOtherMethodIdentifier               5
// enum StDetectorId {kUnknownId   = kUnknownIdentifier,
//                    kTpcId       = kTpcIdentifier,
//                    kSvtId       = kSvtIdentifier,
//                    kRichId      = kRichIdentifier,
//                    kFtpcWestId  = kFtpcWestIdentifier,
//                    kFtpcEastId  = kFtpcEastIdentifier,
//                    kTofId       = kTofIdentifier,
//                    kCtbId       = kCtbIdentifier,
//                    kSsdId       = kSsdIdentifier,
//                    kBarrelEmcTowerId     = kBarrelEmcTowerIdentifier,
//                    kBarrelEmcPreShowerId = kBarrelEmcPreShowerIdentifier,
//                    kBarrelSmdEtaStripId  = kBarrelSmdEtaStripIdentifier,
//                    kBarrelSmdPhiStripId  = kBarrelSmdPhiStripIdentifier,
//                    kEndcapEmcTowerId     = kEndcapEmcTowerIdentifier,
//                    kEndcapEmcPreShowerId = kEndcapEmcPreShowerIdentifier,
//                    kEndcapSmdUStripId    = kEndcapSmdUStripIdentifier,
//                    kEndcapSmdVStripId    = kEndcapSmdVStripIdentifier,
//                    kZdcWestId   = kZdcWestIdentifier,
//                    kZdcEastId   = kZdcEastIdentifier,
//                    kMwpcWestId  = kMwpcWestIdentifier,
//                    kMwpcEastId  = kMwpcEastIdentifier,
//                    kTpcSsdId    = kTpcSsdIdentifier,
//                    kTpcSvtId    = kTpcSvtIdentifier,
//                    kTpcSsdSvtId = kTpcSsdSvtIdentifier,
//                    kSsdSvtId    = kSsdSvtIdentifier,
//                    kPhmdCpvId   = kPhmdCpvIdentifier,
//                    kPhmdId      = kPhmdIdentifier};

// enum StDedxMethod {
//   kUndefinedMethodId         = kUndefinedMethodIdentifier,
//   kTruncatedMeanId           = kTruncatedMeanIdentifier,
//   kEnsembleTruncatedMeanId   = kEnsembleTruncatedMeanIdentifier,
//   kLikelihoodFitId           = kLikelihoodFitIdentifier,
//   kWeightedTruncatedMeanId   = kWeightedTruncatedMeanIdentifier,
//   kOtherMethodId             = kOtherMethodIdentifier
// };
enum StTrackType {global, primary, tpt, secondary, estGlobal, estPrimary};
static const Int_t NB = 4;
static const Int_t NL[4] = { 8, 12, 16, 20};
static const Int_t NW[4] = { 4,  6,  7, 16};
static const Double_t Du[2] = {3.000, 3.65};
static const Double_t Dv[2] = {3.000, 2.00};
static TH1D* primZ = 0;
static TH1D* primZC = 0;
static TH2F *hist[4];
Char_t *trackType[] = {"global", "primary", "tpt", "secondary", "estGlobal", "estPrimary"};
//________________________________________________________________________________  
void CalcOccup() {
  if (! primZC) primZC = (TH1D*) gDirectory->Get("PrimZC");
  if (! primZC) return;
  Int_t NoEvts =  primZC->GetEntries();
  TString Name;
  for (Int_t b = 1; b <= 4; b++) {
    Name = Form("B%i",b);
    //    TCanvas *c1 = new TCanvas(Name,Name);
    TH2 *h2 = (TH2* )gDirectory->Get(Form("B%i",b));
    if (! h2) continue;
    Double_t s = 2*Du[0]*2*Dv[0];
    if (b == 4) s = 2*Du[1]*2*Dv[1];
    Double_t scale = 1./s;//NoEvts/s;
//     h2->Scale(scale);
//     h2->Draw("text");
    Int_t nx = h2->GetNbinsX();
    Int_t ny = h2->GetNbinsY();
    Double_t occup = 0;
    Int_t entries = 0;
    Double_t hitPerWafer = 0;
    TH1F *occuph = new TH1F(Form("Occup%i",b),Form("Occupancy distribution for barrel %i"),400,0,4);
    for (int i=1;i<=nx;i++){
      for (int j=1;j<=ny;j++){
	Int_t bin = h2->GetBin(i,j,0);
	Double_t cont = h2->GetBinContent(bin);
	if (cont > 0) {
#if 0
	  cout << "b = " << b << "\tladder = " << i << "\twafer = " << j;
	  cout << "\tcont = " << cont << "\ttrak = " << track << "\tscale = " << scale;
	  cout << "\t" << cont/track*scale << endl;
#endif
	  hitPerWafer += cont;
	  Double_t oc = cont*scale/NoEvts;
	  occup += oc;
	  occuph->Fill(oc);
	  entries++;
	}
      }
    }
    if (entries > 0) {
      occup /= entries;
      hitPerWafer /= entries*NoEvts;
      cout << "B " << b << "\tNo.Alive Wafers = " << entries 
	   << "\tHit/Wafer = " << hitPerWafer
	   << "\tOccupancy = " << Form("%9.3f",occup) << " [hit/cm**2]" << endl; 
    }
  }
}
//________________________________________________________________________________
void PlotSvtHits() {
  if (! primZ) {
    primZ = new TH1D("PrimZ","Distribution of primary Z",200,-200,200);
    primZC = new TH1D("PrimZC","Distribution of primary Z after cut",200,-200,200);
    TString Name, Title;
    for (Int_t Barrel = 1; Barrel <= NB; Barrel++) {
      Name = Form("B%i",Barrel);
      Title = Form("hits wafer versus ladder for Barrel %i",Barrel);
      hist[Barrel-1] = new TH2F(Name,Title,NL[Barrel-1],1,NL[Barrel-1]+1,NW[Barrel-1],1,NW[Barrel-1]+1);
    }
  }
  Int_t i,j,k,l;
  Double_t zPrim = 0;
  StEvent* pEvent = ((StChain *)StMaker::GetChain())->GetInputDS("StEvent");
  if (!pEvent) return;
  StPrimaryVertex *primaryVertex = pEvent->primaryVertex();
  if (!  primaryVertex) return;
  StThreeVectorF &primXYZ = primaryVertex->position();
  cout << "primaryVertex \t" << primXYZ.x() << "\t" << primXYZ.y() << "\t" << primXYZ.z() << endl;
  primZ->Fill(primXYZ.z());
  if (TMath::Abs(primXYZ.z()) > 10) continue;
  primZC->Fill(primXYZ.z());
  Int_t TotalNoOfSvtHits = 0;
  StSvtHitCollection* SvtHitCollection = pEvent->svtHitCollection();
  if (! SvtHitCollection) { cout << "No SVT Hit Collection" << endl; return;}
  UInt_t numberOfBarrels = SvtHitCollection->numberOfBarrels();
  Int_t vers = gClassTable->GetID("StSvtHit");
  for ( i = 0; i< numberOfBarrels; i++) {
    StSvtBarrelHitCollection* barrelCollection = SvtHitCollection->barrel(i);
    if (barrelCollection) {
      Int_t numberOfLadders = barrelCollection->numberOfLadders();
      Int_t noHits = 0;
      for (j = 0; j< numberOfLadders; j++) {
	StSvtLadderHitCollection *ladderCollection = barrelCollection->ladder(j);
	if (ladderCollection) {
	  Int_t numberOfWafers = ladderCollection->numberOfWafers();
	  for (k = 0; k < numberOfWafers; k++) {
	    StSvtWaferHitCollection* waferCollection = ladderCollection->wafer(k);
	    StSPtrVecSvtHit &hits = waferCollection->hits();
	    UInt_t NoHits = hits.size();
	    for (l = 0; l < NoHits; l++) {
	      StSvtHit *hit = hits[l];
	      if (hit) {
		//		cout << *((StHit *) hit) << endl;
		TotalNoOfSvtHits++;
#if 0
		StThreeVectorF &P = hit->position();
		printf("b:%2i l:%2i w:%2i h:%5i",i+1,j+1,k+1,l+1);
		printf(" x: %8.3f y: %8.3f z: %8.3f ", P.x(), P.y(), P.z());
		printf("b:%2i l:%2i d: %2i w:%2i H:%2i P:%8.3f F %3i U %1i",
		       hit->barrel(), hit->layer(),hit->ladder(), hit->wafer(), hit->hybrid(), hit->peakADC(), hit->flag(), hit->usedInFit());
		printf(" Id: %4i Q: %4i",hit->idTruth(), hit->qaTruth());
		printf("\n");
#endif
		if (hit->flag()>=4) continue;
		if (hit->flag()< 0) continue;
		Int_t lad = j+1;
		Int_t waf = k+1;
		hist[hit->barrel()-1]->Fill(hit->ladder(), hit->wafer());
	      }
	    }
	  }
	}
      }
    }
  }
  cout << "Total no. of Svt Hits " <<   TotalNoOfSvtHits << endl;
}
//________________________________________________________________________________
void OccupEv(Int_t triggerId = 15105, Int_t NenvMx=0) {
  Int_t ev = 0;
  TFile *f = new TFile("OccupEv.root","recreate");
  while (! ((StChain *)StMaker::GetChain())->MakeEvent() && (NenvMx == 0 || ev < NenvMx)) {
    StEvent* pEvent = ((StChain *)StMaker::GetChain())->GetInputDS("StEvent");
    if (!pEvent) break;
    ev++;
    const StTriggerIdCollection*      colid = pEvent->triggerIdCollection();
    printf ("event no. %i\n",ev);
    if (! colid->nominal()->isTrigger(triggerId)) continue;
    f->cd();
    PlotSvtHits();
  }
  f->Write();
  CalcOccup();
}

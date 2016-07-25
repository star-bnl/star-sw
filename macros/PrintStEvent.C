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
Char_t *trackType[] = {"global", "primary", "tpt", "secondary", "estGlobal", "estPrimary"};
//________________________________________________________________________________
void PrintStEvent(Int_t k = 0) {
  StEvent* pEvent = (StEvent*) chain->GetInputDS("StEvent");
  if (!pEvent) return;
  cout << "Event: Run "<< pEvent->runId() << " Event No: " << pEvent->id() << endl;
  cout << "Vertex Positions" << endl;
  const StPrimaryVertex *pvertex = pEvent->primaryVertex();
  if (pvertex ) {
    StThreeVectorF &position = pvertex->position();
    cout << "Event: Vertex Position " 
	 << "\t" << position.x()
	 << "\t" << position.y()
	 << "\t" << position.z()
	 << endl;
  }
  else {
    cout << "Event: Vertex Not Found" << endl;
  }
  
  StSPtrVecTrackNode& trackNode = pEvent->trackNodes();
  UInt_t nTracks = trackNode.size();
  StTrackNode *node = 0;
  Int_t line = 0;
  for (unsigned int i=0; i < nTracks; i++) {
    node = trackNode[i]; if (!node) continue;
    StGlobalTrack* gTrack = static_cast<StGlobalTrack*>(node->track(global));
    StDcaGeometry* dca    = gTrack->dcaGeometry();
    StPrimaryTrack *pTrack = 	static_cast<StPrimaryTrack*>(node->track(primary));
    for (int l = 0; l < 2; l++) {
      StTrack        *track = 0;
      if (k%10 > 0 && k%10 != l+1) continue;
      if (l == global)  track = gTrack;
      if (l == primary) track = pTrack;
      if (track) {
	if (! track->detectorInfo()) {cout << "detectorInfo is missing for track " << l << " ==================" << endl;}
#if 1
	if (dca && l == global) {
	  if (! line) {
	    cout << "track# type   flag       z     mom      pT     eta     phi  c   imp  +/-            z    +/-           rho    "
		 << "NPP length    DCA    NFP   chi2   NP  hit  X        Y        Z" << endl;
	    line++;
	  }
	  Double_t eta = - TMath::Log(TMath::Tan((TMath::Pi()/2-dca->dipAngle())/2));
	  cout << Form("%4d%10s%4d%8.3f%8.3f%8.3f%8.3f%8.2f",
		       i,trackType[l],track->flag(),dca->z(),dca->momentum().mag(),dca->pt(),eta,180./TMath::Pi()*dca->psi());
	  Short_t charge = track->geometry()->charge();
	  const Float_t *errMx =  dca->errMatrix();
	  Double_t sigmaX = TMath::Sqrt(errMx[0]);
	  Double_t sigmaZ = TMath::Sqrt(errMx[2]);
	  Double_t rho = errMx[1]/(sigmaX*sigmaZ);
	  cout << Form(" %2d %8.3f+-%8.3f",charge,dca->impact(), sigmaX);
	  cout << Form(" %8.3f+-%8.3f %8.3f",dca->z(), sigmaZ, rho);
	  Double_t length = track->length();
	  if (length > 9999.) length = 9999.;
	  cout << Form(" %4d%8.3f%8.3f", track->numberOfPossiblePoints(),length,track->impactParameter());
	  cout << Form(" %4d%8.3f%4d",track->fitTraits().numberOfFitPoints(), track->fitTraits().chi2(), track->detectorInfo()->numberOfPoints());
	} else 
#endif
	  {
  //                      1234567890123456781234567812345678 12345678 12345678 12345678   123 12345678 12345678 123 12345678 
	  if (! line) {
	    cout << "track# type   flag       z     mom     pT     eta     phi  c      pX      pY      pZ  "
		 << "Max  length     dca  NFP    chi2 NP BEMC" << endl; //FhitXYZ" << endl;
	    line++;
	  }
	  StSPtrVecTrackPidTraits &traits = track->pidTraits();
	  unsigned int size = traits.size();
	  Short_t charge = track->geometry()->charge();
	  StThreeVectorD g3 = track->geometry()->momentum(); // p of global track
	  //      if (TMath::Abs(g3.pseudoRapidity()) > 1) continue;
	  //	cout << "track #" << i << " type #" << trackType[l]  << "\tflag\t" << track->flag() 
	  //	     << " mom: " << g3.mag() << "\tpT:" << g3.perp() << "\ttraits.size " << size << endl;
	  cout << Form("%4d%10s%4d%8.3f%8.3f%8.3f%8.3f%8.2f",
		       i,trackType[l],track->flag(),track->geometry()->origin().z(),
		       g3.mag(),g3.perp(),g3.pseudoRapidity(),180/TMath::Pi()*g3.phi());
	  cout << Form(" %2d%8.3f%8.3f%8.3f",charge,g3.x(),g3.y(),g3.z());
	  Double_t length = track->length();
	  if (length > 9999.) length = 9999.;
	  cout << Form(" %4d%8.3f%8.3f", track->numberOfPossiblePoints(),length,track->impactParameter());
	  cout << Form(" %4d%8.3f%4d",track->fitTraits().numberOfFitPoints(), track->fitTraits().chi2(), track->detectorInfo()->numberOfPoints());
	  if (track->vertex())
	    cout << Form(" B%3i",((StPrimaryVertex *)track->vertex())->numMatchesWithBEMC());
	}
#if 0
	//      if (k/10 == 0) continue;
      StSPtrVecTrackPidTraits &traits = track->pidTraits();
      StDedxPidTraits *pid, *pid70 = 0, *pidF = 0;
      StProbPidTraits *pidprob = 0;
      Double_t I70 = 0, D70 = 0;
      Double_t chisq = 1e10, fitZ = 0, fitdZ = 1e10;
      Int_t N70 = 0, NF = 0;
      Double_t TrackLength70 = 0, TrackLength = 0;
      //      cout << Form(" %3i",traits.size());
      StTrackDetectorInfo*    dInfo = track->detectorInfo();
      StPtrVecHit hits =  dInfo->hits();
      StHit *hit = hits[0];
      if (hit) cout << Form("%8.3f %8.3f %8.3f",hit->position().x(),hit->position().y(),hit->position().z());
#endif	
#if 0
      for (unsigned int j = 0; j < traits.size(); j++) {
	if (! traits[j]) continue;
	//	      if ( traits[j]->IsZombie()) continue;
	pid = dynamic_cast<StDedxPidTraits*>(traits[j]); //pid->Print(""); 
	if (pid) {
	  if (pid->method() == kTruncatedMeanIdentifier) {
	    pid70 = pid; I70 = pid70->mean(); N70 = pid70->numberOfPoints();
	    TrackLength70 = pid70->length(); D70 = pid70->errorOnMean();
	    //	    cout << "I70 " << I70 << " N70 " << N70 << " TrackLength70 " << TrackLength70 << " D70 " << D70 << endl;
	    cout << Form(" %3i",N70);
	  }
	  if (pid->method() == kLikelihoodFitIdentifier) {
	    pidF = pid;
	    fitZ = TMath::Log(pidF->mean()); NF = pidF->numberOfPoints(); 
	    TrackLength = pidF->length(); fitdZ = pidF->errorOnMean(); 
	    //	    cout << "fitZ " << fitZ << " NF " << NF << " TrackLength " << TrackLength << " fitdZ " << fitdZ << endl;
	  }
	}
	else {
	  pidprob = dynamic_cast<StProbPidTraits*>(traits[j]);
	  //	  if (pidprob) pidprob->Print("");
	}
      }
#endif
      cout << " Svt p/h/f" << track->numberOfPossiblePoints(kSvtId) 
	   << "/" << track->detectorInfo()->hits(kSvtId).size()
	   << "/" << track->fitTraits().numberOfFitPoints(kSvtId);
      // 	   << "/" << track->detectorInfo()->numberOfPoints(kSvtId)
      // 	   << "/" << track->fitTraits().numberOfFitPoints(kSvtId);
      cout << endl;
      }
    } // l
    //    if (i > 5) break;
  }  
}
//________________________________________________________________________________
void PrintTpcHits(Int_t sector = 0, Int_t row = 0, Bool_t plot = kFALSE) {
  struct BPoint_t {
    Float_t sector, row, x, y, z, q;
  };
  static const Char_t *vname = "sector:row:x:y:z:q";
  BPoint_t BPoint;
  static TNtuple *Nt = 0;
  if (plot && Nt == 0) Nt = new TNtuple("TpcHit","TpcHit",vname);
    
  StEvent* pEvent = (StEvent*) chain->GetInputDS("StEvent");
  if (!pEvent) { cout << "Can't find StEvent" << endl; return;}
  StSPtrVecTrackNode& trackNode = pEvent->trackNodes();
  Int_t TotalNoOfTpcHits = 0;
  StTpcHitCollection* TpcHitCollection = pEvent->tpcHitCollection();
  if (! TpcHitCollection) { cout << "No TPC Hit Collection" << endl; return;}
  UInt_t numberOfSectors = TpcHitCollection->numberOfSectors();
  for (UInt_t i = 0; i< numberOfSectors; i++) {
    if (sector == 0 || i+1 == sector) {
      StTpcSectorHitCollection* sectorCollection = TpcHitCollection->sector(i);
      if (sectorCollection) {
	Int_t numberOfPadrows = sectorCollection->numberOfPadrows();
	Int_t noHits = 0;
	for (int j = 0; j< numberOfPadrows; j++) {
	  if (row == 0 || j+1 == row) {
	    StTpcPadrowHitCollection *rowCollection = sectorCollection->padrow(j);
	    if (rowCollection) {
	      StSPtrVecTpcHit &hits = rowCollection->hits();
#if ROOT_VERSION_CODE < 334081
	      Long_t NoHits = hits.size();
	      TArrayL idxT(NoHits); Long_t *idx = idxT.GetArray();
#else
	      Long64_t NoHits = hits.size();
	      TArrayL64 idxT(NoHits); Long64_t *idx = idxT.GetArray();
#endif
	      TotalNoOfTpcHits += NoHits;
#if 0
	      cout << "Sector:" << i+1 << "\tRow:" << j+1 
		   << "\tNoHits = " << NoHits << "\tTotal = " << TotalNoOfTpcHits << endl;
#endif
	      TArrayD dT(NoHits);   Double_t *d = dT.GetArray();
	      for (Long64_t k = 0; k < NoHits; k++) {
		StTpcHit *tpcHit = static_cast<StTpcHit *> (hits[k]);
		StThreeVectorF& xyz = tpcHit->position();
		d[k] = xyz.z();
	      }
	      TMath::Sort(NoHits,d,idx,kFALSE);
	      for (Long64_t k = 0; k < NoHits; k++) {
		Int_t l = idx[k];
		StTpcHit *tpcHit = static_cast<StTpcHit *> (hits[l]);
		if (! tpcHit) continue;
		StThreeVectorF& xyz = tpcHit->position();
		if (TMath::Abs(xyz.z()) < 195) continue;
		//	    if (! tpcHit->idTruth()) continue;
		//	    if (tpcHit->padsInHit() < 5 && tpcHit->timeBucketsInHit() < 10) continue;
		// 	    if (tpcHit->maxPad()  - tpcHit->minPad()  + 1 == tpcHit->padsInHit() ||
		// 		tpcHit->maxTmbk() - tpcHit->minTmbk() + 1 == tpcHit->timeBucketsInHit()) continue;
		tpcHit->Print();
		if (Nt) {
		  BPoint.sector = i+1;
		  BPoint.row = j+1;
		  BPoint.x = xyz.x();
		  BPoint.y = xyz.y();
		  BPoint.z = xyz.z();
		  BPoint.q = 1.e6*tpcHit->charge();
		  Nt->Fill(&BPoint.sector);
		}
	      }
	    }
	  }
	}
      }
    }
    //    break;
  }
  cout << "TotalNoOfTpcHits = " << TotalNoOfTpcHits << endl;
}
void FindEvent(Int_t run =  6175008, Int_t event = 80347) {
}
void DrawHits() {
  TLegend *leg = new TLegend(0.1,0.7,0.3,0.9,"");
  TH1D *nPossible = (TH1D *) gDirectory->Get("nPossible"); 
  TH1D *nHits = (TH1D *) gDirectory->Get("nHits");
  TH1D *nFits = (TH1D *) gDirectory->Get("nFits");
  leg->AddEntry(nPossible,"Possible");
  nPossible->Draw(); 
  nHits->SetLineColor(2);
  nHits->Draw("same");
  leg->AddEntry(nHits,"Hits");
  nFits->SetLineColor(3);
  nFits->Draw("same");
  leg->AddEntry(nFits,"Fits");
  leg->Draw();
  
}
//________________________________________________________________________________
void PrintSvtHits() {
  Int_t i,j,k,l;
  Double_t zPrim = 0;
  StEvent* pEvent = (StEvent*) chain->GetInputDS("StEvent");
  if (!pEvent) return;
  //  if (pEvent->numberOfPrimaryVertices() != 1) return;
  StPrimaryVertex *primaryVertex = pEvent->primaryVertex();
  if ( primaryVertex) {
    StThreeVectorF &primXYZ = primaryVertex->position();
    //  cout << "primaryVertex " << primXYZ << endl;
    cout << "primaryVertex \t" << primXYZ.x() << "\t" << primXYZ.y() << "\t" << primXYZ.z() << endl;
  }
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
#if 0
		cout << Form("b:%2i l:%2i w:%2i h:%5i",i,j,k,l)
		     << P
		  //		     << *hit
		  //		     << Form(" x: %8.3f y: %8.3f z: %8.3f", P.x(), P.y(), P.z())
		     << Form("b:%2i l:%2i d: %2i w:%2i H:%2i P:%8.3f F %3i",hit->barrel(), hit->layer(), 
			     hit->ladder(), hit->wafer(), hit->hybrid(), hit->peakADC(), hit->flag())
		     << endl;
#else
		printf("b:%2i l:%2i w:%2i h:%5i",i+1,j+1,k+1,l+1);
		printf(" x: %8.3f y: %8.3f z: %8.3f ", P.x(), P.y(), P.z());
		printf("b:%2i l:%2i d: %2i w:%2i H:%2i P:%8.3f F %3i U %1i",
		       hit->barrel(), hit->layer(),hit->ladder(), hit->wafer(), hit->hybrid(), hit->peakADC(), hit->flag(), hit->usedInFit());
		printf(" Id: %4i Q: %4i",hit->idTruth(), hit->qaTruth());
#ifdef NEWEVENT
		if (vers > 1) printf(" Luv: %8.3f %8.3f anode %8.3f timeb %8.3f",hit->localPosition(0),hit->localPosition(1),hit->anode(),hit->timebucket());
#endif
		printf("\n");
#endif
#else
		hit->Print();
#endif
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
void PrintSsdHits() {
  Int_t i,j,k,l;
  Double_t zPrim = 0;
  StEvent* pEvent = (StEvent*) chain->GetInputDS("StEvent");
  if (!pEvent) return;
  //  if (pEvent->numberOfPrimaryVertices() != 1) return;
  StPrimaryVertex *primaryVertex = pEvent->primaryVertex();
  if ( primaryVertex) {
    StThreeVectorF &primXYZ = primaryVertex->position();
    //  cout << "primaryVertex " << primXYZ << endl;
    cout << "primaryVertex \t" << primXYZ.x() << "\t" << primXYZ.y() << "\t" << primXYZ.z() << endl;
  }
  Int_t TotalNoOfSsdHits = 0;
  StSsdHitCollection* SsdHitCollection = pEvent->ssdHitCollection();
  if (! SsdHitCollection) { cout << "No SSD Hit Collection" << endl; return;}
  UInt_t numberOfLadders = SsdHitCollection->numberOfLadders();
  Int_t vers = gClassTable->GetID("StSsdHit");
  for ( i = 0; i< numberOfLadders; i++) {
    StSsdLadderHitCollection* ladderCollection = SsdHitCollection->ladder(i);
    if (ladderCollection) {
      Int_t numberOfWafers = ladderCollection->numberOfWafers();
      for (k = 0; k < numberOfWafers; k++) {
	StSsdWaferHitCollection* waferCollection = ladderCollection->wafer(k);
	StSPtrVecSsdHit &hits = waferCollection->hits();
	UInt_t NoHits = hits.size();
	for (l = 0; l < NoHits; l++) {
	  StSsdHit *hit = hits[l];
	  if (hit) {
#if 0
	    //		cout << *((StHit *) hit) << endl;
	    StThreeVectorF &P = hit->position();
#if 0
	    cout << Form("l:%2i w:%2i",i,k)
		 << P
	      //		     << *hit
	      //		     << Form(" x: %8.3f y: %8.3f z: %8.3f", P.x(), P.y(), P.z())
		 << Form("l:%2i d: %2i w:%2i",
			 hit->ladder(), hit->wafer())
		 << endl;
#else
	    printf("l:%2i w:%2i",i+1,k+1);
	    printf(" x: %8.3f y: %8.3f z: %8.3f ", P.x(), P.y(), P.z());
	    printf("l:%2i w:%2i  F %3i U %1i",
		   hit->ladder(), hit->wafer(), hit->flag(), hit->usedInFit());
	    printf(" Id: %4i Q: %4i",hit->idTruth(), hit->qaTruth());
#ifdef NEWEVENT
	    if (vers > 1)    printf(" Luv: %8.3f %8.3f",hit->localPosition(0),hit->localPosition(1));
#endif
	    printf("\n");
#endif
#else
	    hit->Print("");
#endif
	  }
	}
      }
    }
  }
}
//________________________________________________________________________________
void PrintRnDHits() {
  Int_t i,j,k,l;
  Double_t zPrim = 0;
  StEvent* pEvent = (StEvent*) chain->GetInputDS("StEvent");
  if (!pEvent) return;
  //  if (pEvent->numberOfPrimaryVertices() != 1) return;
  StPrimaryVertex *primaryVertex = pEvent->primaryVertex();
  if ( primaryVertex) {
    StThreeVectorF &primXYZ = primaryVertex->position();
    //  cout << "primaryVertex " << primXYZ << endl;
    cout << "primaryVertex \t" << primXYZ.x() << "\t" << primXYZ.y() << "\t" << primXYZ.z() << endl;
  }
  Int_t TotalNoOfRnDHits = 0;
  StRnDHitCollection* RnDHitCollection = pEvent->rndHitCollection();
  if (! RnDHitCollection) { cout << "No RND Hit Collection" << endl; return;}
  StSPtrVecRnDHit &hits = RnDHitCollection->hits();
  UInt_t NoHits = hits.size();
  for (l = 0; l < NoHits; l++) {
    StRnDHit *hit = hits[l];
    if (hit) {
      //		cout << *((StHit *) hit) << endl;
      StThreeVectorF &P = hit->position();
      printf("l:%2i w:%2i",i+1,k+1);
      printf(" x: %8.3f y: %8.3f z: %8.3f ", P.x(), P.y(), P.z());
      printf("l:%2i w:%2i",
	     hit->ladder(), hit->wafer());
      printf(" Id: %4i Q: %4i",hit->idTruth(), hit->qaTruth());
      printf(" Flag: %4i Fit: %3i",hit->flag(), hit->usedInFit());
      printf("\n");
    }
  }
}
//________________________________________________________________________________
void PlotGlobalZ() {// root.exe 'bfc.C(1,"in,StEvent,analysis,nodefault","/star/rcf/test/dev/daq_sl302.ittf/Thu/year_2005/CuCu200_HighTower/st_physics_6054016_raw_1020005.event.root")'
  StEvent* pEvent = (StEvent*) chain->GetInputDS("StEvent");
  static const Int_t NHists = 8;
  struct HistDef_t {
    Char_t *name;
    Char_t *title;
    Int_t  order;
    Int_t  nxbins;
    Double_t xmin;
    Double_t xmax;
  };
  HistDef_t HistDef[8] = {
    {"Z","Z of track Impact distribution",0,1200,-300,300},
    {"Zk3","Z of track Impact of order 3",3,1200,-300,300},
    {"Zk5","Z of track Impact of order 5",5,1200,-300,300},
    {"Zk8","Z of track Impact of order 8",8,1200,-300,300},
    {"Zk16","Z of track Impact of order 16",16,1200,-300,300},
    {"Zk32","Z of track Impact of order 32",32,1200,-300,300},
    {"Zk64","Z of track Impact of order 16",64,1200,-300,300},
    {"Zk128","Z of track Impact of order 32",128,1200,-300,300},
  };
  static TH1* hists[8] = {0,0,0,0,0,0};
  if (!pEvent) return;
  for (Int_t k = 0; k < NHists; k++) {
    hists[k] = (TH1 *) gDirectory->Get(HistDef[k].name);
    if (hists[k]) hists[k]->Reset();
    else          {
      if (! HistDef[k].order) hists[k] = 
	new TH1F(HistDef[k].name, HistDef[k].title,HistDef[k].nxbins,HistDef[k].xmin,HistDef[k].xmax);
      else	             hists[k] = 
	new TH1K(HistDef[k].name, HistDef[k].title,HistDef[k].nxbins,HistDef[k].xmin,HistDef[k].xmax,HistDef[k].order);
    }
  }
  StSPtrVecTrackNode& nodes = pEvent->trackNodes();
  for (unsigned int l=0; l<nodes.size(); l++) {
    StGlobalTrack* g = ( StGlobalTrack*) nodes[l]->track(global);
    if (! g) continue;
    StDcaGeometry* gDCA = g->dcaGeometry();
    if (! gDCA) continue;
    Double_t z = gDCA->z();
    for (Int_t j = 0; j < NHists; j++) {
      hists[j]->Fill(z);
    }
  }
  Int_t npeaks = 10;
  TSpectrum *sp = new TSpectrum(2*npeaks);
  for (Int_t k = 0; k < NHists; k++) {
    Int_t nfound = sp->Search(hists[k]);
    cout << hists[k]->GetName() << "\tfound\t" << nfound << " peaks" << endl;
  }
}

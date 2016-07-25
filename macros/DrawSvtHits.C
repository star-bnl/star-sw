#define kUndefinedMethodIdentifier           0
#define kTruncatedMeanIdentifier             1
#define kEnsembleTruncatedMeanIdentifier     2
#define kLikelihoodFitIdentifier             3
#define kWeightedTruncatedMeanIdentifier     4
#define kOtherMethodIdentifier               5
//#define DEBUG
enum StDedxMethod {
  kUndefinedMethodId         = kUndefinedMethodIdentifier,
  kTruncatedMeanId           = kTruncatedMeanIdentifier,
  kEnsembleTruncatedMeanId   = kEnsembleTruncatedMeanIdentifier,
  kLikelihoodFitId           = kLikelihoodFitIdentifier,
  kWeightedTruncatedMeanId   = kWeightedTruncatedMeanIdentifier,
  kOtherMethodId             = kOtherMethodIdentifier
};
enum StTrackType {global, primary, tpt, secondary, estGlobal, estPrimary};
Char_t *trackType[] = {"global", "primary", "tpt", "secondary", "estGlobal", "estPrimary"};
TCanvas *c1 = 0;
const Int_t NG = 8;
Int_t   nk[8];
TGraph *gr[8];
const Char_t *Names[10] = {"All","NotUsed 1","NotUsed 2","NotUsed 3","Used 1","Used 2","Used 3","PrTrack"};
//Double_t SvtLayerRadia[6] = {6.37+0.025,7.38+0.025,10.38+0.025,11.27+0.025,14.19+0.025,15.13+0.025};
Double_t SvtLayerRadia[3] = {6.90, 10.85, 14.685};
//________________________________________________________________________________
Char_t *GetFileName() {
  static const Char_t *Unknown = "Unknown";
  TList *list = (TList *) gROOT->GetListOfFiles();
  TIter next(list);
  TNamed *o = 0;
  while ((o = (TNamed*) next())) {
    TString Name(o->GetName());
    if (Name.Contains(".event.root")) {
      cout << "File: " <<  o->GetName() << endl;
      return o->GetName();
    }
  }
  return Unknown;
}
//________________________________________________________________________________
void DrawSvtHits() {
  class Svt_t : public TObject {
  public:
    Svt_t(Int_t b, Int_t u,  Float_t x, Float_t y, Float_t z) : barrel(b), usedInFit(u), position(x,y,z) {
      Phi = TMath::ATan2(y,x); 
      CosTheta = z/TMath::Sqrt(x*x+y*y+z*z); //position.cosTheta();
#ifdef DEBUG
      cout << "new point b:" << barrel  << "\tu:" <<  usedInFit
	   << "\txyz\t" << position.x() << "\t" << position.y() << "\t" << position.z()
	   << "\tPhi " << Phi << "\tCosTheta " << CosTheta << endl;
#endif
    }
    ~Svt_t();
    Int_t  barrel;
    Int_t  usedInFit;
    StThreeVectorF position;
    Double_t Phi;
    Double_t CosTheta;
  };
  Int_t i,j,k,l;
  Double_t zPrim = 0;
  StEvent* pEvent = (StEvent*) chain->GetInputDS("StEvent");
  if (!pEvent) return;
  if (pEvent->numberOfPrimaryVertices() != 1) return;
  StPrimaryVertex *primaryVertex = pEvent->primaryVertex();
  if (! primaryVertex) return;
  StThreeVectorF &primXYZ = primaryVertex->position();
  //  cout << "primaryVertex " << primXYZ << endl;
  cout << "primaryVertex \t" << primXYZ.x() << "\t" << primXYZ.y() << "\t" << primXYZ.z() << endl;
  zPrim = primXYZ.z();
  //  if (TMath::Abs(primXYZ.z()) > 10) return;
  TObjArray *points = 0;
  if (points) {points->delete(); points = 0;}
  else        {points = new TObjArray(); points->SetOwner();}
  StSPtrVecTrackNode& trackNode = pEvent->trackNodes();
  UInt_t nTracks = trackNode.size();
  StTrackNode *node = 0;
  for (i=0; i < nTracks; i++) {
    node = trackNode[i]; if (!node) continue;
    //    StGlobalTrack* track = static_cast<StGlobalTrack*>(node->track(global));
    StPrimaryTrack *track = 	static_cast<StPrimaryTrack*>(node->track(primary));
    if (track) {
      StThreeVectorD momentum = track->geometry()->momentum();
      Double_t Phi = momentum.phi();
      Double_t cosTheta = momentum.cosTheta();
      Float_t xp = SvtLayerRadia[1]*TMath::Cos(Phi);
      Float_t yp = SvtLayerRadia[1]*TMath::Sin(Phi);
      Double_t tangTheta = TMath::Sqrt(1. - cosTheta*cosTheta)/cosTheta;
      Float_t zp = SvtLayerRadia[1]/tangTheta;
      if (TMath::Abs(zp + primXYZ.z()) < 25) 
	points->AddLast(new Svt_t(2,-1,xp,yp,zp));
    } 
  }
  Int_t TotalNoOfSvtHits = 0;
  StSvtHitCollection* SvtHitCollection = pEvent->svtHitCollection();
  if (! SvtHitCollection) { cout << "No SVT Hit Collection" << endl; return;}
  UInt_t numberOfBarrels = SvtHitCollection->numberOfBarrels();
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
#ifdef DEBUG
	    cout << "Barrel:" << i+1 << "\tLadder:" << j+1 << "\tWafer:" << k+1 
		 << "\tNoHits = " << NoHits << "\tTotal = " << TotalNoOfSvtHits << endl;
#endif
	    for (l = 0; l < NoHits; l++) {
	      StSvtHit *hit = hits[l];
	      if (hit && hit->flag() > 3) {
		//		cout << *((StHit *) hit) << endl;
		StThreeVectorF svthitPosition = hit->position();
		StThreeVectorF hitPosition = svthitPosition - primXYZ;
		points->AddLast(new Svt_t(hit->barrel(),hit->usedInFit(),hitPosition.x(),hitPosition.y(),hitPosition.z()));
	      }
	    }
	    TotalNoOfSvtHits += NoHits;
	  }
	}
      }
    }
  }
  Int_t N = points->GetEntriesFast();
  if (! N) return;
  if (!c1) c1 = new TCanvas();
  //  c1->SetFillColor(42);
  c1->SetGrid();
  for (j = 0; j < NG; j++) {gr[j] = new TGraph(); nk[j] = 0;}
  Double_t zmin = 99, zmax = -99, ymin = 99, ymax = -99;
  for (i = 0; i < N; i++) {
    Svt_t *h = (Svt_t *) points->UncheckedAt(i);
    if (! h) continue;
    Double_t tanTheta = TMath::Sqrt(1. - h->CosTheta*h->CosTheta)/h->CosTheta;
    Double_t z = SvtLayerRadia[1]/tanTheta + primXYZ.z();
    Double_t y = SvtLayerRadia[1]*h->Phi;
#ifdef DEBUG
    cout << "b:" << h->barrel << "\tu:" << h->usedInFit << "\tP:" << h->position
	 << "\tPhi:" << h->Phi << "\tcosT:" << h->CosTheta << "\ttanTheta:" << tanTheta
	 << "\tz:" << z << "\trho*phi:" << y << endl;
#endif
    if (z < zmin) zmin = z;
    if (z > zmax) zmax = z;
    if (y < ymin) ymin = y;
    if (y > ymax) ymax = y;
    if (h->usedInFit >= 0) {
      gr[0]->SetPoint(nk[0]++,z,y);
      k = h->barrel + 3*h->usedInFit;
    }
    else
      k = 7;
    gr[k]->SetPoint(nk[k]++,z,y);
  }
  TH1F *hist = c1->DrawFrame(zmin-0.01,ymin-0.01,zmax+0.01,ymax+0.01);
  //  c1->DrawFrame(-1,-TMath::Pi(),1,TMath::Pi());
  hist->SetTitle(Form("z of Primary Vertex = %7.2f from %s",zPrim,GetFileName()));
  hist->SetYTitle(Form("#rho#phi at %7.2f (cm)",SvtLayerRadia[1]));
  hist->SetXTitle(Form("Z_{SVT} at %7.2f (cm)",SvtLayerRadia[1]));
  TLegend *leg = new TLegend(0.91,0.11,1.00,0.89,"");
  for (k = 0; k < NG; k++) {
    if (gr[k]->GetN()>0) {
      //     gr[k]->SetMarkerSize(0.2);
      if (k > 0) {
	Int_t c = (k-1)%3+2;      
	Int_t st = 24 + (k-1)/3;
	gr[k]->SetMarkerColor(c);
	if (k < 7) 
	  gr[k]->SetMarkerStyle(st);
	else 
	  gr[k]->SetMarkerStyle(5);
      }
      gr[k]->Draw("P");
      leg->AddEntry(gr[k],Names[k],"p");
    }
  }
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
		StThreeVectorF &P = hit->position();
#if 0
		cout << Form("b:%2i l:%2i w:%2i h:%5i",i,j,k,l)
		     << P
		  //		     << *hit
		  //		     << Form(" x: %8.3f y: %8.3f z: %8.3f", P.x(), P.y(), P.z())
		     << Form("b:%2i l:%2i d: %2i w:%2i H:%2i P:%8.3f",hit->barrel(), hit->layer(), 
			     hit->ladder(), hit->wafer(), hit->hybrid(), hit->peakADC())
		     << endl;
#else
		printf("b:%2i l:%2i w:%2i h:%5i",i+1,j+1,k+1,l+1);
		printf(" x: %8.3f y: %8.3f z: %8.3f ", P.x(), P.y(), P.z());
		printf("b:%2i l:%2i d: %2i w:%2i H:%2i P:%8.3f",
		       hit->barrel(), hit->layer(),hit->ladder(), hit->wafer(), hit->hybrid(), hit->peakADC());
		printf(" Id: %4i Q: %4i",hit->idTruth(), hit->qaTruth());
		printf(" Luv: %8.3f %8.3f anode %8.3f timeb %8.3f",hit->localU(),hit->localV(),hit->anode(),hit->timeBucket());
		printf("\n");
#endif
	      }
	    }
	  }
	}
      }
    }
  }
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
	    printf("l:%2i w:%2i",
		   hit->ladder(), hit->wafer());
	    printf(" Id: %4i Q: %4i",hit->idTruth(), hit->qaTruth());
	    printf(" Luv: %8.3f %8.3f",hit->localU(),hit->localV());
	    printf("\n");
#endif
	  }
	}
      }
    }
  }
}

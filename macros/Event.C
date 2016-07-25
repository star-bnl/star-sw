#define kUndefinedMethodIdentifier           0
#define kTruncatedMeanIdentifier             1
#define kEnsembleTruncatedMeanIdentifier     2
#define kLikelihoodFitIdentifier             3
#define kWeightedTruncatedMeanIdentifier     4
#define kOtherMethodIdentifier               5
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
void Event(const Char_t *file = "/star/data19/reco/dAuMinBias/ReversedFullField/P03ih/2003/049/st_physics_4049029_raw_0040037.event.root") {// read and print StEvent
 if (gClassTable->GetID("TTable") < 0) {
    gSystem->Load("libGeom");
    gSystem->Load("libTable");
 }
  gROOT->LoadMacro("bfc.C");
  //  TString Chain("in dEdxY2 StEvent debug");
  TString Chain("in,StEvent,NoDefault");
  bfc(-1,Chain.Data(),file);
  chain->Init();
  chain->Make();
  StEvent* pEvent = (StEvent*) chain->GetInputDS("StEvent");
  if (!pEvent) return;
  StSPtrVecTrackNode& trackNode = pEvent->trackNodes();
  UInt_t nTracks = trackNode.size();
  for (unsigned int i=0; i < nTracks; i++) {
    node = trackNode[i]; if (!node) continue;
    StGlobalTrack* gTrack = static_cast<StGlobalTrack*>(node->track(global));
    //    if (gTrack && gTrack->flag() > 0) {
      //      StPtrVecHit hvec = gTrack->detectorInfo()->hits(kTpcId);
      //	if (! hvec.size()) continue;
      StPrimaryTrack *pTrack = 	static_cast<StPrimaryTrack*>(node->track(primary));
      StTptTrack     *tTrack =	static_cast<StTptTrack    *>(node->track(tpt));
      StTrack        *track;
      for (int l = 0; l < 3; l++) {
	if (l == 0) track = gTrack;
	if (l == 1) track = pTrack;
	if (l == 2) track = tTrack;
	if (track) {
	  cout << trackType[l] << " flag = " << track->flag() << endl;
	  StSPtrVecTrackPidTraits &traits = track->pidTraits();
	  unsigned int size = traits.size();
	  StThreeVectorD g3 = track->geometry()->momentum(); // p of global track
	  cout << "track #" << i << " type #" << trackType[l]  << " mom: " << g3.mag() << " traits.size " << size << endl;
	  StSPtrVecTrackPidTraits &traits = track->pidTraits();
	  StDedxPidTraits *pid, *pid70 = 0, *pidF = 0;
	  StProbPidTraits *pidprob = 0;
	  Double_t I70 = 0, D70 = 0;
	  Double_t chisq = 1e10, fitZ = 0, fitdZ = 1e10;
	  Int_t N70 = 0, NF = 0;
	  Double_t TrackLength70 = 0, TrackLength = 0;
	  for (unsigned int i = 0; i < traits.size(); i++) {
	    if (! traits[i]) continue;
	    //	      if ( traits[i]->IsZombie()) continue;
	    pid = dynamic_cast<StDedxPidTraits*>(traits[i]); pid->Print(""); 
	    if (pid) {
	      if (pid->method() == kTruncatedMeanIdentifier) {
		pid70 = pid; I70 = pid70->mean(); N70 = pid70->numberOfPoints();
		TrackLength70 = pid70->length(); D70 = pid70->errorOnMean();
		cout << "I70 " << I70 << " N70 " << N70 << " TrackLength70 " << TrackLength70 << " D70 " << D70 << endl;
	      }
	      if (pid->method() == kLikelihoodFitIdentifier) {
		pidF = pid;
		fitZ = TMath::Log(pidF->mean()); NF = pidF->numberOfPoints(); 
		TrackLength = pidF->length(); fitdZ = pidF->errorOnMean(); 
		  cout << "fitZ " << fitZ << " NF " << NF << " TrackLength " << TrackLength << " fitdZ " << fitdZ << endl;
	      }
	    }
	    else {
	      pidprob = dynamic_cast<StProbPidTraits*>(traits[i]);
	      if (pidprob) pidprob->Print("");
	    }
	  }
	}
      } // l
      //    }
  }  
}

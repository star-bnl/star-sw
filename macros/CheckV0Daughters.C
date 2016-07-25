class StBFChain;
StBFChain *chain = 0;
void CheckV0Daughters(const Char_t *fName=" /star/data07/reco/dAuCombined/FullField/P03if/2003/065/st_physics_4065069_raw_0040010.event.root") {
  gROOT->LoadMacro("bfc.C");
  //  TString Chain("in dEdxY2 StEvent debug");
  TString Chain("in,StEvent,nodefault");
  bfc(-1,Chain.Data(),fName);
  chain->Init();
  int istat=0,iev=0;
  int global = 0;
 EventLoop: if (!istat) {
   iev++;
   chain->Clear();
   istat = chain->Make(iev); // This should call the Make() method in ALL makers
   StEvent* pEvent = dynamic_cast<StEvent*> (chain->GetInputDS("StEvent"));
   if (! pEvent) {cout << "StEvent is missing in event #" << iev << endl;}
   else {
     StSPtrVecTrackNode& trackNode = pEvent->trackNodes();
     UInt_t nTracks = trackNode.size();
     if (nTracks) {
       StSPtrVecV0Vertex &v0 = pEvent->v0Vertices();
       Int_t nV0 = v0.size();
       cout << "Event#" << iev << "\t No. of V0\t" << nV0 << endl;
       for (int iv = 0; iv < nV0; iv++) {
	 StV0Vertex &V0v = v0[iv];
	 for (int s = 0; s < 2; s++) {
	    StTrack *daughter = static_cast<StTrack *>(V0v.daughter(s));
	    if (daughter) {
	      StTrackNode  *node = daughter->node();
	      if (node) {
		StGlobalTrack* vTrack = static_cast<StGlobalTrack*>(node->track(global));
		if (vTrack) {
		  Int_t iok = 0;
		  for (unsigned int i=0; i < nTracks; i++) {
		    StGlobalTrack* gTrack = static_cast<StGlobalTrack*>(trackNode[i]->track(global));
		    if (gTrack == vTrack) {
		      iok++;
		      cout << " =====================  found " << iok << endl;
		      break;
		    }
		  }
		  if (!iok) cout << "V0\t" << iv << "\tdaughter \t"
				 << s << "\thas not been found in StSPtrVecTrackNode's" << endl;
		}
	      }
	    }
	 }
       }
     }
   }
   goto EventLoop;
 } // Event Loop
}

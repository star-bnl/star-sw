{{
/* This is a very simple example macro using the StMuDstMaker to open
 * a MuDst file and print out some event and charged track information
 */
  gROOT->Reset();
  gROOT->Macro("loadMuDst.C");

  mudst_mk=new StMuDstMaker(0,0,"/star/data15/reco/productionMinBias/ReversedFullField/P05ia/2004/064/","st_physics_5064011_raw_3010002.MuDst.root");
  mudst_mk->Init();
  
  Int_t n_evt=1;
  for (Int_t i_evt=0; i_evt<n_evt; i_evt++) {
    mudst_mk->Make();
    if (mudst_mk->muDst()==0) {
      cout << "No event" << endl;
      continue;
    }
    StMuEvent *event=mudst_mk->muDst()->event();
    
    cout << "Event: " << event->eventId() << endl;
    StThreeVectorF vtx_pos = event->primaryVertexPosition();
    cout << "Vertex at " << vtx_pos.x() << " " << vtx_pos.y() << " " << vtx_pos.z() << endl;
    Int_t n_prim=mudst_mk->muDst()->GetNPrimaryTrack();
    Int_t n_glob=mudst_mk->muDst()->GetNGlobalTrack();
    cout << n_prim << "primary tracks and " << n_glob << " global tracks "<< endl;
    Int_t prim_step=n_prim/100;	
    if (prim_step==0)
   	prim_step=1;
    Int_t i_prim=0;
    while (i_prim < n_prim) {
      StMuTrack *pr_track=mudst_mk->muDst()->primaryTracks(i_prim);
      cout << "Primary track " << i_prim << " momentum " << pr_track->p() << endl;
      cout << "\t flag " << pr_track->flag() << " " << pr_track->nHits() 
	   << " hits ( " << pr_track->topologyMap()->numberOfHits(kSvtId) 
	   << " svt, " << pr_track->topologyMap()->numberOfHits(kTpcId) 
	   << " tpc, " << pr_track->topologyMap()->numberOfHits(kFtpcWestId) +
	                  pr_track->topologyMap()->numberOfHits(kFtpcEastId) 
	   << " ftpc )" << endl; 
      cout << "\t fitted points " << pr_track->nHitsFit() << " ( " 
	   << pr_track->nHitsFit(kSvtId) << " svt, "
	   << pr_track->nHitsFit(kSsdId) << " ssd, " 
	   << pr_track->nHitsFit(kTpcId) << " tpc, " 
	   << pr_track->nHitsFit(kFtpcEastId) << " Ftpc east, " 
	   << pr_track->nHitsFit(kFtpcWestId) << " Ftpc west)" << endl;
      cout << "\t possible points " << pr_track->nHitsPoss() << " ( " 
	   << pr_track->nHitsPoss(kSvtId) << " svt, "
	   << pr_track->nHitsPoss(kSsdId) << " ssd, " 
	   << pr_track->nHitsPoss(kTpcId) << " tpc, " 
	   << pr_track->nHitsPoss(kFtpcEastId) << " Ftpc east, " 
	   << pr_track->nHitsPoss(kFtpcWestId) << " Ftpc west)" << endl;

      cout << "\t first point " << pr_track->firstPoint() << endl;
      cout << "\t last point " << pr_track->lastPoint() << endl;
      cout << "\t inner helix origin " << pr_track->helix().origin() << endl; 
      cout << "Pid probabilities ";
      for (Int_t i=0; i<7; i++)
        cout << pr_track->probPidTraits()->probability(i) << " ";
      cout << endl;
      StMuTrack *gl_track=pr_track->globalTrack();
      cout << "Global track momentum " << gl_track->p() << endl;
      cout << "\t first point " << gl_track->firstPoint() << endl;
      cout << "\t last point " << gl_track->lastPoint() << endl;
      cout << "\t inner helix origin " << gl_track->helix().origin() << endl;
      cout << "Pid probabilities ";
      for (Int_t i=0; i<7; i++)
        cout << gl_track->probPidTraits()->probability(i) << " ";
      cout << endl;
      i_prim+=prim_step;
    }
  }

}}

dumpL2info() {
  gROOT->Macro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  gStyle->SetPadGridX(0);
  gStyle->SetPadGridY(0);

  muMk=new StMuDstMaker(0,0,"","/star/data42/reco/productionMinBias/ReversedFullField/P05ic/2004/008/st_physics_5008027_raw_4020001.MuDst.root");
  muMk->Init();

  const Int_t n_evt=100; 
  const Int_t max_trig=32;
  TrgDataType2005 *trgDataStruct = new TrgDataType2005;
  memset(trgDataStruct,0,sizeof(TrgDataType2005));
  for (Int_t i_evt=0; i_evt<n_evt; i_evt++) {
    Int_t ret=muMk->Make();
    if (ret)
      break;
    StMuEvent *event=muMk->muDst()->event();
    cout << "Run " << event->runId() << ", event " << event->eventId() << endl;
    StTriggerId &nom_trig=event->triggerIdCollection().nominal();
    cout << "Triggers ";    
    for (Int_t i_trig=0; i_trig<max_trig; i_trig++) { 
      if (nom_trig.triggerId(i_trig)!=0)
	cout << nom_trig.triggerId(i_trig) << " (v " << nom_trig.version(i_trig)<< ") ";
      else if (i_trig==0) 
	cout << "WHAAAAAAT??? No trigger Id ????" << endl;
    }
    cout << endl;
    cout << "vertex: " << event->primaryVertexPosition() << endl;

    // L2 stuff
    cout  << "L2Result.GetSize " << event->L2Result().GetSize() << endl;
    StTriggerData2005 *trgData = new StTriggerData2005(trgDataStruct,event->runId());
    for (Int_t i_trig=0; i_trig<7; i_trig++) {
      cout << "L2 trig " << i_trig << " :  isTrig " 
           << trgData->isL2Triggered((StL2AlgorithmId)i_trig) << endl;
    }
    int L2ResSize=event->L2Result().GetSize();
    int k;
    cout << "\nL2result size="<<L2ResSize<<" dump raw values :"<<endl;
    for(k=0;k<L2ResSize;k++) {
      cout<<"k="<<k<<" val/dec="<<L2Res[k]<<endl;
    } 
    delete trgData;
  }
}

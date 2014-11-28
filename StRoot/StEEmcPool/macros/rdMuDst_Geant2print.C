// read muDst & geant.root in sync

class StChain;
StChain *chain=0;

int rdMuDst_Geant2print( int maxEve=2){ 
  char * muDstPath="/star/data26/reco/pp200/pythia_6.203/default/minbias/year2003/hadronic_on/trs_ic/";
  char * muDstFile="rcf1200_2723_2000evts.MuDst.root";

  TString geantFile = "/star/data26/reco/pp200/pythia_6.203/default/minbias/year2003/hadronic_on/trs_ic/rcf1200_2723_2000evts.geant.root";
  
  
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;
  
    
  // create chain    
  chain = new StChain("StChain");   
  //chain->SetDebug();
  //chain->PrintInfo();
  
  // set up maker in read mode  
  StIOMaker* IOMk = new StIOMaker("IO","r",geantFile,"bfcTree");
  IOMk->SetDebug();
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");
  IOMk->SetBranch("geantBranch",0,"r");
  
  
  int maxList=1;
  muMk = new StMuDstMaker(0,0,muDstPath,muDstFile,"MuDst.root",maxList);   // set up maker in read mode
  
  chain->Init();
  chain->ls(3);

  // StMuDebug::setLevel(1);  // switch of some debug output
  
  int eventCounter=0;
  
  printf(" requested maxEve=%d\n",maxEve);
  //---------------------------------------------------
  while ( 1) {// loop over events
    eventCounter++;;
    if(eventCounter >maxEve) break;
    chain->Clear();
    int stat = chain->Make();
    if(stat) break;

    // Access to muDst .......................
    StMuEvent* muEve = muMk->muDst()->event();  
    int n = muMk->muDst()->primaryTracks()->GetEntries();  // get number of primary tracks
    StEventInfo &info=muEve->eventInfo();
    
    printf("eve=%d Nprim=%d ID=%d\n",eventCounter,n,info.id());
    
    
    // Access to geant-tables .......................

    St_DataSet* Event = chain->GetDataSet("geant");
    //Event->ls(3);
    St_DataSetIter geantDstI(Event);

    // Event generator info ........................
    St_g2t_event *Pg2t_event=(St_g2t_event *) geantDstI("g2t_event");
    //Pg2t_event->Print();
    g2t_event_st *g2t_event1=Pg2t_event->GetTable();
    printf("nr=%d %p\n",Pg2t_event->GetNRows(),g2t_event1);
    int k1=	g2t_event1->eg_label;
    int k2=	g2t_event1->n_event;
    int k3=	g2t_event1->subprocess_id;
    
    printf("eg_label=%d n_event=%d subprocess_id=%d\n", k1,k2,k3);
    assert(info.id()==g2t_event1->n_event);

    
    // This is an example to access the particle table directly.
    St_particle    *particleTabPtr    =  (St_particle    *) geantDstI("particle");
    particle_st* particleTable = particleTabPtr->GetTable();
    
    for (int i=0; i<particleTabPtr->GetNRows();++i) {
      if(i>10) break;
      cout << "track " << i << endl;
      cout << "   id = " << particleTable[i].idhep << endl;
      cout << "   px = " << particleTable[i].phep[0] << endl;
      cout << "   py = " << particleTable[i].phep[1] << endl;
      cout << "   pz = " << particleTable[i].phep[2] << endl;
      cout << "   e  = " << particleTable[i].phep[3] << endl;
      cout << "   m  = " << particleTable[i].phep[4] << endl;
      cout << "   moth1  = " << particleTable[i].jmohep[0] << endl;
      cout << "   moth2  = " << particleTable[i].jmohep[1] << endl;
    }
    
  }
  
  
  
}





//  /star/data31/reco/ppMinBias/FullField/P03if/2002/...
// /star/data31/reco/ppMinBias/ReversedFullField/P03if/2002/...

///star/data29/reco/pp200/pythia6_203/default/pt5/year2003/gheisha_on/trs_if/","rcf1202_2178_1000evts.MuDst.root",

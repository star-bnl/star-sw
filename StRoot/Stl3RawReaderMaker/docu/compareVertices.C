{

// This macro reads a dst extracts the vertex from the offline chain
// then another dst (now a .event.root file) is opened which contains
// l3 data. Interesting information are filled into an ntuple.

gSystem->Load("St_base");
gSystem->Load("StChain");
gSystem->Load("StUtilities");
gSystem->Load("libglobal_Tables");
gSystem->Load("StAnalysisUtilities");
gSystem->Load("StIOMaker");
gSystem->Load("StarClassLibrary");
gSystem->Load("StEvent");

// create ntuple
TNtuple *ntuple = new TNtuple("vgl","l3 vs off","offVerX:offVerY:offVerZ:L3VerX:L3VerY:L3VerZ:L3VerZrough:L3mult");

// create super chain which contains 2 subchains
superchain =  new StChain("MyChain0");

// create l3 chain
l3chain = new StChain("MyChain1");
StIOMaker *IOMk1 = new StIOMaker("IO1","r","/direct/star+rcf/daq/l3/vertexStudy/st_physics_1230069_raw.event.root","bfcTree");
IOMk1->SetDebug();
IOMk1->SetBranch("*",0,"0");   
IOMk1->SetBranch("eventBranch",0,"r");
IOMk1->SetIOMode("r");
// one has to know : AddMaker removes IOMk1 from superchain -> otherwise you can't avoid to call it twice
l3chain->AddMaker(IOMk1) ;


// create off line chain
offchain = new StChain("MyChain2");
StIOMaker *IOMk2 = new StIOMaker("IO2","r","","bfcTree");
StFile* fileset = new StFile();
fileset->AddFile("/star/data08/reco/P00hg/2000/08/st_physics_1230069_raw_0001.dst.root");
fileset->AddFile("/star/data08/reco/P00hg/2000/08/st_physics_1230069_raw_0002.dst.root");
fileset->AddFile("/star/data08/reco/P00hg/2000/08/st_physics_1230069_raw_0003.dst.root");
fileset->AddFile("/star/data08/reco/P00hg/2000/08/st_physics_1230069_raw_0004.dst.root");
IOMk2->SetFileSet(fileset);

IOMk2->SetDebug();
IOMk2->SetBranch("*",0,"0");
IOMk2->SetBranch("dstBranch",0,"r");
IOMk2->SetIOMode("r");
offchain->AddMaker(IOMk2) ;


// init
superchain->Init();

for (Int_t evloop=0; evloop<350;evloop++)
{
 
  // call clear and make for superchain and thus for subchains
  cout << endl << endl << "Working on Event number " << (evloop+1) << endl ; 
  superchain->Clear(); 
  superchain->Make(evloop);

  ///// 
  // look into offline dst to get offline vertex
  /////
  
  // get dst containing off line vertex
  deventBranch = offchain->GetDataSet("dstBranch") ;  
  St_dst_vertex* ver = (St_dst_vertex*) deventBranch->FindObject("vertex");
  
  Int_t max_ndaugthers = 10 ;
  Double_t offvertX = 999 ;
  Double_t offvertY = 999 ;
  Double_t offvertZ = 999 ;
  
  if (ver)
    {
      for (Int_t ii =0; ii <ver->GetNRows() ; ii++)
	{
	  if (ver->GetTable()[ii].n_daughters>max_ndaugthers)
	    {
	      max_ndaugthers = ver->GetTable()[ii].n_daughters ;
	      offvertX = ver->GetTable()[ii].x ;
	      offvertY = ver->GetTable()[ii].y ;
	      offvertZ = ver->GetTable()[ii].z ;
	    }  
	}
      cout << "  Main vertex offline: " << offvertX << "\t" ;
      cout << offvertY << "\t" << offvertZ << endl;
    }

  ///// 
  // look into event.root to get l3 vertex
  /////

  // get StEvent containing l3 vertex
  deventBranch2 = l3chain->GetDataSet("eventBranch") ;
  TDataSetIter eventBIter2(deventBranch2);
  mevent = (StEvent*) eventBIter2->FindByName("StEvent") ;
  if (!mevent) { cout <<"No StEvent found.\n" ; continue; }
    
  // get L3 data
  ml3trigger = (StL3Trigger*) mevent->l3Trigger() ;
  if (!ml3trigger) { cout <<"No l3 found inside StEvent.\n" ; continue; }
  if (ml3trigger->primaryVertex())
    {
      cout << "  Main vertex l3: " << "\t" ;
      cout << ml3trigger->primaryVertex(1)->position().x() << "\t" ;
      cout << ml3trigger->primaryVertex(1)->position().y() << "\t" ;
      cout << ml3trigger->primaryVertex(1)->position().z() << endl ;
    }

  // print some output
  cout << "dst : " <<  offvertZ << endl  ;
  cout << "l3 : " <<  ml3trigger->primaryVertex(1)->position().z() << endl ;
  Double_t d = offvertZ - ml3trigger->primaryVertex(1)->position().z() ;
  cout << "dif : " << d <<endl;
  
  StSPtrVecTrackNode& mtracknodes = (StSPtrVecTrackNode&) ml3trigger->trackNodes() ;
  cout << " nubmer of l3 tracks " << mtracknodes->size() << endl; 
  Int_t mult =mtracknodes->size() ; 

  // Fill ntuple
  ntuple->Fill(offvertX,offvertY,offvertZ,ml3trigger->primaryVertex(1)->position().x(),ml3trigger->primaryVertex(1)->position().y(),ml3trigger->primaryVertex(1)->position().z(),ml3trigger->primaryVertex(0)->position().z(),mult) ;
  
}
superchain->Finish();
}

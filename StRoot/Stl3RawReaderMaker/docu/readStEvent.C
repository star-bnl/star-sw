{
gSystem->Load("St_base");
gSystem->Load("StChain");
gSystem->Load("StUtilities");
gSystem->Load("libglobal_Tables");
gSystem->Load("StAnalysisUtilities");
gSystem->Load("StIOMaker");
gSystem->Load("StarClassLibrary");
gSystem->Load("StEvent");

// create chain
chain = new StChain("MyChain");

// connect to the .event.root file
StIOMaker *IOMk = new StIOMaker("IO","r","/afs/rhic/star/users/flierl/public/test/st_physics_1243037_raw_0001.event.root","bfcTree");
IOMk->SetDebug();
IOMk->SetBranch("*",0,"0");                 //deactivate all branches
IOMk->SetBranch("eventBranch",0,"r");
IOMk->SetIOMode("r");

Int_t iInit = chain->Init();

for (Int_t evloop=0; evloop<5;evloop++)
{
  // reset chain for upcoming event
  chain->Clear();
  // call Make only for StIOMaker
  Int_t iMake = chain->Make(evloop);
  cout <<"Working on Event number " << (evloop+1) << endl ; 
  // get StEvent
  mevent = (StEvent*) chain->GetInputDS("StEvent") ;
  if (!mevent) { cout <<"No StEvent found.\n" ; break; }
  // get L3
  ml3trigger = (StL3Trigger*) mevent->l3Trigger() ;
  if (!ml3trigger) { cout <<"No l3 found inside StEvent.\n" ; break; }
  // now do some silly looping over tracks
  StSPtrVecTrackNode& mtracknodes = (StSPtrVecTrackNode&) ml3trigger->trackNodes() ;
  cout << " nubmer of tracks " << mtracknodes->size() << endl; 
  for(Int_t i=0; i < mtracknodes->size() ; i++)
    {
      if(i<10 || i%1000==0)
	{
	  cout << "L3 Track : " << "\t" ;
	  cout << "px : " << Double_t (mtracknodes[i]->track(0)->geometry()->momentum()->x()) << "\t" ;
	  cout << "py : " << Double_t (mtracknodes[i]->track(0)->geometry()->momentum()->y()) << "\t" ;
	  cout << "pz : " << Double_t ( mtracknodes[i]->track(0)->geometry()->momentum()->z()) << "\t" ;
	  cout << "x : " << mtracknodes[i]->track(0)->geometry()->origin()->x() << "\t" ;
	  cout << "y : " << mtracknodes[i]->track(0)->geometry()->origin()->y() << "\t" ;
	  cout << "z : " << mtracknodes[i]->track(0)->geometry()->origin()->z() << "\n" ;
	}
    }
  
}
}

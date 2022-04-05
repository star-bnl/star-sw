// NOTE - chain needs to be declared global so for StHbtEventReader
//==========================================================================================
class StMuDstMaker;

StMuDstMaker* maker;

void exampleEmc(const char* list) {
   if (gClassTable->GetID("TTable") < 0) {
     gSystem->Load("libTable");
     gSystem->Load("libPhysics");
   }     
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StMagF");
  gSystem->Load("StUtilities");  // new addition 22jul99
  gSystem->Load("StTreeMaker");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StEvent");
  gSystem->Load("StEventUtilities"); 
  gSystem->Load("StMcEvent"); 
  gSystem->Load("StMcEventMaker"); 
  gSystem->Load("StAssociationMaker");
  gSystem->Load("StMcAnalysisMaker");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StMuDSTMaker");

  cout << " loading done " << endl;
  
  StMuDebug::setLevel(0);  // switch of some debug output

  int iret=0;
  maker = new StMuDstMaker(0,0,"",list,"MuDst.root",20);   // set up maker in read mode
  StMuDbReader* db = StMuDbReader::instance();
  //  db->addDb("/star/u/laue/afsWork/P02gc.db");
  //  db->addDb("/star/u/laue/afsWork/P02gd.db");

  int eventCounter=0;
  while ( !maker->Make() )  {
    cout << " event# " << eventCounter++ << endl;
    StMuEvent* e = maker->muDst()->event();
    StMuEmcCollection* emc = maker->muDst()->emcCollection();
    if (emc) {
      /*
      for (int n=3; n<=4; n++) {
	int nSmdHits = emc->getNSmdHits(n);
	cout << "SmdHits: " << nSmdHits << endl;
	for (int i=0; i<nSmdHits; i++) {
	  StMuEmcHit* h =  emc->getSmdHit(n,i);
	  printf("SmdHit(%02i,%02i): energy=%f adc=%f\n",n,i,h->getEnergy(), h->getAdc());
	}
      }
      */
      for (int n=1; n<=4; n++) {
	int nClusters = emc->getNClusters(n);
	cout << "Clusters: " << nClusters << endl;
	for (int i=0; i<nClusters; i++) {
	  StMuEmcCluster* c =  emc->getCluster(n,i);
	  printf("Cluster(%02i,%02i): energy=%f phi=%f eta=%f nHits=%d \n",n,i,c->getEnergy(), c->getPhi(), c->getEta(), c->getNHits() ); 
	}
      }
      int nPoints = emc->getNPoints();
      for (int i=0; i<nPoints; i++) {
	StMuEmcPoint* p =  emc->getPoint(i);
	printf("Point(%02i) : energy=%f phi=%f eta=%f \n",i,p->getEnergy(), p->getPhi(), p->getEta()); 
      }
    }
  }  
  
}




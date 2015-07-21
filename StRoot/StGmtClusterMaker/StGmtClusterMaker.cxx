//
// First Cluster Maker
// \class StGmtClusterMaker
// \authors K.S. Engle and Richard Witt (witt@usna.edu)
// based on StFgtClusterMaker

#include "StGmtClusterMaker.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StGmtCollection.h"
#include "StGmtIClusterAlgo.h"
#include "StEvent/StGmtHit.h"
#include "StRoot/StGmtUtil/geometry/StGmtGeom.h"
#include "StGmtSimpleClusterAlgo.h"

Int_t StGmtClusterMaker::gmtStat = 0;

StGmtClusterMaker::StGmtClusterMaker( const Char_t* name ) : //StMaker(name),
	StRTSBaseMaker( "clustser", name ),						     
	mClusterAlgoPtr(0),
	ftriviahit(NULL)
{
StGmtClusterMaker::gmtStat = 0;
// noop
};

StGmtClusterMaker::~StGmtClusterMaker()
{
  // noop
};


Int_t StGmtClusterMaker::Make()
{
  Int_t ierr = kStOk;
  StGmtClusterMaker::gmtStat = 0;
  //StEvent* eventPtr = 0;
  //eventPtr = (StEvent*)GetInputDS("StEvent");
  StEvent *eventPtr = dynamic_cast<StEvent *> (GetInputDS("StEvent"));
  if( !eventPtr ) {
     LOG_ERROR << "Error getting pointer to StEvent from '" << ClassName() << "'" << endm;
     ierr = kStErr;
  }
   
   StGmtCollection* gmtCollectionPtr = 0;
   if( eventPtr ) {
     gmtCollectionPtr=eventPtr->gmtCollection();
     if (Debug()) {
       LOG_INFO << "::Make() gor col: " << (void*)gmtCollectionPtr  << endm;
     }
   }
   
   if( !gmtCollectionPtr) {
     LOG_ERROR << "Error getting pointer to StGmtCollection from '" << ClassName() << "'" << endm;
     ierr = kStErr;
   }
   Int_t loc_ierr=0;
   if( !ierr ){
     for( UInt_t moduleIdx=0; moduleIdx<gmtCollectionPtr->getNumModules(); ++moduleIdx ){
       if (Debug()) {
       LOG_INFO << "module: " << moduleIdx << " has strips: " << (void*)eventPtr << "\t" <<  (void*)gmtCollectionPtr  <<"\t" <<  gmtCollectionPtr->getNumStrips(moduleIdx) << endm;
       LOG_INFO << "EventPtr= " << (void*)eventPtr << "\t" << eventPtr->id()  << '\t' << eventPtr->runId()  << '\t' << eventPtr->time()  << '\t'  << endm;
       LOG_INFO << "Collection =" <<  (void*)gmtCollectionPtr  <<"\t" <<  gmtCollectionPtr->getNumModules()  << '\t' << gmtCollectionPtr->getNumStrips()  << '\t'  <<gmtCollectionPtr->getNumHits()  << '\t'  <<gmtCollectionPtr->getNumPoints()  << '\t' << endm;
       }
       
       Int_t nelements = gmtCollectionPtr->getNumStrips(moduleIdx);
       if( nelements  < kGmtNumStripsPerModule )
	 {
	   if (Debug()) {
	   LOG_WARN <<"StClusterMaker::Make(): no data for module " << moduleIdx << endm;
	   }
	   continue;
	 }
       
       StGmtStripCollection *stripCollectionPtr = gmtCollectionPtr->getStripCollection( moduleIdx );
       StGmtHitCollection *hitCollectionPtr = gmtCollectionPtr->getHitCollection( moduleIdx );
       
       if( stripCollectionPtr && hitCollectionPtr ){
	 if (Debug()) {
	 cout <<"doing clustering ..." <<endl;
	 LOG_INFO << "CLSUTER" << stripCollectionPtr->getNumStrips() << "\t" << stripCollectionPtr->getModule() << endm;
	 //  if(stripCollectionPtr->getNumStrips()){
	 // 	   stripCollectionPtr->sortByGeoId();
	 // 	   StGmtStrip* stripPtr = stripCollectionPtr->getSortedStrip(0 );
	 // 	   LOG_INFO << "00BIS :"  << stripPtr->getAdc(0) <<  " " << stripPtr->getAdc(1) << endm; 
	 // 	 }
	     }
	 loc_ierr = mClusterAlgoPtr->doClustering( moduleIdx, *stripCollectionPtr, *hitCollectionPtr ); //CLUSTERING
	 if (Debug()) {
	 cout <<"done ..." <<endl;
	 // 	 if(loc_ierr!=kStOk) {
	 // 	   LOG_WARN <<"StClusterMaker::Make(): clustering for module " << moduleIdx << " returned " << loc_ierr <<endm;
	 // 	   if(loc_ierr>ierr)
	 // 	     ierr=loc_ierr;
	 // 	 }
	 }
       }
       
     }
   }
   if (Debug()) {
   LOG_INFO << "End of gmt-clust-maker, print all strips & clusters: " << endm;
   LOG_INFO <<"  gmtCollnumModule=" << gmtCollectionPtr->getNumModules()<<", tot strip=" <<gmtCollectionPtr->getNumStrips()<<"  totClust=" <<  gmtCollectionPtr->getNumHits() <<endm;
   }
   StGmtClusterMaker::gmtStat = 0;
   if (gmtCollectionPtr->getNumStrips() > 0) {
     StGmtClusterMaker::gmtStat = (gmtCollectionPtr->getNumHits()>0 ? 20 + gmtCollectionPtr->getNumHits() : 10);
   }
   if (Debug()) {
   
   for(int iModule=0; iModule <(int)gmtCollectionPtr->getNumModules(); iModule++) {
     LOG_INFO <<"  content: iModule="<<iModule<< " # of : strips="<<gmtCollectionPtr->getNumStrips(iModule) <<"  hits=" <<gmtCollectionPtr->getNumHits(iModule)<<endm;
     // ..... print all strips ....
     
     StGmtStripCollection *stripPtr= gmtCollectionPtr->getStripCollection(iModule);
     StSPtrVecGmtStrip &stripVec = stripPtr->getStripVec(); //  StGmtStrip* stripPtr = stripCollectionPtr->getStrip( geoId );   
     
     StGmtHitCollection *clustPtr= gmtCollectionPtr->getHitCollection(iModule);
     StSPtrVecGmtHit &clustVec = clustPtr->getHitVec();    
     if(clustVec.size()){
       int ih1=0;
       if (gmtCollectionPtr->getNumStrips(iModule))
	 //if(loc_ierr) //to record when the pedestal calc is ok
	 for( StSPtrVecGmtStripIterator it=stripVec.begin();it!=stripVec.end();++it, ih1++)    
	   {
	     // details of strip localization, use output variables ending w/ X
	     //       Short_t moduleX,  stripX; Char_t  layerX;
	     Short_t moduleX,  stripX; Int_t  layerX;
	     StGmtGeom::decodeGeoId(((*it))->getGeoId(), moduleX, layerX, stripX);
	     ftriviahit = new StGmtTrivia( ((*it))->getGeoId(), moduleX, layerX, stripX); //HERE
	     ftriviahit->setMaxAdc((*it)->getMaxAdc());
	     ftriviahit->setRunId( eventPtr->time());
	     ftriviahit->setMaxPedSubtractedAdc((*it)->getMaxPedSubtractedAdc());
	     ftriviahit->setCharge((*it)->getCharge());
	     ftriviahit->setPed((*it)->getPed());
	     ftriviahit->setPedErr((*it)->getPedErr());
	     ftriviahit->setMaxPedSubtractedAdcTB((*it)->getMaxPedSubtractedAdcTB());
	     ftriviahit->setPedDev((*it)->getPedStdDev());
	     ftriviahit->setCoor((*it)->getCoordNum());;
	     ftriviahit->setPos((*it)->getPosition());
	     int rdo = -99, arm=-99, apv=-99, chan=-99;  
	     (*it)->getElecCoords( rdo, arm,  apv,   chan ); 
	     ftriviahit->setElecCoords( rdo, arm, apv,  chan ); 
	     for (int i=0;i<15;i++) ftriviahit->setAdc(i,(*it)->getPedSubtractedAdc(i));
	     
	     //ClusterRelatedStuff
	     
	     if(clustVec.size()){
	       StGmtHit * cl = clustVec.at(0);
	       ftriviahit->setCX(cl->getLocalX());
	       ftriviahit->setCY(cl->getLocalY());
	       ftriviahit->setCAX(cl->getAdcX());
	       ftriviahit->setCAY(cl->getAdcY());
	       //ftriviahit->setClustSize(clustVec.size());
	       ftriviahit->setClustSize((*it)->isC());
	     }
	     
	     ftriviatree->Fill();
	     //LOG_INFO << "============ iModule="<<iModule<<" ih=" <<ih <<"   strip: geoId=" <<((*it))->getGeoId()<< " ADC=" <<((*it))->getAdc()<<"  charge=" << ((*it))->getCharge()<< " 1f decode0: strip=" <<stripX << " module="<<moduleX << "hitTrivia " << ftriviahit <<endm;
	   }
       ftriviatree->Write();
       ftriviafile->Write();

     }//end   if(clustVec.size()){
   }
     //      // ..... print all 2D clusters (aka GMT HITs) ....
     //      int ih=0;
     //      LOG_INFO << "AFTER CLUSTERING : " << clustVec.size() << " cl. from " << ih1 << "Strips " << endm; 
//      for( StSPtrVecGmtHitIterator it=clustVec.begin(); it!=clustVec.end();++it, ih++)    
//        {
// 	 // details of central strip localization, use output variables ending w/ X
// 	 Short_t moduleX,  stripX; Int_t  isPad;
// 	 //       StGmtGeom::decodeGeoId(((*it))->getCentralStripGeoId(),moduleX, layerX, stripX);      
// 	 moduleX = ((*it))->getModule();
// 	 //       stripX = ((*it))->getStrip()
// 	 //       isPad = ((*it))->isY();
// 	 //       LOG_DEBUG <<"iModule="<<iModule<<" ih="<<ih<<", layer="<<((*it))->getLayer()<<" totCharge="<<(*it)->charge()<< " R/cm="<<(*it)->getPositionR() <<" +/-"<<(*it)->getErrorR() <<" Phi/rad="<<(*it)->getPositionPhi()<<" +/- " << (*it)->getErrorPhi()<< "  Z/cm= " << (*it)->getPositionZ() <<" +/-  "<<(*it)->getErrorZ()<<" centStripId=" << (*it)->getCentralStripGeoId() <<"decode0: strip= " << stripX <<"  module=" <<moduleX <<endm;
// 	 LOG_INFO <<"iModule= "<<iModule << " ih=" << ih << 
// 	   ", totCharge= " << (*it)->charge() << ", X/cm= " << (*it)->getLocalX() <<
// 	   " +/- " << (*it)->getErrorLocalX() << ", Y/cm= " << (*it)->getLocalY() <<
// 	   " +/- " << (*it)->getErrorLocalY() << ",  module= " << moduleX << 
// 	   " TBins " << (*it)->getTbX() << "," << (*it)->getTbY() << endm;
	 
//        }
     
     
   }
   
   return ierr;
   
}


Int_t StGmtClusterMaker::setClusterAlgo(StGmtIClusterAlgo* algo)
{
  
  mClusterAlgoPtr=algo;
  return kStOk;
}
Int_t StGmtClusterMaker::Finish(){
  LOG_INFO <<"========== FINISH"<<endm; 
  //  TTree * newnew =  ftriviatree->CloneTree();
  //newnew
  // ftriviatree->Write();
  if (ftriviafile) {
    ftriviafile->Write();
    ftriviafile->Close();
  }
  return kStOk;
}

Int_t StGmtClusterMaker::Init()
{
  //  cout <<"cluster init " <<endl;
  Int_t ierr = kStOk;

  //
  // Please, conside the Maker's SetMode() method (setting m_Mode but you can get
  // the value using Getmode() as well ... to switch between cluster agos.
  // Extrenal setting a-la gmtClusMkr->setClusterAlgo( seededClusAlgo ); is
  // not appropriate for a chain mades maker.
  //
  if( !mClusterAlgoPtr ){
    //      LOG_INFO << "No gmt cluster algorithm specified, using default seededAlgo" << endm;
    //      mClusterAlgoPtr=new StGmtSeededClusterAlgo();
    LOG_INFO << "No gmt cluster algorithm specified, using default simple algorithm" << endm;
    mClusterAlgoPtr=new StGmtSimpleClusterAlgo();
  }
  
  //trivia tree for display
  StEvent *eventPtr = dynamic_cast<StEvent *> (GetInputDS("StEvent"));
  int runid = 0;
  // if(eventPtr) runid = eventPtr->runId();
//   char filtmp[500];
//   sprintf(filtmp,"treeGMT%d.root",runid);
  ftriviafile  = new TFile("treeGMT.root","RECREATE");
  LOG_INFO << "======\n\n\n\n\n\n\n\n==\n\n\n\n\n==\n\n Opening file treeGMT" << endm;
  if (ftriviafile->IsZombie()) {
	  LOG_INFO << "======\n\n\n\n\n\n\n\n==\n\n\n\n\n==\n\n Error opening file treeGMT" << endm;}

  ftriviatree  = new TTree("tGMT","A Tree with Trivia");
  ftriviatree->SetAutoSave(1000000);
  ftriviahit = new StGmtTrivia();
  ftriviatree->Branch("Hits","StGmtTrivia", &ftriviahit);//t,32000,0);
  if( !ierr )
	  ierr = mClusterAlgoPtr->Init();

  return ierr;
}
  
  

    
ClassImp(StGmtClusterMaker);
    

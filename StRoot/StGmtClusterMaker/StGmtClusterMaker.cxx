//
// First Cluster Maker
// \class StGmtClusterMaker
// \authors K.S. Engle and Richard Witt (witt@usna.edu)
// based on StFgtClusterMaker

#include "StGmtClusterMaker.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StGmtCollection.h"
#include "StEvent/StGmtHit.h"
#include "StRoot/StGmtUtil/geometry/StGmtGeom.h"
#include "StGmtSimpleClusterAlgo.h"
ClassImp(StGmtClusterMaker);

Int_t StGmtClusterMaker::gmtStat = 0;
//________________________________________________________________________________
StGmtClusterMaker::StGmtClusterMaker( const Char_t* name ) : //StMaker(name),
  StRTSBaseMaker( "clustser", name ),						     
  mClusterAlgoPtr(0),
  ftriviahit(NULL) {
  StGmtClusterMaker::gmtStat = 0;
  SetAttr("gmtCosmics"             ,kFALSE);
  SetAttr("gmtClusTree"             ,kFALSE);
};
//________________________________________________________________________________
Int_t StGmtClusterMaker::Make() {
  Int_t ierr = kStOk;
  StGmtClusterMaker::gmtStat = 0;
  //StEvent* eventPtr = 0;
  //eventPtr = (StEvent*)GetInputDS("StEvent");
  StEvent *eventPtr = dynamic_cast<StEvent *> (GetInputDS("StEvent"));
  if( !eventPtr ) {
    LOG_ERROR << "Error getting pointer to StEvent from '" << ClassName() << "'" << endm;
     return kStErr;
  }
   
   StGmtCollection* gmtCollectionPtr = gmtCollectionPtr=eventPtr->gmtCollection();
   if( !gmtCollectionPtr) {
     LOG_WARN << "Error getting pointer to StGmtCollection from '" << ClassName() << "'" << endm;
     return kStWarn;
   }
   Int_t loc_ierr=0;
   for( UInt_t moduleIdx=0; moduleIdx<gmtCollectionPtr->getNumModules(); ++moduleIdx ){
     if (Debug()) {
       LOG_INFO << "module: " << moduleIdx << " has strips: \t" <<  gmtCollectionPtr->getNumStrips(moduleIdx) << endm;
       LOG_INFO << "Collection =\t" << gmtCollectionPtr->getNumStrips()  << "\t"
		<<gmtCollectionPtr->getNumHits()  << '\t'  <<gmtCollectionPtr->getNumPoints()  << '\t' << endm;
     }
     Int_t nelements = gmtCollectionPtr->getNumStrips(moduleIdx);
     if( nelements  < kGmtNumStripsPerModule ) {
       if (Debug()) {
	 LOG_WARN <<"StClusterMaker::Make(): no data for module " << moduleIdx << endm;
       }
       continue;
     }
     StGmtStripCollection *stripCollectionPtr = gmtCollectionPtr->getStripCollection( moduleIdx );
     StGmtHitCollection *hitCollectionPtr = gmtCollectionPtr->getHitCollection( moduleIdx );
     if( stripCollectionPtr && hitCollectionPtr ){
       if (Debug()) {
	 LOG_INFO << "Cluster " << stripCollectionPtr->getNumStrips() << "strips\tin module" << stripCollectionPtr->getModule() << endm;
       }
       loc_ierr = mClusterAlgoPtr->doClustering( moduleIdx, *stripCollectionPtr, *hitCollectionPtr ); //CLUSTERING
       if (Debug()) {
	 if(loc_ierr!=kStOk) {
	   LOG_WARN <<"StClusterMaker::Make(): clustering for module " << moduleIdx << " returned err =" << loc_ierr <<endm;
	 }
       }
     }
   }
   if (Debug()) {
     LOG_INFO << "End of gmt-clust-maker, print all strips & clusters: " << endm;
     LOG_INFO <<"  gmtCollnumModule=" << gmtCollectionPtr->getNumModules()<<", tot strip=" <<gmtCollectionPtr->getNumStrips()
	      <<"  totClust=" <<  gmtCollectionPtr->getNumHits() <<endm;
   }
   StGmtClusterMaker::gmtStat = 0;
   if (gmtCollectionPtr->getNumStrips() > 0) {
     StGmtClusterMaker::gmtStat = (gmtCollectionPtr->getNumHits()>0 ? 20 + gmtCollectionPtr->getNumHits() : 10);
   }
   if (ftriviatree) {
     for(int iModule=0; iModule <(int)gmtCollectionPtr->getNumModules(); iModule++) {
       if (Debug()) {
	 // ..... print all strips ....
	 LOG_INFO <<"  content: iModule="<<iModule<< " # of : strips="<<gmtCollectionPtr->getNumStrips(iModule) <<"  hits=" <<gmtCollectionPtr->getNumHits(iModule)<<endm;
       }
       StGmtStripCollection *stripPtr= gmtCollectionPtr->getStripCollection(iModule);
       StSPtrVecGmtStrip &stripVec = stripPtr->getStripVec(); //  StGmtStrip* stripPtr = stripCollectionPtr->getStrip( geoId );   
       StGmtHitCollection *clustPtr= gmtCollectionPtr->getHitCollection(iModule);
       StSPtrVecGmtHit &clustVec = clustPtr->getHitVec();    
       if(clustVec.size()){
	 int ih1=0;
	 if (gmtCollectionPtr->getNumStrips(iModule))
	   //if(loc_ierr) //to record when the pedestal calc is ok
	   for( StSPtrVecGmtStripIterator it=stripVec.begin();it!=stripVec.end();++it, ih1++) {
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
	     
	     if(clustVec.size() && ftriviahit){
	       StGmtHit * cl = clustVec.at(0);
	       ftriviahit->setCX(cl->getLocalX());
	       ftriviahit->setCY(cl->getLocalY());
	       ftriviahit->setCAX(cl->getAdcX());
	       ftriviahit->setCAY(cl->getAdcY());
	       //ftriviahit->setClustSize(clustVec.size());
	       ftriviahit->setClustSize((*it)->isC());
	     }
	     ftriviatree->Fill();
	   }
       }//end   if(clustVec.size()){
     }
   }
   if (IAttr("gmtCosmics")) {
     if (! gmtCollectionPtr->getNumHits()) return kStERR;
   }
   if (Debug()) {
     UShort_t NumModules = gmtCollectionPtr->getNumModules();
     for (UShort_t m = 0; m < NumModules; m++) {
       const StGmtHitCollection *coll = gmtCollectionPtr->getHitCollection(m);
       if (! coll) continue;
       const StSPtrVecGmtHit &hits = coll->getHitVec();
       UInt_t NoHits = hits.size();
       for (UInt_t l = 0; l < NoHits; l++) {
	 const StGmtHit *hit = hits[l];
	 if (hit) {
	   hit->Print("");
	 }
       }
     }
   }
   return ierr;
}
//________________________________________________________________________________
Int_t StGmtClusterMaker::setClusterAlgo(StGmtSimpleClusterAlgo* algo) {
  mClusterAlgoPtr=algo;
  return kStOk;
}
//________________________________________________________________________________
Int_t StGmtClusterMaker::Init() {
  //  cout <<"cluster init " <<endl;
  Int_t ierr = kStOk;
  if (IAttr("gmtCosmics")) SetAttr(".Privilege",kTRUE);
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
  if (IAttr("gmtClusTree")) {
    TFile *f = GetTFile();
    if (f) {
      f->cd();
      ftriviatree  = new TTree("tGMT","A Tree with Trivia");
      ftriviatree->SetAutoSave(1000000);
      ftriviahit = new StGmtTrivia();
      ftriviatree->Branch("Hits","StGmtTrivia", &ftriviahit);//t,32000,0);
    }
  }
  return mClusterAlgoPtr->Init();
}
//________________________________________________________________________________
  
  

    
    

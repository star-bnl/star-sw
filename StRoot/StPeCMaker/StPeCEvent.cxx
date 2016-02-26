//////////////////////////////////////////////////////////////////////
//
// $Id: StPeCEvent.cxx,v 1.26 2016/02/24 17:14:56 ramdebbe Exp $
// $Log: StPeCEvent.cxx,v $
// Revision 1.26  2016/02/24 17:14:56  ramdebbe
// cleaned up old commented line
//
// Revision 1.25  2015/07/22 20:11:08  ramdebbe
// removed RP container copy to output, needs to come back
//
// Revision 1.24  2015/02/25 01:34:22  ramdebbe
// added copy of the Roman Pot StMuRpsCollection to output tree
//
// Revision 1.23  2014/12/23 20:44:57  ramdebbe
// Modified intantiation of StPeCPair when using MuDst input to get TOF extrapolation in the pPairs branch
//
// Revision 1.22  2014/06/18 16:30:55  ramdebbe
// added more variables to event summary
//
// Revision 1.21  2013/12/27 16:49:50  ramdebbe
// added a set method setTOFgeometry to pass pointer to StBTofGeometry
//
// Revision 1.20  2013/01/24 15:41:16  ramdebbe
// added more flags to choose input or output tracks tof etc.
//
// Revision 1.19  2012/07/08 00:40:20  ramdebbe
// initialized variables per Gene VB advice
//
// Revision 1.18  2012/06/13 15:44:46  ramdebbe
// Added flags to include TOF and Vertex branches in tree
//
// Revision 1.17  2005/08/24 20:58:00  jeromel
// TClone to TObj fix
//
// Revision 1.16  2003/11/25 01:54:26  meissner
// correct several bugs: eta cut for tracks, charge sorting, add counting of FTPC and TPC primary tracks, Add bbc information
//
// Revision 1.15  2003/09/02 17:58:46  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.14  2003/03/20 20:10:58  yepes
// double counting of tracks corrected
//
// Revision 1.13  2003/03/18 21:20:28  yepes
// correcting problem with bField
//
// Revision 1.12  2003/02/11 20:45:39  yepes
// Events without a vertex in MuDst should vertex parameters to -9999
//
// Revision 1.11  2003/02/05 17:14:05  yepes
// Adding bField and pPairs.psi to tree
//
// Revision 1.10  2002/12/19 18:09:53  yepes
// MuDST input added
//
// Revision 1.9  2002/03/19 22:23:26  meissner
// New variables: zdc unatt., Trigger word, MC tree if Geant Branch, DCA  for primary pairs, all tracks for secondary pairs (Test)
//
// Revision 1.8  2001/04/23 21:44:30  meissner
// add dEdx z variable to tree, setFormat(1) for tree, use private BetheBloch (temp solution)
//
// Revision 1.7  2001/02/21 20:41:58  yepes
// Add ctb signals to tree
//
// Revision 1.6  2001/02/13 17:54:40  yepes
// still problems on differnt platforms
//
// Revision 1.5  2001/02/12 22:33:51  yepes
// avoid printing
//
// Revision 1.4  2001/02/12 21:15:42  yepes
// New version of StPeCMaker, lots of changes
//
// Revision 1.3  2000/04/24 19:15:27  nystrand
// Fix of a possible memory leak
//
// Revision 1.2  2000/04/21 19:10:30  nystrand
// Include StPeCPair class
//
// Revision 1.1  2000/03/24 22:37:06  nystrand
// First version of StPeCEvent
//
// Revision 1.0  2000/03/20 23:28:50  nystrand
//
//////////////////////////////////////////////////////////////////////
// TODO: 
//  Understand what the redifinition of the TClone Array does
//   -> dereferencing the pointer to get  just the array 
//  Place the cut on  track->flag() in a unique place (Internal track array?)
//  Understand the dublication of StPeCTrack  (does a tree work with pointers)
// ///////////////////////////////////////////////////////////////////
#include <Stiostream.h>
#include "StPeCEvent.h"
#include "StPeCMaker.h"
#include "StEventTypes.h"
#include "StPeCEnumerations.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuDebug.h"
#include "StPreEclMaker/StPreEclMaker.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StMuDSTMaker/COMMON/StMuDst2StEventMaker.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StEpcMaker/StEpcMaker.h"
#include "StEmcUtil/projection/StEmcPosition.h"
#include "StTofCollection.h"
#include "StBTofCollection.h"
#include "StMuDSTMaker/COMMON/StMuBTofHitCollection.h"
#include "TLine.h"
#include "TCanvas.h"
#include "THelix.h"
#include "TView.h"
#include "TPolyMarker3D.h"
#include "StMuDSTMaker/COMMON/StMuRpsCollection.h"

ClassImp(StPeCEvent)

  StPeCEvent::StPeCEvent(bool useBemc, bool useTOF, bool useVertex, bool useTracks, bool useRP, bool readStMuDst, bool readStEvent, bool readBothInputs) :
    muDst(0), eventP(0), treecalo(0), tofHits(0), tofTracks(0), vertices(0) {
  // StPeCMaxTracks ..... looks more like Fortran......
  pPairs    = new TClonesArray ("StPeCPair", StPeCnMaxTracks);
  sPairs    = new TClonesArray ("StPeCPair", StPeCnMaxTracks);
  tofHits   = new TClonesArray ("StMuBTofHit",5*StPeCnMaxTracks);
  if((useTOFlocal = useTOF)){

    tofTracks = new TClonesArray ("StMuTrack",5*StPeCnMaxTracks);

    LOG_INFO << "StPeCEvent constructor: useTOF ---------- " <<useTOF << endm;
  }
  if((useBemcLocal = useBemc)) {
    treecalo  = new TClonesArray ("StEmcCluster",5*StPeCnMaxTracks);
    LOG_INFO << "StPeCEvent constructor: useBemc ---------- " <<useBemc << endm;

  }
  if((useVertexLocal = useVertex)){
    vertices  = new TClonesArray ("StMuPrimaryVertex",5*StPeCnMaxTracks);
    LOG_INFO << "StPeCEvent constructor: useVertex ---------- " <<useVertex << endm;

  }
  if((useTracksLocal = useTracks)){
 
    LOG_INFO << "StPeCEvent constructor: useTracks ---------- " <<useTracks << endm;
//         tracks    = new TClonesArray ("StPeCTrack",StPeCnMaxTracks);     
    tracks    = new TClonesArray ("StMuTrack", StPeCnMaxTracks);
  }
  if((useRPLocal = useRP)){
 
//     romanPots = new TClonesArray ("StMuRpsCollection", 3);
    LOG_INFO << "StPeCEvent constructor: useRP ---------- " <<useRP << endm;

  }
  if((readStMuDstLocal = readStMuDst)){
 
//     LOG_INFO << "StPeCEvent constructor: readStMuDst ---------- " <<readStMuDst << endm;

  }  if((readStEventLocal = readStEvent)){
 
//     LOG_INFO << "StPeCEvent constructor: readStEvent ---------- " <<readStEvent << endm;

  }  if((readBothInputsLocal = readBothInputs)){
 
    LOG_INFO << "StPeCEvent constructor: both inputs ---------- " <<readBothInputs << endm;

  }



  nPPairs  = 0 ;
  nSPairs  = 0 ;
  nTracks  = 0 ;
  eventP   = 0 ;
  shotCount = 0;
  eventN = 0;
  runN = 0;
  nTot = 0;
  nPrim = 0;
  qTot = 0;
  nGlobalTracks = 0;
  nPrimaryTracks = 0;
  nPrimaryTPC = 0;
  nPrimaryFTPC = 0;
  bField = 0;
  pt = 0;
  xVertex = 0;
  yVertex = 0;
  zVertex = 0;
  rVertex = 0;
  infoLevel = 0;
  nTOFhitsSum = 0;
  nBtofTriggerHitsSum = 0;
  nTOFtracksSum = 0;
  zdcEastUASum = 0;
  zdcWestUASum = 0;
  zdcCoincidenceRateSum = 0;
  lastDSM0Sum = 0;
  lastDSM1Sum = 0;


  LOG_INFO << "StPeCEvent constructor: leaving constructor ---------- " << endm;

}

StPeCEvent::~StPeCEvent() {
  clear() ;
  delete pPairs ;
  delete sPairs ;
  delete tracks ;
  delete tofHits ;
  delete tofTracks;
  delete treecalo;
  delete vertices;
//   delete romanPots;
}


void  StPeCEvent::clear ( ) {
   eventN = 0 ;
   runN   = 0 ;
   nTot   = 0 ;
   nPrim  = 0 ;
   qTot   = 0 ;
   pt     = 0 ;
   xVertex = 0 ;
   yVertex = 0 ;
   zVertex = 0 ;
   rVertex = 0 ;
   nPPairs = 0 ;
   nSPairs = 0 ;
   nTracks = 0 ;

   nTOFhitsSum = 0;
   nBtofTriggerHitsSum = 0;
   nTOFtracksSum = 0;
   zdcEastUASum = 0;
   zdcWestUASum = 0;
   zdcCoincidenceRateSum = 0;
   lastDSM0Sum = 0;
   lastDSM1Sum = 0;

   pPairs->Clear();
   sPairs->Clear();

   if(useTracksLocal)tracks->Clear();
   tofHits->Clear();
   if(useTOFlocal) {

     tofTracks->Clear();
   }
   if(useBemcLocal)treecalo->Clear();
   if(useVertexLocal)vertices->Clear();
}

// The counting of FTPC tracks is not yet debugged
Int_t StPeCEvent::fill ( StEvent *event ) {
  
  eventP = event ;

  
   // I try to get StEvent

    // check if there is a collection
    StEmcCollection *emcStEvent = eventP->emcCollection();


    if(emcStEvent){
        StDetectorId id = static_cast<StDetectorId>(0+kBarrelEmcTowerId);
        StEmcDetector* detector = emcStEvent->detector(id);
        if(detector)  {
            StEmcClusterCollection* coll = detector->cluster();
            if(coll)
            {

                StSPtrVecEmcCluster& clusters = coll->clusters();
                Int_t n = clusters.size();
		cout<<" number of clusters in BEMC "<<n<<endl;
                for(Int_t j = 0;j<n;j++)
                {
                    StEmcCluster *c =clusters[j];
                    if(c)
                    {

                     new((*treecalo)[j]) StEmcCluster((const StEmcCluster &) *c);

		      cout<<" Number of hits in cluster "<<c->nHits()<<endl;
                      cout<<" Energy of cluster "<<c->energy()<<endl;
                      cout<<" eta and phi of cluster "<<c->eta()<<" "<<c->phi()<<endl;
                    }
                }
            }

	}
 
    }
  
  // Set Run and Event Number
  eventN = event->id() ;
  runN   = event->runId(); 
  cout << "StEvent Run ID:  " << runN << endl;
  cout << "StEvent ID: " << eventN << endl;

  bField = event->summary()->magneticField();

  //get trigger info

  Int_t   NGlobal=0; 
  Int_t   NPrimaries=0; 
  Int_t   SumQ=0; 
  Float_t SumPx=0.0; 
  Float_t SumPy=0.0;  
  // separate TPC and FTPC 
  nPrimaryTPC=0;
  nPrimaryFTPC=0;

  // Get the track nodes
  StSPtrVecTrackNode& exnode = event->trackNodes();
  Int_t nnode=exnode.size();


  // local pointer array for track needed for cuts
  for( Int_t in=0; in<nnode; in++ ) {
	UInt_t nprim = exnode[in]->entries(primary);
        UInt_t nglob = exnode[in]->entries(global);

	if( nprim>1 || nglob>1 ){
	   cout<<"There could be a problem! nprim= "<<nprim<<"  nglob= "<<nglob<<endl;
	   
	}
    if( nprim==1 ){
      StTrack *tp = exnode[in]->track(primary);
      // DANGER THAT SHOULD BE DONE IN A PROPPER WAY, CHECK 4 PLACES !
      if (! (tp->flag()>0)) continue;
      // -----------------------------------------------------------
      NPrimaries++;
      float px = tp->geometry()->momentum().x();
      float py = tp->geometry()->momentum().y();
      SumPx = SumPx + px; SumPy = SumPy + py;
      SumQ  = SumQ  + tp->geometry()->charge();
      // 
      // need to dereference the Array pointer first 
      //new((*tracks)[nTracks++]) StPeCTrack(1,tp) ;
      
      if(fabs(tp->geometry()->momentum().pseudoRapidity())<2.0) {
	nPrimaryTPC++;
      } else {
	nPrimaryFTPC++;
      }      
    }
    if( nglob==1 ){
      StTrack *tnp = exnode[in]->track(global);
      // DANGER THAT SHOULD BE DONE IN A PROPPER WAY, CHECK 4 PLACES !

      if (! (tnp->flag()>0)) continue;
//       cout<<" in StPeCEvent after tnp->flag "<< tnp->detectorInfo()->numberOfPoints() <<endl;
//       if ( tnp->detectorInfo()->numberOfPoints() < 11 ) continue ;
//       NGlobal++; 
//       cout<<" in StPeCEvent Nglobal "<<NGlobal<<endl;
//       Do not store tracks in tree any longer PPY 11/5/02
//    new((*tracks)[nTracks++]) StPeCTrack(0,tnp) ;
    }
  }
  // nGlobals not a good UPC criteria any more 

  //  if ( NGlobal > StPeCnMaxTracks ) return 1 ; 
  
  
  if (( nPrimaryTPC > 0 ||  nPrimaryFTPC>0 ) &&   // at least one track in either FTPC or TPC
      ( nPrimaryTPC < StPeCnMaxTracks && nPrimaryFTPC< StPeCnMaxTracks ) &&( nPrimaryFTPC+nPrimaryTPC>=2 ) ) {
    cout << " analyze event !" << endl;
  } else {
    cout << " reject  event !" << endl;
    return 1; 
  } 
  
  nPrim             = NPrimaries ;
  nTot              = NGlobal  ;
  qTot = SumQ ;
  pt   = ::sqrt( SumPx*SumPx + SumPy*SumPy );
  
  nGlobalTracks=  NPrimaries;
  nPrimaryTracks= NGlobal;

  

  StPrimaryVertex* vtx = event->primaryVertex();
  if(vtx) {
    cout << "Vertex flag " << vtx->flag() << endl;    
    xVertex = vtx->position().x();
    yVertex = vtx->position().y();
    zVertex = vtx->position().z();
    rVertex = ::sqrt(xVertex*xVertex + yVertex*yVertex);
    
    if ( infoLevel > 1 ) {
      cout << "StPeCEvent : primary vertex x:" << xVertex << " y: "  <<  yVertex  << " z: " << zVertex << " r: " << rVertex <<endl;
    }
  }   else {
    xVertex = -9999. ;
    yVertex = -9999. ;
    zVertex = -9999. ;
    rVertex = -9999. ;
    cout<<"StPeCEvent: There was no primary vertex!"<<endl;
  }
 
  // HERE  flag must  be tested. 
  StPeCPair* lPair ; 
  nPPairs = 0 ;
  StTrack *trk1, *trk2 ;
  for( Int_t i1=0; i1<nnode-1; i1++ ) {
     if( exnode[i1]->entries(primary) !=1 ) continue ;
     trk1 = exnode[i1]->track(primary);

     if ( trk1->detectorInfo()->numberOfPoints() < 11 ) continue ; 
     for( Int_t i2=i1+1; i2<nnode; i2++ ) {
        if( exnode[i2]->entries(primary) !=1 ) continue ;
        trk2 = exnode[i2]->track(primary);

        if ( trk2->detectorInfo()->numberOfPoints() < 11 ) continue ; 
	// DANGER 
	if (! (trk1->flag()>0)) continue;
	if (! (trk2->flag()>0)) continue;
	// --------
	// get pointer to memebr ?
        // TClonesArray &ppairs = *pPairs;
        // lPair = new(ppairs[nPPairs++]) StPeCPair(trk1,trk2,1,event) ;
        lPair = new((*pPairs)[nPPairs++]) StPeCPair(trk1,trk2,1,event) ;
	cout<<"in StPeCEvent lPair written "<<endl;
	//#ifdef PECPRINT
        cout << "StPeCEvent : Primary Pair : " 
           << "  sumQ = " << lPair->getSumCharge()
           << "  sumPt = " << lPair->getSumPt()
           << "  mInv = " << lPair->getMInv(pion)
           << "  opening angle = " << lPair->getOpeningAngle()
           << "  cos(theta*) = " << lPair->getCosThetaStar(pion) << endl;
	//#endif	   
     }
  }
  //removed secondary pairs:

  return 0 ;
}

//===========================================================================================
Int_t StPeCEvent::fill(StMuDst *mudst) {

  TObjArray* muTracks = 0;
  StMuEvent* event = 0;
  StMuTrack *tp = 0;
  Bool_t acceptEvent;

   //Save the event reference
   muDst = mudst;
   event = muDst->event();

   //Set Run and Event Number
   eventN = event->eventInfo().id();
   
   runN = event->eventInfo().runId(); 
   LOG_INFO << "StPeCEvent fill(muDst ): useBemc ---------- " <<useBemcLocal << endm;
   LOG_INFO << "StPeCEvent fill(muDst ): useTOF ---------- " <<useTOFlocal << endm;
   LOG_INFO << "StPeCEvent fill(muDst ): useVertex ---------- " <<useVertexLocal << endm;
   LOG_INFO << "StPeCEvent fill(muDst ): useTracks ---------- " <<useTracksLocal << endm;
   LOG_INFO << "StPeCEvent fill(muDst ): useRP ---------- " <<useRPLocal << endm;
   LOG_INFO << "StPeCEvent fill(muDst) : TOF geometry pointer ---------- " << mTOFgeoEv<<endm;
   LOG_INFO << "StMuEvent Run ID: " << runN << endm;
   LOG_INFO << "StMuEvent ID: " << eventN << endm;
   
   bField = event->eventSummary().magneticField();
  if (fabs(bField)<0.01)   LOG_INFO << "StPeCEvent fill(muDst ): BField off "  << endm;;
  acceptEvent = kFALSE;
   // RD 11-July 2013 to get BEMC data need to find it out of StMuDst (TO DO)
    // Get EMC calorimeter clusters from StEvent  RD

    // check if there is a collection
    StMuEmcCollection *emcStEvent = mudst->muEmcCollection();

    if(emcStEvent){

	    Int_t n = emcStEvent->getNClusters(1);//enum {bemc=1, bprs=2, bsmde=3, bsmdp=4, eemc=5, eprs=6, esmdu=7, esmdv=8};

	    LOG_INFO<<"StPeCEvent::fill(MuDst)  number of clusters in BEMC "<<n<<endm;
	    for(Int_t j = 0;j<n;j++)
	      {
		StMuEmcCluster *c = emcStEvent->getCluster(j, 1); 
		if(c)
		  {
		    if(useBemcLocal){
		      new((*treecalo)[j]) StMuEmcCluster((const StMuEmcCluster &) *c);
		    }

		  }
	      }

    }    //if emc
    //
    //here we transfer TOF information in StMuEvent to UPC ntuple  RD
    //

     LOG_INFO <<"StPeCEvent::fill number of btof hits "<<mudst->numberOfBTofHit()<<endm;
    int nMax = mudst->numberOfBTofHit();
    int globalTrackCounter = 0;
    for(int i=0;i<nMax;i++) {
      StMuBTofHit *aHit = (StMuBTofHit *)mudst->btofHit(i);
		if(aHit)
		  {
		    new((*tofHits)[i]) StMuBTofHit((const StMuBTofHit &) *aHit);
		    if(useTOFlocal) {

		      // global track that matched the hit
		      int trayId = aHit->tray();
		      if(trayId<=120&&trayId>=0) {//TOF
			StMuTrack *TofGlobalTrack = aHit->globalTrack();
			if(!TofGlobalTrack) continue;
			new((*tofTracks)[globalTrackCounter]) StMuTrack((const StMuTrack &) *TofGlobalTrack);
			globalTrackCounter++;
		      }
		    }

		  }
    }
   
    //
    // write RP collection to output tree
    StMuRpsCollection *rp = (StMuRpsCollection*)mudst->RpsCollection();
    if(!rp)    LOG_INFO << "StPeCEvent fill(mudst): No Roman Pot collections " << endm;
//     if(rp){
//       new((*romanPots)) StMuRpsCollection((const StMuRpsCollection &) *rp);
//       romanPots = rp;
//     }
   // number of vertices not a good number anymore ! FLK 07/03
   // if ( event->eventSummary().numberOfVertices() ) {
   
   StThreeVectorF vtx = event->primaryVertexPosition();
   // vertex is set to 0,0,0 of no prim vertex found  FLK 07/03
   if(vtx.x() !=0 && vtx.y()!=0 && vtx.z()!=0) {
     xVertex = vtx.x();
     yVertex = vtx.y();
     zVertex = vtx.z();
     rVertex = ::sqrt(xVertex*xVertex + yVertex*yVertex);
   }
   else {
     xVertex = -9999.;
     yVertex = -9999.;
     zVertex = -9999.;
     rVertex = -9999.;
   }
   
   // directly from StEvent Summary 
   nGlobalTracks= event->eventSummary().numberOfGoodTracks();
   nPrimaryTracks= event->eventSummary().numberOfGoodPrimaryTracks();
   nPrimaryTPC=0;
   nPrimaryFTPC=0;
   
   //Retrieve the primary tracks
   muTracks = muDst->primaryTracks();
   // nPrim    = muTracks->GetLast() + 1; // this ignores quality check FLK
   // backwards compatibility, FLK
   nPrim    = nPrimaryTracks; 
   nTot     = nGlobalTracks;
   size_t Nvert = muDst->numberOfPrimaryVertices();
   LOG_INFO << "StPeCEvent::fill(event mudst) #vertices: "  <<Nvert<< endm; 
   zdcCoincidenceRateSum = mudst->event()->runInfo().zdcCoincidenceRate();
   const StTriggerData * trigData;
   trigData =  const_cast< StTriggerData *> (mudst->event()->triggerData());

   if(!trigData) {
     LOG_ERROR << "In StPeCEvent summary: StTriggerData not available in StMuDst "<< endm;
     return 1;  //reject event
   }
   lastDSM0Sum = trigData->lastDSM(0);
   lastDSM1Sum = trigData->lastDSM(1);
   // attenuated signals 
   zdcWestUASum = trigData->zdcAttenuated(west);
   zdcEastUASum = trigData->zdcAttenuated(east);
   //TOF hits as seen by trigger (OR of 8 cells)
   nBtofTriggerHitsSum =  trigData->tofMultiplicity();
   nTOFtracksSum = globalTrackCounter;
    //
    // select tracks that match TOF hits and reconstruct vertex  RD
    //
   //  if(Nvert>=1&&nPrimaryTracks<20)matchTOFhitsToTracks(mudst);

   //
   //RD as I found the code, it only reads the tracks related to the last vertex
   //I do not know yet if the best vertex is placed at the end
   //I will now read all vertices and try all tracks
   //26-MAR-2010


   nPPairs = 0 ;
   for (size_t verti = 0;verti<Nvert;++verti){
     //LOG_INFO << "StPeCEvent::  vertex Index: "<<verti<<endm;
     StMuPrimaryVertex* V= muDst->primaryVertex(verti);
     //
     if(useVertexLocal){
       new((*vertices)[verti]) StMuPrimaryVertex((const StMuPrimaryVertex &) *V);
     }
     //    assert(V);
     //      Float_t rank = V->ranking();
     LOG_INFO << "StPeCEvent::  vertex index: "<<verti<<endm;
     StMuDst::setVertexIndex(verti);                                //assert vertex; this selects tracks connected to this vertex
     size_t Ntracks = muDst->primaryTracks()->GetEntries();
//      LOG_INFO << "StPeCEvent::fill(ev mudst) #track in vertex : "  <<Ntracks<< endm;
     nPrimaryTPC = 0;
     nPrimaryFTPC = 0;
     for (size_t trackiter = 0;trackiter<Ntracks;trackiter++){
       tp = ( StMuTrack*)muDst->primaryTracks(trackiter); 

       if (! (tp->flag()>0)) continue; // Quality check on the track
         
       if(fabs(tp->eta())<2.0) {
	 nPrimaryTPC++;
       } else {
	 nPrimaryFTPC++;
       }
     
       // do not fill track list for now to save space 
       // nTracks should be the same as nPrim afterward
       if(useTracksLocal){
//           new((*tracks)[nTracks++])StPeCTrack(0, tp);  //old scheme
// 	 new((*tracks)[nTracks++]) StPeCTrack((const StPeCTrack &) *tp);
	 new((*tracks)[nTracks++]) StMuTrack((const StMuTrack &) *tp);

       }
     }    // loop over tracks in vertex
  

   if (( nPrimaryTPC > 0 ||  nPrimaryFTPC>0 ) &&   // at least one track in either FTPC or TPC
       ( nPrimaryTPC < StPeCnMaxTracks && nPrimaryFTPC< StPeCnMaxTracks )&& (nPrimaryFTPC+nPrimaryTPC>=2)) {
     LOG_INFO << " analyze vertex !" << endm;
     acceptEvent = kTRUE;
     //     nPPairs = 0 ;
     StPeCPair* lPair ; 
     StMuTrack *trk1, *trk2 ;
     muTracks = muDst->primaryTracks();
     for(int i1 = 0; i1  <= muTracks->GetLast(); i1++) {
       trk1 = (StMuTrack*)muTracks->UncheckedAt(i1);
       for(int i2 = i1+1; i2  <= muTracks->GetLast(); i2++) {
	 trk2 = (StMuTrack*)muTracks->UncheckedAt(i2);

       
	 // DANGER 
	 if (! (trk1->flag()>0)) continue;
	 if (! (trk2->flag()>0)) continue;
	 //LOG_INFO << " wrote the pair !" << endm;

	 lPair = new((*pPairs)[nPPairs++]) StPeCPair(trk1,trk2,1,event, mTOFgeoEv) ;
 

       }  //lopp mutracks 2
     } //loop mutracks 1
     LOG_INFO << " number of pairs " << nPPairs<<"  in vertex index "<<verti<<endm;
   } else {   //accept ev
     LOG_INFO << " reject  vertex !" << endm;
     // return 1; 
   }    

   
#ifdef SPAIRS
   // HERE  flag must  be tested. 
   nSPairs = 0 ;
   StMuTrack *muTrk1, *muTrk2 ;
   muTracks = muDst->globalTracks();

   for(int i1 = 0; i1 < nTot; i1++) {
     muTrk1 = (StMuTrack*)muTracks->UncheckedAt(i1);
     for(int i2 = i1+1; i2 < nTot; i2++) {
       muTrk2 = (StMuTrack*)muTracks->UncheckedAt(i2);
	 // DANGER 
       if (! (muTrk1->flag()>0)) continue;
       if (! (muTrk2->flag()>0)) continue;
       // --------
       // get pointer to memebr ?
       // TClonesArray &ppairs = *pPairs;
       lPair = new((*sPairs)[nSPairs++]) StPeCPair(muTrk1,muTrk2,0,event) ;

#ifdef PECPRINT
       cout << "StPeCEvent : Primary Pair : " 
	    << "  sumQ = " << lPair->getSumCharge()
	    << "  sumPt = " << lPair->getSumPt()
	    << "  mInv = " << lPair->getMInv(pion)
	    << "  opening angle = " << lPair->getOpeningAngle()
	    << "  cos(theta*) = " << lPair->getCosThetaStar(pion) << endl;
#endif	   
     }
   }
#endif

   //   return 0;  RD 4NOV2013
   }   // loop over vertices
    if(acceptEvent){
      return 0;
    }
    if(!acceptEvent){
      return 1;
    }
    return 0;
}

  std::vector<int>  StPeCEvent::fill(StEvent * eventP, StMuDst *mudst) {


  TObjArray* muTracks = 0;
  StThreeVectorD position,momentum;
  StMuEvent* event = 0;
  StMuTrack *tp = 0;
  Bool_t acceptEvent;
  int countAccVtx           = 0;
  int countVtxWithTOF       = 0;
  int countRejectVtxWithTOF = 0;
  int saveNumTracks         = 0;
  float saveRejectedVtxZ    = 0.;
  float saveAccepteVtxZ     = 0.;

  std::vector<int> vector(15);

  LOG_INFO << "StPeCEvent fill(muDst StEvent): useBemc ---------- " <<useBemcLocal << endm;
  LOG_INFO << "StPeCEvent fill(muDst StEvent): useTOF ---------- " <<useTOFlocal << endm;
  LOG_INFO << "StPeCEvent fill(muDst StEvent): useVertex ---------- " <<useVertexLocal << endm;
  LOG_INFO << "StPeCEvent fill(muDst StEvent): useTracks ---------- " <<useTracksLocal << endm;
  LOG_INFO << "StPeCEvent fill(muDst StEvent): useRP ---------- " <<useRPLocal << endm;
  LOG_INFO << "StPeCEvent fill(muDst StEvent): TOF geometry pointer ---------- " << mTOFgeoEv<<endm;
  //
  // Get Magnetic field from event summary
  //
  Double_t bFld;  
  StEventSummary* summary = eventP->summary();  
  if(summary) bFld=summary->magneticField()/10.;  
  if (fabs(bFld)<0.01)   LOG_INFO << "StPeCEvent fill(muDst StEvent): BField off "  << endm;;
  acceptEvent = kFALSE;
 
    // Get EMC calorimeter clusters from StEvent  RD

    // check if there is a collection
    StEmcCollection *emcStEvent = eventP->emcCollection();

    if(emcStEvent){
      StDetectorId id = static_cast<StDetectorId>(0+kBarrelEmcTowerId);
      StEmcDetector* detector = emcStEvent->detector(id);
      if(detector)  {
	StEmcClusterCollection* coll = detector->cluster();
	if(coll)
	  {

	    StSPtrVecEmcCluster& clusters = coll->clusters();
	    Int_t n = clusters.size();
// 	    LOG_INFO<<"StPeCEvent::fill  number of clusters in BEMC "<<n<<endm;
	    for(Int_t j = 0;j<n;j++)
	      {
		StEmcCluster *c =clusters[j];
		if(c)
		  {
		    if(useBemcLocal){
		      new((*treecalo)[j]) StEmcCluster((const StEmcCluster &) *c);
		    }

		  }
	      }
	  }

      }  // if detector
    }    //if emc
  
    //
    //here we transfer TOF information in StMuEvent to UPC ntuple  RD
    //

//      LOG_INFO <<"StPeCEvent::fill number of btof hits "<<mudst->numberOfBTofHit()<<endm;
     nTOFhitsSum = mudst->numberOfBTofHit();
 
    int nMax = mudst->numberOfBTofHit();
    int globalTrackCounter = 0;
    for(int i=0;i<nMax;i++) {
      StMuBTofHit *aHit = (StMuBTofHit *)mudst->btofHit(i);
		if(aHit)
		  {
		    new((*tofHits)[i]) StMuBTofHit((const StMuBTofHit &) *aHit);
		    if(useTOFlocal) {

		      // global track that matched the hit
		      int trayId = aHit->tray();
		      if(trayId<=120&&trayId>=0) {//TOF
			StMuTrack *TofGlobalTrack = aHit->globalTrack();
			if(!TofGlobalTrack) continue;
			new((*tofTracks)[globalTrackCounter]) StMuTrack((const StMuTrack &) *TofGlobalTrack);
			globalTrackCounter++;
		      }
		    }

		  }
    }
    // write RP collection to output tree
    StMuRpsCollection *rp = (StMuRpsCollection*)mudst->RpsCollection();
    if(!rp)    LOG_INFO << "StPeCEvent fill(mudst StEvent): No Roman Pot collections " << endm;
//     if(rp){
//       new((*romanPots)) StMuRpsCollection((const StMuRpsCollection &) *rp);
//       romanPots = rp;
//     }

   //Save the event reference
   muDst = mudst;
   event = muDst->event();
  zdcCoincidenceRateSum = mudst->event()->runInfo().zdcCoincidenceRate();
  const StTriggerData * trigData;
  trigData =  const_cast< StTriggerData *> (mudst->event()->triggerData());

  if(!trigData) {
    LOG_ERROR << "In StPeCEvent summary: StTriggerData not available in StMuDst StEvent"<< endm;
    return vector;
  }
    lastDSM0Sum = trigData->lastDSM(0);
    lastDSM1Sum = trigData->lastDSM(1);
    // attenuated signals 
    zdcWestUASum = trigData->zdcAttenuated(west);
    zdcEastUASum = trigData->zdcAttenuated(east);
    //TOF hits as seen by trigger (OR of 8 cells)
    nBtofTriggerHitsSum =  trigData->tofMultiplicity();
    nTOFtracksSum = globalTrackCounter;
   //Set Run and Event Number
   eventN = event->eventInfo().id();
   
   runN = event->eventInfo().runId(); 
   LOG_INFO <<"StEvent StMuEvent Run ID: " << runN <<  " StMuEvent ID: " << eventN << endm;
//    LOG_INFO << "StMuEvent ID: " << eventN << endm;
   LOG_INFO << "Found: " << globalTrackCounter <<" tracks with TOF hit match" << endm;
   
   bField = event->eventSummary().magneticField();
   
   // number of vertices not a good number anymore ! FLK 07/03
   // if ( event->eventSummary().numberOfVertices() ) {
   nVertices = event->eventSummary().numberOfVertices();
   StThreeVectorF vtx = event->primaryVertexPosition();
   // vertex is set to 0,0,0 of no prim vertex found  FLK 07/03
   if(vtx.x() !=0 && vtx.y()!=0 && vtx.z()!=0) {
     xVertex = vtx.x();
     yVertex = vtx.y();
     zVertex = vtx.z();
     rVertex = ::sqrt(xVertex*xVertex + yVertex*yVertex);
   }
   else {
     xVertex = -9999.;
     yVertex = -9999.;
     zVertex = -9999.;
     rVertex = -9999.;
   }
   
   // directly from StEvent Summary 
   nGlobalTracks= event->eventSummary().numberOfGoodTracks();
   nPrimaryTracks= event->eventSummary().numberOfGoodPrimaryTracks();
   nPrimaryTPC=0;
   nPrimaryFTPC=0;
   
   //Retrieve the primary tracks
   muTracks = muDst->primaryTracks();
   // nPrim    = muTracks->GetLast() + 1; // this ignores quality check FLK
   // backwards compatibility, FLK
   nPrim    = nPrimaryTracks; 
   nTot     = nGlobalTracks;

   size_t Nvert = muDst->numberOfPrimaryVertices();
   LOG_INFO << "StPeCEvent::fill(event mudst) #vertices: "  <<Nvert<< endm; 
    //
    // select tracks that match TOF hits and reconstruct vertex  RD
    //
   //  if(Nvert>=1&&nPrimaryTracks<20)matchTOFhitsToTracks(mudst);

   //
   //RD as I found the code, it only reads the tracks related to the last vertex
   //I do not know yet if the best vertex is placed at the end
   //I will now read all vertices and try all tracks
   //26-MRA-2010
//    for(int i = 0; i <= muTracks->GetLast(); i++) {
//      tp = (StMuTrack*)muTracks->UncheckedAt(i);
     //
   nPPairs = 0 ;
   for (size_t verti = 0;verti<Nvert;++verti){
     //LOG_INFO << "StPeCEvent::  vertex Index: "<<verti<<endm;
     StMuPrimaryVertex* V= muDst->primaryVertex(verti);
     //
     if(useVertexLocal){
       new((*vertices)[verti]) StMuPrimaryVertex((const StMuPrimaryVertex &) *V);
     }
     //    assert(V);
     //      Float_t rank = V->ranking();

     LOG_INFO << "StPeCEvent:: loop vertices #BEMC match: "  <<V->nBEMCMatch()<<" num tracks used: "<<V->nTracksUsed()<< endm; 
     StMuDst::setVertexIndex(verti); 
                               //assert vertex; this selects tracks connected to this vertex
     size_t Ntracks = muDst->primaryTracks()->GetEntries();
     LOG_INFO << "StPeCEvent::fill(ev mudst) #track in vertex : "  <<Ntracks<<" vertex index "<<verti<< endm;
     nPrimaryTPC = 0;
     nPrimaryFTPC = 0;
     int countTOF = 0;
     for (int trackiter = 0;trackiter<Ntracks;trackiter++){
       tp = ( StMuTrack*)muDst->primaryTracks(trackiter); 
       if(tp->index2BTofHit()>-1){
	 LOG_INFO << "StPeCEvent:: a tracks with TOF match -----index to global  "  <<tp->index2Global()<<" num BTof hits "<<mudst->numberOfBTofHit()<<endm;
	 countTOF++;
       }
       if (! (tp->flag()>0)) continue; // Quality check on the track
//        LOG_INFO << "StPeCEvent:: a tracks key nPrimary loop "<<tp->id()<<endm; 
       if(fabs(tp->eta())<2.0) {
	 nPrimaryTPC++;
       } else {
	 nPrimaryFTPC++;
       }
     
       // do not fill track list for now to save space 
       // nTracks should be the same as nPrim afterwards
       if(useTracksLocal){
//           new((*tracks)[nTracks++])StPeCTrack(0, tp);  //old scheme
// 	 new((*tracks)[nTracks++]) StPeCTrack((const StPeCTrack &) *tp);
	 new((*tracks)[nTracks++]) StMuTrack((const StMuTrack &) *tp);

       }
     }
   LOG_INFO << "Number of primary  TPC  tracks: " << nPrimaryTPC << " FTPC tracks " << nPrimaryFTPC<<endm; 
//    LOG_INFO << "Number of primary  tracks event summary: " << nPrimaryTracks << " global tracks " << nGlobalTracks<<endm;  
   
   //
   // RD 19 APR 2013 comment this and modify to work with UPCpp to make pairs with tracks that have TOF hit match  **START**
   if (( nPrimaryTPC > 0 ||  nPrimaryFTPC>0 ) &&   // at least one track in either FTPC or TPC
       ( nPrimaryTPC < StPeCnMaxTracks && nPrimaryFTPC< StPeCnMaxTracks )&& (nPrimaryFTPC+nPrimaryTPC>=2)) {

     acceptEvent = kTRUE;
     countAccVtx++;
     if(countTOF>0) {
       countVtxWithTOF++;
       saveAccepteVtxZ = V->position().z();
  }
     LOG_INFO << " analyze vertex !  " <<countAccVtx<< endm;
     //     nPPairs = 0 ;
     StPeCPair* lPair ; 
     StMuTrack *trk1, *trk2 ;
     muTracks = muDst->primaryTracks();
     for(int i1 = 0; i1  <= muTracks->GetLast(); i1++) {
       trk1 = (StMuTrack*)muTracks->UncheckedAt(i1);
//        LOG_INFO << "StPeCEvent:: a tracks key lPair loop "<<trk1->id()<<endm; 
       for(int i2 = i1+1; i2  <= muTracks->GetLast(); i2++) {
	 trk2 = (StMuTrack*)muTracks->UncheckedAt(i2);

	 // DANGER 
	 if (! (trk1->flag()>0)) continue;
	 if (! (trk2->flag()>0)) continue;


	 lPair = new((*pPairs)[nPPairs++]) StPeCPair(trk1,trk2,1,event, eventP, mTOFgeoEv) ;
 	 LOG_INFO << " wrote the pair !  nPPair: " <<nPPairs<< endm;

       }  //lopp mutracks 2
     } //loop mutracks 1
   } else {   //accept ev
     LOG_INFO << " reject  vertex !" << endm;
     if(countTOF>0)  {
       countRejectVtxWithTOF++;
       saveNumTracks = nPrimaryTPC;
       saveRejectedVtxZ = V->position().z();
     }
     // return 1; 
   }                                                    //    **END**


   } //verti loop 

   
#ifdef SPAIRS
   // HERE  flag must  be tested. 
   nSPairs = 0 ;
   StMuTrack *muTrk1, *muTrk2 ;
   muTracks = muDst->globalTracks();

   for(int i1 = 0; i1 < nTot; i1++) {
     muTrk1 = (StMuTrack*)muTracks->UncheckedAt(i1);
     for(int i2 = i1+1; i2 < nTot; i2++) {
       muTrk2 = (StMuTrack*)muTracks->UncheckedAt(i2);
	 // DANGER 
       if (! (muTrk1->flag()>0)) continue;
       if (! (muTrk2->flag()>0)) continue;
       // --------
       // get pointer to member ?
       // TClonesArray &ppairs = *pPairs;
       lPair = new((*sPairs)[nSPairs++]) StPeCPair(muTrk1,muTrk2,0,event) ;

#ifdef PECPRINT
       cout << "StPeCEvent : Primary Pair : " 
	    << "  sumQ = " << lPair->getSumCharge()
	    << "  sumPt = " << lPair->getSumPt()
	    << "  mInv = " << lPair->getMInv(pion)
	    << "  opening angle = " << lPair->getOpeningAngle()
	    << "  cos(theta*) = " << lPair->getCosThetaStar(pion) << endl;
#endif	   
     }
   }
#endif
   LOG_INFO << "Exiting StPeCEvent::fill(StEvent *event, StMuDst *mudst) acceptEvent  " << countAccVtx<<endm;

   // transfer counts to returning vector
   vector[0] = countAccVtx;
   vector[1] = countVtxWithTOF;
   vector[2] = countRejectVtxWithTOF;
   vector[3] = Nvert;
   vector[4] = saveNumTracks;
   vector[5] = saveRejectedVtxZ;
   vector[6] = saveAccepteVtxZ;

    if(acceptEvent){
      matchTOFhitsToTracks(mudst);
      return vector;
    }
    if(!acceptEvent){
      //     matchTOFhitsToTracks(mudst);
      return vector;
    }
    return vector;
}

StPeCPair* StPeCEvent::getPriPair ( Int_t i ) {
   if ( i < 0 || i >= nPPairs ) return 0 ;
   TClonesArray &pairs = *pPairs;
   return (StPeCPair *)pairs[i] ;
}

StPeCPair* StPeCEvent::getSecPair ( Int_t i ) {
   if ( i < 0 || i >= nSPairs ) return 0 ;
   TClonesArray &pairs = *sPairs;
   return (StPeCPair *)pairs[i] ;
}

void StPeCEvent::reset() { 
   delete pPairs ;
   delete sPairs ;
   delete tracks ;
   pPairs  = 0 ;
   sPairs  = 0 ;
   nPPairs  = 0 ;
   nSPairs  = 0 ;
   tracks = 0 ;
   nTracks = 0 ;
}

Long_t  StPeCEvent::eventNumber() const{ return eventN; }
Long_t  StPeCEvent::runNumber() const{ return runN; }
Int_t   StPeCEvent::getNTot() const{ return nTot; }
Int_t   StPeCEvent::getNPrim() const{ return nPrim; }
Int_t   StPeCEvent::getQTot() const{ return qTot; }
Float_t StPeCEvent::getPt() const{ return pt; }
Float_t StPeCEvent::getXVertex() const{ return xVertex; }
Float_t StPeCEvent::getYVertex() const{ return yVertex; }
Float_t StPeCEvent::getZVertex() const{ return zVertex; }
#ifndef __CINT__


StLorentzVectorF StPeCEvent::getEvent4Momentum(StPeCSpecies pid) const{
  Float_t mptcle=0.0;
  if(pid==pion){
    mptcle = pion_plus_mass_c2;
  }
  if(pid==kaon){
    mptcle = 493.677*MeV;
  }
  if(pid==proton){
    mptcle = proton_mass_c2;
  }
  if(pid==electron){
    mptcle = electron_mass_c2;
  }
  if(pid==muon){
    mptcle = 105.6584*MeV; 
  }
  StLorentzVectorF p4event(0.0,0.0,0.0,0.0);

  StSPtrVecTrackNode& exnode = eventP->trackNodes();
  if ( !eventP ) {
     printf ( "StPeCEvent::getEvent4Momentum eventP null \n" ) ;
     return p4event ;
  }
  Int_t nnode=exnode.size();

  for( Int_t in=0; in<nnode; in++ ) {
    if( exnode[in]->entries(global) != 1 ) continue ;
    StTrack* trk = exnode[in]->track(primary);
    StThreeVectorF p = trk->geometry()->momentum();
    Float_t energy = p.massHypothesis(mptcle);
    StLorentzVectorF pfour(energy,p);
    p4event = p4event + pfour;
  }

  return p4event;
}
#endif /*__CINT__*/

Float_t StPeCEvent::mInv(StPeCSpecies pid) const{

  StLorentzVectorF p4event = getEvent4Momentum(pid);

  return p4event.m();
}

Float_t StPeCEvent::yRap(StPeCSpecies pid) const{
 

  StLorentzVectorF p4event = getEvent4Momentum(pid);

  return p4event.rapidity();
}

void StPeCEvent::matchTOFhitsToTracks(StMuDst *mudst) {

  TObjArray* priTracks = 0;
  TObjArray* gloTracks = 0;
//   StMuTrack *trk;
  StMuEvent* event = 0;
  Int_t snapLimit =  0;
  Float_t angleHit;

  if (gROOT->IsBatch()) return;

  event = mudst->event();
  //Set Run and Event Number
  eventN = event->eventInfo().id();
   
  runN = event->eventInfo().runId(); 
  LOG_INFO << "-------------- got into matchTOFhitsToTracks number of shots "<<shotCount<< endm;
  TDirectory * saveDir = gDirectory;
    //
    //create a canvas to draw the event and then save to pdf file
    //

  TCanvas * cSnap;
  TCanvas * cSnap3D;

  int nMax = mudst->numberOfBTofHit();
  if(shotCount<snapLimit && snapLimit >0) {
    //
    //create a canvas to draw the event and then save to pdf file
    //

    cSnap   = new TCanvas("cSnap",  "Snapshot", 600, 600);
    cSnap3D = new TCanvas("cSnap3D","Snapshot3D", 600, 600);

    gDirectory->pwd();
    gDirectory->cd("test1.tree.root:/snapShots/");
    LOG_INFO << "StPeCEvent::matchTOFhitsToTracks after directory: " << endm;
    gDirectory->pwd();
    //    gDirectory->ls();
    TH2F * hist = (TH2F*)gDirectory->FindObject(Form("snapShot%d",shotCount));
    if(!hist)LOG_INFO << "FindObject failed " << endm;
    LOG_INFO << "StPeCEvent::matchTOFhitsToTracks name of hist: " << hist->GetName()<<endm;

    StMuTriggerIdCollection  tt=mudst->event()->triggerIdCollection();

    const StTriggerId  ttid= tt.nominal();
    hist->SetTitle(Form("Run: %d Event number %d Main: %d topo: %d #tof: %d",runN, eventN, ttid.isTrigger(400631), ttid.isTrigger(1), nMax));
    cSnap->SetName(Form("snapShot%d",shotCount));
    cSnap3D->SetName(Form("snapShot%d_3D",shotCount));
    cSnap->Draw();
    //  cSnap3D->Draw();
    cSnap->cd();
    hist->Draw();
  }


  priTracks = mudst->primaryTracks();
  //LOG_INFO << "StPeCEvent::matchTOFhitsToTracks #primary tracks: "  <<priTracks->GetEntries()<< endm;
   gloTracks = mudst->globalTracks();

//   int nGlobalTracks  = gloTracks->GetEntries();
//   int nPrimaryTracks = priTracks->GetEntries();
  int globalTrackCounter = 0;
  //
  //loop on vertex tracks to find those that match TOF hits
  //
  size_t Nvert = mudst->numberOfPrimaryVertices();

  gFile->cd("test1.tree.root:/snapShots");
  //gDirectory->ls();

  TH1F * histV = (TH1F*)gDirectory->FindObject("hNumVtx");

  histV->Fill(Nvert);
  LOG_INFO << "StPeCEvent::matchTOFhitsToTracks #vertices: "  <<Nvert<< endm; 
  if(shotCount<snapLimit){
  cSnap3D->cd();
  Double_t  xyz[3]={0.,0.,0.};
  Double_t  v[3]={0.,0.,0.};
  Double_t angularFreq = 0.;
  Double_t range[2] = {0., 0.3};

   for (size_t verti = 0;verti<Nvert;++verti){
     LOG_INFO << "StPeCEvent::  vertex Index: "<<verti<<endm;
     StMuDst::setVertexIndex(verti);
     size_t Ntracks = mudst->primaryTracks()->GetEntries();
     //     LOG_INFO << "StPeCEvent::matchTOFhitsToTracks #track in vertex : "  <<Ntracks<< endm;
     for (size_t trackiter = 0;trackiter<Ntracks;trackiter++){
       StMuTrack* track = mudst->primaryTracks(trackiter);
       //LOG_INFO << "Y track end-point : "  <<track->lastPoint().y()<< " start point "<< track->firstPoint().y()<<endm;
       LOG_INFO << "track start-point x : "  <<track->firstPoint().x()<< " start point y "<< track->firstPoint().y()<< " start point z "<< track->firstPoint().z()<<endm;
       LOG_INFO << "track charge : "  <<track->charge()<< " mag field "<< event->eventSummary().magneticField()<<endm;
       LOG_INFO << "track momentum x : "  <<track->momentum().x()<< " p y "<< track->momentum().y()<< " p z "<< track->momentum().z()<<endm;
       if(shotCount<snapLimit) {
	 TLine * straightTrack = new TLine(track->firstPoint().z(), track->firstPoint().y(), track->lastPoint().z(), track->lastPoint().y());
	 straightTrack->SetLineColor(verti+1);

// 	 THelix * myHelix = new THelix(track->helix().origin().x(),track->helix().origin().y(),
//                                        track->helix().origin().z(),
//                                        track->momentum().x(),track->momentum().y(),
//                                        track->momentum().z(),track->charge()*track->helix().h());
// 	 THelix * myHelix = new THelix(track->firstPoint().x(),track->firstPoint().y(),
//                                        0.,
//                                        track->momentum().x(),track->momentum().y(),
//                                        track->momentum().z(),track->charge()*event->eventSummary().magneticField());
	 xyz[0] = track->firstPoint().x();
	 xyz[1] = track->firstPoint().y();
	 xyz[2] = track->firstPoint().z();
	 v[0] = track->momentum().x();
	 v[1] = track->momentum().y();
	 v[2] = track->momentum().z();
	 angularFreq = track->charge()*event->eventSummary().magneticField();
	 THelix * myHelix = new THelix(&xyz[0],&v[0],angularFreq, &range[0],kHelixZ);
	 gPad->SetFillColor(37);
	 // myHelix->SetRange(0.,0.1, kHelixZ);
       	 myHelix->Draw();
       }



     }  // loop over primary tracks

   }    // loop over vertices
   Float_t radTOF = 0.06;
   //test polymarker
   TPolyMarker3D *pm = new TPolyMarker3D(360, 20);
   pm->SetMarkerSize(0.001);
   for(int deg=0;deg<360;deg++){
     pm->SetPoint(deg, radTOF*TMath::Cos(deg),radTOF*TMath::Sin(deg),0.);
   }
   pm->Draw();

   TPolyMarker3D *pmTOF = new TPolyMarker3D(60, 20);
   pmTOF->SetMarkerSize(0.004);
   pmTOF->SetMarkerColor(kBlue);

   for(int i=0;i<nMax;i++) {
     StMuBTofHit *aHit = (StMuBTofHit *)mudst->btofHit(i);
     if(aHit)
       {
	 // global track that matched the hit
	 int trayId = aHit->tray();
	 if(trayId<=120&&trayId>=0) {//TOF
	   LOG_INFO << "StPeCEvent::matchTOFhitsToTracks tray id: "<<trayId<<" index i "<<i<<endm;
	   trayId -= 1;
	   angleHit = (72-trayId*6)*(3.14/180.);
	   if(trayId>59) {
	     trayId -= 61;
	     angleHit = (108+trayId*6)*(3.14/180.);
	   }
	   LOG_INFO << "StPeCEvent::matchTOFhitsToTracks angle: "<<angleHit<<endm;
	   pmTOF->SetPoint(i, radTOF*TMath::Cos(angleHit),radTOF*TMath::Sin(angleHit),0.);
	 }
       }

   }
   pmTOF->Draw();

   if(shotCount<snapLimit){
   gPad->GetViewer3D("ogl"); 
   cSnap->cd();
  }
   //
   //repeat loops to display yz view
   //

   for (size_t verti = 0;verti<Nvert;++verti){

     StMuDst::setVertexIndex(verti);
     size_t Ntracks = mudst->primaryTracks()->GetEntries();
     //     LOG_INFO << "StPeCEvent::matchTOFhitsToTracks #track in vertex : "  <<Ntracks<< endm;
     for (size_t trackiter = 0;trackiter<Ntracks;trackiter++){
       StMuTrack* track = mudst->primaryTracks(trackiter);

       if(shotCount<snapLimit) {
	 TLine * straightTrack = new TLine(track->firstPoint().z(), track->firstPoint().y(), track->lastPoint().z(), track->lastPoint().y());
	 straightTrack->SetLineColor(verti+1);
	 straightTrack->Draw("same");
       }

       //for each track loop over tof hits
       for(int i=0;i<nMax;i++) {
	 StMuBTofHit *aHit = (StMuBTofHit *)mudst->btofHit(i);
	 if(aHit)
	   {
	     int trayId = aHit->tray();
	     if(trayId<=120&&trayId>=0) {//TOF
	       StMuTrack *TofGlobalTrack = aHit->globalTrack();
	       if(!TofGlobalTrack) continue;

	       //we loop over global tracks and exit loop as match is made
	       //
	       int tofHitTrkId = TofGlobalTrack->id();
	       if(track->id()==tofHitTrkId) {
       		 LOG_INFO << "StPeCEvent::match  GOT MATCH: "<<track->id()<<" vertex index: "<<track->vertexIndex()<<endm;
		 if(shotCount<snapLimit) {
		   TLine * straightTof = new TLine(track->firstPoint().z(), track->firstPoint().y(), track->lastPoint().z(), track->lastPoint().y());
		   straightTof->SetLineWidth(3.3);
		   straightTof->Draw("same");
		 }
		 break;
	       }
	       // 	  }
	       globalTrackCounter++;
	     }

	   }
       }
     }  //
   }    // loop over vertices yz
  }     //if shotCount...


   //
   //display global tracks
   size_t Gtracks = mudst->globalTracks()->GetEntries();
   LOG_INFO << "StPeCEvent::matchTOFhitsToTracks #Global tracks : "  <<Gtracks<< endm;
   for (size_t Gtrackiter = 0;Gtrackiter<Gtracks;Gtrackiter++){
     StMuTrack* track = mudst->globalTracks(Gtrackiter);

     if(shotCount<snapLimit) {
       TLine * straightTrack = new TLine(track->firstPoint().z(), track->firstPoint().y(), track->lastPoint().z(), track->lastPoint().y());
       straightTrack->SetLineColor(2);
       // straightTrack->Draw("same");
     }
   }

   gDirectory = saveDir;
   if(shotCount<snapLimit) {
     cSnap->SaveAs(Form("snapShot%d.pdf",shotCount));
     cSnap->Close();
     cSnap3D->Close();
   }
   shotCount++;
} 


//////////////////////////////////////////////////////////////////////
//
// $Id: StPeCEvent.cxx,v 1.15 2003/09/02 17:58:46 perev Exp $
// $Log: StPeCEvent.cxx,v $
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
//  Place the cut on  track->flag() in a unuque place (Internal track array?)
//  Understand the dublication of StPeCTrack  (does a tree work with pointers)
// ///////////////////////////////////////////////////////////////////
#include <Stiostream.h>
#include "StPeCEvent.h"
#include "StEventTypes.h"
#include "StPeCEnumerations.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"


ClassImp(StPeCEvent)

StPeCEvent::StPeCEvent() {
  // StPeCMaxTracks ..... looks more like Fortran......
  pPairs  = new TClonesArray ("StPeCPair", StPeCnMaxTracks);
  sPairs  = new TClonesArray ("StPeCPair", StPeCnMaxTracks);
  tracks  = new TClonesArray ("StPeCTrack",StPeCnMaxTracks);
  nPPairs  = 0 ;
  nSPairs  = 0 ;
  nTracks  = 0 ;
  eventP   = 0 ;
}

StPeCEvent::~StPeCEvent() {
  clear() ;
  delete pPairs ;
  delete sPairs ;
  delete tracks ;
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


   pPairs->Clear();
   sPairs->Clear();
   tracks->Clear();
}

Int_t StPeCEvent::fill ( StEvent *event ) {

  eventP = event ;

  // Set Run and Event Number
  eventN = event->id() ;
  runN   = event->runId(); 
  cout << "Event Run ID: " << runN << endl;
  cout << "Event ID: " << eventN << endl;

  bField = event->summary()->magneticField();

  //get trigger info

  Int_t   NGlobal=0; 
  Int_t   NPrimaries=0; 
  Int_t   SumQ=0; 
  Float_t SumPx=0.0; 
  Float_t SumPy=0.0;  

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
      
    }
    if( nglob==1 ){
      StTrack *tnp = exnode[in]->track(global);
      // DANGER THAT SHOULD BE DONE IN A PROPPER WAY, CHECK 4 PLACES !
      if (! (tnp->flag()>0)) continue;
      if ( tnp->detectorInfo()->numberOfPoints() < 11 ) continue ;
      NGlobal++; 
//       Do not store tracks in tree any longer PPY 11/5/02
//    new((*tracks)[nTracks++]) StPeCTrack(0,tnp) ;
    }
  }
  if ( NGlobal > StPeCnMaxTracks ) return 1 ; 

  nPrim             = NPrimaries ;
  nTot              = NGlobal  ;
  qTot = SumQ ;
  pt   = ::sqrt( SumPx*SumPx + SumPy*SumPy );

  //cout << "Number of Primary Vertices " << event->numberOfPrimaryVertices() << endl;
  StPrimaryVertex* vtx = event->primaryVertex();
  if(vtx) {
    //cout << "Vertex flag " << vtx->flag() << endl;    
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

#ifdef SPAIRS
  //
  //   Look for V0
  //   Do not skip the primaries here for the time being.....
  // 
  // HERE must flag be tested. 
  for( Int_t i=0; i<nnode-1; i++ ) {
    // if ( exnode[i]->entries(primary)  ) continue ; 
    if ( exnode[i]->entries(global)  !=1 ) continue ;
    for( Int_t j=i+1; j<nnode; j++ ) {
      //if ( exnode[j]->entries(primary)     ) continue ; 
      if ( exnode[j]->entries(global)  !=1 ) continue ;
      StTrack *trk1 = exnode[i]->track(global);
      StTrack *trk2 = exnode[j]->track(global);
      
      // DANGER 
      if (! (trk1->flag()>0)) continue;
      if (! (trk2->flag()>0)) continue;
      // --------
      StPhysicalHelixD h1 = trk1->geometry()->helix() ;
      StPhysicalHelixD h2 = trk2->geometry()->helix() ;
      
       pairD dcaLengths = h1.pathLengths(h2);
       StThreeVectorD x1 = h1.at(dcaLengths.first);
       StThreeVectorD x2 = h2.at(dcaLengths.second);
       StThreeVectorD x = (x1-x2) ;
	if ( x.mag() > 10 ) continue ; // Hardwire cut

        // TClonesArray &spairs = *sPairs;
        // lPair = new(spairs[nSPairs++]) StPeCPair(trk1,trk2,0,event) ;
	// 
        lPair = new((*sPairs)[nSPairs++]) StPeCPair(trk1,trk2,0,event) ;
#ifdef PECPRINT
        cout << "StPeCEvent : Secondary Pair : " 
           << "  sumQ = " << lPair->getSumCharge()
           << "  sumPt = " << lPair->getSumPt()
           << "  mInv = " << lPair->getMInv(pion)
           << "  opening angle = " << lPair->getOpeningAngle()
           << "  cos(theta*) = " << lPair->getCosThetaStar(pion) << endl;
#endif	   
     }
  }
#endif
  return 0 ;
}

Int_t StPeCEvent::fill(StMuDst *mudst) {
   //cout << "Entering StPeCEvent::fill(StMuDst *mudst)" << endl;
   Int_t nGlobals = 0, SumQ = 0; 
   Float_t SumPx = 0.0, SumPy = 0.0;  
   float px, py;
   TClonesArray* muTracks = 0;
   StMuEvent* event = 0;
   StMuTrack *tp = 0;

   //Save the event reference
   muDst = mudst;
   event = muDst->event();

   //Set Run and Event Number
   eventN = event->eventInfo().id();

   runN = event->eventInfo().runId(); 
   cout << "Event Run ID: " << runN << endl;
   cout << "Event ID: " << eventN << endl;

   bField = event->eventSummary().magneticField();

   if ( event->eventSummary().numberOfVertices() ) {
      StThreeVectorF vtx = event->primaryVertexPosition();
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
 
   //Retrieve the primary tracks
   muTracks = muDst->primaryTracks();
   nPrim    = muTracks->GetLast() + 1;

   for(int i = 0; i <= muTracks->GetLast(); i++) {
      tp = (StMuTrack*)muTracks->UncheckedAt(i);

      px = tp->momentum().x();
      py = tp->momentum().y();

      SumPx += px;
      SumPy += py;
      SumQ += tp->charge();
   }


   //Retrieve the global tracks
   muTracks = muDst->globalTracks();
   for(int i = 0; i <= muTracks->GetLast(); i++) {
      tp = (StMuTrack*)muTracks->UncheckedAt(i);
      if ( tp->nHits() < 11 ) continue ;
// No track structure: trying to make trees smaller
//    new((*tracks)[i])StPeCTrack(0, tp);
      nGlobals++;
   }

   nTot = nGlobals ;
   qTot = SumQ;
   pt = ::sqrt(SumPx * SumPx + SumPy * SumPy );
    
   if ( nGlobals > StPeCnMaxTracks ) return 1 ; 


   nPPairs = 0 ;
   StPeCPair* lPair ; 
   StMuTrack *trk1, *trk2 ;
   muTracks = muDst->primaryTracks();
   for(int i1 = 0; i1 < nPrim; i1++) {
      trk1 = (StMuTrack*)muTracks->UncheckedAt(i1);
      for(int i2 = i1+1; i2 < nPrim; i2++) {
	 trk2 = (StMuTrack*)muTracks->UncheckedAt(i2);

	 // DANGER 
	 if (! (trk1->flag()>0)) continue;
	 if (! (trk2->flag()>0)) continue;
	 // --------
	 // get pointer to memebr ?
	 // TClonesArray &ppairs = *pPairs;
	 lPair = new((*pPairs)[nPPairs++]) StPeCPair(trk1,trk2,1,event) ;
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
   //cout << "Exiting StPeCEvent::fill(StMuDst *mudst)" << endl;
   return 0;
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


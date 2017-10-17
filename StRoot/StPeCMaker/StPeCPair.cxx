//////////////////////////////////////////////////////////////////////
//
// $Id: StPeCPair.cxx,v 1.19 2015/07/22 19:55:20 ramdebbe Exp $
// $Log: StPeCPair.cxx,v $
// Revision 1.19  2015/07/22 19:55:20  ramdebbe
// stopped doing extrapolation to TOF, now copy information from StMuBTofPidTraits, needs to do the same for fill(mudst, stevent)
//
// Revision 1.18  2014/12/23 20:43:10  ramdebbe
// copied the fill functionality of method with both inputs to the one with MuDst input. This gives TOF extrapolation in the pPairs branch
//
// Revision 1.17  2013/12/27 16:52:20  ramdebbe
// added a input argument StBTofGeometry to fill method (StMuDst + StEvent) and x y z coordinates of intercept to TOF cylinder
//
// Revision 1.16  2012/06/13 15:10:22  ramdebbe
// added tof information to both tracks
//
// Revision 1.15  2004/01/26 23:01:03  perev
// WarnOff
//
// Revision 1.14  2003/11/25 01:54:33  meissner
// correct several bugs: eta cut for tracks, charge sorting, add counting of FTPC and TPC primary tracks, Add bbc information
//
// Revision 1.13  2003/09/02 17:58:46  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.12  2003/03/19 15:35:47  yepes
// *** empty log message ***
//
// Revision 1.11  2003/03/18 21:20:41  yepes
// correcting problem with bField
//
// Revision 1.10  2003/02/05 17:14:05  yepes
// Adding bField and pPairs.psi to tree
//
// Revision 1.9  2002/12/16 23:05:38  yepes
// *** empty log message ***
//
// Revision 1.8  2002/12/16 23:04:02  yepes
// Field comes in KGauss and should be passed to routines in Teslas
// problem pointed out by Vladimir
//
// 
// Revision 1.7  2002/03/19 22:23:49  meissner
// New variables: zdc unatt., Trigger word, MC tree if Geant Branch, DCA  for primary pairs, all tracks for secondary pairs (Test)
//
// Revision 1.6  2001/02/21 20:42:12  yepes
// Add ctb signals to tree
//
// Revision 1.5  2001/02/13 17:54:43  yepes
// still problems on differnt platforms
//
// Revision 1.4  2001/02/12 21:15:59  yepes
// New version of StPeCMaker, lots of changes
//
// Revision 1.1  2000/04/21 19:12:30  nystrand
// First Version
//
// Revision 1.1  2000/03/24 22:37:06  nystrand
// First version of StPeCPair
//
// Revision 1.0  2000/03/20 23:28:50  nystrand
//
//////////////////////////////////////////////////////////////////////
#include <Stiostream.h>
#include "StPeCPair.h"
#include "StEventTypes.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/projection/StEmcPosition.h"
#include "StEmcUtil/filters/StEmcFilter.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "St_geant_Maker/St_geant_Maker.h"



ClassImp(StPeCPair)

StPeCPair::StPeCPair() {
} 

StPeCPair::~StPeCPair() {
}

void StPeCPair::Clear(const char *) {
  pCharge=0;
  pPt=0.;
  pPz =0.;
  pPsi =0.;
  pAngle =0.;
  pXyAngle =0.;
  pPtArm =0.; 
  pAlpha =0.; 
  pPartDca =0.; 
  pV0Dca =0.;   
  rV0 =0.;
  phiV0 =0.;
  zV0 =0.;  
  
  // Not implemented 
  // tr1.Clear(); 
  // tr2.Clear(); 
  // pionH.Clear();
  // kaonH.Clear();
  // protonH.Clear();
  // electronH.Clear();
  // muonH.Clear();
  
  track1=NULL;
  track2=NULL;
  muTrack1=NULL;
  muTrack2=NULL;
}

#ifndef __CINT__
StPeCPair::StPeCPair ( StTrack* trk1, StTrack* trk2, 
                       Bool_t primaryFlag, StEvent* event ) {
  this->Clear();

  if(trk1->geometry()->charge() < 0 && trk2->geometry()->charge()>0 ) { // swap to 1+ 2- if different charges 
    track1 = trk2;
    track2 = trk1;
  } else {  // keep +-, ++, --
    track1 = trk1;
    track2 = trk2;
  }
  muTrack1=0;   // clean, might crash otherwise 
  muTrack2=0;
  fill ( primaryFlag, event ) ;
}

StPeCPair::StPeCPair ( StMuTrack* trk1, StMuTrack* trk2, 
                       Bool_t primaryFlag, StMuEvent* event,  StBTofGeometry * pairTOFgeo ) {
  this->Clear();
  //
  // Get Magnetic field from event summary
  //
  Double_t bFld; 

  pairTOFgeoLocal = pairTOFgeo; 

  bFld=event->eventSummary().magneticField()/10.;  



  if (trk1->charge() <0 && trk2->charge() >0  ) { // swap to 1+ 2- if different charges 
    muTrack1 = trk2;
    muTrack2 = trk1;
  } else {    // keep for +- and ++, -- 
    muTrack1 = trk1;
    muTrack2 = trk2;
  }
  track1=0;  // clean; might crash otherwise 
  track2=0; 
  fill ( primaryFlag, event ) ;
}

StPeCPair::StPeCPair ( StMuTrack* trk1, StMuTrack* trk2, 
                       Bool_t primaryFlag, StMuEvent* event, StEvent* eventP,  StBTofGeometry * pairTOFgeo ) {
  this->Clear();
  //
  // Get Magnetic field from event summary
  //


  bFld=event->eventSummary().magneticField()/10.;  
  pairTOFgeoLocal = pairTOFgeo; 
  cout<<" StPeCPair StEvent StMuEvent ************  Mag field from summary:    "<<bFld<<"   TOF geometry pointer: "<<pairTOFgeo<<endl;
//   // Get EMC calorimeter clusters from StEvent

//   // check if there is a collection
//   StEmcCollection *emcStEvent = eventP->emcCollection();

//   //
//   //EMC detector information
//   //
//   StEmcDetector* EMCdetector = emcStEvent->detector(kBarrelSmdEtaStripId);  
//   if (!EMCdetector)
//     {cout<<"There is no kBarrelSmdEtaStripId Detector ---------"<<endl; return;}


  if (trk1->charge() <0 && trk2->charge() >0  ) { // swap to 1+ 2- if different charges 
    muTrack1 = trk2;
    muTrack2 = trk1;
  } else {    // keep for +- and ++, -- 
    muTrack1 = trk1;
    muTrack2 = trk2;
  }
  track1=0;  // clean; might crash otherwise 
  track2=0; 
  fill ( primaryFlag, event, eventP ) ;
}



void StPeCPair::setTrack1(StTrack* trk) {
  track1 = trk;
}
void StPeCPair::setTrack2(StTrack* trk) {
  track2 = trk;
}

void StPeCPair::setTrack1(StMuTrack* trk) {
  muTrack1 = trk;
}
void StPeCPair::setTrack2(StMuTrack* trk) {
  muTrack2 = trk;
}

StTrack* StPeCPair::getTrack1() { return track1; }
StTrack* StPeCPair::getTrack2() { return track2; }

StMuTrack* StPeCPair::getMuTrack1() { return muTrack1; }
StMuTrack* StPeCPair::getMuTrack2() { return muTrack2; }

StLorentzVectorF StPeCPair::getPair4Momentum(StPeCSpecies pid) const{
  StLorentzVectorF p4pair(0.0,0.0,0.0,0.0);
  if      ( pid == 0 ) return pionH.Mom4 ;
  else if ( pid == 1 ) return kaonH.Mom4 ;
  else if ( pid == 2 ) return protonH.Mom4 ;
  else if ( pid == 3 ) return electronH.Mom4 ;
  else if ( pid == 4 ) return muonH.Mom4 ;
  else {
     printf ( "StPeCPair::getPair4Momentum: wrong pid %d \n", pid ) ;
     return p4pair ; 
  }
}
#endif /*__CINT__*/



Int_t StPeCPair::fill ( Bool_t primaryFlag, StEventSummary* summary, 
                        StThreeVectorF& p1, StPhysicalHelixD& h1, short charge1,
                        StThreeVectorF& p2, StPhysicalHelixD& h2, short charge2,  
                        StThreeVectorF& primaryVertexPosition ) {
//
//  Check whether tracks are primary or secondary
//  if they are secondary find point of closest approach
//  and work with momentum at that point
//
   pPartDca  = 0. ;
   pV0Dca    = 0. ;
   rV0       = 0. ;
   phiV0     = 0. ;
   zV0       = 0. ;
   // Want the DCA Vertex info  also for the Primary Pair !!
   //  if ( !primaryFlag ) {
   pairD dcaLengths ;
   dcaLengths = h1.pathLengths(h2);

   Float_t bField ;
   if ( summary != 0 ) bField = summary->magneticField();
   else bField = 2.5 ;

   // The momentum we do not need for the primary pair ....
   if ( !primaryFlag ) {
      p1 = h1.momentumAt(dcaLengths.first, tesla*bField*0.1 ) ;
      p2 = h2.momentumAt(dcaLengths.second, tesla*bField*0.1 ) ;
   }
   
   StThreeVectorD x1 = h1.at(dcaLengths.first);
   StThreeVectorD x2 = h2.at(dcaLengths.second);
   StThreeVectorD x = (x1-x2) ;
   pPartDca = x.mag();
   //
   //  Construct a helix with very large momentums
   //  to get intersection of V0 with vertex
   //
   StThreeVectorD xMean = (x1+x2)/2. ;
   rV0   = xMean.perp();
   phiV0 = xMean.phi();
   zV0   = xMean.z();
   
   StThreeVectorD pSum  = p1+p2 ;
   StPhysicalHelixD v0Helix ( pSum, xMean, 0.1*bField/tesla, 100000./GeV ) ;
   pV0Dca = v0Helix.distance ( primaryVertexPosition ) ;
   //
   StThreeVectorF p = p1 + p2 ;
   pPt              = p.perp() ;
   pPz              = p.z() ;
   pPsi             = p.phi();
  // Opening angle of the pair in the CM (lab)
  // cos(theta) = (p1"dot"p2)/(abs(p1)*abs(p2))
   Float_t ScalarProduct = p1*p2;
   Float_t Denominator   = p1.mag()*p2.mag();
   if(Denominator) {
     pAngle = acos(ScalarProduct/Denominator);
   }else{
     pAngle = -999;
   }
   if (p1.perp() * p2.perp()) {
     pXyAngle = acos((p1.x()*p2.x()+p1.y()*p2.y())/p1.perp()/p2.perp());
   } else { 
     pXyAngle = -999;
   }
//
//  Calculate Armenteros variables
//
   Float_t p1AlongPtot = p*p1/p.mag() ; 
   Float_t p2AlongPtot = p*p2/p.mag() ;

   Float_t pt1Ptot = ::sqrt(p1.mag()*p1.mag()-p1AlongPtot*p1AlongPtot);

   pPtArm = pt1Ptot ;
   pAlpha = (p1AlongPtot-p2AlongPtot)/(p1AlongPtot+p2AlongPtot);


   Float_t mptcle=0.0;
//
//   Loop over different species
//
   Float_t mInv, cosThetaStar ;
   StLorentzVectorF FourMomentum ; 
   StPeCSpec* species ;
   for ( int i = 0 ; i < nSpecies ; i++ )
     {
       if ( i == pion )    {
         mptcle = pion_plus_mass_c2;
	 species = &pionH ;
       }
       else if ( i == kaon ) {
         mptcle = 493.677*MeV;
	 species = &kaonH ;
      }
       else if ( i == proton ) {
         mptcle = proton_mass_c2;
	 species = &protonH ;
       }
       else if ( i == electron ) {
         mptcle = electron_mass_c2;
	 species = &electronH ;
       }
      else if ( i == muon ) {
	mptcle = 105.6584*MeV; 
	species = &muonH ;
      }
      else {
	printf ( "StPecPair:calculatePair4Momentum; wrong pid %d \n", i ) ;
 	continue ;
      }
       
       StLorentzVectorF p4pair(0.0,0.0,0.0,0.0);
       Float_t          e1 = p1.massHypothesis(mptcle);
       Float_t          e2 = p2.massHypothesis(mptcle);
       StLorentzVectorF pf1(e1,p1);
       StLorentzVectorF pf2(e2,p2);
       p4pair = pf1 + pf2;
       FourMomentum = p4pair ;
       mInv = p4pair.m() ;

  // ThetaStar is the angle between of one of the daughter tracks
  // and the Z-axis in the Helicity frame. The Helicity frame is
  // that rest frame of the parent particle in which the direction of 
  // the scattered nucleus is along the negative Z-axis. See K.Schilling,
  // P.Seyboth, G. Wolf Nucl. Phys. B 15(1970)397-412.
  // Since the outgoing nuclei are not tagged, this direction cannot 
  // be determined exactly. Because of the low momentum transfers it is,
  // however, a reasonable approximation to assume that the nuclei move 
  // parallel to the Z-axis of the lab frame. 

//
// Get the sign right for the boost lab --> parent rest frame
// Default is in the other direction
      StThreeVectorF sp = -1.0*p4pair.vect();
      p4pair.setVect(sp);
      pf1 = pf1.boost(p4pair);
      pf2 = pf2.boost(p4pair);
      Float_t d1th = pf1.cosTheta();  
      Float_t d2th = pf2.cosTheta();
  // Define cosThetaStar in the interval 0<cosThetaStar<1
      if( d1th > 0 ) cosThetaStar = d1th;
      else           cosThetaStar = d2th;

      species->pid = i ;
      species->mInv         = mInv ;
      species->yRap         = FourMomentum.rapidity() ;
      species->Mom4         = FourMomentum ;
      species->cosThetaStar = cosThetaStar ;
     }

//
//  fill our local Track class; not save if track pointers not properly reset !!!
// set does not belong here ! FLK works with pointers which are not arguments of this routine !!
// #ifndef __CINT__
//    Int_t prim = 1 ;
//    if (track1 && track2)
//      {
//        tr1.set(prim,track1);
//        tr2.set(prim,track2);
//      }
//    else if (muTrack1 && muTrack2)
//      {
//        tr1.set(prim,muTrack1);
//        tr2.set(prim,muTrack2);
//      }
// #endif


   return 0 ;
}
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Int_t StPeCPair::fill ( Bool_t primaryFlag, StMuEvent* event  ) {

   pCharge           = muTrack1->charge()+muTrack2->charge();
   
   StThreeVectorF p1 ; 
   StThreeVectorF p2 ;
   StPhysicalHelixD h1, hOuter1 ;
   StPhysicalHelixD h2, hOuter2 ;

   short charge1, charge2 ;
//    Int_t mod,eta,sub; 
   StThreeVectorD position,momentum;
//

   tr1_bemcModule   = -999;
   tr1_bemcEtabin   = -999;
   tr1_bemcEtaValue = -999;
   tr1_bemcPhiValue = -999;
   tr1_bemcSub      = -999;  
   tr1_bemcEnergy   = -999;
   tr2_bemcModule   = -999;
   tr2_bemcEtabin   = -999;
   tr2_bemcEtaValue = -999;
   tr2_bemcPhiValue = -999;
   tr2_bemcSub      = -999;  
   tr2_bemcEnergy   = -999;

   tr1_bsmdeModule   = -999;
   tr1_bsmdeEtabin   = -999;
   tr1_bsmdeEtaValue = -999;
   tr1_bsmdePhiValue = -999;
   tr1_bsmdeSub      = -999;  
   tr1_bsmdeEnergy   = -999;
   tr2_bsmdeModule   = -999;
   tr2_bsmdeEtabin   = -999;
   tr2_bsmdeEtaValue = -999;
   tr2_bsmdePhiValue = -999;
   tr2_bsmdeSub      = -999;  
   tr2_bsmdeEnergy   = -999;

   tr1_timeOfFlight = -999;
   tr1_pathLength   = -999;
   tr1_Beta        = -999;
   tr2_timeOfFlight = -999;
   tr2_pathLength   = -999;
   tr2_Beta        = -999;

   tr1_extrapolatedTOF_mX = -999;
   tr1_extrapolatedTOF_mY = -999;
   tr1_extrapolatedTOF_mZ = -999;

   tr2_extrapolatedTOF_mX = -999;
   tr2_extrapolatedTOF_mY = -999;
   tr2_extrapolatedTOF_mZ = -999;


   p1      = muTrack1->momentum();
   p2      = muTrack2->momentum();
   charge1 = muTrack1->charge();
   charge2 = muTrack2->charge();
   h1      = muTrack1->helix() ;
   h2      = muTrack2->helix() ;
   hOuter1      = muTrack1->outerHelix() ;
   hOuter2      = muTrack2->outerHelix() ;
   
   StThreeVectorF vtx = event->primaryVertexPosition() ;


   StMuBTofPidTraits mBTofPidTraits_1 = muTrack1->btofPidTraits();
   StMuBTofPidTraits mBTofPidTraits_2 = muTrack2->btofPidTraits();

   //skip calorimeter cluster information, if necessary, it should be filled with the method using StEvent and MuDst

   

     fill ( primaryFlag, &(event->eventSummary()), 
	  p1, h1, charge1, p2, h2, charge2, vtx ) ; 

#ifndef __CINT__
     tr1.set(1,muTrack1, event); // 1=primary
     tr2.set(1,muTrack2, event);
#endif
     tr1_timeOfFlight = mBTofPidTraits_1.timeOfFlight();
     tr1_pathLength   = mBTofPidTraits_1.pathLength();
     tr1_Beta         = mBTofPidTraits_1.beta();
     const float a     = mBTofPidTraits_1.position().x();
     const float b     = mBTofPidTraits_1.position().y();
     const float c     = mBTofPidTraits_1.position().z();
     tr1_extrapolatedTOF_mX     = a;
     tr1_extrapolatedTOF_mY     = b;
     tr1_extrapolatedTOF_mZ     = c;

     tr2_timeOfFlight = mBTofPidTraits_2.timeOfFlight();
     tr2_pathLength   = mBTofPidTraits_2.pathLength();
     tr2_Beta         = mBTofPidTraits_2.beta();
     const float a2   = mBTofPidTraits_2.position().x();
     const float b2   = mBTofPidTraits_2.position().y();
     const float c2   = mBTofPidTraits_2.position().z();
     tr2_extrapolatedTOF_mX     = a2;
     tr2_extrapolatedTOF_mY     = b2;
     tr2_extrapolatedTOF_mZ     = c2;
   //
   //extrapolate tracks to TOF
   //
   vector<Int_t> idVec;
   vector<Double_t> pathVec;
   PointVec  crossVec; 


   //no more private extrapolation; commented out 18-June-2015 RD


//    if(pairTOFgeoLocal->HelixCrossCellIds(hOuter1,idVec,pathVec,crossVec)) 
//      {

//        Int_t cellId    = -999;
//        Int_t  moduleId = -999; 
//        Int_t  trayId   = -999;
//        pairTOFgeoLocal->DecodeCellId(idVec[0],cellId,moduleId,trayId);
//        tr1_extrapolatedTOF_mX     = crossVec[0].x();
//        tr1_extrapolatedTOF_mY     = crossVec[0].y();
//        tr1_extrapolatedTOF_mZ = crossVec[0].z();
//      }  
//    if(pairTOFgeoLocal->HelixCrossCellIds(hOuter2,idVec,pathVec,crossVec)) 
//      {
//        Int_t cellId    = -999;
//        Int_t  moduleId = -999; 
//        Int_t  trayId   = -999;
//        pairTOFgeoLocal->DecodeCellId(idVec[0],cellId,moduleId,trayId);
//        tr2_extrapolatedTOF_mX     = crossVec[0].x();
//        tr2_extrapolatedTOF_mY     = crossVec[0].y();
//        tr2_extrapolatedTOF_mZ = crossVec[0].z();
//      }  

   return 0 ;
   
}
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Int_t StPeCPair::fill ( Bool_t primaryFlag, StMuEvent* event, StEvent* eventP ) {

   pCharge           = muTrack1->charge()+muTrack2->charge();

   StThreeVectorF p1 ; 
   StThreeVectorF p2 ;
   StPhysicalHelixD h1, hOuter1 ;
   StPhysicalHelixD h2, hOuter2 ;

   short charge1, charge2 ;
   Int_t mod,eta,sub;
   StThreeVectorD position,momentum;



   tr1_bemcModule   = -999;
   tr1_bemcEtabin   = -999;
   tr1_bemcEtaValue = -999;
   tr1_bemcPhiValue = -999;
   tr1_bemcSub      = -999;  
   tr1_bemcEnergy   = -999;
   tr2_bemcModule   = -999;
   tr2_bemcEtabin   = -999;
   tr2_bemcEtaValue = -999;
   tr2_bemcPhiValue = -999;
   tr2_bemcSub      = -999;  
   tr2_bemcEnergy   = -999;

   tr1_bsmdeModule   = -999;
   tr1_bsmdeEtabin   = -999;
   tr1_bsmdeEtaValue = -999;
   tr1_bsmdePhiValue = -999;
   tr1_bsmdeSub      = -999;  
   tr1_bsmdeEnergy   = -999;
   tr2_bsmdeModule   = -999;
   tr2_bsmdeEtabin   = -999;
   tr2_bsmdeEtaValue = -999;
   tr2_bsmdePhiValue = -999;
   tr2_bsmdeSub      = -999;  
   tr2_bsmdeEnergy   = -999;

   tr1_timeOfFlight = -999;
   tr1_pathLength   = -999;
   tr1_Beta        = -999;
   tr2_timeOfFlight = -999;
   tr2_pathLength   = -999;
   tr2_Beta        = -999;

   tr1_extrapolatedTOF_mX = -999;
   tr1_extrapolatedTOF_mY = -999;
   tr1_extrapolatedTOF_mZ = -999;

   tr2_extrapolatedTOF_mX = -999;
   tr2_extrapolatedTOF_mY = -999;
   tr2_extrapolatedTOF_mZ = -999;


   p1      = muTrack1->momentum();
   p2      = muTrack2->momentum();
   charge1 = muTrack1->charge();
   charge2 = muTrack2->charge();
   h1      = muTrack1->helix() ;
   h2      = muTrack2->helix() ;
   hOuter1      = muTrack1->outerHelix() ;
   hOuter2      = muTrack2->outerHelix() ;



   StMuBTofPidTraits mBTofPidTraits_1 = muTrack1->btofPidTraits();
   StMuBTofPidTraits mBTofPidTraits_2 = muTrack2->btofPidTraits();

   
   StThreeVectorF vtx = event->primaryVertexPosition() ;
  // Get EMC calorimeter clusters from StEvent

  // check if there is a collection
  StEmcCollection *emcStEvent = eventP->emcCollection();
  //
  //EMC detector information
  //
  cout<<" in StPeCPair fill "<<" TOF geo pointer inside fill: "<<pairTOFgeoLocal<<endl;
  StEmcDetector* EMCdetector = emcStEvent->detector(kBarrelEmcTowerId);     //kBarrelSmdEtaStripId);  
  if (!EMCdetector)
    {cout<<"There is no kBarrelSmdEtaStripId Detector ---------"<<endl;}
  //
  // instantiate object to project track to EMC barrel
  StEmcPosition * project = new StEmcPosition();
  StEmcGeom * mGeom2=StEmcGeom::instance("bemc");  
  Bool_t ok=project->trackOnEmc(&position,&momentum,muTrack1,bFld);

   //if(! ok) cout<<" track projection failed ************************************"<<endl;
   if(ok) {
     mGeom2->getBin(position.phi(),position.pseudoRapidity(),mod,eta,sub);
       //
       //see if EMC has energy
       //
       for(unsigned int i=1;i<=120;i++)
	 {  
	   if(fabs(mod)!=i) continue;
	   StEmcModule* module=EMCdetector->module(i); 
	   StSPtrVecEmcRawHit& hits=module->hits();
	   for(unsigned int k=0;k<hits.size();k++) if(hits[k]){
	     unsigned int module=hits[k]->module(); 
	     unsigned int Eta=hits[k]->eta();	
// 	     float energyT=hits[k]->energy();
	     int s=fabs(hits[k]->sub());
	     int did(0);
	     if (module==fabs(mod) &&  Eta == fabs(eta)){
	       float energyT1=hits[k]->energy();
	       mGeom2->getId(module,Eta,s,did);
 
	       tr1_bemcModule = module;
	       tr1_bemcEtabin = Eta;
	       tr1_bemcEtaValue = position.pseudoRapidity();
	       tr1_bemcPhiValue = position.phi();
	       tr1_bemcSub = s;  
	       tr1_bemcEnergy = energyT1;

	     }
	   }
	 }
   }
   //second track
  Bool_t ok2=project->trackOnEmc(&position,&momentum,muTrack2,bFld);
   //if(! ok) cout<<" track projection failed ************************************"<<endl;
   if(ok2) {
     mGeom2->getBin(position.phi(),position.pseudoRapidity(),mod,eta,sub);
       //
       //see if EMC has energy
       //
       for(unsigned int i=1;i<=120;i++)
	 {  
	   if(fabs(mod)!=i) continue;
	   StEmcModule* module=EMCdetector->module(i); 
	   StSPtrVecEmcRawHit& hits=module->hits();
	   for(unsigned int k=0;k<hits.size();k++) if(hits[k]){
	     unsigned int module=hits[k]->module(); 
	     unsigned int Eta=hits[k]->eta();	
// 	     float energyT=hits[k]->energy();
	     int s=fabs(hits[k]->sub());
	     int did(0);
	     if (module==fabs(mod) &&  Eta == fabs(eta)){
	       float energyT1=hits[k]->energy();
	       mGeom2->getId(module,Eta,s,did);
	       tr2_bemcModule = module;
	       tr2_bemcEtabin = Eta;
	       tr2_bemcEtaValue = position.pseudoRapidity();
	       tr2_bemcPhiValue = position.phi();
	       tr2_bemcSub = s;  
	       tr2_bemcEnergy = energyT1;

	     }
	   }
	 }
   }
   //
   //repeat extrapolation, this time to shower max detectors
   //
  StEmcDetector* SMDdetector = emcStEvent->detector(kBarrelSmdEtaStripId);    
  if (!SMDdetector)
    {cout<<"There is no kBarrelSmdEtaStripId Detector ---------"<<endl;}
  StEmcPosition * projectSmde = new StEmcPosition();
  StEmcGeom * mGeomSmde=StEmcGeom::instance("bsmde");  
  Bool_t okSmd=projectSmde->trackOnEmc(&position,&momentum,muTrack1,bFld);
   if(okSmd) {
     mGeomSmde->getBin(position.phi(),position.pseudoRapidity(),mod,eta,sub);
       //
       //see if SMDe has energy
       //
       for(unsigned int i=1;i<=120;i++)
	 {  
	   if(fabs(mod)!=i) continue;
	   StEmcModule* module=SMDdetector->module(i); 
	   StSPtrVecEmcRawHit& hits=module->hits();
	   for(unsigned int k=0;k<hits.size();k++) if(hits[k]){
	     unsigned int module=hits[k]->module(); 
	     unsigned int Eta=hits[k]->eta();	
// 	     float energyT=hits[k]->energy();
	     int s=fabs(hits[k]->sub());
	     int did(0);
	     if (module==fabs(mod) &&  Eta == fabs(eta)){
	       float energyT1=hits[k]->energy();
	       mGeomSmde->getId(module,Eta,s,did);

	       tr1_bsmdeModule = module;
	       tr1_bsmdeEtabin = Eta;
	       tr1_bsmdeEtaValue = position.pseudoRapidity();
	       tr1_bsmdePhiValue = position.phi();
	       tr1_bsmdeSub = s;  
	       tr1_bsmdeEnergy = energyT1;

	     }
	   }
	 }
   }
   fill ( primaryFlag, &(event->eventSummary()), 
	  p1, h1, charge1, p2, h2, charge2, vtx ) ; 

#ifndef __CINT__
   tr1.set(1,muTrack1, event); // 1=primary
   tr2.set(1,muTrack2, event);
#endif
   tr1_timeOfFlight = mBTofPidTraits_1.timeOfFlight();
   tr1_pathLength   = mBTofPidTraits_1.pathLength();
   tr1_Beta         = mBTofPidTraits_1.beta();
   tr2_timeOfFlight = mBTofPidTraits_2.timeOfFlight();
   tr2_pathLength   = mBTofPidTraits_2.pathLength();
   tr2_Beta         = mBTofPidTraits_2.beta();
   //
   //extrapolate tracks to TOF
   //

   vector<Int_t> idVec;
   vector<Double_t> pathVec;
   PointVec  crossVec; 





   if(pairTOFgeoLocal->HelixCrossCellIds(hOuter1,idVec,pathVec,crossVec)) 
     {

       Int_t cellId    = -999;
       Int_t  moduleId = -999; 
       Int_t  trayId   = -999;
       pairTOFgeoLocal->DecodeCellId(idVec[0],cellId,moduleId,trayId);
       tr1_extrapolatedTOF_mX     = crossVec[0].x();
       tr1_extrapolatedTOF_mY     = crossVec[0].y();
       tr1_extrapolatedTOF_mZ = crossVec[0].z();
       cout<<" extrapolated first track"<<endl;
     }  
   if(pairTOFgeoLocal->HelixCrossCellIds(hOuter2,idVec,pathVec,crossVec)) 
     {
       Int_t cellId    = -999;
       Int_t  moduleId = -999; 
       Int_t  trayId   = -999;
       pairTOFgeoLocal->DecodeCellId(idVec[0],cellId,moduleId,trayId);
       tr2_extrapolatedTOF_mX     = crossVec[0].x();
       tr2_extrapolatedTOF_mY     = crossVec[0].y();
       tr2_extrapolatedTOF_mZ = crossVec[0].z();
     }  

   return 0 ;
   
}
Int_t StPeCPair::fill ( Bool_t primaryFlag, StEvent* event  ) {
  //
  pCharge           = track1->geometry()->charge()+track2->geometry()->charge();
  
  StThreeVectorF p1 ; 
  StThreeVectorF p2 ;
  StPhysicalHelixD h1 ;
  StPhysicalHelixD h2 ;
  short charge1, charge2 ;
  //
  p1      = track1->geometry()->momentum();
  p2      = track2->geometry()->momentum();
  charge1 = track1->geometry()->charge();
  charge2 = track2->geometry()->charge();
  //   if ( !primaryFlag ) {

   h1 = track1->geometry()->helix() ;
   h2 = track2->geometry()->helix() ;

   // }
   
   StEventSummary* summary = 0 ;
   StPrimaryVertex* vtx = 0;
   vtx = event->primaryVertex();
   summary = event->summary();

   StThreeVectorF vtxP  ;
   //
   //  If there is no primary vertex assume (0,0,0)
   //
   if ( vtx ) {
     vtxP = vtx->position() ;
   } else {
     vtxP.setX(0.);
     vtxP.setY(0.);
     vtxP.setZ(0.);
   }
   
   fill ( primaryFlag, summary, p1, h1, charge1, p2, h2, charge2, vtxP ) ;
   
   // fill local track class 
#ifndef __CINT__

   tr1.set(1,track1); // 1=primary
   tr2.set(1,track2);

#endif
   return 0 ;
}


Int_t StPeCPair::getSumCharge() const{
  return pCharge ; 
}

Float_t StPeCPair::getSumPt() const{
  return pPt ;
}

Float_t StPeCPair::getSumPz() const{
  return pPz ;
}

Float_t StPeCPair::getMInv(StPeCSpecies pid) const{
  if   ( pid == 0 )    return pionH.mInv ;
  else if ( pid == 1 ) return kaonH.mInv ;
  else if ( pid == 2 ) return protonH.mInv ;
  else if ( pid == 3 ) return electronH.mInv ;
  else if ( pid == 4 ) return muonH.mInv ;
  else {
     printf ( "StPeCPair::getMInv: wrong pid %d \n", pid ) ;
     return 0 ; 
  }
}

Float_t StPeCPair::getOpeningAngle() const{
  return pAngle ;
}
Float_t StPeCPair::getCosThetaStar(StPeCSpecies pid) const{
  if   ( pid == 0 )    return pionH.cosThetaStar ;
  else if ( pid == 1 ) return kaonH.cosThetaStar ;
  else if ( pid == 2 ) return protonH.cosThetaStar ;
  else if ( pid == 3 ) return electronH.cosThetaStar ;
  else if ( pid == 4 ) return muonH.cosThetaStar ;
  else {
    printf ( "StPeCPair::getCosThetaStar: wrong pid %d \n", pid ) ;
     return 0 ; 
  }
}



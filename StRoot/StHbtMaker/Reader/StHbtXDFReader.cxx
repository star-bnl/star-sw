/***************************************************************************
 *
 * $Id: StHbtXDFReader.cxx,v 1.6 2000/04/13 18:15:33 fine Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 * for more describtion see correponding header file
 *
 **************************************************************************/
#define HBT_BFIELD 0.5*tesla


#include "StHbtMaker/Reader/StHbtXDFReader.h"

#include "TOrdCollection.h"
  
#include "StParticleTable.hh"
#include "StParticleTypes.hh"

#include <math.h>
#include <list>

#include "SystemOfUnits.h"   // has "tesla" in it
#include "StHbtMaker/Infrastructure/StHbtTrackCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtV0Collection.hh"
#include "StEventMaker/StEventMaker.h"

#ifdef __ROOT__
ClassImp(StHbtXDFReader)
#endif

#if !(ST_NO_NAMESPACES)
  using namespace units;
#endif


inline double min(double a, double b) { return (a<b) ? a : b; }
inline double max(double a, double b) { return (a<b) ? b : a; }

//__________________
StHbtXDFReader::StHbtXDFReader(const char* dataSetName, const char* particleTableDirectory){
  mDataSetName = dataSetName;
  mParticleTableDirectory = particleTableDirectory;

  mTheEventMaker=0;
  mEventCut=0;
  mTrackCut=0;
  mV0Cut=0;
  mReaderStatus = 0;  // "good"

  mAcceptedParticles = new pdgIdList;
  mAcceptedMothers = new pdgIdList;
  mAcceptedDaughters = new pdgIdList;

}
//__________________
StHbtXDFReader::~StHbtXDFReader(){
  if (mEventCut) delete mEventCut;
  if (mTrackCut) delete mTrackCut;
  if (mV0Cut) delete mV0Cut;
  delete mAcceptedParticles;
  delete mAcceptedMothers;
  delete mAcceptedDaughters;
}
//__________________
StHbtString StHbtXDFReader::Report(){
  StHbtString temp = "\n This is the StHbtXDFReader\n";  temp += "---> EventCuts in Reader: ";
  if (mEventCut) {
    temp += mEventCut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n---> TrackCuts in Reader:\n ";
  if (mTrackCut) {
    temp += mTrackCut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n";
  temp += "\n---> V0Cuts in Reader:\n ";
  if (mV0Cut) {
    temp += mV0Cut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n";

  char Ctemp[10];
  temp += "\n  Accepted Particles (pdgId): ";
  for (pdgIdListIterator iter=mAcceptedParticles->begin(); iter!=mAcceptedParticles->end(); iter++) {
    sprintf(Ctemp," %i",*iter);
    temp += Ctemp;
  }
  temp += "\n  Accepted Mothers (pdgId): ";
  for (pdgIdListIterator iter=mAcceptedMothers->begin(); iter!=mAcceptedMothers->end(); iter++) {
    sprintf(Ctemp," %i",*iter);
    temp += Ctemp;
  }
  temp += "\n  Accepted Daughters (pdgId): ";
  for (pdgIdListIterator iter=mAcceptedDaughters->begin(); iter!=mAcceptedDaughters->end(); iter++) {
    sprintf(Ctemp," %i",*iter);
    temp += Ctemp;
  }
  temp += "\n";

  return temp;
}
//__________________
void StHbtXDFReader::AddAcceptedParticle( int pdgCode ){
  mAcceptedParticles->push_back( pdgCode );
    }
//__________________
void StHbtXDFReader::AddAcceptedMother( int pdgCode ){
  mAcceptedMothers->push_back( pdgCode );
}
//__________________
void StHbtXDFReader::AddAcceptedDaughter( int pdgCode ){
  mAcceptedDaughters->push_back( pdgCode );
}
//__________________
int StHbtXDFReader::CheckPdgIdList( pdgIdList* list, int pdgCode ){
  if (list->size()==0) return 1; // if there are no specified particles at all, accept everything
  for (pdgIdListIterator iter=list->begin(); iter!=list->end(); iter++)
    if ( (*iter)==pdgCode) return 1; // particle accepted
  return 0; // particle refused
}
//__________________
StHbtEvent* StHbtXDFReader::ReturnHbtEvent(){

  cout << "StHbtXDFReader::ReturnHbtEvent" << endl;

  StEvtHddr* fEvtHddr = (StEvtHddr*)GetDataSet("EvtHddr");
#ifdef STHBTDEBUG
  if (fEvtHddr) {
    cout << " got event headder " << fEvtHddr << endl;
  }
#endif

  St_DataSet* event; 
  event = GetDataSet(mDataSetName);
#ifdef STHBTDEBUG
  cout << " event " << event << endl;
#endif
  if (!event) {
    cout << "StHbtXDFReader - no Event found, check dataSetName !!! " << endl;
    return 0;
  }
  St_DataSetIter iter(event);	// create a new iterator object for each event pointing to latest event gotten
#ifdef STHBTDEBUG
  cout << " iter " << endl;
#endif
  St_DataSet* set = iter.Cd(mParticleTableDirectory); // change directory to where particles are and pass address to set
  if (!set) {
    cout << "StHbtXDFReader - ,could 'nt change to particle table directory, check particleTableDirectory !!! " << endl;
    return 0;
  }
  St_particle* part = (St_particle *)set;	// pass that address to part (casting set to type St_particle)
#ifdef STHBTDEBUG
  cout << " part " << part << endl;
#endif
  particle_st* particle = part->GetTable();	// get the table and have 'particle' point to it
  if (!particle) {
    cout << "StHbtXDFReader - couldn't find  particle table!!! " << endl;
    return 0;
  }
  
  StHbtEvent* hbtEvent = new StHbtEvent;
  
  int mult = part->GetNRows();

  hbtEvent->SetNumberOfTracks(mult);
  hbtEvent->SetNumberOfGoodTracks(mult);

  StHbtThreeVector VertexPosition(0.,0.,0.); // vertex position is 0.,0.,0. for the event generators
  hbtEvent->SetPrimVertPos(VertexPosition);
#ifdef STHBTDEBUG
  cout << " primary vertex : " << VertexPosition << endl;
#endif  
  // By now, all event-wise information has been extracted and stored in hbtEvent
  // see if it passes any front-loaded event cut
  if (mEventCut){
    if (!(mEventCut->Pass(hbtEvent))){    // event failed! - return null pointer (but leave Reader status flag as "good")
      delete hbtEvent;
      return 0;
    }
  }

  int acceptedParticles=mAcceptedParticles->size();
  int acceptedMothers=mAcceptedMothers->size();
  int acceptedDaughters=mAcceptedDaughters->size();
  int checkParticle; 
  int checkMother;
  int checkDaughter;
  bool checkPdgIdLists;
  if (acceptedParticles+acceptedMothers+acceptedDaughters>0)
    checkPdgIdLists = 1;
  
  cout << "StHbtXDFReader::ReturnHbtEvent - We have " << mult << " tracks to store " << endl;
  // loop over all the tracks
  for (int icount=0; icount<mult; icount++){
#ifdef STHBTDEBUG
    cout << " track# " << icount << endl;
#endif    
     // particle identification
    long particleId=particle[icount].idhep; // according to the PDG standard
    //cout << " particleId " << particleId << endl;
    StParticleDefinition* StParticle= StParticleTable::instance()->findParticle(particleId);
    if (!StParticle) {
#ifdef STHBTDEBUG
      cout << " unknown particleId " << particleId << endl;
#endif
      continue;
    }
    
    if ( StParticle->charge()==0 ) {
#ifdef STHBTDEBUG
      cout << " neutral particle, track deleted " << endl; 
#endif
      continue;
    }
    
    if (checkPdgIdLists) {
      checkParticle=0;
      checkMother=0;
      checkDaughter=0;
      // check particle
      checkParticle = CheckPdgIdList(mAcceptedParticles, particleId);
      //cout << " particleID " << particleId << " " << checkParticle << endl;
      // check mothers
      for (int iMother=0; iMother<2; iMother++) { 
	checkMother+= CheckPdgIdList(mAcceptedMothers,particle[particle[icount].jmohep[iMother]].idhep);
#ifdef STHBTDEBUG
	cout << " motherID " << particle[particle[icount].jmohep[iMother]].idhep << " " << checkMother << endl;
#endif
      }
      // check daughters
      int firstDaughterPosition = particle[icount].jdahep[0];
      int lastDaughterPosition = particle[icount].jdahep[1];
      for (int iDaughter=firstDaughterPosition; iDaughter<=lastDaughterPosition; iDaughter++) {
	checkDaughter+= CheckPdgIdList(mAcceptedDaughters,particle[iDaughter].idhep);
#ifdef STHBTDEBUG
	cout << " daughterID " << particle[iDaughter].idhep << " " << checkDaughter << endl;
#endif
      }
      // all together
      if ( !(checkParticle && checkMother && checkDaughter) ) {
	continue;
      }
    }
    

    StHbtTrack* hbtTrack = new StHbtTrack;
#ifdef STHBTDEBUG
    cout << "StHbtTrack instantiated " << endl;
#endif    

    hbtTrack->SetTrackId(icount);

    switch (particleId) {
    case 11:  // intentional fall-through
    case -11:  // gid=211,-211 is pion
      hbtTrack->SetNSigmaElectron(0.);
      hbtTrack->SetNSigmaPion(-999.);
      hbtTrack->SetNSigmaKaon(-999.);
      hbtTrack->SetNSigmaProton(-999.);
      break;
    case 211:  // intentional fall-through
    case -211:  // gid=211,-211 is pion
      hbtTrack->SetNSigmaElectron(999.);
      hbtTrack->SetNSigmaPion(0.);
      hbtTrack->SetNSigmaKaon(-999.);
      hbtTrack->SetNSigmaProton(-999.);
      break;
    case 321:  // intentional fall-through
    case -321:  // gid=321,-321 is kaon
      hbtTrack->SetNSigmaElectron(999.);
      hbtTrack->SetNSigmaPion(999.0);
      hbtTrack->SetNSigmaKaon(0.);
      hbtTrack->SetNSigmaProton(-999.);
      break;
    case 2212:  // intentional fall-through
    case -2212:  // gid=2212,-2212 is proton
      hbtTrack->SetNSigmaElectron(999.);
      hbtTrack->SetNSigmaPion(999.);
      hbtTrack->SetNSigmaKaon(999.);
      hbtTrack->SetNSigmaProton(0.);
      break;
    default:
      hbtTrack->SetNSigmaElectron(999.);
      hbtTrack->SetNSigmaPion(999.);
      hbtTrack->SetNSigmaKaon(999.);
      hbtTrack->SetNSigmaProton(999.);
      break;
    }
    
    StHbtThreeVector p(particle[icount].phep[0],particle[icount].phep[1],particle[icount].phep[2]); 
    hbtTrack->SetP(p);
#ifdef STHBTDEBUG
    cout << "p: " << p << endl;
#endif
    hbtTrack->SetPt( p.perp() );   
#ifdef STHBTDEBUG
    cout << " Pt " << hbtTrack->Pt() << endl;
#endif


    hbtTrack->SetCharge( StParticle->charge() ); 
#ifdef STHBTDEBUG
    cout << " charge " << hbtTrack->Charge() << endl;
#endif 
    hbtTrack->SetdEdx( dedxMean( StParticle->mass(), p.mag() ) ); 
#ifdef STHBTDEBUG
    cout << " dedx " << hbtTrack->dEdx() << endl;
#endif    
   StHbtThreeVector productionVertex(particle[icount].vhep[0], particle[icount].vhep[1], particle[icount].vhep[2]);
   productionVertex*=millimeter;
#ifdef TheWorldIsNice
    StPhysicalHelix helix = StPhysicalHelix( hbtTrack->P(), productionVertex, HBT_BFIELD, hbtTrack->Charge() ); 
#else
    StHbtThreeVector tmpSV;    
    tmpSV.setX( productionVertex.x() );
    tmpSV.setY( productionVertex.y() );
    tmpSV.setZ( productionVertex.z() );
    StPhysicalHelixD helix = StPhysicalHelixD( hbtTrack->P(), tmpSV, HBT_BFIELD, hbtTrack->Charge() ); 
#endif
    hbtTrack->SetHelix(helix);

    double pathlength = helix.pathLength(VertexPosition);
#ifdef STHBTDEBUG
    cout << "pathlength\t" << pathlength << endl;
#endif    
    StHbtThreeVector DCAxyz = helix.at(pathlength)-VertexPosition;
#ifdef STHBTDEBUG
    cout << "DCA\t\t" << DCAxyz << " " << DCAxyz.perp() << endl;
#endif
    double maxRadius1 = min( fabs(200.*p.perp()/p.z()), 200.); //
    double maxRadius2 = min( fabs(1./helix.curvature()), 200.);
    double maxRadius =  max( maxRadius1, maxRadius2 );
    unsigned short maxHits = (unsigned short) floor( max((maxRadius-50.)*45./150., 0.) ); // 45 padRows from r=50cm---200cm 
    hbtTrack->SetNHits(maxHits); // for now
    hbtTrack->SetNHitsPossible(maxHits); // for now
#ifdef STHBTDEBUG
    cout << maxHits << endl;
#endif
    hbtTrack->SetDCAxy( DCAxyz.perp() );     // in xy-plane
    hbtTrack->SetDCAz( DCAxyz.z() );         // in z direction

    hbtTrack->SetChiSquaredXY( 0.); 
    hbtTrack->SetChiSquaredZ( 0.); 

    hbtTrack->SetPt(p.perp());
#ifdef STHBTDEBUG
    cout << "pushing..." <<endl;
#endif
    // By now, all track-wise information has been extracted and stored in hbtTrack
    // see if it passes any front-loaded event cut
    if (mTrackCut){
      if (!(mTrackCut->Pass(hbtTrack))){                  // track failed - delete it and skip the push_back
	delete hbtTrack;
	continue;
      }
    }

    hbtEvent->TrackCollection()->push_back(hbtTrack);
  }

  printf("%8i(%i) tracks pushed into collection \n",hbtEvent->TrackCollection()->size(),mult); 

  return hbtEvent;
}


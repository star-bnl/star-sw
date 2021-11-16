/*!
 * \class  StMcEvent
 * \brief  Event data structure to hold all information from a Monte Carlo simulation.
 * This class is the interface to access all information from the GEANT simulations
 * in STAR. From here one can get all vertices, tracks, hits in the event.
 * \author Manuel Calderon de la Barca Sanchez (Yale, BNL, IU) calderon@iucf.indiana.edu
 * \date   July 1999
 *
 ***************************************************************************
 *
 * $Id: StMcEvent.cc,v 2.40 2016/05/17 19:41:50 perev Exp $
 * $Log: StMcEvent.cc,v $
 * Revision 2.40  2016/05/17 19:41:50  perev
 * Coverity
 *
 * Revision 2.39  2016/05/16 23:47:09  perev
 * Coverity fix
 *
 * Revision 2.38  2015/03/13 18:44:58  perev
 * Roll back
 *
 * Revision 2.36  2013/03/25 23:26:39  perev
 * Mustafa.Pxl corrs+Cleanup
 *
 * Revision 2.35  2012/04/19 16:22:51  perev
 * Cleanup
 *
 * Revision 2.34  2012/03/22 13:56:27  fisyak
 * Add missing parentheses
 *
 * Revision 2.33  2012/03/22 00:36:41  perev
 * Etr added
 *
 * Revision 2.32  2012/03/01 16:48:29  perev
 * method Browse() added
 *
 * Revision 2.31  2011/10/11 01:12:14  perev
 * Mtd added
 *
 * Revision 2.30  2011/03/22 22:30:33  perev
 * Bug#2111 Remove redundant zeroing
 *
 * Revision 2.29  2009/07/24 19:08:06  perev
 * Cleanup + Btof added (Geurts)
 *
 * Revision 2.28  2007/10/16 19:49:13  fisyak
 * rename Hft => Pxl, remove Hpd, Igt and Fst
 *
 * Revision 2.27  2006/09/25 14:31:15  fisyak
 * remove duplicated SafeDelete(mTofHits)
 *
 * Revision 2.24  2005/11/22 21:44:51  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.23  2005/10/03 16:58:17  calderon
 * Fixed bug in logic of printing 2nd vertex.
 *
 * Revision 2.22  2005/10/01 00:01:10  calderon
 * Fixed bug in printing of parent tracks.  Was calling the hit array out of bounds.
 *
 * Revision 2.21  2005/09/29 01:01:10  calderon
 * Fixed bugs in printing event and hit information.
 * Format operator<< for various classes.
 *
 * Revision 2.20  2005/09/28 21:30:14  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.19  2005/07/07 18:20:49  calderon
 * Added support for IGT detector.
 *
 * Revision 2.18  2005/05/27 23:37:25  calderon
 * Update for EEMC, add eprs, esmdu esdmv hits to StMcEvent.
 *
 * Revision 2.17  2005/04/18 20:11:32  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 * Revision 2.16  2005/01/27 23:40:47  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.15  2004/09/14 05:00:29  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
 * Revision 2.14  2003/12/04 05:56:47  calderon
 * Inclusion of Endcap EMC hit collection in StMcEvent and
 * of the Endcap hit vector in StMcTrack.
 * fix const of StMcVertex::parent() to avoid warnings in user code
 *
 * Revision 2.13  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 * Revision 2.12  2003/05/15 18:28:47  calderon
 * Added data members from modified g2t_event table:
 * Event Generator Final State Tracks, N Binary Collisions,
 * N Wounded Nucleons East and West, N Jets.
 *
 * Revision 2.11  2003/04/17 18:01:24  calderon
 * print the subprocess id when dumping event info in operator<<
 *
 * Revision 2.10  2003/03/18 22:37:39  calderon
 * Added member mSubProcessId which is used for Pythia events.
 * Only is set from constructor from g2t_event table.
 *
 * Revision 2.9  2003/02/19 03:16:05  calderon
 * Introduction of Ctb Hit Class and Ctb Hit Collection class, modified
 * StMcTrack, and StMcEvent accordingly.  Clearing of hits in StMcSvtWaferHitCollection.
 *
 * Revision 2.8  2000/06/06 02:58:40  calderon
 * Introduction of Calorimeter classes.  Modified several classes
 * accordingly.
 *
 * Revision 2.7  2000/05/11 14:27:23  calderon
 * use clear() in destructors to reduce size of containers
 *
 * Revision 2.6  2000/04/17 23:01:14  calderon
 * Added local momentum to hits as per Lee's request
 *
 * Revision 2.5  2000/03/06 18:05:21  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 * Revision 2.4  2000/01/18 20:52:31  calderon
 * Works with CC5
 *
 * Revision 2.3  1999/12/14 07:04:49  calderon
 * Numbering scheme as per SVT request.
 *
 * Revision 2.2  1999/12/03 00:51:51  calderon
 * Tested with new StMcEventMaker.  Added messages for
 * diagnostics.
 *
 * Revision 2.1  1999/11/19 19:06:31  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:12:15  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.3  1999/09/23 21:25:50  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 * Revision 1.2  1999/07/28 20:27:32  calderon
 * Version with SL99f libraries
 *
 *
 **************************************************************************/
#include "StThreeVectorF.hh"

#include "StMcEvent.hh"
#include "StMcTpcHitCollection.hh"
#include "StMcFtpcHitCollection.hh"
#include "StMcRichHitCollection.hh"
#include "StMcCtbHitCollection.hh"
#include "StMcSvtHitCollection.hh"
#include "StMcSsdHitCollection.hh"
#include "StMcEmcHitCollection.hh"
#include "StMcTofHitCollection.hh"
#include "StMcBTofHitCollection.hh"
#include "StMcMtdHitCollection.hh"
#include "StMcPxlHitCollection.hh"
#include "StMcIstHitCollection.hh"
#include "StMcFgtHitCollection.hh"
#include "StMcEtrHitCollection.hh"
#include "StMcContainers.hh" 
#include "StMcVertex.hh"
#include "StMcTrack.hh"
#include "StMcTpcHit.hh"
#include "StMcFtpcHit.hh"
#include "StMcRichHit.hh"
#include "StMcSvtHit.hh"
#include "StMcSsdHit.hh"
#include "StMcCalorimeterHit.hh"
#include "StMcTofHit.hh"
#include "StMcBTofHit.hh"
#include "StMcMtdHit.hh"
#include "StMcPxlHit.hh"
#include "StMcIstHit.hh"
#include "StMcFgtHit.hh"
#include "StMcEtrHit.hh"
#include "tables/St_g2t_event_Table.h"
#include "TDataSetIter.h"


TString StMcEvent::mCvsTag = "$Id: StMcEvent.cc,v 2.40 2016/05/17 19:41:50 perev Exp $";
static const char rcsid[] = "$Id: StMcEvent.cc,v 2.40 2016/05/17 19:41:50 perev Exp $";
ClassImp(StMcEvent);
//______________________________________________________________________________
void StMcEvent::initToZero()
{
    memset(mBeg,0,mEnd-mBeg+1);    
}
//______________________________________________________________________________
void StMcEvent::makeColls()
{
    // Create the collections
    
    mTpcHits  = new StMcTpcHitCollection();
    mSvtHits  = new StMcSvtHitCollection();
    mSsdHits  = new StMcSsdHitCollection();
    mFtpcHits = new StMcFtpcHitCollection();
    mRichHits = new StMcRichHitCollection();
    mCtbHits  = new StMcCtbHitCollection();
    mTofHits  = new StMcTofHitCollection();
    mBTofHits = new StMcBTofHitCollection();
    mMtdHits  = new StMcMtdHitCollection();
    mPxlHits  = new StMcPxlHitCollection();
    mIstHits  = new StMcIstHitCollection();
    mFgtHits  = new StMcFgtHitCollection();
    mEtrHits  = new StMcEtrHitCollection();
}

//______________________________________________________________________________
StMcEvent::StMcEvent() :TDataSet("StMcEvent")
{
    initToZero();
}

//______________________________________________________________________________
StMcEvent::StMcEvent(g2t_event_st* evTable)
    :TDataSet("StMcEvent")
{
     initToZero();
     mEventGeneratorEventLabel = evTable->eg_label;
     mEventNumber = evTable->n_event;
     mRunNumber = evTable->n_run;
     mType  = evTable->event_type;
     mZWest = evTable->n_part_prot_west;
     mNWest = evTable->n_part_neut_west;
     mZEast = evTable->n_part_prot_east;
     mNEast = evTable->n_part_neut_east;
     mEvGenFSTracks = evTable->n_track_eg_fs;
     mPrimaryTracks = evTable->n_track_prim;
     mSubProcessId = evTable->subprocess_id;
     mImpactParameter = evTable->b_impact;
     mPhiReactionPlane = evTable->phi_impact;
     mTriggerTimeOffset = evTable->time_offset;
     mNBinary = evTable->n_binary;
     mNWoundedEast = evTable->n_wounded_east;
     mNWoundedWest = evTable->n_wounded_west;
     mNJets = evTable->njets;
     makeColls();
}


//______________________________________________________________________________
StMcEvent::StMcEvent(const StMcEvent&) { /* noop */} // private

//______________________________________________________________________________
const StMcEvent& StMcEvent::operator=(const StMcEvent&) { return *this;} // private

//______________________________________________________________________________
StMcEvent::~StMcEvent()
{
  for (auto jk=mBegColl; jk<mEndColl;jk++) { 
    delete *jk; *jk=0;
  }
  for(auto jk=0;jk<(int)mTracks.size()  ;jk++) {delete mTracks[jk]  ;}    
  mTracks.clear();
  
  for(int jk=0;jk<(int)mVertices.size();jk++) {delete mVertices[jk];}
  mVertices.clear();
}

//______________________________________________________________________________
int StMcEvent::operator==(const StMcEvent& e) const
{
  return (e.mEventNumber == mEventNumber &&
	  e.mRunNumber   == mRunNumber &&
	  e.mType        == mType
	  );  
}

//______________________________________________________________________________
int StMcEvent::operator!=(const StMcEvent& e) const
{
    return !(e == *this);   // invoke operator==()
}

//______________________________________________________________________________
ostream&  operator<<(ostream& os, const StMcEvent& e)
{
    os << "Label: " << e.eventGeneratorEventLabel()
       << "\tRun: " << e.runNumber()
       << "\tId: " << e.eventNumber() 
       << "\tType: " << e.type() << endl;
    os << "Participant Protons  East: " << e.zEast() << "\tWest: " << e.zWest() << endl;
    os << "Participant Neutrons East: " << e.nEast() << "\tWest: " << e.nWest() << endl;
    os << "# Ev. Gen. Fin. St. Track: " << e.eventGeneratorFinalStateTracks() 
       << "\tNumber of Primary Tracks : " << e.numberOfPrimaryTracks() << endl;
    os << "Subprocess Id    : " << e.subProcessId() << endl;
    os << "Impact Parameter : " << e.impactParameter() << endl;
    os << "Phi Reaction Pl. : " << e.phiReactionPlane() << endl;
    os << "Trig. Time Offset: " << e.triggerTimeOffset() << endl;
    os << "N Binary Coll.   : " << e.nBinary() << endl;
    os << "N Wounded East   : " << e.nWoundedEast() << endl;
    os << "N Wounded West   : " << e.nWoundedWest() << endl;
    os << "N Jets           : " << e.nJets() << endl;
    return os;
}


//______________________________________________________________________________
void StMcEvent::setEventGeneratorEventLabel(unsigned long val) { mEventGeneratorEventLabel = val; }

//______________________________________________________________________________
void StMcEvent::setEventNumber(unsigned long  val) { mEventNumber = val;  }

//______________________________________________________________________________
void StMcEvent::setRunNumber(unsigned long val) { mRunNumber = val; }                

//______________________________________________________________________________
void StMcEvent::setType(unsigned long val) { mType = val; }              

//______________________________________________________________________________
void StMcEvent::setZWest(unsigned long val) { mZWest = val; }              
//______________________________________________________________________________

void StMcEvent::setNWest(unsigned long val) { mNWest = val; }     

//______________________________________________________________________________
void StMcEvent::setZEast(unsigned long val) { mZEast = val; }              

//______________________________________________________________________________
void StMcEvent::setNEast(unsigned long val) { mNEast = val; }     

//______________________________________________________________________________
void StMcEvent::setEventGeneratorFinalStateTracks(unsigned long val){ mEvGenFSTracks = val; } 

//______________________________________________________________________________
void StMcEvent::setNumberOfPrimaryTracks(unsigned long val){ mPrimaryTracks = val; } 

//______________________________________________________________________________
void StMcEvent::setImpactParameter(float val) { mImpactParameter = val; }               

//______________________________________________________________________________
void StMcEvent::setPhiReactionPlane(float val) { mPhiReactionPlane = val; } 

//______________________________________________________________________________
void StMcEvent::setTriggerTimeOffset(float val) { mTriggerTimeOffset = val; }

//______________________________________________________________________________
void StMcEvent::setNBinary(unsigned long val) { mNBinary = val; }

//______________________________________________________________________________
void StMcEvent::setNWoundedEast(unsigned long val) { mNWoundedEast = val; }

//______________________________________________________________________________
void StMcEvent::setNWoundedWest(unsigned long val) { mNWoundedWest = val; }

//______________________________________________________________________________
void StMcEvent::setNJets(unsigned long val) { mNJets = val; }

//______________________________________________________________________________
void StMcEvent::setPrimaryVertex(StMcVertex* val) {  mPrimaryVertex = val; }

//______________________________________________________________________________
void StMcEvent::setTpcHitCollection(StMcTpcHitCollection* val)
{   
    if (mTpcHits!= val) delete mTpcHits;
    mTpcHits = val;
}               

//______________________________________________________________________________
void StMcEvent::setSvtHitCollection(StMcSvtHitCollection* val)
{
    if (mSvtHits!= val) delete mSvtHits;
    mSvtHits = val;
}               

//______________________________________________________________________________
void StMcEvent::setSsdHitCollection(StMcSsdHitCollection* val)
{
    if (mSsdHits!= val) delete mSsdHits;
    mSsdHits = val;
}               

//______________________________________________________________________________
void StMcEvent::setFtpcHitCollection(StMcFtpcHitCollection* val)
{
    if (mFtpcHits!= val) delete mFtpcHits;
    mFtpcHits = val;
}              

//______________________________________________________________________________
void StMcEvent::setRichHitCollection(StMcRichHitCollection* val)
{
    if (mRichHits!= val) delete mRichHits;
    mRichHits = val;
}              
//______________________________________________________________________________
void StMcEvent::setTofHitCollection(StMcTofHitCollection* val)
{
    if (mTofHits!= val) delete mTofHits;
    mTofHits = val;
}
//______________________________________________________________________________
void StMcEvent::setBTofHitCollection(StMcBTofHitCollection* val)
{
    if (mBTofHits!= val) delete mBTofHits;
    mBTofHits = val;
}
//______________________________________________________________________________
void StMcEvent::setMtdHitCollection(StMcMtdHitCollection* val)
{
    if (mMtdHits!= val) delete mMtdHits;
    mMtdHits = val;
}
//______________________________________________________________________________
void StMcEvent::setPxlHitCollection(StMcPxlHitCollection* val)
{
    if (mPxlHits!= val) delete mPxlHits;
    mPxlHits = val;
}   

//______________________________________________________________________________
void StMcEvent::setIstHitCollection(StMcIstHitCollection* val)
{
    if (mIstHits!= val) delete mIstHits;
    mIstHits = val;
}
   

//______________________________________________________________________________
void StMcEvent::setFgtHitCollection(StMcFgtHitCollection* val)
{
    if (mFgtHits!= val) delete mFgtHits;
    mFgtHits = val;
}
//______________________________________________________________________________
//______________________________________________________________________________
#define PrintHeader(Name,name) \
  const StMc ## Name ## HitCollection *name ## Coll = name ## HitCollection();\
  cout << "---------------------------------------------------------" << endl;\
  if (! all) {\
    cout << "StMc"#Name"HitCollection at " << (void*) name ## Coll             << endl;	\
    cout << "Dumping collection size";					\
    cout << " and one hit only.";					\
    cout << endl;							\
  }									\
  cout << "---------------------------------------------------------" << endl;\
  nhits = name ## Coll->numberOfHits();\
  cout << "# of hits in StMc"#Name"HitCollection = " << nhits << endl;
#define PrintHitCollection(Name,name)					\
  PrintHeader(Name,name)						\
    if ( name ## Coll &&  nhits) {					\
      nh = name ## Coll->hits().size();					\
      if (nh) {								\
	if (! all) {nh = 1;  gotOneHit = kTRUE;}			\
	for (i = 0; i < nh; i++) {                                      \
	  if (! all) cout << *(name ## Coll->hits()[i]);		\
	  else (name ## Coll->hits()[i])->Print();			\
	  cout << endl;							\
	  if (! all) {							\
	    cout << "Parent Track of this hit" << endl;			\
	    cout << *(name ## Coll->hits()[i]->parentTrack()) << endl;	\
	  }								\
        }                                                               \
      }									\
    }
#define PrintHitCollectionL(Name,name,layer,Layers)			\
  PrintHeader(Name,name)						\
    if ( name ## Coll &&  nhits) {					\
      gotOneHit = kFALSE;						\
      for (k=0; !gotOneHit && k<name ## Coll->numberOf ## Layers(); k++) \
	if (name ## Coll->layer(k))				\
	  {								\
	    { nh = name ## Coll->layer(k)->hits().size();	\
	      if (nh) {							\
		  if (! all) {nh = 1;  gotOneHit = kTRUE;}		\
		  for (i = 0; i < nh; i++) {                            \
		    if (! all) cout << *(name ## Coll->layer(k)->hits()[i]); \
		    else   (name ## Coll->layer(k)->hits()[i])->Print(); \
		    cout << endl;					\
		    if (! all) {					\
		      cout << "Parent Track of this hit" << endl;	\
		      cout << *(name ## Coll->layer(k)->hits()[i]->parentTrack()) << endl; \
		    }							\
                  }							\
	      }								\
	    }								\
	  }								\
    }
#define PrintHitCollectionW(Name,name,ladder,Ladders,wafer,Wafers)	\
  PrintHeader(Name,name);						\
  if ( name ## Coll &&  nhits) {					\
    gotOneHit = kFALSE;							\
    for (k=0; !gotOneHit && k<name ## Coll->numberOf ## Ladders(); k++) \
      if (name ## Coll->ladder(k))					\
	{								\
	  for (l = 0; gotOneHit && k<name ## Coll->ladder(k)->numberOf ## Wafers(); l++) \
	    { nh = name ## Coll->ladder(k)->wafer(l)->hits().size();	\
	      if (nh) {							\
		if (! all) {nh = 1;  gotOneHit = kTRUE;}		\
		for (i = 0; i < nh; i++) {				\
		  if (! all) cout << *(name ## Coll->ladder(k)->wafer(l)->hits()[i]); \
		  else   (name ## Coll->ladder(k)->wafer(l)->hits()[i])->Print(); \
		  cout << endl;						\
		  if (! all) {						\
		    cout << "Parent Track of this hit" << endl;		\
		    cout << *(name ## Coll->ladder(k)->wafer(l)->hits()[i]->parentTrack()) << endl; \
		  }							\
		}							\
	      }								\
	    }								\
	}								\
  }

//________________________________________________________________________________
void StMcEvent::Print(Option_t *option) const {
  TString Opt(option);
  Int_t all = 0;
  if (Opt.Contains("all",TString::kIgnoreCase)) all = 1;
  //
  // Printing all the information of the components of StMcEvent.
  // For printing the simple data members of StMcEvent, use operator<<
  //
  //
  
  cout << "---------------------------------------------------------" << endl;
  cout << "StSPtrVecMcTrack"                                          << endl;
  if (! all) 
      cout << "Dumping first element in collection only (if available). " << endl;
  cout << "---------------------------------------------------------" << endl;
  Int_t Ntracks = tracks().size();
  cout << "collection size = " << Ntracks    << endl;
  if (Ntracks) {
      if (! all) { // Option for printing only the first track.
	  cout << "---------------------------------------------------------" << endl;
	  cout << "StMcTrack at " << tracks()[0] << endl;
	  cout << "---------------------------------------------------------" << endl;
	  cout << *(tracks()[0])                             << endl;
      }
      else { // Option for printing "all" tracks.
	  tracks()[0]->Print("desc");
	  for (int i = 0; i < Ntracks; i++) {
	    (tracks()[i])->particleDefinition();
	    (tracks()[i])->Print("all");
	  }
      }
  }
  cout << "---------------------------------------------------------" << endl;
  if (! all) {
    cout << "StMcVertex" << endl;
    cout << "Dumping vertex info and first daughter track.            " << endl;
    cout << "---------------------------------------------------------" << endl;
    if (primaryVertex()) {
      cout << "Primary Vertex at " << primaryVertex() << endl;
      cout << "---------------------------------------------------------" << endl;
      cout << *primaryVertex() << endl;
      cout << "---------------------------------------------------------" << endl;
      cout << "First Daughter of Primary Vertex" << endl;
      cout << *(primaryVertex()->daughter(0)) << endl;
      cout << "---------------------------------------------------------" << endl;
    }
    else cout << "No Primary Vertex " << endl;
  }
  Int_t i1 = 0;
  Int_t nVertices = vertices().size();
  cout << "StSPtrVecMcVertex"                                         << endl;
  cout << "# of Vertices    : " << nVertices << endl;
  cout << "---------------------------------------------------------" << endl;
  if (nVertices > 1) {
    if (! all) {
      cout << "Daughters of second Vertex : " << vertices()[1]->numberOfDaughters() << endl;
      if (vertices()[1]->numberOfDaughters()) {
	cout << "First Daughter of this Vertex" << endl;
	cout << *(vertices()[1]->daughter(0)) << endl;
      }
      i1 = 1; 
      nVertices = 2;
      cout << "---------------------------------------------------------" << endl;
      cout << "Dumping vertices" << endl;
    }
    for (int i = i1; i < nVertices; i++) {
      if (! all) {
	cout << "---------------------------------------------------------" << endl;
	cout << "StMcVertex " << i+1 << " at " << vertices()[i]  << endl;
	cout << "---------------------------------------------------------" << endl;
	cout << *(vertices()[i]) << endl;
	cout << "---------------------------------------------------------" << endl;	  
      }
      else {
	cout << "StMcVertex " << i+1 << " at ";
	(vertices()[i])->Print();
      }
    }
  }
  UInt_t       i, j, k, l, ii, nhits, nh;
  Bool_t             gotOneHit;
  PrintHeader(Tpc,tpc)
  if (tpcColl && nhits) {
      gotOneHit = kFALSE;
      for (k=0; !gotOneHit && k<tpcColl->numberOfSectors(); k++) {
	  for (j=0; !gotOneHit && j<tpcColl->sector(k)->numberOfPadrows(); j++) {
	      const StSPtrVecMcTpcHit &hits = tpcColl->sector(k)->padrow(j)->hits();
	      nh = hits.size();
	      if (nh) {
		  if (! all ) {
		      cout << *hits[0] << endl;
		      cout << "Parent Track of this hit" << endl;
		      cout << *(hits[0]->parentTrack()) << endl;
		      gotOneHit = kTRUE;
		      cout << "Dumping all the z coordinates and track key in this padrow" << endl;
		      cout << "Should be sorted according to z: " << endl;
		      cout << "---------------------------------------------------------" << endl;
		      for (i = 0; i < nh; i++) 
			  cout << "\t" << hits[i]->position().z() << ":" <<  hits[i]->parentTrack()->key();
		      cout << endl;
		  }
		  else for (i = 0; i < nh; i++) {
		    hits[i]->Print(); cout << endl;
		  }
	      } // check for nh
	      
	  } // padrow loop
      }// sector loop
  }
  
  PrintHitCollectionL(Ftpc,ftpc,plane,Planes);
  PrintHitCollection(Rich,rich);
  
  PrintHeader(Svt,svt);
  nhits = svtColl->numberOfHits();
  if (svtColl && nhits) {
      
    gotOneHit = kFALSE;
    for (k=0; !gotOneHit && k<svtColl->numberOfBarrels(); k++)
	for (j=0; !gotOneHit && j<svtColl->barrel(k)->numberOfLadders(); j++)
	    for (i=0; !gotOneHit && i<svtColl->barrel(k)->ladder(j)->numberOfWafers(); i++) {
		nh = svtColl->barrel(k)->ladder(j)->wafer(i)->hits().size();
		
		if (nh) {
		    if (! all) {nh = 1;  gotOneHit = kTRUE;}
		    for (ii = 0; ii < nh; ii++) {
			if (! all) {
			  cout << *(svtColl->barrel(k)->ladder(j)->wafer(i)->hits()[ii]) << endl;
			  cout << "Parent track of this Hit" << endl;
			  cout << *(svtColl->barrel(k)->ladder(j)->wafer(i)->hits()[ii]->parentTrack()) << endl;
			} else {(svtColl->barrel(k)->ladder(j)->wafer(i)->hits()[ii])->Print(); cout << endl;}
		    }
		}
	    }
  }
  PrintHitCollectionW(Ssd,ssd,ladder,Ladders,wafer,Wafers);
  PrintHitCollection(Tof,tof);
  PrintHitCollection(BTof,btof);
  PrintHitCollection(Mtd,mtd);
  //PrintHitCollectionL(Pxl,pxl,sector,Sectors);
  PrintHitCollectionL(Ist,ist,layer,Layers);
  PrintHitCollectionL(Fgt,fgt,layer,Layers);
  PrintHitCollection(Etr,etr);
  
  TDataSet *mcEvent = (TDataSet *) this;
  TDataSetIter next(mcEvent);
  TDataSet *ds = 0;
  while( (ds = next())) {
    StMcEmcHitCollection *emcColl = dynamic_cast<StMcEmcHitCollection *>(ds);
    if (emcColl) emcColl->Print(option);
  }
}  
//______________________________________________________________________________
void StMcEvent::Browse(TBrowser *b)
{
  // Browse this event (called by TBrowser).
   for (auto jk=mBegColl; jk<mEndColl;jk++) { 
     TObject *obj = *jk; if (!obj) continue;
     b->Add(obj,obj->GetName());
   }
   TDataSet::Browse(b);
}
//______________________________________________________________________________
#undef  PrintHitCollection
#undef  PrintHitCollectionL
#undef  PrintHeader
  

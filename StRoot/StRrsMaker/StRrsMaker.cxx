/******************************************************
 * $Id: StRrsMaker.cxx,v 1.19 2000/04/26 18:58:39 lasiuk Exp $
 * Description:
 *  Implementation of the Maker main module.
 *
 * $Log: StRrsMaker.cxx,v $
 * Revision 1.19  2000/04/26 18:58:39  lasiuk
 * comment diagnostics
 *
 * Revision 1.18  2000/04/14 22:38:09  lasiuk
 * add print diagnostic for crash
 * extra careful on clearing the dataset
 *
 * Revision 1.17  2000/04/05 15:57:11  lasiuk
 * SIMU 2 protocol
 *
 * Revision 1.16  2000/04/03 22:52:28  lasiuk
 * check pointer for non-specified GEANT particles
 *
 * Revision 1.15  2000/03/21 17:04:21  lasiuk
 * remove forced delete of singleton classes
 *
 * Revision 1.14  2000/03/17 14:55:12  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 * Revision 1.13  2000/03/13 22:17:37  lasiuk
 * unbelievable!  I can't stand it.
 * Comment the Ring drawing routines
 *
 * Revision 1.12  2000/03/13 21:58:01  lasiuk
 * singleton classes
 *
 * Revision 1.11  2000/02/29 18:05:00  lasiuk
 * include FREO, QUAR volumes
 * rotate coordinate inputs (x->-x, y->-y) for local
 * use units consistently
 *
 * Revision 1.10  2000/02/15 18:07:20  lasiuk
 * check if pointer exists.  If not, return a warning status.
 *
 * Revision 1.9  2000/02/14 01:08:02  lasiuk
 * write the data set
 * add two member functions for pedestal and noise switches
 * add coordinate conditional and StCoordinateTransform
 * incorporate track_p into GHit
 *
 * Revision 1.8  2000/02/12 21:54:25  lasiuk
 * Introduce provisions to read in local coordinates
 *
 * Revision 1.7  2000/02/08 23:46:46  lasiuk
 * comment to prevent streamer for ionize and inducesignal. Remove filter
 *
 * Revision 1.6  2000/02/08 16:36:49  lasiuk
 * Bring into line with HP
 *
 * Revision 1.5  2000/01/28 20:35:08  lasiuk
 * namespace std is NOT in!
 *
 * Revision 1.4  2000/01/27 17:10:03  lasiuk
 * modify to work stand-alone from ROOT
 *
 ******************************************************/
#ifdef __ROOT__
#include "StRrsMaker.h"             

// SWITCHES
#define rICH_DIAGNOSTIC 1
#define rICH_DECODE_DATA 1
#define rICH_WITH_PADMONITOR 1

#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_ObjectSet.h"

//#include <iostream.h>
#include <string>

#ifndef ST_NO_NAMESPACES
using std::string;
#endif

// SCL
#include "StGlobals.hh"
#include "StThreeVector.hh"

#ifdef USE_MEMORY_INFO
#include "StMemoryInfo.hh"
#endif

#include "StParticleTable.hh"
#include "StParticleTypes.hh"

// DataBases
#include "StRichGeometryDb.h"
#include "StRichPhysicsDb.h"

// Coordinates
#include "StRichCoordinates.h"
#include "StRichCoordinateTransform.h"
#include "StRichMomentumTransform.h"

#include "StRichPadPlane.h"
#include "StRichWriter.h"
#include "StRichAnalogSignalGenerator.h"

#include "StRichGHit.h"
#include "StRichMiniHit.h"


#ifdef RICH_WITH_PADMONITOR
#include <vector>
#  ifndef ST_NO_NAMESPACES
using std::vector;
#  endif
#endif

#ifdef RICH_WITH_PADMONITOR
#include "StRichSinglePixel.h"
#include "StRichSingleMCPixel.h"
#include "StRichPadMonitor.h"
#include "StRichG2TInfo.h"
#endif

#ifdef RICH_DECODE_DATA
#include "StRrsReader.h"
#endif

#ifdef  RICH_WITH_VIEWER
#include "StRichViewer.h"              // view class
#endif


// g2t tables
#include "tables/St_g2t_tpc_hit_Table.h"
#include "tables/St_g2t_rch_hit_Table.h"
#include "tables/St_g2t_track_Table.h"


// Magnetic Field
#ifdef __ROOT__
#define gufld   gufld_
//#define gufld   GUFLD
extern "C" {void gufld(Float_t *, Float_t *);}
#endif

ClassImp(StRrsMaker)          // Root Macro

StRrsMaker::StRrsMaker(const char *name)
    : StMaker(name), mUseLocalCoordinate(0), mAddPedestal(0), mAddElectricNoise(0)
{     }

StRrsMaker::~StRrsMaker()
{  /* nopt */ }

int StRrsMaker::readFile(char* file)
{
    mInputFileName = file;
    mReadFromFile = 1;
    PR(mInputFileName);
    PR(mReadFromFile);
    return kStOK;
}

int StRrsMaker::writeFile(char* file, int numEvents)
{
    mOutputFileName = file;
    mNumberOfEvents = numEvents;
    mWriteToFile = 1;
    PR(mNumberOfEvents);
    PR(mOutputFileName);
    return kStOK;
}

//
// Flags that are macro settable
// Inital value is 0!
void StRrsMaker::useLocalCoordinate(int b)
{
    mUseLocalCoordinate = b;
}

void StRrsMaker::addPedestal(int b)
{
    mAddPedestal = b;
}

void StRrsMaker::addElectricNoise(int b)
{
    mAddElectricNoise = b;
}
///////////////////////////////////////////////////////
//
//  Standard Public Methods (Init, Make, Finish)
//

Int_t StRrsMaker::Init()
{
#ifdef USE_MEMORY_INFO
    StMemoryInfo* info = StMemoryInfo::instance();
    info->snapshot();
    info->print();
#endif
    // DBs are here
    mPhysicsDb  = StRichPhysicsDb::getDb();
    mGeometryDb = StRichGeometryDb::getDb();

    if ( !mGeometryDb ) {
      cerr << "Geometry database could not be initialized. Aborting!!!\n";
      return 1;
    }

    if ( !mPhysicsDb ) {
      cerr << "Physics database could not be initialized. Aborting!!!\n";
      return 1;
    }

    //mGeometryDb->print();
    //mPhysicsDb->print();

    //
    // GEANT Table
    mTable = StParticleTable::instance();
    
    mCoordinateTransform = StRichCoordinateTransform::getTransform(mGeometryDb);
    mMomentumTransform   = StRichMomentumTransform::getTransform(mGeometryDb);

    //
    // Construct constant data set.  This is what is passed downstream
    // -->PadPlane
    //

    mPadPlane =
	new StRichPadPlane(mGeometryDb->numberOfRowsInAColumn(),
			   mGeometryDb->numberOfPadsInARow());

    AddConst(new St_ObjectSet("richPixels", mPadPlane));

    // Data Writer is here
    mWriter = StRichWriter::getInstance(mPadPlane);

    if ( !mWriter ) {
      cerr << "Output module could not be initialized. Aborting!!!\n";
      return 1;
    }

    //
    // The processors
    // ionization is a data member by value
    //
    // selectWire is a data member by value
    //
    // gasGain    is a data member by value
    //
    // Analog2dig is a data member by value
    // adds a DC level to each pad
    mADC.setAddPedestal(mAddPedestal);

    // ASG        is a SINGLETON
    mAnalogSignalGenerator = StRichAnalogSignalGenerator::getInstance(mWriter);
#ifdef USE_MEMORY_INFO
    info->snapshot();
    info->print();
#endif
	     
    return Init(true);
}

Int_t StRrsMaker::Init(int useHistos)
{
#ifdef RICH_WITH_VIEWER
    // Viewer is here
    cout << "Try make a viewer" << endl;
    StRichViewer* view = 0;
    StRichViewer::histograms = useHistos;
    //StRichViewer::foo = useHistos;
    if (StRichViewer::histograms )
	view = StRichViewer::getView();

    if ( !view ) {
	cerr << "No histograming will be done.\n";
	StRichViewer::histograms = 0;
    }
#endif

    return kStOk;
}

int StRrsMaker::whichVolume(int val, string* vName)
{
    //
    // coding from GEANT is:
    //    volume+Isys*1000
    // where:
    //  Isys = 1 for RGAP
    //  Isys = 2 for RCSI
    int volume = val/1000;
    switch(volume) {
    case 1:
	*vName = string("RGAP");
	break;
    case 2:
	*vName = string("RCSI");
	break;
    case 3:
	*vName = string("QUAR");
	break;
    case 4:
	*vName = string("FREO");
	break;	
    default:
	*vName = string("");
	cerr << "StRrsMaker::whicVolume() UNKNOWN Volume" << endl;
	break;
    }
    int volumeNumber = (val - (volume*1000));
    return volumeNumber;
}

Int_t StRrsMaker::Make()
{
    cout << " -- Begin RRS Processing --" << endl;

//       cout << "-- Press return to continue -- ";
//       char c = cin.get();
#ifdef USE_MEMORY_INFO
    StMemoryInfo* info = StMemoryInfo::instance();
    info->snapshot();
    info->print();
#endif

    //cout << "Clear Pad Plane" << endl;
    mPadPlane->clear();
    //cout << "Done clear" << endl;

#ifdef RICH_DIAGNOSTIC
    ofstream raw("/afs/rhic/star/users/lasiuk/data/rings.txt");
#endif

    StRichGHit hit;
    //mWriter->clear();  // Done already in padplane

#ifdef RICH_WITH_PADMONITOR
    vector<StRichG2TInfo> g2tInfo;
    g2tInfo.clear();
#endif
    //
    // Make a list of segments to process:
    //
    list<StRichMiniHit*> theList;
    list<StRichMiniHit*>::iterator iter;
    
    //
    // scope for transformed momentum
    //
    StThreeVector<double> lTrackMomentum;
    StThreeVector<double> gTrackMomentum;

    
    //
    // Either  Read mPadPlane from file
    //
    if (mReadFromFile) {
	//mInputStream->fillTrsEvent(mPadPlane);
	cout << "Done Filling mPadPlane from File" << endl;
    }
    //
    // or do the normal processing of RRS from GEANT
    //
    else {  // else process from stream
	if (!m_DataSet->GetList())  {
	    //if DataSet is empty fill it
	    //
	    // Read the GEANT info
	    // these structures/classes are defined in:
	    // $STAR/pams/sim/idl/g2t_tpc_hit.idl 
	    // $STAR/StRoot/base/St_DataSet.h & St_Table.h 
	    //
	    
	    St_DataSetIter geant(GetDataSet("geant"));
	
	    St_g2t_track *g2t_track =
		static_cast<St_g2t_track *>(geant("g2t_track"));

 	    if(!g2t_track){
 		cout << "StRrsMaker::Make()";
 		cout << "\tNo g2t_track pointer";
 		cout << "\treturn from StRrsMaker::Make()" << endl;
 		return kStWarn;
 	    }

	    int numberOfTracks          =  g2t_track->GetNRows();
	    //PR(numberOfTracks);
	    
	    g2t_track_st *track =  g2t_track->GetTable();
	    
	    //
	    // TPC HITS
	    St_g2t_tpc_hit *g2t_tpc_hit =
		static_cast<St_g2t_tpc_hit *>(geant("g2t_tpc_hit"));

	    if(!g2t_tpc_hit){
		cout << "StRrsMaker::Make()";
		cout << "\tNo g2t_tpc_hit pointer";
		cout << "\treturn from StRrsMaker::Make()" << endl;
		return kStWarn;
	    }

	    int no_tpc_hits         =  g2t_tpc_hit->GetNRows();
	    //PR(no_tpc_hits);

	    g2t_tpc_hit_st *tpc_hit =  g2t_tpc_hit->GetTable();

	    St_g2t_rch_hit *g2t_rch_hit =
		static_cast<St_g2t_rch_hit *>(geant("g2t_rch_hit"));

	    if(!g2t_rch_hit){
		cout << "StRrsMaker::Make()";
		cout << "\tNo g2t_rch_hit pointer";
		cout << "\treturn from StRrsMaker::Make()" << endl;
		return kStWarn;
	    }
	    
	    int numberOfRichHits        =  g2t_rch_hit->GetNRows();
	    PR(numberOfRichHits);

	    g2t_rch_hit_st *rch_hit     =  g2t_rch_hit->GetTable();

	    string volumeName;
	    int    quadrant;

	    //
	    // Declarations for Transformation routines
	    //
	    StRichLocalCoordinate  local;
	    StThreeVector<double>  momentum;
	    
	    for(int ii=0; ii<numberOfRichHits; ii++) {
		quadrant = whichVolume(rch_hit->volume_id, &volumeName);

		//
		// Input is in local or global coordinates
		// By default we read GLOBAL coordinates
		//
		StThreeVector<double> tmpMomentum(rch_hit->p[0]*GeV,
						  rch_hit->p[1]*GeV,
						  rch_hit->p[2]*GeV);

		//
		// Default: READ Global.  Must Transform to RichLocal
		//
		if(!mUseLocalCoordinate) {
		    //
		    // Transform coordinates
		    StGlobalCoordinate global(rch_hit->x[0]*centimeter,
					      rch_hit->x[1]*centimeter,
					      rch_hit->x[2]*centimeter);

		    (*mCoordinateTransform)(global,local);
		    mMomentumTransform->localMomentum(tmpMomentum,momentum);
		}
		//
		// if expecting local, the RCSI is in quadrant format
		// and x-> -x, y-> -y
		else if (mUseLocalCoordinate && volumeName == "RCSI") {
		    StRichQuadrantCoordinate quad(-1.*rch_hit->x[0]*centimeter,
						  -1.*rch_hit->x[1]*centimeter,
						  rch_hit->x[2]*centimeter,quadrant);
		    (*mCoordinateTransform)(quad,local);

		    momentum.setX(-1.*rch_hit->p[0]*GeV);
		    momentum.setY(-1.*rch_hit->p[1]*GeV);
		    // z-component okay
		}
		else {  // if in the gap, freon, quartz AND in local:
		    local.position().setX(-1.*rch_hit->x[0]*centimeter);
		    local.position().setY(-1.*rch_hit->x[1]*centimeter);
		    local.position().setZ(rch_hit->x[2]*centimeter);

		    momentum.setX(-1.*rch_hit->p[0]*GeV);
		    momentum.setY(-1.*rch_hit->p[1]*GeV);
		    // z-component okay
		}

		//
		// If a C photon,
		//  1) take the parent ID,
		//  2) store the parent GID
		//
		int tmpTrackP = (track[(rch_hit->track_p)-1].ge_pid == 50) ?
		    track[(rch_hit->track_p)-1].next_parent_p : rch_hit->track_p;


		double particleMass =
		    (!mTable->findParticleByGeantId((track[(rch_hit->track_p)-1].ge_pid))) ?
		    0. : mTable->findParticleByGeantId((track[(rch_hit->track_p)-1].ge_pid))->mass();
		    
		hit.fill(local.position(),
			 momentum,
			 tmpTrackP,
			 (momentum.x()/abs(momentum)),
			 (momentum.y()/abs(momentum)),
			 (momentum.z()/abs(momentum)),
			 rch_hit->ds*centimeter,
			 rch_hit->de*GeV,
			 particleMass,
			 rch_hit->id,
			 track[(tmpTrackP)-1].ge_pid,
			 volumeName);

#ifdef RICH_DIAGNOSTIC
// 		raw << "ii " << ii << "/" << numberOfRichHits << hit << endl;
//   		raw << volumeName.c_str() << endl;
// 		raw << "volume_id= "  << rch_hit->volume_id << endl;
// 		//raw << " hit= "        << hit;
//   		raw << abs(momentum)/GeV << endl;
//  		raw << momentum.perp()/GeV << endl;
// 		raw << rch_hit->track_p << endl;
//   		raw << rch_hit->id << endl;
// 		raw << hit.mass() << endl;
//  		raw << "egd " << track[(rch_hit->track_p)-1].eg_pid    << endl;
//   		raw << "gid " << track[(rch_hit->track_p)-1].ge_pid    << endl;
//   		raw << "dE: " << (rch_hit->de)        << endl;
//  		raw << "ds: " << (rch_hit->ds)        << endl;
//   		raw << track[(rch_hit->track_p)-1].ptot      << endl;

// 		// momentum of track
//  		raw << track[(rch_hit->track_p)-1].p[0]      << endl;
//  		raw << track[(rch_hit->track_p)-1].p[1]      << endl;
//  		raw << track[(rch_hit->track_p)-1].p[2]      << endl;
//  		raw << track[(rch_hit->track_p)-1].n_tpc_hit << endl;
// 		int ctr =0;
// 		for(int zz=0; zz<no_tpc_hits; zz++) {
// 		    raw << tpc_hit[zz].track_p << " ";
// 		    if (tpc_hit[zz].track_p == (rch_hit->track_p) ) {
// 			ctr++;
// 			raw << zz << " "
// 			    << tpc_hit[zz].x[0] << " "
// 			    << tpc_hit[zz].x[1] << " "
// 			    << tpc_hit[zz].x[2] << " ";
// 			double ptot = (sqrt(tpc_hit[zz].p[0]*tpc_hit[zz].p[0]+
// 					    tpc_hit[zz].p[1]*tpc_hit[zz].p[1]+
// 					    tpc_hit[zz].p[2]*tpc_hit[zz].p[2]));
// 			raw << ptot                << endl;
// 		    }
// 		}
// 		raw << "ctr= " << ctr << endl;
//  		raw << endl;
#endif
		//PR((hit.volumeID().c_str()));
		if(hit.volumeID() == "RGAP") {
		    //cout << "RGAP" << "ii/size " << ii << " " << theList.size() << endl;
		    mIonize.splitSegment(&hit,theList);
#ifdef RICH_WITH_PADMONITOR
		    g2tInfo.push_back(StRichG2TInfo(hit.position().x(),
						    hit.position().y(),
						    hit.trackp(),"c"));
#endif
		}
		else if(hit.volumeID() == "RCSI") {
		    // cout << "RCSI" << "ii/size " << ii << " " << theList.size() << endl;
		    // if it is photon, add to the list
		    if ( hit.dE() < 0 ) {
			theList.push_back(new StRichMiniHit(hit.position(),
							    hit.momentum(),
							    hit.trackp(),
							    hit.id(),
							    hit.gid(),
							    0,  // mass
							    ePhoton));
#ifdef RICH_WITH_PADMONITOR
			g2tInfo.push_back(StRichG2TInfo(hit.position().x(),
							hit.position().y(),
							hit.trackp(),"p"));
#endif
		    }
		}
#ifdef RICH_WITH_VIEWER
// 		if (StRichViewer::histograms )
// 		    StRichViewer::getView()->update();
#endif
		rch_hit++;
		
	    }  // loop over hits and store for 2nd round of processing
#ifdef USE_MEMORY_INFO
	    info->snapshot();
	    info->print();
#endif
// 	    cout << "Signals on Pads" << endl;
	    //
	    // Now generate the signal on the pad plane
	    //
	    //PR(theList.size());
	    double wireNumber;
	    double chargeMultiplied;
	    int numberOfSegments = 0;
	    for(iter  = theList.begin();
		iter != theList.end();
		iter++) {
		wireNumber = mWireSelector.whichWire(*iter);
 		chargeMultiplied = mAmplification.avalanche(*iter, wireNumber, theList);
		mAnalogSignalGenerator->induceSignal(*iter,chargeMultiplied);


#ifdef RICH_WITH_PADMONITOR
		if((*iter)->process() == eFeedback) {
		    g2tInfo.push_back(StRichG2TInfo((*iter)->position().x(),
						    (*iter)->position().y(),
						    (*iter)->trackp(),"f"));
		}
#endif
		numberOfSegments++;
	    }
	    
	}  // if (m_DataSet)

    } //else process from stream


//     cout << "Try Write" << endl;
    for ( int i = 0; i < mWriter->rows(); i++ ) {
	for ( int j = 0; j < mWriter->cols(); j++ ) {

	    if(mAddElectricNoise)
		mWriter->getSignal(i,j).signal +=  mNoiseSimulator();
	    
	    mWriter->getSignal(i,j).signal =
		mADC( mWriter->getSignal(i,j).signal );
	    
#ifdef RICH_WITH_VIEWER
	    if (StRichViewer::histograms )
		StRichViewer::getView()->mADCSignal->Fill(i,j,mWriter->getSignal(i,j).signal);
#endif
	}
    }

//     cout << "cleanup" << endl;
    mWriter->cleanUpMCInfo();
    
    if(mWriteToFile) {
	cout << "StRrsMaker::Maker()";
	cout << "\tWrite DATA out" << endl; 
	//mOutPutStream->writeRrsEvent(mPadPlane);
    }


    //
    // clear up the list<StRichMiniHit*>
//     cout << "Try clear" << endl;
    for(iter  = theList.begin();
	iter != theList.end();
	iter++) {
	delete *iter;
	*iter = 0;
    }
    theList.clear();

#ifdef RICH_DECODE_DATA
    int version = 1;
    unsigned int theADCValue = 0;
    anIDList     aListOfMCInfo;
    anIDList::const_iterator listIter;
#ifdef RICH_WITH_PADMONITOR
    cout << "Get Instance of Pad Monitor" << endl;
    StRichPadMonitor* thePadMonitor = StRichPadMonitor::getInstance(mGeometryDb);
    cout << "Try Clear" << endl;    
    thePadMonitor->clearAll();  
#endif  // Pad Monitor
    
    StRrsReader theReader(mPadPlane,version);
    cout << "DECODER " << endl;
    for(int iRow=0; iRow<(mGeometryDb->numberOfRowsInAColumn()); iRow++) {
	for(int iCol=0; iCol<(mGeometryDb->numberOfPadsInARow()); iCol++) {
	    
	    theADCValue = theReader.GetADCFromCoord(iCol,iRow);
	    aListOfMCInfo = theReader.GetMCDetectorInfo(iCol,iRow);
	    if(theADCValue) {
#ifdef RICH_DIAGNOSTIC
// 		raw << "c/r/adc: " << iCol << ' ' << iRow << ' ' << theADCValue << endl;
		raw << iCol << ' ' << iRow << ' ' << theADCValue << endl;
// 		for(listIter  = aListOfMCInfo.begin();
// 		    listIter != aListOfMCInfo.end();
// 		    listIter++) {
// 		    raw << "\t" << *listIter << endl;
// 		}
#endif // DIAGNOSTIC
		
#ifdef RICH_WITH_PADMONITOR
 		StRichSingleMCPixel anMCPixel(iCol,iRow,theADCValue,aListOfMCInfo);
 		thePadMonitor->drawPad(anMCPixel);
#endif // PAD MONITOR
// #ifdef RICH_DIAGNOSTIC
//   		raw << "r/c/adc: " << iRow << ' ' << iCol << ' ' << theADCValue << endl;
//  		anIDList MCInfo = theReader.GetMCDetectorInfo(iRow, iCol);
//  		anIDList::iterator iter;
//  		for(iter = MCInfo.begin();
//  		    iter!= MCInfo.end();
//  		    iter++) {
//  		    raw << ">>* MCinfo.G_ID= "
//  			<< iter->mG_ID << "MCinfo.trackp= "
//  			<< iter->mTrackp << "MCinfo.amount= "
//  			<< iter->mAmount << endl;
// 		}
// #endif
	    
	    } // if(theADCValue)
	} // iCol
    }     // iRow
#endif  // DECODE DATA
    
//  #ifdef RICH_WITH_PADMONITOR
//      cout << "g2tInfo.size() " << g2tInfo.size() << endl;
//      if(g2tInfo.size()) {
//  	for(unsigned int jj=0; jj<g2tInfo.size(); jj++) {
//  	    thePadMonitor->drawG2T(g2tInfo[jj]);
//  	}
//      }
    
//      thePadMonitor->update();
//  #endif
	
#ifdef RICH_WITH_PADMONITOR
    thePadMonitor->update();
#endif

#ifdef USE_MEMORY_INFO
    info->snapshot();
    info->print();
#endif
    
    return 0;
}

int StRrsMaker::Finish()
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->update();
//     mRchNTupleFile->Write();
//     mRchNTupleFile->Close();
#endif

    delete mPadPlane;

    return 0;
} 

void StRrsMaker::drawParticleId()      // in Filter 
{
#ifdef RICH_WITH_VIEWER

    StRichViewer::getView()->mParticleId->Draw();
#endif
  }

void StRrsMaker::drawWhichQuadrant()   // in Filter 
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->mWhichQuadrant->Draw();
#endif
}


void StRrsMaker::drawClusterElectrons()// in Ionization 
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->mClusterElectrons->Draw();
#endif
}

void StRrsMaker::drawErrorDetection()  // in Filter 
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->mErrorDetection->Draw();
#endif
}

void StRrsMaker::drawWhichWire()       // in SelectWire 
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->mWhichWire->Draw();
#endif
}

void StRrsMaker::drawFeedback()        // in Gas Gain 
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->mFeedback->Draw();
#endif
}

void StRrsMaker::drawPolia()           // in GasGain 
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->mPolia->Draw();
#endif
}

void StRrsMaker::drawAnalogSignals()       // in ASG 
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->mAnalogSignals->Draw();
#endif
}

void StRrsMaker::drawTotalCharge()     // in ASG 
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->mTotalCharge->Draw();
#endif
}

void StRrsMaker::drawADCSignal()       // in ADC
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->mADCSignal->Draw();
#endif
}

void StRrsMaker::drawPadPlane()        // displays the Pad plane (Filter)
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->mPadPlane->Draw();
#endif
}

void StRrsMaker::drawNoise()           // electrical noise simulation 
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->mNoise->Draw();
#endif
}

//   St_DataSet * StRrsMaker::getPadPlaneTable() {
//     Writer* output = Writer::getInstance();
//     return output->getPadsTable(); 
//   }
//   St_DataSet * StRrsMaker::getIDTable() {
//     Writer* output = Writer::getInstance();
//     return output->getIDTable(); 
//   }
//   int StRrsMaker::getADC(int row, int col) {
//     Writer* output = Writer::getInstance();
//     return output->getADC(row,col);
//   }

#endif // __ROOT__

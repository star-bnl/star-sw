/******************************************************
 * $Id: StRrsMaker.cxx,v 1.12 2000/03/13 21:58:01 lasiuk Exp $
 * Description:
 *  Implementation of the Maker main module.
 *
 * $Log: StRrsMaker.cxx,v $
 * Revision 1.12  2000/03/13 21:58:01  lasiuk
 * singleton classes
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
#define rICH_WITH_RINGS 1

#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_ObjectSet.h"

//#include <iostream.h>
#include "StMemoryInfo.hh"
#endif

#include "StParticleTable.hh"

// DataBases
#include "StRichGeometryDb.h"
#include "StRichPhysicsDb.h"
#include "StRichMomentumTransform.h"
#include "StRichGeantReader.h"
#include "StRichPadPlane.h"
#include "StRichPadPlane.h"
#include "StRichWriter.h"

#include "StRichGHit.h"
#ifdef RICH_WITH_RINGS
#include "StRichRingDefinition.h"
#include "StRichTrack.h"
#include "StRichRingPoint.h"
#include "StRichRingCalculator.h"
#include "StParticleDefinition.hh"
#include "StParticleTypes.hh"
#endif
//////
#endif
// #include "StRichRingDefinition.h"
// #include "StRichTrack.h"
// #include "StRichRingPoint.h"
// #include "StRichRingCalculator.h"
// #include "StParticleDefinition.hh"
// #include "StParticleTypes.hh"
// #endif
//////


#ifdef RICH_DECODE_DATA
#include "StRrsReader.h"
// SCL
#include "StGlobals.hh"
#include "StThreeVector.hh"
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
    //mPhysicsDb->print();

    
    // GEANT Table
    // ADC
    mCoordinateTransform = StRichCoordinateTransform::getTransform(mGeometryDb);
    // adds a DC level to each pad
    mADC.setAddPedestal(mAddPedestal);
    
    // PadPlane
    

    //
    // Construct constant data set.  This is what is passed downstream
    //

    mPadPlane =
	new StRichPadPlane(mGeometryDb->numberOfRowsInAColumn(),
			   mGeometryDb->numberOfPadsInARow());
    AddConst(new St_ObjectSet("richPixels", mPadPlane));

    // Data Writer is here
    mWriter = StRichWriter::getInstance(mPadPlane);

    // Construct constant data set.  This is what is passed downstream
    // The processors
    AddConst(new St_ObjectSet("richPixels", mPadPlane));
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
    StRichViewer::foo = useHistos;
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
	cerr << "StRchMaker::whicVolume() UNKNOWN Volume" << endl;
	break;
    }
    int volumeNumber = (val - (volume*1000));
    return volumeNumber;
//     cout << "-- Press return to continue -- ";
//     char c = cin.get();
//       char c = cin.get();
#ifdef USE_MEMORY_INFO
    StMemoryInfo* info = StMemoryInfo::instance();
    info->snapshot();
    info->print();
#endif

    mPadPlane->clear();
#ifdef RICH_DIAGNOSTIC
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
		hit.fill(local.position().x(),
			 local.position().y(),
			 local.position().z(),
		
		    (!mTable->findParticleByGeantId((track[(rch_hit->track_p)-1].ge_pid))) ?
		    0. : mTable->findParticleByGeantId((track[(rch_hit->track_p)-1].ge_pid))->mass();
		    
		hit.fill(local.position(),
			 momentum,
			 (momentum.x()/abs(momentum)),
			 (momentum.y()/abs(momentum)),
			 mTable->findParticleByGeantId((track[(rch_hit->track_p)-1].ge_pid))->mass(),
#ifdef RICH_WITH_PADMONITOR
		// momentum of track...needed for drawing tracks/rings
		if(volumeName == "RGAP" && track[(rch_hit->track_p)-1].ge_pid == 9) {
		    gTrackMomentum.setX(track[(rch_hit->track_p)-1].p[0]*GeV);
		    gTrackMomentum.setY(track[(rch_hit->track_p)-1].p[1]*GeV);
		    gTrackMomentum.setZ(track[(rch_hit->track_p)-1].p[2]*GeV);
		    mMomentumTransform->localMomentum(gTrackMomentum,lTrackMomentum);
		}
#endif
			 rch_hit->ds*centimeter,
			 rch_hit->de*GeV,
			 particleMass,
			 rch_hit->id,
			 volumeName);

		//cout << "ii " << ii << "/" << numberOfRichHits << hit << endl;
#ifdef RICH_DIAGNOSTIC
//  		raw << volumeName.c_str() << endl;
// 		raw << "volume_id= "  << rch_hit->volume_id << endl;
// 		//raw << " hit= "        << hit;
//  		raw << abs(momentum)/GeV << endl;
//  		raw << momentum.perp()/GeV << endl;
//  		raw << rch_hit->id << endl;
//  		raw << track[(rch_hit->track_p)-1].eg_label  << endl;
//  		raw << track[(rch_hit->track_p)-1].eg_pid    << endl;
//  		raw << track[(rch_hit->track_p)-1].ge_pid    << endl;
//  		raw << track[(rch_hit->track_p)-1].pt        << endl;
//  		raw << track[(rch_hit->track_p)-1].ptot      << endl;
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
		
// 		    }
		if(hit.volumeID() == "RGAP") { 
		    mIonize(hit);		 
#endif

		    //
		    // Check if it is photon, and induce signal if so
		    //
// 		    cout << "RGAP" << "ii/size " << ii << " " << theList.size() << endl;
			mInduceSignal(hit);
			theList.push_back(new StRichMiniHit(hit.position(),
							    hit.momentum(),
		
		    }
		}
		else {
		    //cout << "don't add" << endl;
		}
		//sleep(1);
	    }  // loop over hits
		iter != theList.end();
		wireNumber = mWireSelector.whichWire(*iter);
		chargeMultiplied = mAmplification.avalanche(*iter, wireNumber, theList);
		mAnalogSignalGenerator->induceSignal(*iter,chargeMultiplied);

	    }
	    
	}  // if (m_DataSet)

    } //else process from stream


    //cout << "Try Write" << endl;
    for ( int i = 0; i < mWriter->rows(); i++ )
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

    
	iter++) {
	delete *iter;
	*iter = 0;
    }
    theList.clear();

#ifdef RICH_DECODE_DATA
    int version = 1;
    unsigned int theADCValue = 0;

#ifdef RICH_WITH_PADMONITOR
    cout << "Get Instance of Pad Monitor" << endl;
    StRichPadMonitor* thePadMonitor = StRichPadMonitor::getInstance(mGeometryDb);
    cout << "Try Clear" << endl;
    thePadMonitor->clearPads();  
#endif
    StRrsReader theReader(mPadPlane,version);
    cout << "DECODER " << endl;
    int ctr = 0;
    for(int iRow=0; iRow<(mGeometryDb->numberOfRowsInAColumn()); iRow++) {
	for(int iCol=0; iCol<(mGeometryDb->numberOfPadsInARow()) ; iCol++) {
	    
	    theADCValue = theReader.GetADCFromCoord(iCol,iRow);
	    if(theADCValue) {
#ifdef RICH_DIAGNOSTIC
		raw << "r/c/adc: " << iRow << ' ' << iCol << ' ' << theADCValue << endl;
#endif
#ifdef RICH_WITH_PADMONITOR
		StRichSinglePixel aPixel(iCol,iRow,theADCValue);
		thePadMonitor->drawPad(aPixel);
#endif
// #ifdef RICH_DIAGNOSTIC
//  		raw << "r/c/adc: " << iRow << ' ' << iCol << ' ' << theADCValue << endl;
// 		anIDList MCInfo = theReader.GetMCDetectorInfo(iRow, iCol);
// 		anIDList::iterator iter;
// 		for(iter = MCInfo.begin();
// 		    iter!= MCInfo.end();
// 		    iter++) {
// #ifdef __SUNPRO_CC
// 		    raw << ">>* MCinfo.G_ID= "
// 			<< (*iter).mG_ID << "MCinfo.trackp= "
// 			<< (*iter).mTrackp << "MCinfo.amount= "
// 			<< (*iter).mAmount << endl;
// #else
// 		    raw << ">>* MCinfo.G_ID= "
// 			<< iter->mG_ID << "MCinfo.trackp= "
// 			<< iter->mTrackp << "MCinfo.amount= "
// 			<< iter->mAmount << endl;
// #endif
//	    }
#ifdef RICH_WITH_RINGS
    //try draw a ring:
    StGlobalCoordinate gIP;
    StRichLocalCoordinate localIP(10.4*centimeter,23.4*centimeter,0.);
    (*mCoordinateTransform)(localIP,gIP);
    StRichTrack theTrack(lTrackMomentum, gIP.position());
    PR(lTrackMomentum);
    PR(gIP);
    StRichRingCalculator myCalculator(&theTrack);
    StPionMinus* pion = StPionMinus::instance();
    StKaonMinus* kaon = StKaonMinus::instance();
    StProton* proton = StProton::instance();
    myCalculator.getRing(eInnerRing)->setParticleType(proton);
    myCalculator.getRing(eOuterRing)->setParticleType(proton);

    StThreeVector<double> aPoint;
    StThreeVector<double> bPoint;
    for(int kk=90; kk<270;kk+=5) {
	bool status = myCalculator.getRing(eInnerRing)->getPoint(kk*degree, aPoint);
	thePadMonitor->addInnerRingPoint(aPoint.x(), aPoint.y());
	status = myCalculator.getRing(eOuterRing)->getPoint(kk*degree, bPoint);
	thePadMonitor->addOuterRingPoint(bPoint.x(), bPoint.y());
    }
    thePadMonitor->drawRing();
#endif
//     StThreeVector<double> aPoint;
//     StThreeVector<double> bPoint;
//     for(int kk=90; kk<270;kk+=5) {
// 	bool status = myCalculator.getRing(eInnerRing)->getPoint(kk*degree, aPoint);
// 	thePadMonitor->addInnerRingPoint(aPoint.x(), aPoint.y());
// #endif
    thePadMonitor->update();
#endif
#ifdef USE_MEMORY_INFO
    info->snapshot();
    info->print();
#endif

    return 0;
}


    delete mGeometryDb;
    delete mPhysicsDb;

int StRrsMaker::Finish()
{
    delete mWriter;
#ifdef RICH_WITH_VIEWER
    delete StRichViewer::getView();
#endif
    delete mPadPlane;

    return 0;
  } 

void StRrsMaker::drawParticleId()      // in Filter 
{
#ifdef RICH_WITH_VIEWER

    StRichViewer::getView()->mParticleId->Draw();
    StRichViewer::getView()->mCanvas1->Modified();
    StRichViewer::getView()->mCanvas1->Update();
    StRichViewer::getView()->mHFile->Write();
#endif
  }

void StRrsMaker::drawWhichQuadrant()   // in Filter 
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->mWhichQuadrant->Draw();
    StRichViewer::getView()->mCanvas1->Modified();
    StRichViewer::getView()->mCanvas1->Update();
    StRichViewer::getView()->mHFile->Write();
#endif
}


void StRrsMaker::drawClusterElectrons()// in Ionization 
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->mClusterElectrons->Draw();
    StRichViewer::getView()->mCanvas1->Modified();
    StRichViewer::getView()->mCanvas1->Update();
    StRichViewer::getView()->mHFile->Write();
#endif
}

void StRrsMaker::drawErrorDetection()  // in Filter 
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->mErrorDetection->Draw();
    StRichViewer::getView()->mCanvas1->Modified();
    StRichViewer::getView()->mCanvas1->Update();
    StRichViewer::getView()->mHFile->Write();
#endif
}

void StRrsMaker::drawWhichWire()       // in SelectWire 
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->mWhichWire->Draw();
    StRichViewer::getView()->mCanvas1->Modified();
    StRichViewer::getView()->mCanvas1->Update();
    StRichViewer::getView()->mHFile->Write();
#endif
}

void StRrsMaker::drawFeedback()        // in Gas Gain 
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->mFeedback->Draw();
    StRichViewer::getView()->mCanvas1->Modified();
    StRichViewer::getView()->mCanvas1->Update();
    StRichViewer::getView()->mHFile->Write();
#endif
}

void StRrsMaker::drawPolia()           // in GasGain 
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->mPolia->Draw();
    StRichViewer::getView()->mCanvas1->Modified();
    StRichViewer::getView()->mCanvas1->Update();
    StRichViewer::getView()->mHFile->Write();
#endif
}

void StRrsMaker::drawAnalogSignals()       // in ASG 
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->mAnalogSignals->Draw();
    StRichViewer::getView()->mCanvas1->Modified();
    StRichViewer::getView()->mCanvas1->Update();
    StRichViewer::getView()->mHFile->Write();
#endif
}

void StRrsMaker::drawTotalCharge()     // in ASG 
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->mTotalCharge->Draw();
    StRichViewer::getView()->mCanvas1->Modified();
    StRichViewer::getView()->mCanvas1->Update();
    StRichViewer::getView()->mHFile->Write();
#endif
}

void StRrsMaker::drawADCSignal()       // in ADC
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->mADCSignal->Draw();
    StRichViewer::getView()->mCanvas1->Modified();
    StRichViewer::getView()->mCanvas1->Update();
    StRichViewer::getView()->mHFile->Write();
#endif
}

void StRrsMaker::drawPadPlane()        // displays the Pad plane (Filter)
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->mPadPlane->Draw();
    StRichViewer::getView()->mCanvas1->Modified();
    StRichViewer::getView()->mCanvas1->Update();
    StRichViewer::getView()->mHFile->Write();
#endif
}

void StRrsMaker::drawNoise()           // electrical noise simulation 
{
#ifdef RICH_WITH_VIEWER
    StRichViewer::getView()->mNoise->Draw();
    StRichViewer::getView()->mCanvas1->Modified();
    StRichViewer::getView()->mCanvas1->Update();
    StRichViewer::getView()->mHFile->Write();
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

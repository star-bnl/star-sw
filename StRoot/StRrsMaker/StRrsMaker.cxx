/*******************************************Maker.cxx**\
 *
 *  Implementation of the Maker main module.
 *
\******************************************************/
 * comment to prevent streamer for ionize and inducesignal. Remove filter
 *
 * Revision 1.6  2000/02/08 16:36:49  lasiuk
 * Bring into line with HP
 *
 ******************************************************/
#ifdef __ROOT__
#include "StRrsMaker.h"             

#include <iostream.h>
#define rICH_DIAGNOSTIC 1
#define rICH_DECODE_DATA 1
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
#include "St_ObjectSet.h"

#include "StRichFilter.h"
#include "StRichNoiseSimulator.h"
#include "StRichAnalogToDigitalConverter.h"
// DataBases
//#include "StRichFilter.h"
//#include "StRichNoiseSimulator.h"
//#include "StRichAnalogToDigitalConverter.h"
// #include "StRichRingCalculator.h"
// #include "StParticleDefinition.hh"
// #include "StParticleTypes.hh"
// #endif
//////
#define RICH_DECODE_DATA 1
#ifdef RICH_DECODE_DATA
#include "StRrsReader.h"
#endif


#ifdef RICH_DECODE_DATA
#include "StRrsReader.h"
// SCL
#include "StGlobals.hh"
#include "StThreeVector.hh"
#endif

extern "C" int agfhit0_ (char*, char*, int, int);
extern "C" int agfhit1_ (int*, int*, int*, float*);
#ifdef  RICH_WITH_VIEWER
#include "StRichViewer.h"              // view class
#endif


// g2t tables
#include "tables/St_g2t_tpc_hit_Table.h"
#include "tables/St_g2t_rch_hit_Table.h"
#include "tables/St_g2t_track_Table.h"


    : StMaker(name)
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

    mGeometryDb->print();
    mPhysicsDb->print();
    //exit(0);
      return 1;
    
    //mPhysicsDb->print();
    mADC.setAddPedestal(0);  // adds a DC level to each pad!
    // ADC
    mCoordinateTransform = StRichCoordinateTransform::getTransform(mGeometryDb);
    // adds a DC level to each pad
    mPadPlane = new StRichPadPlane(2*mGeometryDb->n_pad_x, 2*mGeometryDb->n_pad_z);
    

    //
    // Construct constant data set.  This is what is passed downstream
    //

    mPadPlane =
	new StRichPadPlane(mGeometryDb->numberOfRowsInAColumn(),
        
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
#define RICH_DIAGNOSTIC 1
//     cout << "-- Press return to continue -- ";
    ofstream raw("/afs/rhic/star/users/lasiuk/junk/rrs.txt");
//       char c = cin.get();
#ifdef USE_MEMORY_INFO
    StRichGeantReader input;
    StMemoryInfo* info = StMemoryInfo::instance();
    info->snapshot();
    info->print();
    list<StRichMiniHit*>::iterator iter;
    
    //
    // scope for transformed momentum
    //
    StThreeVector<double> lTrackMomentum;
    StThreeVector<double> gTrackMomentum;

    
    else {  // else
    // Either  Read mPadPlane from file
    //
    if (mReadFromFile) {
	//mInputStream->fillTrsEvent(mPadPlane);
	cout << "Done Filling mPadPlane from File" << endl;
    }
    //
    // or do the normal processing of RRS from GEANT
    else {  // else process from stream
	if (!m_DataSet->GetList())  {
	    //if DataSet is empty fill it
	    //
	    // Read the GEANT info
	    St_g2t_track *g2t_track =
	    PR(numberOfTracks);

 	    if(!g2t_track){
	    // TPC HITS
		cout << "\tNo g2t_tpc_hit pointer";
		cout << "\treturn from StRrsMaker::Make()" << endl;
		return kStWarn;
	    // can we check if the dataset exists?
	    St_g2t_rch_hit *g2t_rch_hit =
		static_cast<St_g2t_rch_hit *>(geant("g2t_rch_hit"));
	    if(!g2t_rch_hit){
		cout << "StRrsMaker::Make()";
		cout << "\tNo g2t_rch_hit pointer";
		cout << "\treturn from StRrsMaker::Make()" << endl;
	    //while( input(hit) == 0 )  {   // if OK
	    string volumeName;
	    int    quadrant;

		StThreeVector<double>
		    momentum(rch_hit->p[0],rch_hit->p[1],rch_hit->p[2]);
		double step = 10;
		hit.fill(rch_hit->x[0], rch_hit->x[1], rch_hit->x[2],
			 rch_hit->id,
			 (momentum.x()/abs(momentum)),
			 (momentum.y()/abs(momentum)),
			 (momentum.z()/abs(momentum)),
			 step,
			 rch_hit->de,
		raw << "volumeName= " << volumeName         << endl;
		    
		    mMomentumTransform->localMomentum(gTrackMomentum,lTrackMomentum);
		raw << "volumeName= " << volumeName.c_str() << endl;
		raw << "quadrant= "   << quadrant           << endl;
		raw << "volume_id= "  << rch_hit->volume_id << endl;
		raw << "hit= "        << hit                << endl;
		raw << "p= "          << abs(momentum)
		    << " track id= "  << rch_hit->id
		    << " tpchit= "    << track[(rch_hit->track_p-1)].n_tpc_hit
		    << " eg_lab= "    << track[(rch_hit->track_p-1)].eg_label
		    << " egpid= "     << track[(rch_hit->track_p-1)].eg_pid << endl;
// 			    << tpc_hit[zz].x[1] << " "
		mFilter( hit );
	    
			mInduceSignal(hit);
			theList.push_back(new StRichMiniHit(hit.position(),
							    hit.momentum(),
		
		    }
		else {
		}
    } //else
		iter != theList.end();
		wireNumber = mWireSelector.whichWire(*iter);
    cout << "Try Write" << endl;
		mAnalogSignalGenerator->induceSignal(*iter,chargeMultiplied);

	    
	    //mWriter->getSignal(i,j).signal +=  mNoiseSimulator();

    } //else process from stream


    //cout << "Try Write" << endl;
    for ( int i = 0; i < mWriter->rows(); i++ )
	for ( int j = 0; j < mWriter->cols(); j++ ) {

	    if(mAddElectricNoise)
		mWriter->getSignal(i,j).signal +=  mNoiseSimulator();
	    
	    mWriter->getSignal(i,j).signal =
	cout << "StRrsMaker::Maker() Write DATA out" << endl; 
#ifdef RICH_WITH_VIEWER
	    if (StRichViewer::histograms )
		StRichViewer::getView()->mADCSignal->Fill(i,j,mWriter->getSignal(i,j).signal);
#endif
	}	      

    
#ifdef RICH_DECODE_DATA
    int version = 1;
    for(int iRow=0; iRow<(2*mGeometryDb->n_pad_x); iRow++) {  // 96
	for(int iCol=0; iCol<(2*mGeometryDb->n_pad_z) ; iCol++) {
    cout << "Get Instance of Pad Monitor" << endl;
	    theADCValue = theReader.GetADCFromCoord(iRow,iCol);
    cout << "Try Clear" << endl;
		//cout << "r/c/adc: " << iRow << ' ' << iCol << ' ' << theADCValue << endl;
#ifdef RICH_DIAGNOSTIC
		raw << "r/c/adc: " << iRow << ' ' << iCol << ' ' << theADCValue << endl;
		anIDList MCInfo = theReader.GetMCDetectorInfo(iRow, iCol);
		anIDList::iterator iter;
		for(iter = MCInfo.begin();
		    iter!= MCInfo.end();
		    iter++) {
#ifdef __SUNPRO_CC
		    raw << ">>* MCinfo.G_ID= " << (*iter).G_ID << "MCinfo.amount= "
			<< (*iter).amount << endl;
#else
		    raw << ">>* MCinfo.G_ID= " << iter->G_ID << "MCinfo.amount= "
			<< iter->amount << endl;
#endif
		}
#endif
		
// 			<< (*iter).mAmount << endl;
// 			<< iter->mTrackp << "MCinfo.amount= "
// 			<< iter->mAmount << endl;
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

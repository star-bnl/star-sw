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
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
#include "St_ObjectSet.h"

#include "StRichFilter.h"
#include "StRichNoiseSimulator.h"
#include "StRichAnalogToDigitalConverter.h"
// DataBases
//#include "StRichFilter.h"
#define rICH_WITH_VIEWER 1
//#include "StRichNoiseSimulator.h"
//#include "StRichAnalogToDigitalConverter.h"
// #include "StRichRingCalculator.h"
// #include "StParticleDefinition.hh"
#define rICH_DECODE_DATA 1
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

// g2t tables
#include "tables/St_g2t_tpc_hit_Table.h"
#include "tables/St_g2t_rch_hit_Table.h"
#include "tables/St_g2t_track_Table.h"


    : StMaker(name)
#ifdef __ROOT__
#define gufld   gufld_

    PR(mReadFromFile);
    return kStOK;
}

void StRrsMaker::addPedestal(int b)
{
    mAddPedestal = b;
}
    cout << "StRrsMaker::Init() DBS" << endl;

void StRrsMaker::addElectricNoise(int b)
{
    mAddElectricNoise = b;
}
///////////////////////////////////////////////////////
 
{
#ifdef USE_MEMORY_INFO
    // PadPlane
    cout << "StRrsMaker::Init() Pad Pane" << endl;
    cout << "2*mGeometryDb->n_pad_x " << 2*mGeometryDb->n_pad_x << endl;
    cout << "2*mGeometryDb->n_pad_z " << 2*mGeometryDb->n_pad_z << endl;
    
    mPadPlane = new StRichPadPlane(2*mGeometryDb->n_pad_x, 2*mGeometryDb->n_pad_z);

    StMemoryInfo* info = StMemoryInfo::instance();
    info->snapshot();
    info->print();
#endif
    // DBs are here
    // adds a DC level to each pad
    mPadPlane = new StRichPadPlane(2*mGeometryDb->n_pad_x, 2*mGeometryDb->n_pad_z);
    

    //
    // Construct constant data set.  This is what is passed downstream
    //

    mPadPlane =
	new StRichPadPlane(mGeometryDb->numberOfRowsInAColumn(),
        
Int_t StRrsMaker::Init(int histograms)
    // The processors
    AddConst(new St_ObjectSet("richPixels", mPadPlane));
    // ASG        is a SINGLETON
    mAnalogSignalGenerator = StRichAnalogSignalGenerator::getInstance(mWriter);
    Viewer::histograms = histograms;
    Viewer* view; 
    if ( Viewer::histograms )
	view = new Viewer;
    return Init(true);
}

	Viewer::histograms = 0;
{
#ifdef RICH_WITH_VIEWER
    cout << "Try make a viewer" << endl;
    StRichViewer* view = 0;
    StRichViewer::histograms = useHistos;
	break;
    case 4:
//     cout << "-- Press return to continue -- ";
    ofstream raw("/afs/rhic/star/users/lasiuk/junk/rrs.txt");
//       char c = cin.get();
#ifdef USE_MEMORY_INFO
    if (!m_DataSet->GetList())  {//if DataSet is empty fill it
	//
	// Read the GEANT info
	// these structures/classes are defined in:
	// $STAR/pams/sim/idl/g2t_tpc_hit.idl 
	// $STAR/StRoot/base/St_DataSet.h & St_Table.h 
	//
	St_DataSetIter geant(GetDataSet("geant"));
    //
 	St_g2t_track *g2t_track =
 	    static_cast<St_g2t_track *>(geant("g2t_track"));
 	int noTracks          =  g2t_track->GetNRows();
 	g2t_track_st *track =  g2t_track->GetTable();
 	cout << "--> tracks:  " << noTracks << endl;
	
 	St_g2t_rch_hit *g2t_rch_hit =
 	    static_cast<St_g2t_rch_hit *>(geant("g2t_rch_hit"));
 	int noRichHits          =  g2t_rch_hit->GetNRows();
 	g2t_rch_hit_st *rch_hit =  g2t_rch_hit->GetTable();
	    PR(numberOfTracks);
 	cout << "--> rch_hits:  " << noRichHits << endl;
    
	    // TPC HITS
	cout << "Got here" << endl;
    //while( input(hit) == 0 )  {   // if OK
	//for(int ii=0; ii<noRichHits; ii++) {
	for(int ii=0; ii<noRichHits; ii++) {
	    StThreeVector<double> momentum(rch_hit->p[0],rch_hit->p[1],rch_hit->p[2]);
	    double step = 0;
	    hit.fill(rch_hit->x[0], rch_hit->x[1], rch_hit->x[2],
		     rch_hit->id,
		     (momentum.x()/abs(momentum)),
		     (momentum.y()/abs(momentum)),
		     (momentum.z()/abs(momentum)),
		     step,
		     rch_hit->de,
		     rch_hit->volume_id,
		     "hi");

	    if(rch_hit->volume_id<2000) {
		PR(rch_hit->volume_id);
		PR(hit);
		cout << "p= " << abs(momentum) << " tpchit " << track[(rch_hit->track_p-1)].n_tpc_hit << " eg_lab " << track[(rch_hit->track_p-1)].eg_label << " egpid " << track[(rch_hit->track_p-1)].eg_pid << endl;
		PR(rch_hit->track_p);
	    }
	    mFilter( hit );
		    << " egpid= "     << track[(rch_hit->track_p-1)].eg_pid << endl;
// 			    << tpc_hit[zz].x[1] << " "
            if (StRichViewer::histograms )
		StRichViewer::getView()->update();
			mInduceSignal(hit);
	    rch_hit++;
	}  // loop over hits
	
    } // fill the data set
		}
    } //else
		iter != theList.end();
		wireNumber = mWireSelector.whichWire(*iter);
    cout << "Try Write" << endl;
	    mWriter->getSignal(i,j).signal +=  mNoiseSimulator();

	    
	    //mWriter->getSignal(i,j).signal +=  mNoiseSimulator();

    } //else process from stream


    //cout << "Try Write" << endl;
    for ( int i = 0; i < mWriter->rows(); i++ )
	for ( int j = 0; j < mWriter->cols(); j++ ) {

#ifdef RICH_WITH_VIEWER
	    if (StRichViewer::histograms )
		StRichViewer::getView()->mADCSignal->Fill(i,j,mWriter->getSignal(i,j).signal);
#endif
	}	      
    
    
#ifdef RICH_DECODE_DATA
    int version = 1;
    for(int iRow=0; iRow<(2*mGeometryDb->n_pad_x); iRow++) {  // 96
	for(int iCol=0; iCol<(2*mGeometryDb->n_pad_z) ; iCol++) {
		cout << "r/c/adc: " << iRow << ' ' << iCol << ' ' << theADCValue << endl;
		}
#endif
		
// 			<< (*iter).mAmount << endl;
// 			<< iter->mTrackp << "MCinfo.amount= "


//     for(int kk=90; kk<270;kk+=5) {
// 	bool status = myCalculator.getRing(eInnerRing)->getPoint(kk*degree, aPoint);
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

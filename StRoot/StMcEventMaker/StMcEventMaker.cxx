/*************************************************
 *
 * $Id: StMcEventMaker.cxx,v 1.13 2000/02/04 15:40:52 calderon Exp $
 * $Log: StMcEventMaker.cxx,v $
 * Revision 1.13  2000/02/04 15:40:52  calderon
 * Fix dumping of vertex info when there is just one vertex.
 *
 * Revision 1.12  2000/01/18 20:53:08  calderon
 * Changes to work with CC5
 *
 * Revision 1.11  2000/01/11 23:18:56  calderon
 * Check if there are hits before writing info to screen.
 *
 * Revision 1.10  1999/12/14 07:05:32  calderon
 * Use numbering scheme
 *
 * Revision 1.9  1999/12/03 19:40:42  calderon
 * volume_id for SVT hits can be up to ~8700.
 *
 * Revision 1.8  1999/12/03 00:55:21  calderon
 * Completely revised for StMcEvent 2.0
 * Using StDbUtilities for coordinate transformations.
 * Tested g2t_event table is read properly (when available).
 * Added messages for diagnostics.
 * Tested in Linux, Solaris 4.2  and HP.
 *
 * Revision 1.7  1999/09/24 01:23:18  fisyak
 * Reduced Include Path
 *
 * Revision 1.6  1999/09/23 21:25:59  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 * Revision 1.5  1999/09/15 18:39:28  calderon
 * -Do not require g2t_ftp_hit table for filling of StMcEvent
 * -Update README for changes
 *
 * Revision 1.4  1999/09/10 19:11:54  calderon
 * Write the Ntuple in StMcAnalysisMaker into a file.
 * This way it can be accessed after the macro finishes,
 * otherwise it gets deleted.
 *
 * Revision 1.3  1999/07/28 20:27:42  calderon
 * Version with SL99f libraries
 *
 *
 *************************************************/
#include <iostream.h>
#include <stdlib.h>
#include <string>
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
using std::string;
#endif

#include "TStyle.h"
#include "TCanvas.h"
#include "StMcEventMaker.h"

#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StGlobals.hh"
#include "StMessMgr.h"
#include "StMemoryInfo.hh"
#include "StTimer.hh"

#include "StThreeVectorF.hh"

#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"

#include "tables/St_g2t_event_Table.h"
#include "tables/St_g2t_ftp_hit_Table.h"
#include "tables/St_g2t_svt_hit_Table.h"
#include "tables/St_g2t_tpc_hit_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_vertex_Table.h"

#include "StMcEventTypes.hh"


static double vertexCut = .0000025; // 25 nm (lifetime of the pi0)
struct vertexFlag {
	      StMcVertex* vtx;
	      int primaryFlag; };

static const char rcsid[] = "$Id: StMcEventMaker.cxx,v 1.13 2000/02/04 15:40:52 calderon Exp $";
ClassImp(StMcEventMaker)


//_____________________________________________________________________________

    
StMcEventMaker::StMcEventMaker(const char*name, const char * title):StMaker(name,title)
{
    // StMcEventMaker - constructor
    // - set all pointers defined in the header file to zero

    mCurrentMcEvent = 0; //! I think this tells root not to parse it 
    doPrintEventInfo  = kTRUE;  // TMP
    doPrintMemoryInfo = kTRUE;  
    doPrintCpuInfo    = kTRUE; 

}

//_____________________________________________________________________________


StMcEventMaker::~StMcEventMaker()
{
    // StMcEventMaker - destructor
    cout << "Inside ReaderMaker Destructor" << endl;
    SafeDelete(mCurrentMcEvent);  //

}



//_____________________________________________________________________________

void StMcEventMaker::Clear(const char*)
{
    // StMcEventMaker - Clear,
    delete mCurrentMcEvent;
    mCurrentMcEvent = 0;
    StMaker::Clear();
}


//_____________________________________________________________________________


Int_t StMcEventMaker::Finish()
{
    // StMcEventMaker - Finish, Draw histograms if SetDraw true

    // Right now I'm not doing any histograms, later on, I would need to uncomment
    // the next line, and add a DrawHists() method.  Look in St_QA_Maker.cxx
    //if (drawinit)  DrawHists();
  return StMaker::Finish();
}


//_____________________________________________________________________________

Int_t StMcEventMaker::Init()
{
    return StMaker::Init();
}

//_____________________________________________________________________________

Int_t StMcEventMaker::Make()
{
    // StMcEventMaker - Make; fill StMcEvent objects
    StTimer timer;
    if (doPrintCpuInfo) timer.start();
//     if (doPrintMemoryInfo) {
// 	StMemoryInfo::instance()->snapshot();
// 	StMemoryInfo::instance()->print();
//     }
    
    cout << "Inside StMcEventMaker::Make()" << endl;
    // We're supposed to get the dataset from the chain. I don't know how yet. I think it is:

    
    St_DataSet* dsGeant = GetDataSet("geant");
    if(!dsGeant || !dsGeant->GetList()) return kStWarn;
    // This is done only for one file, though.  I haven't put functionality for
    // multiple file handling.  If needed, it should start in the StMcEventReadMacro
    // and try to follow the doEvents.C macro to read in multiple files.
    // Then, we would need to mimic the nextRootFile() methods and so on.
    
  
    // Now we have the DataSet, but for some reason, we need the Iterator to navigate
    
    St_DataSetIter geantDstI(dsGeant);
  
    // Now the Iterator is set up, and this allows us to access the tables
    // This is done like so:
    // TableClass *instanceOfTableClassPointer = cast to TableClassPointer instanceOfDataSetIter("actual name of table in data set");
    
    St_g2t_event   *g2t_eventTablePointer   =  (St_g2t_event   *) geantDstI("g2t_event");
    St_g2t_vertex  *g2t_vertexTablePointer  =  (St_g2t_vertex  *) geantDstI("g2t_vertex");
    St_g2t_track   *g2t_trackTablePointer   =  (St_g2t_track   *) geantDstI("g2t_track");
    St_g2t_tpc_hit *g2t_tpc_hitTablePointer =  (St_g2t_tpc_hit *) geantDstI("g2t_tpc_hit");
    St_g2t_svt_hit *g2t_svt_hitTablePointer =  (St_g2t_svt_hit *) geantDstI("g2t_svt_hit");
    St_g2t_ftp_hit *g2t_ftp_hitTablePointer =  (St_g2t_ftp_hit *) geantDstI("g2t_ftp_hit");

    // Now we check if we have the pointer, if we do, then we can access the tables!
  
    if (g2t_vertexTablePointer && g2t_trackTablePointer
	&& g2t_tpc_hitTablePointer ){

	//
	// g2t_event Table
	//
	g2t_event_st   *eventTable;
	
	// Check Pointer
	if (g2t_eventTablePointer)
	    eventTable  = g2t_eventTablePointer->GetTable();
	else
	    cerr << "Table g2t_event Not found in Dataset " << geantDstI.Pwd()->GetName() << endl;

	// Check Table
	if(!g2t_eventTablePointer->GetNRows()) {
	    cout << "Event Table is EMPTY!! " << endl;
	    PR(g2t_eventTablePointer->GetNRows());
	    PR(eventTable)
	}
	//
	// Vertex, Track and Tpc Hit table don't have problems normally.
	//
	g2t_vertex_st  *vertexTable = g2t_vertexTablePointer->GetTable();
	g2t_track_st   *trackTable  = g2t_trackTablePointer->GetTable();
	g2t_tpc_hit_st *tpcHitTable = g2t_tpc_hitTablePointer->GetTable();

	//
	// Ftpc Hit Table
	//
	g2t_ftp_hit_st *ftpHitTable;
	if (g2t_ftp_hitTablePointer)
	    ftpHitTable = g2t_ftp_hitTablePointer->GetTable();
	else 
	    cerr << "Table g2t_ftp_hit Not found in Dataset " << geantDstI.Pwd()->GetName() << endl;

	//
	// Svt Hit Table
	//
	g2t_svt_hit_st *svtHitTable;
	if (g2t_svt_hitTablePointer)
	    svtHitTable = g2t_svt_hitTablePointer->GetTable();
	else
	    cerr << "Table g2t_svt_hit Not found in Dataset " << geantDstI.Pwd()->GetName() << endl;
	
       
       // Before filling StMcEvent, we can check whether we can actually
       // access the tables.
       
// 	  cout << "Event Table Examples: " << endl;
// 	  cout << "eg_label: " << eventTable->eg_label << endl; 
// 	  cout << "n_event : " << eventTable->n_event << endl;
// 	  cout << "n_run   : " << eventTable->n_run << endl;
// 	  cout << "n_part_prot_west: " <<  eventTable->n_part_prot_west << endl;
// 	  cout << "n_part_neut_west: " <<  eventTable->n_part_neut_west << endl;
// 	  cout << "n_part_prot_east: " <<  eventTable->n_part_prot_east << endl;
// 	  cout << "n_part_neut_east: " <<  eventTable->n_part_neut_east << endl;

// 	  cout << "Vertex Table Examples:" << endl;
// 	  cout << "ge_x[0] :" << vertexTable[0].ge_x[0] << endl;
// 	  cout << "ge_x[1] :" << vertexTable[0].ge_x[1] << endl;
// 	  cout << "ge_x[2] :" << vertexTable[0].ge_x[2] << endl;

// 	  cout << "Track Table Examples:" << endl;
// 	  cout << "p[0] :" << trackTable[0].p[0] << endl;
// 	  cout << "p[1] :" << trackTable[0].p[1] << endl;
// 	  cout << "p[2] :" << trackTable[0].p[2] << endl;
	  
// 	  cout << "Tpc Hit Table Examples:" << endl;
// 	  cout << "p[0] :" << tpcHitTable[0].p[0] << endl;
// 	  cout << "p[1] :" << tpcHitTable[0].p[1] << endl;
// 	  cout << "p[2] :" << tpcHitTable[0].p[2] << endl;

// 	  cout << "Svt Hit Table Examples:" << endl;
// 	  cout << "p[0] :" << svtHitTable[0].p[0] << endl;
// 	  cout << "p[1] :" << svtHitTable[0].p[1] << endl;
// 	  cout << "p[2] :" << svtHitTable[0].p[2] << endl;

// 	  cout << "Ftpc Hit Table Examples:" << endl;
// 	  cout << "p[0] :" << ftpHitTable[0].p[0] << endl;
// 	  cout << "p[1] :" << ftpHitTable[0].p[1] << endl;
// 	  cout << "p[2] :" << ftpHitTable[0].p[2] << endl;
	  
	// Ok, now we have the g2t tables for this event, now we can create the
	// StMcEvent with them.
	
	// Now Here goes the scheme Mike Lisa and I cooked up
	
	//______________________________________________________________________
	// Step 1 - fill the StMcEvent, already created in the header file
	
	if (eventTable)
	    mCurrentMcEvent = new StMcEvent(eventTable);
	else 
	    mCurrentMcEvent = new StMcEvent;
	if (mCurrentMcEvent) cout << "****  Defined new StMcEvent" << endl;
	
	//______________________________________________________________________
	// Step 2 - Fill Vertices - we do not fill parent/daughters until Step 3
	
	long NVertices = g2t_vertexTablePointer->GetNRows();
	  
#ifndef ST_NO_TEMPLATE_DEF_ARGS
	vector<vertexFlag> vtemp(NVertices); // Temporary array for Step 3
#else
	vector<vertexFlag, allocator<vertexFlag> > vtemp(NVertices);
#endif
       
	cout << "Preparing to process and fill VERTEX information ....." << endl;
	StMcVertex* v = 0;
	
	
	// First vertex is PRIMARY
	
	v = new StMcVertex(&(vertexTable[0]));
	mCurrentMcEvent->vertices().push_back(v);
	vtemp[vertexTable[0].id - 1].vtx = v;
	vtemp[vertexTable[0].id - 1].primaryFlag = 1;
	mCurrentMcEvent->setPrimaryVertex(v);
	
	StThreeVectorF primVertexPos = v->position();
	
	StThreeVectorF testVertexPos;
	int nThrownVertices = 0;
	for (long ivtx=1; ivtx<NVertices; ivtx++)
	    {    
		v = new StMcVertex(&(vertexTable[ivtx]));
		
		// get the position of the current vertex
		testVertexPos = v->position();
		
		if (vertexTable[ivtx].eg_label == 0 ||
		    abs(primVertexPos - testVertexPos) > vertexCut ) {
		    // GEANT vertex or (generator vertex that satisfies cut) ...
		    mCurrentMcEvent->vertices().push_back(v);  // adds vertex v to master collection
		    vtemp[vertexTable[ivtx].id - 1].primaryFlag = 0;
		    vtemp[vertexTable[ivtx].id - 1].vtx = v;
		}
		else { // These "vertices" are the same as primary, so flag them
		    nThrownVertices++;
		    vtemp[vertexTable[ivtx].id - 1].primaryFlag = 1;
		    
		    delete v;
		    vtemp[vertexTable[ivtx].id - 1].vtx = mCurrentMcEvent->primaryVertex();
		}
	    }
	if (nThrownVertices) 
	    gMessMgr->Warning() << "StMcEventMaker::Make(): Throwing " << nThrownVertices
				<< " that are the same as the primary vertex." << endm;
       
	//______________________________________________________________________
	// Step 3 - Fill Tracks - we do not fill associated hits until Step 4

	long NTracks = g2t_trackTablePointer->GetNRows();
#ifndef ST_NO_TEMPLATE_DEF_ARGS	  
	vector<StMcTrack*> ttemp(NTracks); // Temporary array for Step 4
#else
	vector<StMcTrack*, allocator<StMcTrack*> > ttemp(NTracks);
#endif

	cout << "Preparing to process and fill TRACK information ....." << endl;
	StMcTrack* t = 0;
	long iStartVtxId = 0;
	long iStopVtxId = 0;
	long iItrmdVtxId = 0;
	
	int nThrownTracks = 0;
	for (long itrk=0; itrk<NTracks; itrk++) {
	    iStopVtxId = (trackTable[itrk].stop_vertex_p) - 1;
	    
	    if (iStopVtxId >= 0) {
		if (vtemp[iStopVtxId].primaryFlag == 1) {
		    
		    nThrownTracks++;
		    continue; // This skips until the next itrk
		    
		}
	    }    
	    t = new StMcTrack(&(trackTable[itrk]));
	    ttemp[ trackTable[itrk].id - 1 ] = t; // This we do for all accepted tracks
	    
	    
	    mCurrentMcEvent->tracks().push_back(t); // adds track t to master collection
	    
	    // point track to its stop vertex,
	    // and tell stop vertex that this is its parent
	    if (iStopVtxId >= 0) {
		t->setStopVertex(vtemp[iStopVtxId].vtx);
		vtemp[iStopVtxId].vtx->setParent(t);
	    }
	    //cout << "Established relation btw track & STOP vertex" << endl;
	    
	    // point track to its parent vertex,
	    // and tell parent vertex that this is its daughter
	    
	    iStartVtxId = trackTable[itrk].start_vertex_p - 1;
	    
	    v = vtemp[iStartVtxId].vtx; // This should already know the right vertex.
	    
	    t->setStartVertex(v);
	    v->addDaughter(t);
	    //cout << "Established relation btw track & START vertex" << endl;
	    
	    // now fill track's intermediate vertex collection,
	    // and tell those vertices their parents
	    
	    iItrmdVtxId = (trackTable[itrk].itrmd_vertex_p) - 1;
	    
	    if (iItrmdVtxId >= 0) {
		t->intermediateVertices().push_back(vtemp[iItrmdVtxId].vtx);
		
		vtemp[iItrmdVtxId].vtx->setParent(t);
		
		// follow the "linked list" of g2t_vertex table to get the rest
		
		while(vertexTable[iItrmdVtxId].next_itrmd_p != 0) {
		    iItrmdVtxId = (vertexTable[iItrmdVtxId].next_itrmd_p) - 1;
		    t->intermediateVertices().push_back(vtemp[iItrmdVtxId].vtx);
		    vtemp[iItrmdVtxId].vtx->setParent(t);
		}
		
		
	    } // Intermediate vertices
	    
	} // Track loop

	if (nThrownTracks)
	    gMessMgr->Warning() << "StMcEventMaker::Make(): Throwing " << nThrownTracks
				<< " whose stop vertex is the same as primary vertex." << endm;
	vtemp.clear();
	
	
	//______________________________________________________________________
	// Step 4 - Fill Hits
		
	cout << "Preparing to process and fill HIT information ....." << endl;
	
	StMcTpcHit* th = 0;
	StMcSvtHit* sh = 0;
	StMcFtpcHit* fh = 0;

	//
	// TPC Hits
	//
	
	long NHits = g2t_tpc_hitTablePointer->GetNRows();
	long iTrkId = 0;
	long nBadVolId = 0;

	long ihit;
	for(ihit=0; ihit<NHits; ihit++) {
	    if (tpcHitTable[ihit].volume_id < 101 || tpcHitTable[ihit].volume_id > 202445) {
		
		nBadVolId++;
		continue;
	    }
	    
	    th = new StMcTpcHit(&tpcHitTable[ihit]);
	    
	    mCurrentMcEvent->tpcHitCollection()->addHit(th); // adds hit th to collection
	    
	    // point hit to its parent and add it to collection
	    // of the appropriate track
	    
	    iTrkId = (tpcHitTable[ihit].track_p) - 1;
	    
	    th->setParentTrack(ttemp[iTrkId]);
	    ttemp[iTrkId]->addTpcHit(th);
	    
	}
	cout << "Filled TPC Hits" << endl;
	if (nBadVolId) gMessMgr->Warning() << "StMcEventMaker::Make(): cannot store " << nBadVolId
					   << " TPC hits, wrong Volume Id." << endm;
	
	//
	// SVT Hits
	//
	if (g2t_svt_hitTablePointer) {
	    NHits = g2t_svt_hitTablePointer->GetNRows();
	    iTrkId = 0;
	    nBadVolId = 0;
	    for(ihit=0; ihit<NHits; ihit++) {
		if (svtHitTable[ihit].volume_id < 1101 || svtHitTable[ihit].volume_id > 9000) {
		    nBadVolId++;
		    continue;
		}
		sh = new StMcSvtHit(&svtHitTable[ihit]);
		mCurrentMcEvent->svtHitCollection()->addHit(sh); // adds hit sh to collection
		
		// point hit to its parent and add it to collection
		// of the appropriate track
		
		iTrkId = (svtHitTable[ihit].track_p) - 1;
		sh->setParentTrack(ttemp[iTrkId]);
		ttemp[iTrkId]->addSvtHit(sh);
		
	    }
	    cout << "Filled SVT Hits" << endl;
	    if (nBadVolId)
		gMessMgr->Warning() << "StMcEventMaker::Make(): cannot store " << nBadVolId
				    << " SVT hits, wrong Volume Id." << endm;
	    
	}
	else {
	    cout << "No SVT Hits in this file" << endl;
	}
	
	// FTPC Hits
	if (g2t_ftp_hitTablePointer) {
	    NHits = g2t_ftp_hitTablePointer->GetNRows();
	    iTrkId = 0;
	    nBadVolId = 0;
	    for(ihit=0; ihit<NHits; ihit++) {
		if (ftpHitTable[ihit].volume_id < 101 || ftpHitTable[ihit].volume_id > 210) {
		    nBadVolId++;
		    continue;
		}

		fh = new StMcFtpcHit(&ftpHitTable[ihit]);
		mCurrentMcEvent->ftpcHitCollection()->addHit(fh); // adds hit fh to collection
		
		// point hit to its parent and add it to collection
		// of the appropriate track
		
		iTrkId = (ftpHitTable[ihit].track_p) - 1;
		fh->setParentTrack(ttemp[iTrkId]);
		ttemp[iTrkId]->addFtpcHit(fh);
		
	    }
	    cout << "Filled FTPC Hits" << endl;
	    if (nBadVolId)
		gMessMgr->Warning() << "StMcEventMaker::Make(): cannot store " << nBadVolId
				    << " FTPC hits, wrong Volume Id." << endm;
	}
	else {
	    cout << "No FTPC Hits in this file" << endl;
	}
	
	
	ttemp.clear();
	
	//_______________________________________________________________
	// At this point StMcEvent should be loaded.
		
    }
    if (doPrintEventInfo) printEventInfo();
    if (doPrintMemoryInfo) {
	StMemoryInfo::instance()->snapshot();
	StMemoryInfo::instance()->print();
    }
    if (doPrintCpuInfo) {
	timer.stop();
	cout << "CPU time for StMcEventMaker::Make(): "
	     << timer.elapsedTime() << " sec\n" << endl;
    }
  
  return kStOK;

}
    
void
StMcEventMaker::printEventInfo()
{
    cout << "*********************************************************" << endl;
    cout << "*                  StMcEvent Information                *" << endl;
    cout << "*********************************************************" << endl;

    cout << "---------------------------------------------------------" << endl;
    cout << "StMcEvent at " << (void*) mCurrentMcEvent                  << endl;
    cout << "---------------------------------------------------------" << endl;
    if (mCurrentMcEvent)
	cout << *mCurrentMcEvent << endl;
    else
	return;

    cout << "---------------------------------------------------------" << endl;
    cout << "StSPtrVecMcTrack"                                          << endl;
    cout << "Dumping first element in collection only (if available). " << endl;
    cout << "---------------------------------------------------------" << endl;
    cout << "collection size = " << mCurrentMcEvent->tracks().size()    << endl;
    if (mCurrentMcEvent->tracks().size()) {
	cout << "---------------------------------------------------------" << endl;
	cout << "StMcTrack at "
	     << (void*) mCurrentMcEvent->tracks()[0]                        << endl;
	cout << "---------------------------------------------------------" << endl;
	cout << *(mCurrentMcEvent->tracks()[0])                             << endl;
    }

    cout << "---------------------------------------------------------" << endl;
    cout << "StMcVertex"                                                << endl;
    cout << "Dumping vertex info and first daughter track.            " << endl;
    cout << "---------------------------------------------------------" << endl;
    cout << "Primary Vertex at "
	 << (void*) mCurrentMcEvent->primaryVertex()                    << endl;
    cout << "---------------------------------------------------------" << endl;
    cout << *(mCurrentMcEvent->primaryVertex())                         << endl;
    cout << "---------------------------------------------------------" << endl;
    cout << "Daughters of Primary Vertex : "
	 << mCurrentMcEvent->primaryVertex()->numberOfDaughters() << endl;
    if (mCurrentMcEvent->primaryVertex()->numberOfDaughters()) {
	cout << "First Daughter of Primary Vertex" << endl;
	cout << *(mCurrentMcEvent->primaryVertex()->daughter(0));
    }
    cout << "---------------------------------------------------------" << endl;
    cout << "StSPtrVecMcVertex"                                         << endl;
    cout << "# of Vertices    : " << mCurrentMcEvent->vertices().size() << endl;
    cout << "---------------------------------------------------------" << endl;
    cout << "Dumping second element in collection (First is Primary). " << endl;
    if (mCurrentMcEvent->vertices().size()>1) {
	cout << "---------------------------------------------------------" << endl;
	cout << "Second StMcVertex at "
	     << (void*) mCurrentMcEvent->vertices()[1]                      << endl;
	cout << "---------------------------------------------------------" << endl;
	cout << *(mCurrentMcEvent->vertices()[1])                           << endl;
	cout << "---------------------------------------------------------" << endl;
	cout << "Daughters of second Vertex : "
	     << mCurrentMcEvent->vertices()[1]->numberOfDaughters() << endl;
	if (mCurrentMcEvent->vertices()[1]->numberOfDaughters()) {
	    cout << "First Daughter of this Vertex" << endl;
	    cout << *(mCurrentMcEvent->vertices()[1]->daughter(0));
	}
	
    }

    
    unsigned int       i, j, k, nhits;
    Bool_t             gotOneHit;
    StMcTpcHitCollection *tpcColl = mCurrentMcEvent->tpcHitCollection();
    cout << "---------------------------------------------------------" << endl;
    cout << "StMcTpcHitCollection at " << (void*) tpcColl               << endl;
    cout << "Dumping collection size and one hit only."                 << endl;
    cout << "---------------------------------------------------------" << endl;
    nhits = tpcColl->numberOfHits();
    cout << "# of hits in collection = " << nhits << endl;
    if (tpcColl && nhits) {
	
	
	gotOneHit = kFALSE;
	for (k=0; !gotOneHit && k<tpcColl->numberOfSectors(); k++)
	    for (j=0; !gotOneHit && j<tpcColl->sector(k)->numberOfPadrows(); j++)
		if (tpcColl->sector(k)->padrow(j)->hits().size()) {
		    cout << "Tpc Hit" << endl;
		    cout << *(tpcColl->sector(k)->padrow(j)->hits()[0]);
		    cout << "Parent track of this Hit" << endl;
		    cout << *(tpcColl->sector(k)->padrow(j)->hits()[0]->parentTrack()) << endl;
		    gotOneHit = kTRUE;
		}
    }
    
    StMcFtpcHitCollection *ftpcColl = mCurrentMcEvent->ftpcHitCollection();
    cout << "---------------------------------------------------------" << endl;
    cout << "StMcFtpcHitCollection at " << (void*) ftpcColl             << endl;
    cout << "Dumping collection size and one hit only."                 << endl;
    cout << "---------------------------------------------------------" << endl;
    nhits = ftpcColl->numberOfHits();
    cout << "# of hits in collection = " << nhits << endl;
    if (ftpcColl &&  nhits) {
	
	
	gotOneHit = kFALSE;
	for (k=0; !gotOneHit && k<ftpcColl->numberOfPlanes(); k++)
	    if (ftpcColl->plane(k)->hits().size()) {
		cout << "Ftpc Hit" << endl;
		cout << *(ftpcColl->plane(k)->hits()[0]);
		cout << "Parent track of this Hit" << endl;
		cout << *(ftpcColl->plane(k)->hits()[0]->parentTrack()) << endl;
		gotOneHit = kTRUE;
	    }
    }
    
    StMcSvtHitCollection *svtColl = mCurrentMcEvent->svtHitCollection();
    cout << "---------------------------------------------------------" << endl;
    cout << "StMcSvtHitCollection at " << (void*) svtColl               << endl;
    cout << "Dumping collection size and one hit only."                 << endl;
    cout << "---------------------------------------------------------" << endl;
    nhits = svtColl->numberOfHits();
    cout << "# of hits in collection = " << nhits << endl;

    if (svtColl && nhits) {
	
	gotOneHit = kFALSE;
	for (k=1; !gotOneHit && k<=svtColl->numberOfLayers(); k++)
	    for (j=1; !gotOneHit && j<=svtColl->layer(k)->numberOfLadders(); j++)
		for (i=1; !gotOneHit && i<=svtColl->layer(k)->ladder(j)->numberOfWafers(); i++)
		    if (svtColl->layer(k)->ladder(j)->wafer(i)->hits().size()) {
			cout << "Svt Hit" << endl;
			cout << *(svtColl->layer(k)->ladder(j)->wafer(i)->hits()[0]);
			cout << "Parent track of this Hit" << endl;
			cout << *(svtColl->layer(k)->ladder(j)->wafer(i)->hits()[0]->parentTrack()) << endl;
			gotOneHit = kTRUE;
		    }
    }
        
    cout << endl;
}  

    
    

/*************************************************
 *
 * $Id: StMcEventMaker.cxx,v 1.19 2000/04/12 21:32:36 calderon Exp $
 * $Log: StMcEventMaker.cxx,v $
 * Revision 1.19  2000/04/12 21:32:36  calderon
 * check for eg_label in range of particle table
 *
 * Revision 1.18  2000/04/06 23:29:40  calderon
 * The parent track is now stored for all tracks.
 *
 * Revision 1.17  2000/04/06 20:26:55  calderon
 * All particles in the particle table are now filled.
 * Relationships to parents can be followed to event generator particles.
 *
 * Revision 1.16  2000/04/06 08:38:08  calderon
 * First version using the particle table.
 * The parent daughter relationship(if it exists) between tracks in the g2t_track table
 * and tracks that come from the
 * event generator is successfully established.  Next step is to load
 * the rest of the particle table entries that don't have any descendants
 * in the g2t table (which is the majority of the entries).
 *
 * Revision 1.15  2000/04/04 23:15:43  calderon
 * Report number of hits successfully stored and additional
 * reporting of hits with bad volume id.
 *
 * Revision 1.14  2000/03/06 18:07:36  calderon
 * 1) Check tpc hit volume id to not load hits in pseudo pad rows.
 * 2) Sort the hits in the collections, in order to save time
 * later during hit associations.
 *
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
#include <algorithm>
#ifndef ST_NO_NAMESPACES
using std::vector;
using std::string;
using std::sort;
using std::find;
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
#include "tables/St_g2t_rch_hit_Table.h"
#include "tables/St_g2t_svt_hit_Table.h"
#include "tables/St_g2t_tpc_hit_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_particle_Table.h"

#include "StMcEventTypes.hh"


static double vertexCut = .0000025; // 25 nm (lifetime of the pi0)
struct vertexFlag {
	      StMcVertex* vtx;
	      int primaryFlag; };

static const char rcsid[] = "$Id: StMcEventMaker.cxx,v 1.19 2000/04/12 21:32:36 calderon Exp $";
ClassImp(StMcEventMaker)


//_____________________________________________________________________________

    
StMcEventMaker::StMcEventMaker(const char*name, const char * title):StMaker(name,title)
{
    // StMcEventMaker - constructor
    // - set all pointers defined in the header file to zero

    mCurrentMcEvent = 0; //! I think this tells root not to parse it 
    doPrintEventInfo  = kFALSE;  // TMP
    doPrintMemoryInfo = kFALSE;  
    doPrintCpuInfo    = kFALSE; 

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
    St_DataSet* dsDst   = GetDataSet("dst");
    if(!dsGeant || !dsGeant->GetList()) return kStWarn;
    // This is done only for one file, though.  I haven't put functionality for
    // multiple file handling.  If needed, it should start in the StMcEventReadMacro
    // and try to follow the doEvents.C macro to read in multiple files.
    // Then, we would need to mimic the nextRootFile() methods and so on.
    
  
    // Now we have the DataSet, but for some reason, we need the Iterator to navigate
    
    St_DataSetIter geantDstI(dsGeant);
    St_DataSetIter dstDstI(dsDst);
  
    // Now the Iterator is set up, and this allows us to access the tables
    // This is done like so:
    // TableClass *instanceOfTableClassPointer = cast to TableClassPointer instanceOfDataSetIter("actual name of table in data set");
    
    St_g2t_event   *g2t_eventTablePointer   =  (St_g2t_event   *) geantDstI("g2t_event");
    St_g2t_vertex  *g2t_vertexTablePointer  =  (St_g2t_vertex  *) geantDstI("g2t_vertex");
    St_g2t_track   *g2t_trackTablePointer   =  (St_g2t_track   *) geantDstI("g2t_track");
    St_g2t_tpc_hit *g2t_tpc_hitTablePointer =  (St_g2t_tpc_hit *) geantDstI("g2t_tpc_hit");
    St_g2t_svt_hit *g2t_svt_hitTablePointer =  (St_g2t_svt_hit *) geantDstI("g2t_svt_hit");
    St_g2t_ftp_hit *g2t_ftp_hitTablePointer =  (St_g2t_ftp_hit *) geantDstI("g2t_ftp_hit");

    St_g2t_rch_hit *g2t_rch_hitTablePointer =  (St_g2t_rch_hit *) dstDstI("g2t_rch_hit");
    St_particle    *particleTablePointer    =  (St_particle    *) dstDstI("particle");

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
	
	//
	// Rich Hit Table
	//
	g2t_rch_hit_st *rchHitTable;
	if (g2t_rch_hitTablePointer)
	    rchHitTable = g2t_rch_hitTablePointer->GetTable();
	else
	    cerr << "Table g2t_rch_hit Not found in Dataset " << dstDstI.Pwd()->GetName() << endl;
	
	//
	// particle Table
	//
	particle_st *particleTable;
	if (particleTablePointer)
	    particleTable = particleTablePointer->GetTable();
	else
	    cerr << "Table particle Not found in Dataset " << dstDstI.Pwd()->GetName() << endl;
       
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
	  
// 	  cout << "Rich Hit Table Examples:" << endl;
// 	  cout << "p[0] :" << rchHitTable[0].p[0] << endl;
// 	  cout << "p[1] :" << rchHitTable[0].p[1] << endl;
// 	  cout << "p[2] :" << rchHitTable[0].p[2] << endl;
	  
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
	size_t usedTracksG2t = 0;
	long NGeneratorTracks = particleTablePointer->GetNRows();
	size_t usedTracksEvGen = 0;
#ifndef ST_NO_TEMPLATE_DEF_ARGS	  
	vector<StMcTrack*> ttemp(NTracks); // Temporary array for Step 4
	vector<StMcTrack*> ttempParticle(NGeneratorTracks);
#else
	vector<StMcTrack*, allocator<StMcTrack*> > ttemp(NTracks);
	vector<StMcTrack*, allocator<StMcTrack*> > ttempParticle(NGeneratorTracks);
#endif

	cout << "Preparing to process and fill TRACK information ....." << endl;
	StMcTrack* egTrk = 0;
	size_t nParticlesInBothTables = 0;
	cout << "Event Generator Tracks..." << endl;

	// The filling of the event generator track assumes that if we encounter a particle that
	// has a mother, the mother should ALREADY HAVE BEEN CREATED, i.e. that the mother indices
	// of the particles in the particle table is never more than the current index.

	long motherIndex = -1;  // Set it to some unused number. 
	for (long gtrk=0; gtrk<NGeneratorTracks; gtrk++) {
	    egTrk = new StMcTrack(&(particleTable[gtrk]));
	    egTrk->setEventGenLabel(gtrk+1);
	    ttempParticle[gtrk] = egTrk;
	    mCurrentMcEvent->tracks().push_back(egTrk); // adds track egTrk to master collection 
	    usedTracksEvGen++;
	    // Find Mother...
	    motherIndex = particleTable[gtrk].jmohep[0];
	    if ((motherIndex > 0) && (motherIndex < NGeneratorTracks))
		if (motherIndex > gtrk) 
		    gMessMgr->Warning()
			<< "Wrong ordering!  Track " << gtrk+1 << "from particle table: "
			<< "Can't assign mother track " << motherIndex
			<< "because it has not been created yet!" << endm;
		else egTrk->setParent(ttempParticle[motherIndex-1]);
	    
	} // Generator Tracks Loop
	
	StMcTrack* t = 0;
	long iStartVtxId = 0;
	long iStopVtxId = 0;
	long iItrmdVtxId = 0;
	
	int nThrownTracks = 0;
	cout << "g2t Tracks..." << endl;
	for (long itrk=0; itrk<NTracks; itrk++) {
	    iStopVtxId = (trackTable[itrk].stop_vertex_p) - 1;
	    
	    if (iStopVtxId >= 0) {
		if (vtemp[iStopVtxId].primaryFlag == 1) {
		    
		    nThrownTracks++;
		    continue; // This skips until the next itrk
		    
		}
	    }    
	    t = new StMcTrack(&(trackTable[itrk]));
	    usedTracksG2t++;
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

	    // Look in the particle table
	    // for particles from event generator
	    long iEventGeneratorLabel = (trackTable[itrk].eg_label) - 1;
	    if (iEventGeneratorLabel >=0 ) {

		// Now make sure that this track is really from the table,
		// When embedding, the particle got an eg_label = 99999 even
		// though there was only one entry in the particle table.
		if (iEventGeneratorLabel < ttempParticle.size()) {
		    // Track should already be loaded from the particle table
		    // i.e. t & ttempParticle[iEventGeneratorLabel] are the same tracks,
		    // obtained from different tables.
		    // We should rather keep the one in the g2t table, but we
		    // need to keep the information of its parentage.
		    nParticlesInBothTables++;
		    t->setParent(ttempParticle[iEventGeneratorLabel]->parent());
		    StMcTrackIterator trkToErase = find (mCurrentMcEvent->tracks().begin(),
							 mCurrentMcEvent->tracks().end(),
							 ttempParticle[iEventGeneratorLabel]);
		    mCurrentMcEvent->tracks().erase(trkToErase);
		}
		               		    
	    }
	    else {
		// Track from GEANT, use next_parent_p to get to the parent
		// track.  Use the same scheme as for the particle table.
		motherIndex = trackTable[itrk].next_parent_p;
		if ((motherIndex > 0) && (motherIndex < NTracks))
		    if (motherIndex > itrk) 
			gMessMgr->Warning()
			    << "Wrong ordering!  Track " << itrk+1 << "from particle table: "
			    << "Can't assign mother track " << motherIndex
			    << "because it has not been created yet!" << endm;
		    else t->setParent(ttemp[motherIndex-1]);
	    }
	    
	} // Track loop

	if (nThrownTracks)
	    gMessMgr->Warning() << "StMcEventMaker::Make(): Throwing " << nThrownTracks
				<< " whose stop vertex is the same as primary vertex." << endm;
	if (Debug()) {
	    cout << "Used   tracks from g2t_track table: " << usedTracksG2t << endl;
	    cout << "Avail. tracks from g2t_track table: " << NTracks       << endl;
	    cout << "Used   tracks from particle  table: " << usedTracksEvGen  << endl;
	    cout << "Avail. tracks from particle  table: " << NGeneratorTracks << endl;
	    cout << "Tracks appearing in both tables   : " << nParticlesInBothTables << endl;
	    cout << "Total tracks in StMcEvent         : " << mCurrentMcEvent->tracks().size() << endl;
	}
	vtemp.clear();
	ttempParticle.clear();
	
	//______________________________________________________________________
	// Step 4 - Fill Hits
		
	cout << "Preparing to process and fill HIT information ....." << endl;
	
	StMcTpcHit* th = 0;
	StMcSvtHit* sh = 0;
	StMcFtpcHit* fh = 0;
	StMcRichHit* rh = 0;

	//
	// TPC Hits
	//
	
	long NHits = g2t_tpc_hitTablePointer->GetNRows();
	long iTrkId = 0;
	long nBadVolId = 0;
	long nPseudoPadrow = 0;
	long ihit;
	for(ihit=0; ihit<NHits; ihit++) {
	    if (tpcHitTable[ihit].volume_id < 101 || tpcHitTable[ihit].volume_id > 2445) {
		if (tpcHitTable[ihit].volume_id <= 202445 &&
		    tpcHitTable[ihit].volume_id > 2445) nPseudoPadrow++; 
		else nBadVolId++;
		continue;
	    }
	    
	    th = new StMcTpcHit(&tpcHitTable[ihit]);
	    
	    if(!mCurrentMcEvent->tpcHitCollection()->addHit(th)) // adds hit th to collection
		nBadVolId++;
	    // point hit to its parent and add it to collection
	    // of the appropriate track
	    
	    iTrkId = (tpcHitTable[ihit].track_p) - 1;
	    
	    th->setParentTrack(ttemp[iTrkId]);
	    ttemp[iTrkId]->addTpcHit(th);
	    
	}
	cout << "Filled " << mCurrentMcEvent->tpcHitCollection()->numberOfHits() << " TPC Hits" << endl;
	cout << "Found " << nPseudoPadrow << " Hits in Pseudo-Padrows." << endl;
	if (nBadVolId) gMessMgr->Warning() << "StMcEventMaker::Make(): cannot store " << nBadVolId
					   << " TPC hits, wrong Volume Id." << endm;
	// Sort the hits
	for (unsigned int iSector=0;
	     iSector<mCurrentMcEvent->tpcHitCollection()->numberOfSectors(); iSector++)
	    for (unsigned int iPadrow=0;
		 iPadrow<mCurrentMcEvent->tpcHitCollection()->sector(iSector)->numberOfPadrows();
		 iPadrow++) {
		StSPtrVecMcTpcHit& tpcHits = mCurrentMcEvent->tpcHitCollection()->sector(iSector)->padrow(iPadrow)->hits();
		sort (tpcHits.begin(), tpcHits.end(), compMcTpcHit() );
	        
	    }
	
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
		if (!mCurrentMcEvent->svtHitCollection()->addHit(sh)) // adds hit sh to collection
		    nBadVolId++; 
		
		// point hit to its parent and add it to collection
		// of the appropriate track
		
		iTrkId = (svtHitTable[ihit].track_p) - 1;
		sh->setParentTrack(ttemp[iTrkId]);
		ttemp[iTrkId]->addSvtHit(sh);
		
	    }
	    cout << "Filled " << mCurrentMcEvent->svtHitCollection()->numberOfHits() << " SVT Hits" << endl;
	    if (nBadVolId)
		gMessMgr->Warning() << "StMcEventMaker::Make(): cannot store " << nBadVolId
				    << " SVT hits, wrong Volume Id." << endm;
	    // Sort the hits
	    for (unsigned int iBarrel=0;
		 iBarrel<mCurrentMcEvent->svtHitCollection()->numberOfBarrels(); iBarrel++)
		for (unsigned int iLadder=0;
		     iLadder<mCurrentMcEvent->svtHitCollection()->barrel(iBarrel)->numberOfLadders();
		     iLadder++)
		    for (unsigned int iWafer=0;
			 iWafer<mCurrentMcEvent->svtHitCollection()->barrel(iBarrel)->ladder(iLadder)->numberOfWafers();
			 iWafer++) {
			StSPtrVecMcSvtHit& svtHits = mCurrentMcEvent->svtHitCollection()->barrel(iBarrel)->ladder(iLadder)->wafer(iWafer)->hits();
			sort (svtHits.begin(), svtHits.end(), compMcSvtHit() );
	        
	    }

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

		if (!mCurrentMcEvent->ftpcHitCollection()->addHit(fh)) // adds hit fh to collection
		    nBadVolId++;
		// point hit to its parent and add it to collection
		// of the appropriate track
		
		iTrkId = (ftpHitTable[ihit].track_p) - 1;
		fh->setParentTrack(ttemp[iTrkId]);
		ttemp[iTrkId]->addFtpcHit(fh);
		
	    }
	    cout << "Filled " << mCurrentMcEvent->ftpcHitCollection()->numberOfHits() << " FTPC Hits" << endl;
	    if (nBadVolId)
		gMessMgr->Warning() << "StMcEventMaker::Make(): cannot store " << nBadVolId
				    << " FTPC hits, wrong Volume Id." << endm;
	    // Sort the hits
	    for (unsigned int iPlane=0;
		 iPlane<mCurrentMcEvent->ftpcHitCollection()->numberOfPlanes(); iPlane++) {
		StSPtrVecMcFtpcHit& ftpcHits = mCurrentMcEvent->ftpcHitCollection()->plane(iPlane)->hits();
		sort (ftpcHits.begin(), ftpcHits.end(), compMcFtpcHit() );
	        
	    }
	}
	else {
	    cout << "No FTPC Hits in this file" << endl;
	}
	
	// RICH Hits
	if (g2t_rch_hitTablePointer) {
	    NHits = g2t_rch_hitTablePointer->GetNRows();
	    iTrkId = 0;
	    nBadVolId = 0;
	    for(ihit=0; ihit<NHits; ihit++) {
		if (rchHitTable[ihit].volume_id < 257 // 2^8 + 1 
		    || ftpHitTable[ihit].volume_id > 2560) { // 10*2^8
		    nBadVolId++;
		    continue;
		}

		rh = new StMcRichHit(&rchHitTable[ihit]);
		mCurrentMcEvent->richHitCollection()->addHit(rh); // adds hit rh to collection
		
		// point hit to its parent and add it to collection
		// of the appropriate track
		
		iTrkId = (rchHitTable[ihit].track_p) - 1;
		rh->setParentTrack(ttemp[iTrkId]);
		ttemp[iTrkId]->addRichHit(rh);
		
	    }
	    cout << "Filled " << mCurrentMcEvent->richHitCollection()->numberOfHits() << " RICH Hits" << endl;
	    if (nBadVolId)
		gMessMgr->Warning() << "StMcEventMaker::Make(): cannot store " << nBadVolId
				    << " RICH hits, wrong Volume Id." << endm;
	}
	else {
	    cout << "No RICH Hits in this file" << endl;
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
		    cout << "Dumping all the z coordinates in this padrow" << endl;
		    cout << "Should be sorted according to z: " << endl;
		    cout << "---------------------------------------------------------" << endl;
    		    for (StMcTpcHitIterator thi = tpcColl->sector(k)->padrow(j)->hits().begin();
			 thi!=tpcColl->sector(k)->padrow(j)->hits().end(); thi++)
			cout << (*thi)->position().z() << " ";
		    cout << endl;
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
    
    StMcRichHitCollection *richColl = mCurrentMcEvent->richHitCollection();
    cout << "---------------------------------------------------------" << endl;
    cout << "StMcRichHitCollection at " << (void*) richColl             << endl;
    cout << "Dumping collection size and one hit only."                 << endl;
    cout << "---------------------------------------------------------" << endl;
    nhits = richColl->numberOfHits();
    cout << "# of hits in collection = " << nhits << endl;
    if (richColl &&  nhits) {
	
	if (richColl->hits().size()) {
	    cout << "Rich Hit" << endl;
	    cout << *(richColl->hits()[0]);
	    cout << "Parent track of this Hit" << endl;
	    cout << *(richColl->hits()[0]->parentTrack()) << endl;
		
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
	for (k=1; !gotOneHit && k<=svtColl->numberOfBarrels(); k++)
	    for (j=1; !gotOneHit && j<=svtColl->barrel(k)->numberOfLadders(); j++)
		for (i=1; !gotOneHit && i<=svtColl->barrel(k)->ladder(j)->numberOfWafers(); i++)
		    if (svtColl->barrel(k)->ladder(j)->wafer(i)->hits().size()) {
			cout << "Svt Hit" << endl;
			cout << *(svtColl->barrel(k)->ladder(j)->wafer(i)->hits()[0]);
			cout << "Parent track of this Hit" << endl;
			cout << *(svtColl->barrel(k)->ladder(j)->wafer(i)->hits()[0]->parentTrack()) << endl;
			gotOneHit = kTRUE;
		    }
    }
        
    cout << endl;
}  

    
    

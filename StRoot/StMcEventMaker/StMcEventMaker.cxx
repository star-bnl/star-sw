/*************************************************
 *
 * $Id: StMcEventMaker.cxx,v 1.6 1999/09/23 21:25:59 calderon Exp $
 * $Log: StMcEventMaker.cxx,v $
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
#include "TStyle.h"
#include "TCanvas.h"
#include "StMcEventMaker.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"

#include "StThreeVectorF.hh"

#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"

#include "tables/g2t_event.h"
#include "tables/g2t_ftp_hit.h"
#include "tables/g2t_svt_hit.h"
#include "tables/g2t_tpc_hit.h"
#include "tables/g2t_track.h"
#include "tables/g2t_vertex.h"

#include "St_g2t_event_Table.h"
#include "St_g2t_ftp_hit_Table.h"
#include "St_g2t_svt_hit_Table.h"
#include "St_g2t_tpc_hit_Table.h"
#include "St_g2t_track_Table.h"
#include "St_g2t_vertex_Table.h"

// #include "StMcEvent/StMemoryInfo.hh"

#include "StMcEvent.hh"
#include "StMcTrack.hh"
#include "StMcTpcHit.hh"
#include "StMcFtpcHit.hh"
#include "StMcSvtHit.hh"
#include "StMcVertex.hh"

#ifndef ST_NO_NAMESPACES
using namespace std;
#endif

static double vertexCut = .0000025; // 25 nm (lifetime of the pi0)
struct vertexFlag {
	      StMcVertex* vtx;
	      int primaryFlag; };

// Here one gives values to data members that need them.
// So far the only data members I have are drawinit and currentMcEvent.
// but might be useful later on.  Look at St_QA_Maker.cxx file

ClassImp(StMcEventMaker)


//_____________________________________________________________________________

    
StMcEventMaker::StMcEventMaker(const char*name, const char * title):StMaker(name,title)
{
    // StMcEventMaker - constructor
    // - set all pointers defined in the header file to zero

    mCurrentMcEvent = 0; //! I think this tells root not to parse it 

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

void StMcEventMaker::PrintInfo() {
    // StMcEventMaker - PrintInfo,
    printf("**************************************************************\n");
    printf("* $Id: StMcEventMaker.cxx,v 1.6 1999/09/23 21:25:59 calderon Exp $\n");
    printf("**************************************************************\n");

   
    StMaker::PrintInfo();
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
    // StMcEventMaker - Init; book histograms if needed and set defaults for
    // member functions. For now I think I don't need anything but the default.
    cout << "******** StMcEventMaker::Init() " << endl;
    return StMaker::Init();
}

//_____________________________________________________________________________

Int_t StMcEventMaker::Make()
{

//   StMemoryInfo* info = StMemoryInfo::instance();
//   info->snapshot();
//   info->print();
  // StMcEventMaker - Make; fill StMcEvent objects
    
  cout << "Inside StMcEventMaker::Make()" << endl;
  // We're supposed to get the dataset from the chain. I don't know how yet. I think it is:

  
    St_DataSet* Event = GetDataSet("geant");
        
  // This is done only for one file, though.  I haven't put functionality for
  // multiple file handling.  If needed, it should start in the StMcEventReadMacro
  // and try to follow the doEvents.C macro to read in multiple files.
  // Then, we would need to mimic the nextRootFile() methods and so on.

  
  // Now we have the DataSet, but for some reason, we need the Iterator to navigate

  St_DataSetIter geantDstI(Event);
  
  // Now the Iterator is set up, and this allows us to access the tables
  // This is done like so:
  // TableClass *instanceOfTableClassPointer = cast to TableClassPointer instanceOfDataSetIter("actual name of table in data set");
  
  //St_g2t_event   *g2t_eventTablePointer   = (St_g2t_event *)   geantDstI("g2t_event");
  St_g2t_vertex  *g2t_vertexTablePointer  =  (St_g2t_vertex *)  geantDstI("g2t_vertex");
  St_g2t_track   *g2t_trackTablePointer   =  (St_g2t_track *)   geantDstI("g2t_track");
  St_g2t_tpc_hit *g2t_tpc_hitTablePointer =  (St_g2t_tpc_hit *) geantDstI("g2t_tpc_hit");
  St_g2t_svt_hit *g2t_svt_hitTablePointer =  (St_g2t_svt_hit *) geantDstI("g2t_svt_hit");
  St_g2t_ftp_hit *g2t_ftp_hitTablePointer =  (St_g2t_ftp_hit *) geantDstI("g2t_ftp_hit");

  // Now we check if we have the pointer, if we do, then we can access the tables!

   if (g2t_vertexTablePointer && g2t_trackTablePointer
       && g2t_tpc_hitTablePointer ){
       
       //g2t_event_st   *eventTable  = g2t_eventTablePointer->GetTable();
       g2t_vertex_st  *vertexTable = g2t_vertexTablePointer->GetTable();
       g2t_track_st   *trackTable  = g2t_trackTablePointer->GetTable();
       g2t_tpc_hit_st *tpcHitTable = g2t_tpc_hitTablePointer->GetTable();
       g2t_ftp_hit_st *ftpHitTable;
       if (g2t_ftp_hitTablePointer) ftpHitTable = g2t_ftp_hitTablePointer->GetTable();
       g2t_svt_hit_st *svtHitTable;
       if (g2t_svt_hitTablePointer) svtHitTable = g2t_svt_hitTablePointer->GetTable();
       
       
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

	  //mCurrentMcEvent = new StMcEvent((*eventTable));
	  
	  mCurrentMcEvent = new StMcEvent;
	  if (mCurrentMcEvent) cout << "****  Defined new StMcEvent" << endl;
	  
	  // Careful, the constructor for StMcEvent expects a reference,
	  // eventTable is a pointer. Is this OK?
	  // All the other StMc objects are initialized with a pointer to a table.

	  // However, the files don't seem to include the g2t_event table, so we won't use
	  // the table based constructor.

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
	  mCurrentMcEvent->vertexCollection()->push_back(v);
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
		      mCurrentMcEvent->vertexCollection()->push_back(v);  // adds vertex v to master collection
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
		      continue; // This gets you out of the loop
		      
		  }
	      }    
	      t = new StMcTrack(&(trackTable[itrk]));
	      ttemp[ trackTable[itrk].id - 1 ] = t; // This we do for all accepted tracks
	      
	      
	      mCurrentMcEvent->trackCollection()->push_back(t); // adds track t to master collection
	      
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
		  t->intermediateVertices()->push_back(vtemp[iItrmdVtxId].vtx);
		  
		  vtemp[iItrmdVtxId].vtx->setParent(t);
		  
		  // follow the "linked list" of g2t_vertex table to get the rest
		  
		  while(vertexTable[iItrmdVtxId].next_itrmd_p != 0)
		      {
			  iItrmdVtxId = (vertexTable[iItrmdVtxId].next_itrmd_p) - 1;
			  t->intermediateVertices()->push_back(vtemp[iItrmdVtxId].vtx);
			  vtemp[iItrmdVtxId].vtx->setParent(t);
		      };
		  
		  
	      }; // Intermediate vertices
	  
	  }; // Track loop
	  
	  //vtemp.clear();

	  //delete vtemp[];
	  //______________________________________________________________________
	  // Step 4 - Fill Hits

	  

	  cout << "Preparing to process and fill HITS information ....." << endl;
	  
	  StMcTpcHit* th = 0;
	  StMcSvtHit* sh = 0;
	  StMcFtpcHit* fh = 0;

	  // TPC Hits
	  //cout << "Trying to establish relationship btw Tpc Hit & Parent Track" << endl;
	  long NHits = g2t_tpc_hitTablePointer->GetNRows();
	  long iTrkId = 0;
	  for(long ihit=0; ihit<NHits; ihit++) {
	      if (tpcHitTable[ihit].volume_id < 101) {
		  
		  cout << "Bad Tpc Hit volume_id : " << tpcHitTable[ihit].volume_id << endl;
		  continue;
	      }
	      else if (tpcHitTable[ihit].volume_id > 2445) {
		  continue;
	      }
	      
	      th = new StMcTpcHit(&tpcHitTable[ihit]);
	      mCurrentMcEvent->tpcHitCollection()->push_back(th); // adds hit th to master collection
	      
	      // point hit to its parent and add it to collection
	      // of the appropriate track
	      
	      
	      
	      iTrkId = (tpcHitTable[ihit].track_p) - 1;
	      
	      th->setParentTrack(ttemp[iTrkId]);
	      ttemp[iTrkId]->addTpcHit(th);
	      
	  };
	  cout << "Filled TPC Hits" << endl;
	  

	  // SVT Hits
	  if (g2t_svt_hitTablePointer) {
	  NHits = g2t_svt_hitTablePointer->GetNRows();
	  iTrkId = 0;
	  for(ihit=0; ihit<NHits; ihit++) {
	      sh = new StMcSvtHit(&svtHitTable[ihit]);
	      mCurrentMcEvent->svtHitCollection()->push_back(sh); // adds hit sh to master collection
	      
	      // point hit to its parent and add it to collection
	      // of the appropriate track
	      
	      iTrkId = (svtHitTable[ihit].track_p) - 1;
	      sh->setParentTrack(ttemp[iTrkId]);
	      ttemp[iTrkId]->addSvtHit(sh);
	      
	  };
	  cout << "Filled SVT Hits" << endl;
	  }
	  else {
	      cout << "No SVT Hits in this file" << endl;
	      mCurrentMcEvent->svtHitCollection()->clear();
	  }
	  
	  // FTPC Hits
	  if (g2t_ftp_hitTablePointer) {
	  NHits = g2t_ftp_hitTablePointer->GetNRows();
	  iTrkId = 0;
	  for(ihit=0; ihit<NHits; ihit++) {
	      fh = new StMcFtpcHit(&ftpHitTable[ihit]);
	      mCurrentMcEvent->ftpcHitCollection()->push_back(fh); // adds hit fh to master collection
	      
	      // point hit to its parent and add it to collection
	      // of the appropriate track
	      
	      iTrkId = (ftpHitTable[ihit].track_p) - 1;
	      fh->setParentTrack(ttemp[iTrkId]);
	      ttemp[iTrkId]->addFtpcHit(fh);
	      
	  };
	  cout << "Filled FTPC Hits" << endl;
	  }
	  else {
	      cout << "No FTPC Hits in this file" << endl;
	      mCurrentMcEvent->ftpcHitCollection()->clear();
	  }
	  

	  ttemp.clear();

	  //_______________________________________________________________
	  // At this point StMcEvent should be loaded.

	  
	  cout << "Daughters of Primary Vertex : ";
	  cout << mCurrentMcEvent->primaryVertex()->numberOfDaughters() << endl;
	  cout << "Number of Vertices : " << mCurrentMcEvent->vertexCollection()->size() << endl;
	  cout << "Number of Thrown Vertices   : " << nThrownVertices << endl;
	  cout << "Number of Tracks   : " << mCurrentMcEvent->trackCollection()->size() << endl;
	  cout << "Number of Thrown Tracks  : " << nThrownTracks << endl;
	  cout << "Number of TPC Hits       : " << mCurrentMcEvent->tpcHitCollection()->size() << endl;
	  cout << "Number of SVT Hits       : " << mCurrentMcEvent->svtHitCollection()->size() << endl;
	  cout << "Number of FTPC Hits      : " << mCurrentMcEvent->ftpcHitCollection()->size() << endl;
	  
      }  
	
//   info->snapshot();
//   info->print();
  
  return kStOK;

}
    


    

    
    

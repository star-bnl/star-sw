// $Id: StEventMaker.cxx,v 1.15 1999/07/24 00:25:49 fisyak Exp $
// $Log: StEventMaker.cxx,v $
// Revision 1.15  1999/07/24 00:25:49  fisyak
// Gene corrections
//
// Revision 1.14  1999/07/18 22:49:45  perev
// Used StVecPtrVertex instead of StVertexCollection
//
// Revision 1.13  1999/07/15 13:57:02  perev
// cleanup
//
// Revision 1.11  1999/07/11 23:27:49  fisyak
// dst_TriggerDetectors => dst_TrgDet
//
// Revision 1.10  1999/07/09 01:17:54  fisyak
// clean up
//
// Revision 1.9  1999/06/27 22:45:28  fisyak
// Merge StRootEvent and StEvent
//
// Revision 1.7  1999/06/24 17:31:22  fisyak
// Add protection for hits which do not belongs to any tracks
//
// Revision 1.6  1999/06/11 17:43:57  perev
// remove StRun from .const
//
// Revision 1.5  1999/05/22 17:59:01  perev
// Can read also mdc2 and last format
//
// Revision 1.4  1999/05/10 16:53:12  fisyak
// Rename reader
//
// Revision 1.3  1999/05/10 13:09:46  fisyak
// Fix for HP
//
// Revision 1.2  1999/05/05 22:38:00  fisyak
// Add gulfd for magnetic field
//
// Revision 1.1  1999/05/04 22:40:35  fisyak
// Initial revision of persistent StEventMaker
//
// Revision 1.12  1999/05/03 01:39:22  fisyak
// Remove tables from DST, add access to different makers
//
// Revision 1.11  1999/05/01 01:49:15  fisyak
// Add StRootEvent fill
//
// Revision 1.10  1999/05/01 00:57:03  fisyak
// Change Clear function to defualt
//
// Revision 1.19  1999/03/30 15:47:30  wenaus
// update to new maker scheme
//
// Revision 1.18  1999/03/19 17:53:20  wenaus
// don't fill empty CTB,MWC bins
//
// Revision 1.17  1999/03/19 17:35:33  wenaus
// incorporate tolerance of missing start, stop vertex from Joakim Nystrand
//
// Revision 1.16  1999/03/19 17:23:12  wenaus
// Load trigger detectors
//
// Revision 1.15  1999/03/01 01:24:08  genevb
// Assign primary vertex, get start and stop vertices right for tracks
//
// Revision 1.14  1999/02/25 22:36:30  wenaus
// corrected FTPC hit IDs
//
// Revision 1.13  1999/02/25 17:08:54  wenaus
// temporary suppression of unknown ID messages
//
// Revision 1.12  1999/02/24 23:13:45  wenaus
// gentler EOF message
//
// Revision 1.11  1999/02/24 18:30:30  wenaus
// dst_tof elimination; changes for ROOT
//
//
// Revision 1.10  1999/02/24 12:51:38  ullrich
// Modified calculation of helix parameters of global tracks.
// New argument passed to StGlobalTrack constructor.
//
// Revision 1.9  1999/02/24 01:56:13  genevb
// Add Xi vertices
//
// Revision 1.8  1999/02/22 20:52:18  wenaus
// clean up delete handling
//
// Revision 1.7  1999/02/22 20:26:24  genevb
// Temporary fix of vertex indexing
//
// Revision 1.6  1999/02/21 20:33:05  genevb
// Improve StV0Vertex code
//
// Revision 1.5  1999/02/12 02:00:06  wenaus
// load tracks using new constructor
//
// Revision 1.4  1999/02/10 23:15:19  wenaus
// multi-file processing changes; leak fixes
//
// Revision 1.3  1999/02/10 15:33:25  wenaus
// Speed up vtx->trk ref loading. Tested on Solaris, Linux
//
// Revision 1.2  1999/02/05 17:51:54  wenaus
// Vertex loading via index() method
//
// Revision 1.1  1999/01/30 23:06:37  wenaus
// Maker to read from tables or Objy into StEvent
//
//
///////////////////////////////////////////////////////////////////////////////
//
// StEventMaker
//
// Description: 
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Torre Wenaus, BNL
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
// $Id: StEventMaker.cxx,v 1.15 1999/07/24 00:25:49 fisyak Exp $
// $Log: StEventMaker.cxx,v $
// Revision 1.15  1999/07/24 00:25:49  fisyak
// Gene corrections
//
// Revision 1.14  1999/07/18 22:49:45  perev
// Used StVecPtrVertex instead of StVertexCollection
//
// Revision 1.13  1999/07/15 13:57:02  perev
// cleanup
//
// Revision 1.11  1999/07/11 23:27:49  fisyak
// dst_TriggerDetectors => dst_TrgDet
//
// Revision 1.10  1999/07/09 01:17:54  fisyak
// clean up
//
// Revision 1.9  1999/06/27 22:45:28  fisyak
// Merge StRootEvent and StEvent
//
// Revision 1.7  1999/06/24 17:31:22  fisyak
// Add protection for hits which do not belongs to any tracks
//
// Revision 1.6  1999/06/11 17:43:57  perev
// remove StRun from .const
//
// Revision 1.5  1999/05/22 17:59:01  perev
// Can read also mdc2 and last format
//
// Revision 1.4  1999/05/10 16:53:12  fisyak
// Rename reader
//
// Revision 1.3  1999/05/10 13:09:46  fisyak
// Fix for HP
//
// Revision 1.2  1999/05/05 22:38:00  fisyak
// Add gulfd for magnetic field
//
// Revision 1.1  1999/05/04 22:40:35  fisyak
// Initial revision of persistent StEventMaker
//
// Revision 1.12  1999/05/03 01:39:22  fisyak
// Remove tables from DST, add access to different makers
//
// Revision 1.11  1999/05/01 01:49:15  fisyak
// Add StRootEvent fill
//
// Revision 1.10  1999/05/01 00:57:03  fisyak
// Change Clear function to defualt
//
// Revision 1.9  1999/03/11 03:12:17  perev
// new schema
//
// Revision 1.8  1999/02/26 02:31:55  fisyak
// Replace emc hits by emc raw tables
//
// Revision 1.7  1999/02/23 02:09:02  fisyak
// Add emc hits to dst
//
// Revision 1.6  1999/02/20 18:49:16  fisyak
// Add event/run information
//
// Revision 1.5  1999/02/19 17:37:42  fisyak
// Add RICH hits to dst
//
// Revision 1.4  1999/02/19 17:35:47  fisyak
// Add RICH hits to dst
//
// Revision 1.2  1999/01/20 23:58:03  fisyak
// Tree 2 GetTree
//
// Revision 1.1  1999/01/02 19:09:22  fisyak
// Add Clones
//
// Revision 1.7  1998/10/31 00:25:45  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:29  perev
// cleanup
//
// Revision 1.5  1998/10/02 13:46:08  fine
// DataSet->DataSetIter
//
// Revision 1.4  1998/08/14 15:25:58  fisyak
// add options
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEventMaker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

 * no dst dataset is available.
 *
 *
 * Revision 2.24  2000/05/24 15:48:15  ullrich
 * Instance of StEvent now also created if no DST dataset
 * is available.

#ifndef TRANSIENT_STEVENT
  #include "StThreeVectorD.hh"
  #include "StThreeVectorF.hh"
  #include "StRun.h"
  #include "StEvent.h"
  #include "StGlobalTrack.h"
  #include "StDedx.h"
  #include "StTpcHit.h"
  #include "StFtpcHit.h"
  #include "StSvtHit.h"
  #include "StHit.h"
  #include "StVertex.h"
  #include "StV0Vertex.h"
  #include "StXiVertex.h"
#else
  #include "StEvent/StRun.hh"
  #include "StEvent/StEvent.hh"
  #include "StEvent/StGlobalTrack.hh"
  #include "StEvent/StDedx.hh"
  #include "StEvent/StTpcHit.hh"
  #include "StEvent/StFtpcHit.hh"
  #include "StEvent/StSvtHit.hh"
  #include "StEvent/StHit.hh"
  #include "StEvent/StVertex.hh"
  #include "StEvent/StV0Vertex.hh"
  #include "StEvent/StXiVertex.hh"
  typedef StThreeVector<float> StThreeVectorF;
  typedef StThreeVector<double> StThreeVectorD;
#endif
#include "StEventMaker/StRootEventManager.hh"
#include <new.h>
static const char thisClass[] = "StEventMaker: ";

static const char rcsid[] = "$Id: StEventMaker.cxx,v 1.15 1999/07/24 00:25:49 fisyak Exp $";
#include "StEventManager.hh"
 * Revision 2.23  2000/05/22 21:53:41  ullrich
#include <vector>
#ifdef ST_NO_TEMPLATE_DEF_ARGS
// Syntax currently required by Solaris compiler
 * to StEvent. printEventInfo() and makeEvent() modified.
 *
 * Revision 2.22  2000/04/26 20:29:13  ullrich
 * Create instance of StEvent not StBrowsableEvent.
 * Added further checks for case were tables exist but have
const long detid_tpc = 1;
const long detid_svt = 2;
const long detid_ftpcWest = 4;
const long detid_ftpcEast = 5;
#include "fortranc.h"
extern "C" {void type_of_call F77_NAME(gufld,GUFLD)(float *x, float *b);}
#define gufld F77_NAME(gufld,GUFLD)
#include "StTrack.h"
 * zero length. Added for primary and global tracks.
#include "St_ObjectSet.h"
  //_____________________________________________________________________________
  StEventMaker::StEventMaker(const char *name,const char *titl):StMaker(name){
  if(titl) SetTitle(titl);
  setEventManager(new StRootEventManager());
  theEventManager = new StRootEventManager();
  theEventManager->setMaker(this);
  currentRun = 0;
  currentEvent = 0;

//		create empty objects
  defRunHeader		= new dst_run_header_st   ();	
  defEventHeader	= new dst_event_header_st ();	
  defEventSummary	= new dst_event_summary_st();	
  defMonitorHard	= new dst_monitor_hard_st ();	
  defMonitorSoft	= new dst_monitor_soft_st ();	
  defTriggerDetectors	= new dst_TrgDet_st();	
//		...and fill it by zeros
  memset(defRunHeader	    ,0,sizeof(dst_run_header_st      ));	
  memset(defEventHeader	    ,0,sizeof(dst_event_header_st    ));	
  memset(defEventSummary    ,0,sizeof(dst_event_summary_st   ));	
  memset(defMonitorHard	    ,0,sizeof(dst_monitor_hard_st    ));	
  memset(defMonitorSoft	    ,0,sizeof(dst_monitor_soft_st    ));	
  memset(defTriggerDetectors,0,sizeof(dst_TrgDet_st));	
//
 * Revision 2.17  2000/02/08 21:14:16  genevb
    doLoadTpcHits  = kTRUE;
  doLoad=kTRUE;
    doPrintEventInfo  = kTRUE;  // TMP, set to fFALSE later
//_____________________________________________________________________________
StEventMaker::~StEventMaker()
    doPrintEventInfo  = kFALSE;
 *
 * Revision 2.7  1999/11/17 14:10:27  ullrich
//_____________________________________________________________________________
Int_t StEventMaker::Init(){
  // Create Histograms    
  return StMaker::Init();
    doPrintRunInfo    = kTRUE;  // TMP 
//_____________________________________________________________________________
Int_t StEventMaker::Make(){
  long nrows;
  int i;

  status = theEventManager->openEvent("dst");
  if (!status) return kStOK; 
  dstRunHeader = theEventManager->returnTable_dst_run_header (nrows);
  if (!dstRunHeader) dstRunHeader = defRunHeader;

  if (dstRunHeader!=defRunHeader) {
     gMessMgr->Info() << thisClass << "Run header: ID " <<
       dstRunHeader->run_id << endm;
    /* run summary not used
       dstRunsummary_st* dstRunSummary = theEventManager->returnTable_dst_run_summary (nrows);
       delete currentRun;
       if (dstRunSummary) {
       gMessMgr->Info() << thisClass << "Run summary found" << endm;
       // Create transient run header
       currentRun = new StRun(*dstRunHeader, *dstRunSummary);
       } else {
       currentRun = new StRun(*dstRunHeader);
       }
    */
  }
  if (doLoad) {
    currentRun = new StRun(*dstRunHeader);
  }
 * Revised to build new StEvent version
  
  gMessMgr->Info() << thisClass << "Reading Event" << endm;
  dstEventHeader = theEventManager->returnTable_dst_event_header(nrows);
  if(!dstEventHeader) dstEventHeader=defEventHeader;

  dstEventSummary = theEventManager->returnTable_dst_event_summary(nrows);
  if(!dstEventSummary) dstEventSummary = defEventSummary;

  // Create transient event header
  if (doLoad ) {
    currentEvent = new StEvent(currentRun,
                               *dstEventHeader,
                               *dstEventSummary);
#ifndef TRANSIENT_STEVENT
    AddData(currentEvent);
#endif
  }//endif doLoad
  
    dstMonitorHard = theEventManager->returnTable_dst_monitor_hard(nrows);
    if (dstMonitorHard) 
         gMessMgr->Info() << thisClass << "Found dstMonitorHard" << endm;
    else dstMonitorHard = defMonitorHard;

    dstMonitorSoft = theEventManager->returnTable_dst_monitor_soft(nrows);
    if (dstMonitorSoft) 
         gMessMgr->Info() << thisClass << "Found dstMonitorSoft" << endm;
    else dstMonitorSoft = defMonitorSoft;
  
// 		Read and load trigger detector data
        else
  status =   theEventManager->openEvent("trg");
	theEventManager->closeEvent();
  if (status) {
    dstTriggerDetectors = theEventManager->returnTable_dst_TrgDet(nrows); 
    if (dstTriggerDetectors){
      gMessMgr->Info() << thisClass << "Loading triggerDetectors" << endm;
      StTriggerDetectorCollection *trgDets =
	currentEvent->triggerDetectorCollection();
      // Load CTB data
      StCtbCounter* ctb;
      for (i=0; i<240; i++) {
	if (dstTriggerDetectors->nCtb[i] > 0) {
	  ctb = new StCtbCounter( i, 
				  dstTriggerDetectors->nCtb[i],
				  dstTriggerDetectors->timeCtb[i]);
	  trgDets->ctbCounters().push_back(*ctb);
	}
      }
      // Load MWC data
      StMwcSector* mwc;
      for (i=0; i<96; i++) {
	if (dstTriggerDetectors->nMwc[i] > 0) {
	  mwc = new StMwcSector( i, dstTriggerDetectors->nMwc[i]);
	  trgDets->mwcSectors().push_back(*mwc);
	}
      }
      // Load VPD data
      StVpdCounter* vpd;
      for (i=0; i<48; i++) {
	// No filling code exists, so no criteria to ignore empty bins
	vpd = new StVpdCounter( i,
				dstTriggerDetectors->adcVPD[i],
				dstTriggerDetectors->timeVPD[i]);
	trgDets->vpdCounters().push_back(*vpd);
      }
      trgDets->vpdSummary().setVertexZ(dstTriggerDetectors->vertexZ);
      trgDets->vpdSummary().setMinimumTime(east,dstTriggerDetectors->TimeEastVpd);
      trgDets->vpdSummary().setMinimumTime(west,dstTriggerDetectors->TimeWestVpd);
      // Load ZDC data
      StZdcSegment* zdc;
      for (i=0; i<6; i++) {
	// No filling code exists, so no criteria to ignore empty bins
	zdc = new StZdcSegment( i,
				dstTriggerDetectors->adcZDC[i],
				dstTriggerDetectors->tdcZDC[i]);
	trgDets->zdcSegments().push_back(*zdc);
      }
      trgDets->zdcSummary().setAdcSum(dstTriggerDetectors->adcZDCsum);
      trgDets->zdcSummary().setAdcSum(east,dstTriggerDetectors->adcZDCEast);
      trgDets->zdcSummary().setAdcSum(west,dstTriggerDetectors->adcZDCWest);
    }
  } // end of Trigger part

  status =   theEventManager->openEvent("geant");
    if (status) {
      particle = theEventManager->returnTable_particle(nrows); 
      if (particle) {      
/* skip particle
	// load genHeader table from particle
	genHeader.bimp = particle.phep[0];
	genHeader.phi = particle.phep[1];
	genHeader.genid = particle.phep[2];
	genHeader.ecms = particle.phep[3];
	int aWest = int(particle.phep[4]);
	int aEast = (particle.phep[4]-aWest)*1000;
	genHeader.awest = aWest;
	genHeader.aeast = aEast;
	genHeader.run = particle.vhep[0];
	genHeader.event = particle.vhep[1];
	genHeader.date = particle.vhep[2];
	genHeader.time = particle.vhep[3];
	objyEventManager->loadTable(&genHeader);
*/
        return kStWarn;
  }
  
  status = theEventManager->openEvent("dst");
  if (status) {
        mCurrentEvent = new StBrowsableEvent(*dstEventHeader, *dstEventSummary, *mDstSummaryParam);
// 		Load and create tracks, vertices etc. and add to collections
    long nDedx, nPoint, nTrack, nTrackAux, nVertex, nV0Vertex, nXiVertex;
    long nTofTrk, nTofEvt;
    
    // *** BEGIN VERTEX BUILDING ***
    // build a vector of vertex addresses to use in lieu of an index
    // during vertex->track pointer loading. Assumes (requires!) that
    // vertices are ordered
    
    // First, find the total number of vertices and DST tracks
    dst_vertex_st* dstVertex = theEventManager->returnTable_dst_vertex(nVertex);
    dst_v0_vertex_st* dstV0Vertex = theEventManager->returnTable_dst_v0_vertex(nV0Vertex);
    dst_xi_vertex_st* dstXiVertex = theEventManager->returnTable_dst_xi_vertex(nXiVertex);
    dst_track_st* dstTrack = theEventManager->returnTable_dst_track(nTrack);


    // Second, set up structures used in making relationships
    StVecPtrVertex vtxPtr;
    int indexCount = 0;                  // count vertices added to collection
    typedef StVector(int) intVector;

    intVector vertexMatchIndex;
    if (nVertex) vertexMatchIndex.resize(nVertex,-1);

    StVector(intVector) v0XiIndex;
    if (nVertex) v0XiIndex.resize(nVertex);

    StVector(intVector) trackVertexIndex;
    if (nTrack) trackVertexIndex.resize(nTrack);


    // Third, deal with xi's
    if (nXiVertex > nVertex)
      gMessMgr->Warning() << thisClass << "more Xi's than vertices" << endm;
    if (dstXiVertex) {
      gMessMgr->Info() << thisClass << nXiVertex << " dst_xi_vertex" << endm;
      // Add Xi vertices to vertex collection
      if (doLoad) {
        StVertex* vtx = NULL;
        long vertex_id;
        long v0vertex_id;
        long track_id;
        for (i=0; i<nXiVertex; i++) {
          vertex_id = dstXiVertex[i].id_xi - 1;
          v0vertex_id = dstXiVertex[i].id_v0 - 1;
          track_id = dstXiVertex[i].id_b - 1;
          if (vertex_id >= nVertex)
            gMessMgr->Warning() << thisClass <<
               "Xi associated vertex index too large: " << ++vertex_id << endm;
          else vertexMatchIndex[vertex_id] = indexCount;
          if (v0vertex_id >= nVertex)
            gMessMgr->Warning() << thisClass <<
              "Xi associated V0 index too large: " << ++v0vertex_id << endm;
          else v0XiIndex[v0vertex_id].push_back(indexCount);
          if (track_id >= nTrack)
            gMessMgr->Warning() << thisClass <<
              "Xi associated track index too large: " << ++track_id << endm;
          else trackVertexIndex[track_id].push_back(indexCount);
          vtx = (StVertex*) new StXiVertex(&(dstXiVertex[i]), &(dstVertex[vertex_id]) );
          // Add associated v0's during v0 loop
          vtx->setIndex(indexCount++);
          currentEvent->vertexCollection()->push_back(vtx);
          vtxPtr.push_back(vtx);
    
      }
        StXiVertex *xi = new StXiVertex(dstVertices[id], dstXiVertices[i]);
        id = dstXiVertices[i].id_v0 - 1;
        if (id < v0Vertices.size()) xi->setV0Vertex(v0Vertices[id]);
    // Fourth, deal with v0's
    if (nV0Vertex > nVertex)
      gMessMgr->Warning() << thisClass << "more V0's than vertices" << endm;
    if (dstV0Vertex) {
      gMessMgr->Info() << thisClass << nV0Vertex << " dst_v0_vertex" << endm;
      // Add V0 vertices to vertex collection
      if (doLoad) {
        StVertex* vtx = NULL;
        long vertex_id;
        long track_id;
        long xi_id;
        for (i=0; i<nV0Vertex; i++) {
          vertex_id = dstV0Vertex[i].id_vertex - 1;
          track_id = dstV0Vertex[i].idneg - 1;
          if (vertex_id >= nVertex)
            gMessMgr->Warning() << thisClass <<
              "V0 associated vertex index too large: " << ++vertex_id << endm;
          else vertexMatchIndex[vertex_id] = indexCount;
          if (track_id >= nTrack)
            gMessMgr->Warning() << thisClass <<
              "V0 associated track index too large: " << ++track_id << endm;
          else trackVertexIndex[track_id].push_back(indexCount);
          track_id = dstV0Vertex[i].idpos - 1;
          if (track_id >= nTrack)
            gMessMgr->Warning() << thisClass <<
              "V0 associated track index too large: " << ++track_id << endm;
          else trackVertexIndex[track_id].push_back(indexCount);
          vtx = (StVertex*) new StV0Vertex(&(dstV0Vertex[i]), &(dstVertex[vertex_id]) );
          vtx->setIndex(indexCount++);
          currentEvent->vertexCollection()->push_back(vtx);
          vtxPtr.push_back(vtx);
          // Add associated v0's to xi's
          while (!v0XiIndex[vertex_id].empty()) {
            xi_id = v0XiIndex[vertex_id].back();
            v0XiIndex[vertex_id].pop_back();
            ((StXiVertex*) (vtxPtr[xi_id]))->setV0Vertex((StV0Vertex*) vtx);
          }
        if (id < vecGlobalTracks.size()) kink->addDaughter(vecGlobalTracks[id]);
      }
    }  
        id = dstKinkVertices[i].idp;
    if (nfailed)
    // Last for the vertices, add any un-accounted-for vertices (primary)
    if (dstVertex) {
      gMessMgr->Info() << thisClass << nVertex << " dst_vertex" << endm;
      // Build vertex collection
      if (doLoad) {
        StVertex* vtx = NULL;
        for (i=0; i<nVertex; i++) {
          if (vertexMatchIndex[i]<0) {
            vertexMatchIndex[i] = indexCount;
            vtx = new StVertex(&(dstVertex[i]) );
            vtx->setIndex(indexCount++);
            currentEvent->vertexCollection()->push_back(vtx);
            vtxPtr.push_back(vtx);
          }
                buf += sizeof(StTpcHit);
        // Last remaining vertex is primary? Should be...
        if (vtx) currentEvent->setPrimaryVertex(vtx);
      }
    }
    // *** END VERTEX BUILDING ***


    // dstTrack table loaded before vertex portion above
    if (dstTrack) {
      gMessMgr->Info() << thisClass << nTrack << " dst_track" << endm;
      if (doLoad) {
	StGlobalTrack* trk = NULL;
	for (i=0; i<nTrack; i++, dstTrack++) {
	  // Extract fit params: curv, dip, phase, origin
	  dst_track_st& tktbl = *dstTrack;
	  // $$$ get field from somewhere!
	  Float_t x[3] = {0,0,0};
	  Float_t b[3];
	  gufld(x,b);
	  double B     = b[2]*kilogauss;
	  //
	  //   Derive the helix parameter from the variables
	  //   stored in the table.
	  //
	  double dip   = atan(tktbl.tanl);
	  int    h     = (B*tktbl.icharge > 0 ? -1 : 1);  // -sign(q*B)
	  double phase = tktbl.psi*degree-h*pi/2;
	  double pt    = (1./tktbl.invpt)*GeV;
	  double curvature = fabs(c_light*nanosecond/meter*tktbl.icharge*B/tesla)/(pt/GeV); // in meter^-1	
	  StThreeVectorD origin(tktbl.x0, tktbl.y0, tktbl.z0);  // in centimeter
	  
	  //   Create the track, pass the helix parameter (note h)
	  trk = new StGlobalTrack(dstTrack,
				  curvature/meter,
				  dip*radian,
				  phase*radian,
				  origin, //*centimeter),
				  h);
          trk->setLength(dstTrack->length);
	  currentEvent->trackCollection()->push_back(trk);
	  // add the track to vertex
          long track_id = dstTrack->id - 1;
	  unsigned long idStartVertex = dstTrack->id_start_vertex;
	  unsigned long idStopVertex = dstTrack->id_stop_vertex;
          StVertex* startVertex = 0;
          StVertex* stopVertex = 0;

          // Load the appropriate vertex info using the vertex pointer vector

          // Set track Start Vertex
          // For now, if start vertex id is zero, assume the primary vertex
          if ( idStartVertex >= 0 && idStartVertex <= vtxPtr.size() ) {
            if ( idStartVertex ) {
              idStartVertex = vertexMatchIndex[--idStartVertex];
              startVertex = vtxPtr[idStartVertex];
            } else {
              startVertex = currentEvent->primaryVertex();
              idStartVertex = startVertex->index();
                            << " Xi vertices, invalid foreign key to vertex table." << endm;
            trk->setStartVertex(startVertex);
            // Add track to start vertex daughters
            startVertex->daughters().push_back(trk);
            // Check for any other vertices of which this may be a daughter
            long vertex_id;
            while (!(trackVertexIndex[track_id].empty())) {
              vertex_id = trackVertexIndex[track_id].back();
              trackVertexIndex[track_id].pop_back();
              if (vertex_id != idStartVertex) {
                startVertex = vtxPtr[vertex_id];
                startVertex->daughters().push_back(trk);
              }
			info->addHit(tpcHit);
          } else {
            gMessMgr->Warning() << thisClass << "idStartVertex " << idStartVertex <<
              " either negative or larger than vertex list " << vtxPtr.size() << endm;
          }

          // Set track Stop Vertex
          // For now, if stop vertex id is zero, do not set
          if ( idStopVertex >= 0 && idStopVertex <= vtxPtr.size() ) {
            if ( idStopVertex ) {
              idStopVertex  = vertexMatchIndex[--idStopVertex];
              stopVertex  = vtxPtr[idStopVertex];
              trk->setStopVertex(stopVertex);
              // Set track to stop vertex parent
              stopVertex->setParent(trk);
		else
          } else {
            gMessMgr->Warning() << thisClass << "idStopVertex " << idStopVertex <<
              " either negative or larger than vertex list " << vtxPtr.size() << endm;
          }
	}
      }
    }  
    
    dst_track_aux_st* dstTrackAux = theEventManager->returnTable_dst_track_aux(nTrackAux);
    if (dstTrackAux) {
      gMessMgr->Info() << thisClass << nTrackAux << " dst_track_aux" << endm;
      // Add auxiliary info to tracks
      if (doLoad) {
	StGlobalTrack* theTrack;
	StTrackCollection* theTrackCollection = currentEvent->trackCollection();
	long itrk;
	for (i=0; i<nTrackAux; i++) {
	  dst_track_aux_st* theTrackAux = &dstTrackAux[i];
	  itrk = theTrackAux->id_track-1;
	  // theTrack = (*theTrackCollection)[itrk]; // id_track isn't filled, apparently
	  theTrack = (*theTrackCollection)[i]; // Less safe, but with no ID, what else can we do
	  // Load auxiliary info into track
	  if (theTrack) {
	    // $$$ where to put resids. They aren't loaded at present either.
	    //        = dstTrackAux[i].residuals[0];
	    //        = dstTrackAux[i].residuals[1];

const static int iTab[]={1,2, 1,3, 2,3, 1,4, 2,4, 3,4, 1,5, 2,5, 3,5, 4,5, 0};
            for (const int *jTab=iTab; jTab[0]; jTab+=2) {
              double qwe = dstTrackAux[i].covar_off_diag[0];
	      theTrack->fitTraits().covariantMatrix()(jTab[0],jTab[1]) = qwe; 
	      theTrack->fitTraits().covariantMatrix()(jTab[1],jTab[0]) = qwe;}
	  } else {
            gMessMgr->Error() << thisClass <<
              "Track find failed for ID " << itrk << endm;
	  }
	}
      }
    }    
    
    dst_dedx_st* dstDedx = theEventManager->returnTable_dst_dedx(nDedx);
    if (dstDedx) {
      gMessMgr->Info() << thisClass << nDedx << " dst_dedx" << endm;
      // Add dedx info to tracks
      if (doLoad) {
	StDedx* dedx = NULL;
	long itrk, idet;
	StGlobalTrack* theTrack;
	StTrackCollection* theTrackCollection = currentEvent->trackCollection();
	for (i=0; i<nDedx; i++) {
	  dst_dedx_st* theDedx = &dstDedx[i];
	  dedx = new StDedx(theDedx);
	  dedx->setNumberOfPointsUsed(theDedx->ndedx);
	  dedx->setMean(theDedx->dedx[0]);
	  dedx->setVariance(theDedx->dedx[1]);
	  dedx->setStatus(theDedx->iflag);
	  itrk = theDedx->id_track-1;
	  theTrack = (*theTrackCollection)[itrk];
	  if (theTrack) {
	    idet = theDedx->det_id;
	    if (idet == detid_tpc) {
	      theTrack->setTpcDedx(dedx);
	    } else if (idet == detid_ftpcWest || idet == detid_ftpcEast) {
	      theTrack->setFtpcDedx(dedx);
	    } else if (idet == detid_svt) {
	      theTrack->setSvtDedx(dedx);
	    } else {
              gMessMgr->Error() << thisClass <<
	        "ERROR: Dedx: Det ID " << idet << " not recognized" << endm;
	      delete dedx;
	      dedx=0;
	    }
	  } else {
            gMessMgr->Error() << thisClass <<
              "Track find failed for ID " << itrk << endm;
	  }
	}
      }
    }    
    
    dst_tof_evt_st* dstTofEvt = theEventManager->returnTable_dst_tof_evt(nTofEvt);
    if (dstTofEvt) {
      if (doLoad) {
      }
    }    
    
    dst_tof_trk_st* dstTofTrk = theEventManager->returnTable_dst_tof_trk(nTofTrk);
    if (dstTofTrk) {
      gMessMgr->Info() << thisClass << nTofTrk << " dst_tof_trk" << endm;
      if (doLoad) {
      }
    }    
    
    dst_point_st* dstPoint = theEventManager->returnTable_dst_point(nPoint);
    dst_point_st* thePoint = NULL;  
    if (dstPoint) {
      gMessMgr->Info() << thisClass << nPoint << " dst_point" << endm;
      if (doLoad) {
	StTpcHit* tpcHit = NULL;
	StFtpcHit* ftpcHit = NULL;
	StSvtHit* svtHit = NULL;
	long idet;
	for (i=0; i<nPoint; i++) {
	  thePoint = &(dstPoint[i]);
	  long itrk = thePoint->id_track-1;
	  StTrackCollection* theTrackCollection = currentEvent->trackCollection();
	  StGlobalTrack* theTrack = 0;
	  if (itrk >=0) theTrack = (*theTrackCollection)[itrk];



	      // Handle point depending on what detector it comes from
	  idet = thePoint->hw_position%16;
	  if (idet == detid_tpc) {
	    tpcHit = new StTpcHit(thePoint);
	    currentEvent->tpcHitCollection()->push_back(tpcHit);
	    if (theTrack) theTrack->addTpcHit(tpcHit);
	  }
	  else if (idet == detid_svt) {
	    svtHit = new StSvtHit(thePoint);
	    currentEvent->svtHitCollection()->push_back(svtHit);
	    if (theTrack) theTrack->addSvtHit(svtHit);
	  }
	  else if (idet == detid_ftpcWest || idet == detid_ftpcEast ) {
	    ftpcHit = new StFtpcHit(thePoint);
	    currentEvent->ftpcHitCollection()->push_back(ftpcHit);
	    if (theTrack) theTrack->addFtpcHit(ftpcHit);
	  }
	  else {
            gMessMgr->Error() << thisClass <<
              "Detector ID " << idet << " not known" << endm;
	  }
	}
      }
    }    
  }
  theEventManager->closeEvent();
  return kStOK;
}
void StEventMaker::Clear(const char*)
{ 
#ifdef TRANSIENT_STEVENT
  delete currentEvent;
#endif
  currentEvent=0;
  StMaker::Clear();
}
void StEventMaker::setEventManager(StEventManager* mgr)
{
  theEventManager = mgr;
                    id = dstPoints[i].id_track;
//_____________________________________________________________________________
                ftpcHitColl->addHit(ftpcHit);
                id = dstPoints[i].id_track;
                if (id < vecGlobalTracks.size() && vecGlobalTracks[id])
                    vecGlobalTracks[id]->detectorInfo()->addHit(ftpcHit);
                else if (id < vecPrimaryTracks.size() && vecPrimaryTracks[id])
                    vecPrimaryTracks[id]->detectorInfo()->addHit(ftpcHit);
        nhits = tpcColl->numberOfHits();
        cout << "# of hits in collection = " << nhits << endl;
        gotOneHit = kFALSE;
	for (k=0; !gotOneHit && k<svtColl->numberOfLayers(); k++)
	    for (j=0; !gotOneHit && j<svtColl->layer(k)->numberOfLadders(); j++)
		for (i=0; !gotOneHit && i<svtColl->layer(k)->ladder(j)->numberOfWafers(); i++)
		    if (svtColl->layer(k)->ladder(j)->wafer(i)->hits().size()) {
	gotOneHit = kFALSE;
	for (k=0; !gotOneHit && k<svtColl->numberOfBarrels(); k++)
	track->geometry()->Dump();
		    if (svtColl->barrel(k)->ladder(j)->wafer(i)->hits().size()) {
			svtColl->barrel(k)->ladder(j)->wafer(i)->hits()[0]->Dump();
	track->detectorInfo()->Dump();
    if (svtColl) {
        nhits = svtColl->numberOfHits();
	track->node()->Dump();
	cout << "collection size = " << richPixels->size() << endl;
	richPixels->Dump();
	
	if (richPixels->size()) {
	    cout << "---------------------------------------------------------" << endl;
	    cout << "StRichPixel (generated from first word in collection)    " << endl;
	    cout << "---------------------------------------------------------" << endl;
	    richPixels->pixel(0).Dump();
	}	
    }    
        if (richPixels->size()) {
            cout << "---------------------------------------------------------" << endl;

    //
    //   Info from some tables for comparisons.
    //   Only tables with varying # of rows are listed.
    //
    cout << "*********************************************************" << endl;
    cout << "*                   Table Information                   *" << endl;
	 << ") at " << (void*) track                                    << endl;
    cout << "globtrk:    ";
	track->Dump();
    cout << "tpt:        ";
	
	cout << "---> StTrack -> StGeometry ("<< track->geometry()->GetName()
	     << ") at " << (void*) (track->geometry()) << endl;
	if (track->geometry()) track->geometry()->Dump();

	cout << "---> StTrack -> StDetectorInfo at "
	     << (void*) (track->detectorInfo()) << endl;
	if (track->detectorInfo()) track->detectorInfo()->Dump();

	cout << "---> StTrack -> StTrackNode at "
	     << (void*) (track->node()) << endl;
	if (track->node()) track->node()->Dump();

	cout << "---> StTrack -> StPidTraits ("
	     << (track->pidTraits().size() ? 1 : 0 ) << " of "
	     <<  track->pidTraits().size() << " entries shown)" << endl;
	if (track->pidTraits().size()) track->pidTraits()[0]->Dump();

void
StEventMaker::printTrackInfo(StTrack* track)
{
    cout << "---------------------------------------------------------" << endl;
    cout << "StTrack (" << (track ? track->GetName() : "n/a")
         << ") at " << (void*) track                                    << endl;
    cout << "---------------------------------------------------------" << endl;
    if (track) {
        track->Dump();
        cout << "covariantMatrix():" << track->fitTraits().covariantMatrix() << endl;
        
        cout << "---> StTrack -> StGeometry ("<< track->geometry()->GetName()
             << ") at " << (void*) (track->geometry()) << endl;
        if (track->geometry()) track->geometry()->Dump();

        cout << "---> StTrack -> StDetectorInfo at "
             << (void*) (track->detectorInfo()) << endl;
        if (track->detectorInfo()) track->detectorInfo()->Dump();

        cout << "---> StTrack -> StTrackNode at "
             << (void*) (track->node()) << endl;
        if (track->node()) track->node()->Dump();

        cout << "---> StTrack -> StPidTraits ("
             << (track->pidTraits().size() ? 1 : 0 ) << " of "
             <<  track->pidTraits().size() << " entries shown)" << endl;
        if (track->pidTraits().size()) track->pidTraits()[0]->Dump();
    }
}


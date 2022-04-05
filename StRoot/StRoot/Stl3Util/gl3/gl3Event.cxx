//:>------------------------------------------------------------------
//: FILE:       gl3Event.cc
//: HISTORY:
//:             3 dec 1999 version 1.00
//:             8 apr 2000 include modifications form Clemens
//:             6 jul 2000 add St_l3_Coordinate_Transformer
//:            12 jul 2000 merge tracks using parameters at closest
//:                        approach (from sl3) and them extrapolate
//:                        them to inner most hit for consumers down the line
//:            13 aug 2000 replace trackMerging with maxSectorNForTrackMerging
//:Jens Berger 03 jul 2001 makeVertex added: calcs vertex of the event     
//:<------------------------------------------------------------------
#include "Stl3Util/gl3/gl3Event.h"

#include "Stl3Util/base/realcc.h"
#include "Stl3Util/base/FtfLog.h"

//####################################################################
// Constructor and Destructor 
//####################################################################

gl3Event::gl3Event(int mxHits, int mxTracks,
		   St_l3_Coordinate_Transformer* inTrans)
{
    hit   = NULL;
    track = NULL;
    busy  = 0;
    
    trackContainer = 0;
    trackIndex     = 0;
    hitProcessing  = 0;
    maxSectorNForTrackMerging   = 1000000;
    coordinateTransformer = inTrans;
    setup( mxHits, mxTracks );
    resetEvent();
};


gl3Event::~gl3Event( )
{ 
    if ( hit            != 0 ) delete[] hit  ;  
    if ( track          != 0 ) delete[] track;  
    if ( trackContainer != 0 ) delete[] trackContainer;
    if ( trackIndex     != 0 ) delete[] trackIndex    ;
};




//####################################################################
// Setters and getters
//####################################################################
gl3Track* gl3Event::getTrack ( int n ) {
    if ( n < 0 || n > nTracks ) {
	fprintf ( stderr, " %d track index out of range \n", n ); 
	return NULL;
    }
    return &(track[n]);
}


gl3Hit* gl3Event::getHit ( int n ) {
    if ( n < 0 || n > nHits ) {
	fprintf ( stderr, " %d hit index out of range \n", n ); 
	return NULL;
    }
    return &(hit[n]);
}

gl3Sector* gl3Event::getSector ( int n ) {
    if ( n < 0 || n > NSECTORS ) {
	fprintf ( stderr, " %d sector index out of range \n", n ); 
	return NULL;
    }
    return &(sectorInfo[n]);
}
    

int gl3Event::getTrgCmd()
{
  return trgData.trgCmd;
};

int gl3Event::getTrgWord()
{
  return trgData.trgWord;
};

int gl3Event::getZDC(int n)
{
  return trgData.ZDC[n];
};

int gl3Event::getCTB(int n)
{
  return trgData.CTB[n];
};


//####################################################################
// addTracks: take a pointer to the local_tracks of a sector and 
//            (possibly) merge them with the cuurently known tracks
//####################################################################
void gl3Event::addTracks ( short sector, int nTrk, local_track* localTrack ) {
    //
    gl3Track*    lTrack = &(track[nTracks]) ;
    local_track *trk    = localTrack ;
    int indexStore = -1 ;
    //
    int idTrack ;
    for ( int i = 0 ; i < nTrk ; i++ ) { 
	lTrack->set ( sector, trk ) ; 
	lTrack->id     = sector * 1000 + abs(trk->id) ;
	lTrack->para   = &para ;
	lTrack->sector = sector ;
	idTrack        = trk->id ; 
	//
	//  Check where to store relation orig track<->final track
	//  but only if requested
	//
	if ( hitProcessing ) {
	    indexStore = -1 ;
	    if ( abs(idTrack) < maxTracksSector ) 
		indexStore = (sector-1)*maxTracksSector + abs(idTrack) ;
	    else {   
		ftfLog ( " gl3Event::addTracks: max number of tracks per Sector reached %d reached\n", 
			 idTrack ) ;
	    } 
	}
	//
	//  if id from buffer is negative track is mergable
	//
	gl3Track* fatterTrack = 0 ;
	if ( maxSectorNForTrackMerging > nTrk && idTrack < 0 ) {

	    fatterTrack = lTrack->merge ( trackContainer ) ;
	    if ( fatterTrack ) {
		//ftfLog ( "gl3Event: track merged !!! \n" ) ;
		if ( hitProcessing && indexStore > 0 ) {
		    trackIndex[indexStore] = 
			((char *)fatterTrack - (char *)track )/sizeof(gl3Track)+1;
		}
		trk++ ;
		nMergedTracks++ ;
		continue ;
	    }
	    nMergableTracks++ ;
	}
   
	//   Store new track in trackIndex array
	if ( hitProcessing && indexStore > 0 ) 
	    trackIndex[indexStore] = nTracks + 1; 

	//   Increment counters
	lTrack++ ;
	nTracks++ ;
	trk++ ;
	if ( nTracks+1 >= maxTracks ) {
	    ftfLog ( " gl3Event::addTracks: max number of tracks %d reached, sector: %i nrSectorTracks: %i\n", maxTracks, sector, nTrk  ) ;
	    nTracks-- ;
	    lTrack--;
	    break;
	}
    }
}



//####################################################################
// fillTracks: fill the gl3Tracks into the L3_GTD
//####################################################################
int gl3Event::fillTracks ( int maxBytes, char* buffer, unsigned int token ){
    //
    //   Check enough space
    //
    int nBytesNeeded = sizeof(L3_GTD) + (nTracks-1) * sizeof(global_track) ; 
    if ( nBytesNeeded > maxBytes ) {
	ftfLog ( " gl3Event::writeTracks: %d bytes needed less than max = %d \n",
		 nBytesNeeded, maxBytes ) ;
	return 0 ;
    }

    L3_GTD* head = (L3_GTD *)buffer ;

    head->nHits   = nHits  ; 
    head->xVert   = 0      ; // each sector has a different vertex???
    head->yVert   = 0      ; 
    head->zVert   = 0      ; 
    // bankHeader
    // 
    memcpy(head->bh.bank_type,CHAR_L3_GTD,8);
    head->bh.bank_id    = 1;
    head->bh.format_ver = DAQ_RAW_FORMAT_VERSION ; 
    head->bh.byte_order = DAQ_RAW_FORMAT_ORDER ;
    head->bh.format_number = 0;
    head->bh.token      = token; 
    head->bh.w9         = DAQ_RAW_FORMAT_WORD9;
    head->bh.crc        = 0; //don't know yet....
    head->bh.length     = (sizeof(struct L3_GTD) 
			   + (nTracks-1) * sizeof(struct global_track))/4 ;


    //
    //   Loop over tracks now
    //
    global_track* oTrack = (global_track *)head->track ;
    int counter = 0 ;
    for ( int i = 0 ; i < nTracks ; i++ ) {
	if ( fabs(track[i].z0) > 205 ) {
	    //       ftfLog ( "gl3Event:fillTracks: problem id %d z0 %f \n", 
	    //                      track[i].id, track[i].z0 ) ;
	    nBadTracks++ ;
	    continue ;
	}
	oTrack->id   = track[i].id ;
	oTrack->flag = track[i].flag ;
	oTrack->innerMostRow = track[i].innerMostRow ;
	oTrack->outerMostRow = track[i].outerMostRow ;
	oTrack->nHits        = track[i].nHits        ;
	oTrack->ndedx        = track[i].nDedx        ;
	oTrack->q            = track[i].q            ;
	oTrack->chi2[0]      = track[i].chi2[0]      ;
	oTrack->chi2[1]      = track[i].chi2[1]      ;
	oTrack->dedx         = track[i].dedx         ;
	oTrack->pt           = track[i].pt           ;
	oTrack->phi0         = track[i].phi0         ;
	oTrack->r0           = track[i].r0           ;
	oTrack->z0           = track[i].z0           ;
	oTrack->psi          = track[i].psi          ;
	oTrack->tanl         = track[i].tanl         ;
	oTrack->length       = track[i].length       ;
	oTrack->dpt          = track[i].dpt          ;
	oTrack->dpsi         = track[i].dpsi         ;
	oTrack->dz0          = track[i].dz0          ;
	oTrack->dtanl        = track[i].dtanl        ;
	oTrack++ ;
	counter++ ;
    }
    head->nTracks = counter ;
    //
    //   return number of bytes
    //
    return ((char *)oTrack-buffer) ;
}


//####################################################################
// readEventDescriptor: get token, trigger command and trigger word 
//                      from the EventDescriptor received with the 
//                      "announce token"
//####################################################################
int gl3Event::readEventDescriptor( EventDescriptor *descr )
{
    if (!descr) {
	ftfLog("gl3Event::readEventDescriptor: got NULL pointer");
	return 0;
    }

    trgData.trgCmd = ((descr->TRG_DAQ_cmds) & 0xf0) >> 4;
    trgData.trgWord = descr->TRG_word;

    return 0;
}



//####################################################################
// readL3Data: read the L3 contributions for an event. Currently 
//             includes TPC data, but SVT and FTPC data will also 
//             be in this data block at some time.
//####################################################################
int gl3Event::readL3Data( L3_P* header )
{

    char* buffer = (char *)header;
    //L3_P* header = (L3_P *)buffer ;
    
    int length, offset ;
    char* trackPointer ;
    char* hitPointer ;
    
    resetEvent ( );
    int i ;
    L3_SECP* sectorP ; 
    for ( i = 0 ; i < NSECTORS ; i++ ) {
	length = header->sector[i].len ;

	if ( length==0 ) continue ;
	
	offset = 4 * header->sector[i].off ;
	sectorP  = (L3_SECP *)&(buffer[offset]);
	trackPointer  = (char *)sectorP + sectorP->trackp.off * 4 ; 
	int nSectorTracks = readSectorTracks ( trackPointer ) ;
	
	if ( nSectorTracks < 0 ) {
	    ftfLog("gl3Event:readEvent: error reading tracks, sector %d \n", 
		   i+1);
	    return -1 ;
	}
	
	if ( hitProcessing && sectorP->sl3clusterp.off ) {
	    hitPointer    = (char *)sectorP + sectorP->sl3clusterp.off * 4 ; 
	    readSectorHits   ( hitPointer, nSectorTracks ) ;
	}
	
	
	
    }

    makeVertex();
    
    
#ifdef EVENTDISPLAY
    //  For best merging (as least as 7/12/00) tracks
    //  are passed from sl3 to gl3 at point of closest approach
    //  the event viewer wants them(at least for now) at
    //  inner most point so we extrapolate to inner most hit radius

    double radius ;

    for ( int i = 0 ; i < nTracks ; i++ ) {

	radius = coordinateTransformer->
	    GetRadialDistanceAtRow(track[i].innerMostRow-1) ;

	track[i].updateToRadius ( radius ) ;

	//   If track outside of TPC move radius since row
	//   radius is equal or larger than DistanceAtRow
	if ( fabs(track[i].z0) > 205 ) track[i].updateToRadius ( radius+5. ) ;
	if ( fabs(track[i].z0) > 205 ) {
	    ftfLog ( "gl3Event:: problem after extrapolation id %d z0 %f\n", 
		     track[i].id, track[i].z0 ) ; 
	} 
    }
#endif
    //
    //   declare event as busy
    //
    busy = 1 ;

    return 0 ;
}


//####################################################################
// readTrgData: Read the data provided by L[0-2] trigger. Mainly 
//              interesting for CTB and ZDC data. 
//####################################################################
int gl3Event::readTrgData(TrgSumData* trgSum, RawTrgDet* rawTrg)
{
    if (trgSum != NULL) {
	// Get the ZDC data
	for (int i=0; i<16; i++) {
	    trgData.ZDC[i] = trgSum->DSM.ZDC[i];
	}
    }

    if (rawTrg != NULL) {
	
	// Read the CTB data
	// The data is delivered in 16 chunks of 16 bytes, but 
	// the last byte of each chunk does not contain CTB 
	// information, thus the 15*16 bytes of slat information 
	// will be packed into an array[240]. 
	for (int chunk=0; chunk<16; chunk++) {
	    for (int slat=0; slat<15; slat++) {
		trgData.CTB[15*chunk + slat] = 
		    rawTrg->CTB[16*chunk + slat];
	    }
	}
    }

    return 0;
}


//####################################################################
// readSectorHits: does what you expect
//####################################################################
int gl3Event::readSectorHits ( char* buffer, int nSectorTracks ){
    L3_SECCD* head = (L3_SECCD *)buffer ;
    //
    //   Check coordinate transformer is there
    //
    if ( !coordinateTransformer ) {
	ftfLog("gl3Event::readSectorHits: there is not Coordinate Transformer \n");
	return 0 ;
    }


    //
    //   Check bank header type
    //
    if ( strncmp(head->bh.bank_type,CHAR_L3_SECCD,8) ) {
	ftfLog ( "gl3Event::readSectorHits: wrong bank type %s\n", 
		 head->bh.bank_type ) ;
	ftfLog ( " right bank type %s \n", CHAR_L3_SECCD ) ;
	return 0 ;
    } 
    int sector = head->bh.bank_id;
    int nSectorHits = head->nrClusters_in_sector ;

    //
    //    Check number of hits
    //
    if ( nHits + nSectorHits > maxHits ) {
	ftfLog("gl3Event:readSectorHits: not enough space for hits in sector %d\n",
	       sector ) ;
	ftfLog ( "         maxHits %d nSectorHits %d nHits %d \n", maxHits,
		 nSectorHits, nHits ) ;
	return 0 ;
    }

    l3_cluster* cluster = (l3_cluster *)head->cluster ;
    l3_cluster* hitP ;
    gl3Hit*     gHitP = 0  ;
    
    for ( int i = 0 ; i < nSectorHits ; i++ ) {
	hitP = &(cluster[i]) ;
	//
	//   Only if hits are going to be used for analysis unpack them
	//
	if ( hitProcessing > 1 ) {
	    gHitP = &(hit[nHits+i]);
	    gHitP->set (coordinateTransformer, sector, hitP);
	}
	//
	//  Now logic to reset trackId in hits
	//  This is to take care of track merging
	//
	int trkId = hitP->trackId ;
	if ( trkId < 0 || trkId > nSectorTracks ) { 
	    ftfLog ( "gl3Event:readSectorHits: %d wrong track id in hit of sector %d \n", 
		     trkId, sector ) ;
	    continue ;
	}
	int indexStore = (sector-1)*maxTracksSector+trkId ;
	if ( indexStore < 0 || indexStore > NSECTORS*maxTracksSector ) { 
	    ftfLog ( "gl3Event:readSectorHits: %d wrong indexStore\n", 
		     indexStore ) ;
	    continue ;
	}
	int index = trackIndex[indexStore] - 1 ;
	if ( index < 0 || index > nTracks ) continue ;
	//
	//   Only if hits are gonig to be used the
	//   linked lists are set
	//
	if ( hitProcessing > 1 ) {
	    if ( track[index].firstHit == 0 )
		track[index].firstHit = (void *)gHitP ;
	    else
		((gl3Hit *)(track[index].lastHit))->nextHit = (void *)gHitP ;
	    track[index].lastHit = (void *)gHitP ;
	    gHitP->trackId = track[index].id ;
	}
	//
	//   Modify trackId of clusters got from sl3
	//
	hitP->trackId = track[index].id ;
	//    ftfLog ( "hit trackId %d \n", track[index].id  ) ;
	
    }
    nHits += nSectorHits ;
    
    return nSectorHits ;
}


//####################################################################
// readSectorTracks: guess what it dows ;)
//        fills some general info and calls addTracks()
//####################################################################
int gl3Event::readSectorTracks ( char* buffer ){

    struct L3_SECTP *head = (struct L3_SECTP *)buffer ;
    
    if ( strncmp(head->bh.bank_type,CHAR_L3_SECTP,8) ) {
	ftfLog ( "gl3Event::readSectorTracks, wrong bank type %s\n", 
		 head->bh.bank_type ) ;
	return -1 ;
    }

    int sector = head->bh.bank_id ;
    if ( sector < 0 || sector > NSECTORS ) {
	ftfLog ( " gl3Event::readSector: %d wrong sector \n", sector ) ;
	return 1 ;
    }
    
    gl3Sector* sectorP = &(sectorInfo[sector-1]) ;
    sectorP->filled = 1 ;
    sectorP->nHits     = head->nHits ;
    sectorP->nTracks   = head->nTracks ;
    sectorP->cpuTime   = head->cpuTime ;
    sectorP->realTime  = head->realTime ;
    sectorP->xVert     = float(head->xVert)/1000000 ;
    sectorP->yVert     = float(head->yVert)/1000000 ;
    sectorP->zVert     = float(head->zVert)/1000000 ;
    sectorP->rVert     = ::sqrt((double)( sectorP->xVert*sectorP->xVert +
					sectorP->yVert*sectorP->yVert)) ;
    sectorP->phiVert   = atan2((double)sectorP->yVert,(double)sectorP->xVert) ;
    if ( sectorP->phiVert < 0 ) sectorP->phiVert += 2. * M_PI ;

    //sectorP->print();
    //
    //   Set vertex parameters for track merging 
    //
    para.xVertex   = sectorP->xVert ;
    para.yVertex   = sectorP->yVert ;
    para.zVertex   = sectorP->zVert ;
    para.rVertex   = sectorP->rVert ;
    para.phiVertex = sectorP->phiVert ;
    // 
    char* pointer = head->banks[0].off * 4 + buffer ;
    int nSectorTracks ;
    //
    if ( (head->banks[0].len > 0) &&  (head->bh.format_number > 0) ) {
	//    ftfLog ( "banks[0].len %d\n", head->banks[0].len ) ;
	nSectorTracks = (4 * head->banks[0].len - sizeof(struct bankHeader))
	    /sizeof(struct local_track);
    }
    else nSectorTracks = 0 ;
    //
    //   Add tracks
    //
    if ( nSectorTracks > 0 ) {
	struct L3_LTD* headerLocal = (struct L3_LTD*)pointer ;
	local_track* localTrack = headerLocal->track ;
	addTracks ( sector, nSectorTracks, localTrack ) ;
    }
    
    //
    return sectorP->nTracks ;
}


//####################################################################
// Reconstruct the vertex and store it in gl3Event::vertex
//####################################################################
void gl3Event::makeVertex (){
    // debug
    //printf ( "doing gl3Vertex process!!!\n");
  
    // init
    //short sector = 0 ;
    gl3Track* gTrack ;
    Ftf3DHit closestHit ;
  
    hVz->Reset();
    hVx->Reset();
    hVy->Reset();
  
    vertex.Setxyz(0.0,0.0,0.0);
  
  
    for(int iter = 0 ; iter<1; iter++ ) {
	// loop over gtracks
	for(int trkcnt = 0 ; trkcnt<getNTracks(); trkcnt++ ) {
	    gTrack = getTrack(trkcnt);
      
	    // acept only tracks with nHits more then minNoOfHitsOnTrack
	    if ( gTrack->nHits > minNoOfHitsOnTrackUsedForVertexCalc &&
		 gTrack->pt > minMomUsedForVertexCalc) {
		// bad bad bad baby! Wouldn't it be nicer to use Vx and Vz!
		closestHit = gTrack->closestApproach(getVertex().Getx(),
						     getVertex().Gety());
		// fill histos with the coordinates of the closest point to x=y=0
		hVz->Fill(closestHit.z,1.0);
		hVx->Fill(closestHit.x,1.0);
		hVy->Fill(closestHit.y,1.0);
	    }
	} // for(int trkcnt = 0 ; trkcnt<event->getNTracks(); trkcnt++ )
    
	// get and set the weighted mean
	vertex.Setxyz(hVx->getWeightedMean(6.0),
		      hVy->getWeightedMean(6.0),
		      hVz->getWeightedMean(4.0));

    } //for(int iter = 0 ; iter<2; iter++ )
  
}

//####################################################################
//
//####################################################################
int gl3Event::resetEvent  (  ){
    nHits         = 0 ;
    nTracks       = 0 ;
    nMergedTracks = 0 ;
    nMergableTracks = 0 ;
    nBadTracks    = 0 ;
    busy          = 0 ;

    // Reset tracks
    memset(trackContainer, 0, 
	   para.nPhiTrackPlusOne*para.nEtaTrackPlusOne*sizeof(FtfContainer));

    // Reset hits
    if ( hitProcessing )
	memset ( trackIndex, 0, maxTracksSector*NSECTORS*sizeof(int) ) ;

    // Reset trigger data
    for (int i=0; i<16; i++)
	trgData.ZDC[i] = -1;

    for (int i=0; i<240; i++)
	trgData.CTB[i] = -1;

    /*
      for ( int i = 0 ; i < nTracks ; i++ ) {
      track[i].firstHit = 0 ;
      track[i].lastHit = 0 ;
      track[i].nextTrack = 0 ;
      }
    */

    return 0 ;
}

//####################################################################
//
//####################################################################
int gl3Event::setup ( int mxHits, int mxTracks )
{
    
    if ( mxHits < 0 || mxHits > 1000000 ) {
	ftfLog(" gl3Event::setup: maxHits %d out of range \n", maxHits ) ;
	mxHits = 500000 ;
    }

    if ( mxTracks < 0 || mxTracks > 1000000 ) {
	ftfLog(" gl3Event::setup: maxTracks %d out of range \n", maxTracks );
	mxTracks = 50000 ;
    }


    maxHits    = mxHits ;
    maxTracks  = mxTracks ;
    maxTracksSector = maxTracks*2/ NSECTORS ;
    hit        = new gl3Hit[maxHits] ;
    track      = new gl3Track[maxTracks] ;
    trackIndex = new int[maxTracksSector*NSECTORS];
    //
    //   Merging variables
    //
    nMergedTracks   = 0 ;
    
    para.nPhiTrackPlusOne = para.nPhiTrack + 1 ;
    para.nEtaTrackPlusOne = para.nEtaTrack + 1 ;
    //-----------------------------------------------------------------------
    //         If needed calculate track area dimensions
    //-----------------------------------------------------------------------
    para.phiSliceTrack = (para.phiMaxTrack - para.phiMinTrack)/para.nPhiTrack;
    para.etaSliceTrack = (para.etaMaxTrack - para.etaMinTrack)/para.nEtaTrack;
    
    int nTrackVolumes = para.nPhiTrackPlusOne* para.nEtaTrackPlusOne ;
    trackContainer = new FtfContainer[nTrackVolumes];
    if(trackContainer == NULL) {
	ftfLog ( "Problem with memory allocation... exiting\n") ;
	return 1 ;
    }
    para.primaries = 1 ;
    para.ptMinHelixFit = 1.e60 ;
    
    nTracks = 0 ;

    //-----------------------------------------------------------------------
    // for vertex calc  
    //-----------------------------------------------------------------------
    minNoOfHitsOnTrackUsedForVertexCalc=14; // points
    minMomUsedForVertexCalc=0.25; // GeV

    //char hid[50] ;
    //char title[100] ;

    //strcpy ( hid, "Vertex_Vz" ) ;
    //strcpy ( title, "Vertex_Vz" ) ;
    hVz = new gl3Histo ( "Vertex_Vz", "Vertex_Vz", 400, -200., 200. ) ;
  
    //strcpy ( hid, "Vertex_Vx" ) ;
    //strcpy ( title, "Vertex_Vx" ) ;
    hVx = new gl3Histo ( "Vertex_Vx", "Vertex_Vx", 100,-10,10);
    //hVx = new gl3Histo ( hid, title, 100,-10,10);

    //strcpy ( hid, "Vertex_Vy" ) ;
    //strcpy ( title, "Vertex_Vy" ) ;
    hVy = new gl3Histo ( "Vertex_Vy", "Vertex_Vy", 100,-10,10);
    //hVy = new gl3Histo ( hid, title, 100,-10,10);
    
    // -----------------------------------------------------------------------

    return 0 ;
}

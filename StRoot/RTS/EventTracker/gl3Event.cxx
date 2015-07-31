///:>------------------------------------------------------------------
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
#include "gl3Event.h"
#include <rtsLog.h>
#include "gl3Histo.h"
#ifdef OLD_DAQ_READER
#include <evpReader.hh>
#else /* OLD_DAQ_READER */
#include <DAQ_READER/daqReader.h>
#endif /* OLD_DAQ_READER */
#include "FtfSl3.h"
#ifndef OLD_DAQ_READER

#include <DAQ_READER/daq_dta.h>
#include <DAQ_TPC/daq_tpc.h>
#include <DAQ_TPX/daq_tpx.h>
#include <DAQ_SC/daq_sc.h>

tpc_t *pTPC=NULL;

#endif /* OLD_DAQ_READER */
//
// evp should already contain the event that we want to read
// 
//
#ifdef OLD_DAQ_READER
int gl3Event::readFromEvpReader(evpReader *evp, 
#else /* OLD_DAQ_READER */
int gl3Event::readFromEvpReader(daqReader *rdr, 
#endif /* OLD_DAQ_READER */
				char *mem, 
				float defaultbField,
				float bField,
				int what)
{
#ifndef OLD_DAQ_READER
  daq_dta *dd;

#endif /* OLD_DAQ_READER */
//   // Read the file...
//   char *mem;
//   for(;;) {
//     mem = evp->get(which,type);
    
//     if(!mem) {
//       return evp->status;
//     }
    
//     if(evp->seq == 235) break;
//   }
    
  LOG(DBG, "Reader from EVP Reader: evt=%d token=%d",rdr->seq,rdr->token);
  // Clear this event...
  resetEvent();
  nHits = 0;

  LOG(DBG, "Check magnetic field");


  if(bField == 1000) {  // try to read from file
    bField = defaultbField;       // use default

#ifdef OLD_DAQ_READER
    int ret = scReader(mem);
    if(ret >= 0) {                // but try file first!
      if(sc.valid) bField = sc.mag_field;
#else /* OLD_DAQ_READER */
    dd = rdr->det("sc")->get("legacy");

    if(dd) {
      dd->iterate();
      sc_t *sc = (sc_t *)dd->Void;
      if(sc->valid) bField = sc->mag_field;
#endif /* OLD_DAQ_READER */
    }
  }       

  if(fabs(bField) < .1) bField = .1;

  LOG(NOTE, "bField set to %f",bField,0,0,0,0);

  setBField(bField);

  // need a tracker...
  coordinateTransformer->Set_parameters_by_hand(0.581, 200.668, 201.138 );
  coordinateTransformer->LoadTPCLookupTable("/RTS/conf/L3/map.bin");

  FtfSl3 *tracker = new FtfSl3(coordinateTransformer);

  tracker->setup();
  tracker->para.bField = fabs(bField); 

  // +1 , -1 or 0 if zero field...
  tracker->para.bFieldPolarity = (bField>0) ? 1 : -1;

  
  ////// check parms  ///////////////
  tracker->setXyError(.12) ; 
  tracker->setZError(.24) ;
  
  tracker->para.ptMinHelixFit = 0.;
  tracker->para.maxChi2Primary = 0.;  
  
  tracker->para.trackChi2Cut = 10 ; 
  tracker->para.hitChi2Cut   = 50 ;
  tracker->para.goodHitChi2  = 20 ; 
  ///////////////////////////////////
 

  tracker->reset();
 
  // need temporary track memory...
  L3_SECTP *sectp = NULL;

  if(what & GL3_READ_TPC_TRACKS) {
    sectp = (L3_SECTP *)malloc(szSECP_max);
  }

  int i;
  for(i=0;i<24;i++) {
    if(what & GL3_READ_TPC_TRACKS) {
      if((i%2) == 0) {
	tracker->nHits = 0;
	tracker->setTrackingAngles(i+1);    // only set angles by hypersector...
      }
    }

    LOG(DBG, "READ TPC data for sector %d  (0x%x)",i+1,rdr);

    // Read the data....
#ifdef OLD_DAQ_READER
    int ret = tpcReader(mem, i);
    if(ret < 0) { 
      LOG(NOTE, "No data for sector %d",i+1,0,0,0,0);
#else /* OLD_DAQ_READER */
    dd = rdr->det("tpx")->get("legacy",i+1);
    if(dd) {
      LOG(NOTE, "There is tpx data...");
      dd->iterate();
      pTPC = (tpc_t *)dd->Void;
    }
    else {
      LOG(NOTE, "No tpx data for sector %d check for TPC",i);
  
      dd = rdr->det("tpc")->get("legacy",i+1);
      if(dd) {
	dd->iterate();
	pTPC = (tpc_t *)dd->Void;
	//LOG("JEFF", "EEE: i=%d 0x%x",i,pTPC);
	// afterburner...
	// daq_tpc *tpc_class = (daq_tpc *)rdr->det("tpc");
	///LOG("JEFF", "EFEF 0x%x",tpc_class);


	//int cl_found = 0;
	//cl_found = tpc_class->fcfReader(i+1,NULL,NULL,pTPC);

	int cl_found = 0;
	for(int pr=0;pr<45;pr++) {
	  cl_found += pTPC->cl_counts[pr];
	}

	LOG(NOTE, "Found tpc data for sector %d... %d clusters found",i,cl_found);
      }
      else {
	pTPC = NULL;
      }
    }

    if(!pTPC) {
      LOG(NOTE, "No data for TPC sector %d",i+1,0,0,0,0);
#endif /* OLD_DAQ_READER */
      continue;
    }
    
    //LOG(NOTE, "Tpc reader done");
#ifdef OLD_DAQ_READER
    if(!tpc.has_clusters) {
      // Construct the clusters...
      // no calibration info, so pretty junky...
      int ncl_recount = fcfReader(i);
      if (ncl_recount) {
      //LOG("JEFF", "fcf[%d] has %d clusters", i, ncl_recount);
      }
#else /* OLD_DAQ_READER */
    if(!pTPC->has_clusters) {
      LOG(WARN, "TPC sector %d has no clusters",i);
      continue;
#endif /* OLD_DAQ_READER */
    }

    // read in clusters...
    if(what & GL3_READ_TPC_CLUSTERS) {

      LOG(DBG, "Reading clusters");
      readClustersFromEvpReader(i+1);

      int nnn=0;
      for(int i=0;i<45;i++) {
#ifdef OLD_DAQ_READER
	nnn += tpc.cl_counts[i];
#else /* OLD_DAQ_READER */
	nnn += pTPC->cl_counts[i];
#endif /* OLD_DAQ_READER */
      }
      LOG(DBG, "clusters done %d",nnn);
    }

    // Do tracking...
    if(what & GL3_READ_TPC_TRACKS) {
      LOG(DBG, "Tracking...");
      tracker->readSectorFromEvpReader(i+1);
      
      // only do tracking on full hypersectors...
      if((i%2) == 1) {
	tracker->processSector();
	tracker->fillTracks(szSECP_max, (char *)sectp, 0);

	LOG(DBG, "SECP size = %d",sectp->bh.length*4 + sectp->banks[0].len*4);

	int n = readSectorTracks((char *)sectp);
	//printf("Got %d tracks\n",n);
	
	if(n < 0) {
	  LOG(WARN, "Error reading tracker: sector %d\n",i,0,0,0,0);
	  continue;
	}
      }
    }
  }
  
  // LOG(NOTE, "FINAL");
  if(what & GL3_READ_TPC_TRACKS) {
    //LOG(INFO, "Finalizing...\n");y
    finalizeReconstruction();
    free(sectp);
  }
  // LOG(NOTE, "FINAL2");
  
#ifdef EMC_LEGACY 
  // If calibrations not loaded nothing should happen here...
#ifdef OLD_DAQ_READER
  emc.readFromEvpReader(evp, mem);
#else /* OLD_DAQ_READER */
  emc.readFromEvpReader(rdr, mem);
#endif /* OLD_DAQ_READER */
#endif

  delete tracker;
  return 0;
}

// Assume global tpc structure already filled....
// s = sector from 1
//
void gl3Event::readClustersFromEvpReader(int sector)
{
#ifdef OLD_DAQ_READER
  if(!tpc.has_clusters) return;
#else /* OLD_DAQ_READER */

  LOG(DBG, "have clusters?  %d",pTPC->has_clusters);
  if(!pTPC->has_clusters) return;
#endif /* OLD_DAQ_READER */

  for(int r=0;r<45;r++) {
#ifdef OLD_DAQ_READER
    for(int j=0;j<tpc.cl_counts[r];j++) {
      tpc_cl *c = &tpc.cl[r][j];
#else /* OLD_DAQ_READER */
    for(int j=0;j<pTPC->cl_counts[r];j++) {
      tpc_cl *c = &pTPC->cl[r][j];
#endif /* OLD_DAQ_READER */
	
      gl3Hit *gl3c = &hit[nHits];
      nHits++;

      l3_cluster sl3c;
      sl3c.pad = (int)((c->p - 0.5) * 64);
      sl3c.time = (int)((c->t - 0.5) * 64);
      sl3c.charge = c->charge;
      sl3c.flags = c->flags;
      sl3c.padrow = r;
      sl3c.RB_MZ = 0;    // need to fake rb=0 so "set" doesn't change my sector
      //c->p1;
      //c->p2;
      //c->t1;
      //c->t2;
	
      gl3c->set(coordinateTransformer, sector, &sl3c);

      //printf("i=%d nHits=%d (%f %f %f) %f %f %f\n",i,nHits,
      //       sl3c.pad / 64.0,
      //       sl3c.time / 64.0,
      //       sl3c.charge,
      //       gl3c->getX(),gl3c->getY(),gl3c->getZ());
	
    }
  }
}
//####################################################################
// Constructor and Destructor 
//####################################################################

gl3Event::gl3Event( l3CoordinateTransformer* inTrans,
		    l3EmcCalibration* inBemcCalib,
		    l3EmcCalibration* inEemcCalib,
		    int mxHits, int mxTracks) 
    : emc(inBemcCalib, inEemcCalib)
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
    if ( n < 0 || n > nSectors ) {
	fprintf ( stderr, " %d sector index out of range \n", n ); 
	return NULL;
    }
    return &(sectorInfo[n]);
}
    

int gl3Event::getTrgCmd()
{
    //return trgData.trgCmd;
    return -1;
};

int gl3Event::getTrgWord()
{
  return trgData.triggerWord;
};

int gl3Event::getZDC(int n)
{
  return trgData.ZDC[n];
};

int gl3Event::getCTB(int n)
{
  return trgData.CTB[n];
};

double gl3Event::getZDCVertex()
{
    return ((double)(trgData.ZDC[9] - trgData.ZDC[8]) + 21.3) * 3.3;
};


// Getters for the 64bit bunch crossing number are available as 
// 32 and 64 bit verions 
//         PLEASE CHECK!!!
unsigned int gl3Event::getBXingLo()
{ 
    return trgData.bunchXing_lo;
}

unsigned int gl3Event::getBXingHi()
{ 
    return trgData.bunchXing_hi;
}

unsigned long long gl3Event::getBXing() 
{
    unsigned long long bx_hi_long = trgData.bunchXing_hi;
    unsigned long long bx_lo_long = trgData.bunchXing_lo;

    return (bx_hi_long << 32) | bx_lo_long;
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
	    LOG(ERR, " gl3Event::addTracks: max number of tracks per Sector reached %d reached", idTrack ,0,0,0,0) ;
	 } 
      }
      //
      //  if id from buffer is negative track is mergable
      //
      gl3Track* fatterTrack = 0 ;
      if ( maxSectorNForTrackMerging > nTrk && idTrack < 0 ) {

         fatterTrack = lTrack->merge ( trackContainer ) ;
         if ( fatterTrack ) {
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
         LOG(ERR," gl3Event::addTracks: max number of tracks %d reached, sector: %i nrSectorTracks: %i", maxTracks, sector, nTrk  ,0,0) ;
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
     LOG(ERR, " gl3Event::writeTracks: %d bytes needed less than max = %d \n",
	    nBytesNeeded, maxBytes ,0,0,0) ;
      return 0 ;
   }

   L3_GTD* head = (L3_GTD *)buffer ;

   head->nHits   = nHits; 
   head->xVert   = vertex.Getx();
   head->yVert   = vertex.Gety(); 
   head->zVert   = vertex.Getz(); 
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
    for ( i = 0 ; i < nSectors ; i++ ) {
	length = header->sector[i].len ;

	if ( length==0 ) continue ;
	
	offset = 4 * header->sector[i].off ;
	sectorP  = (L3_SECP *)&(buffer[offset]);

	trackPointer  = (char *)sectorP + sectorP->trackp.off * 4 ; 


	int nSectorTracks = 0;
	if (sectorP->trackp.off) {
	    nSectorTracks = readSectorTracks ( trackPointer ) ;
	
	    if ( nSectorTracks < 0 ) {
		LOG(ERR, "gl3Event:readEvent: error reading tracks, sector %d", i+1,0,0,0,0);
		return -1 ;
	    }
	}
	
	if ( hitProcessing && sectorP->sl3clusterp.off ) {
	    hitPointer    = (char *)sectorP + sectorP->sl3clusterp.off * 4 ; 
	    readSectorHits   ( hitPointer, nSectorTracks ) ;
	}
	
    }

    if(header->bh.format_number>=5 && header->trig.len){
	//l3Log("Reading TRG data\n");
	//trgData.read( (int*)header + header->trig.off );
	trgData.readL3P(header);

    }
    
    
    if( header->bh.format_number>=7 ){
	//l3Log("Reading EMC data\n");
	emc.readRawData(header);
    } else {
	//l3Log("Not reading EMC: format_number=%i, len=%i" ,
	//    header->bh.format_number, header->emc[0].len);
    }      
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
	 LOG(ERR, "gl3Event:: problem after extrapolation id %d z0 %f", 
	     track[i].id, track[i].z0 ,0,0,0) ; 
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
// Do last reconstruction steps before analysis
//####################################################################
int gl3Event::finalizeReconstruction()
{

  // LOG(NOTE, "F1");
  if (vertexFinder & 0x01) // internal calculation
    makeVertex();
  
  //LOG(NOTE, "F2");
  
  if ((vertexFinder & 0x02) && lmv) { // low mult vertex finder
    //LOG(NOTE, "F2.5");
    lmv->makeVertex(this);
    Ftf3DHit vtx = lmv->getVertex();
    
    lmVertex.Setx(vtx.x);
    lmVertex.Sety(vtx.y);
    lmVertex.Setz(vtx.z);
  }
  
  //LOG(NOTE, "F3");
  Ftf3DHit vertex_ftf;
  vertex_ftf.x = vertex.Getx();
  vertex_ftf.y = vertex.Gety();
  vertex_ftf.z = vertex.Getz();

  // Compute DCAs for all tracks
  for (int i=0 ; i<getNTracks() ; i++) { 
    getTrack(i)->setDca(vertex_ftf);
  }

  //LOG(NOTE, "F4");
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
      LOG(ERR, "gl3Event::readSectorHits: there is not Coordinate Transformer",0,0,0,0,0);
	return 0 ;
    }


    //
    //   Check bank header type
    //
    if ( strncmp(head->bh.bank_type,CHAR_L3_SECCD,8) ) {
      LOG(ERR, "gl3Event::readSectorHits: wrong bank type %s", 
	  head->bh.bank_type,0,0,0,0 ) ;
      LOG(ERR, " correct bank type would be %s", CHAR_L3_SECCD,0,0,0,0 ) ;
	return 0 ;
    } 
    int sector = head->bh.bank_id;
    int nSectorHits = head->nrClusters_in_sector ;

    //
    //    Check number of hits
    //
    if ( nHits + nSectorHits > maxHits ) {
      LOG(ERR, "gl3Event:readSectorHits: not enough space for hits in sector %d", sector,0,0,0,0 ) ;
      LOG(ERR, "         maxHits %d nSectorHits %d nHits %d", maxHits,
		 nSectorHits, nHits ,0,0) ;
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
	  LOG(ERR, "gl3Event:readSectorHits: %d wrong track id in hit of sector %d \n", 
		     trkId, sector ,0,0,0) ;
	    continue ;
	}
	int indexStore = (sector-1)*maxTracksSector+trkId ;
	if ( indexStore < 0 || indexStore > nSectors*maxTracksSector ) { 
	  LOG(ERR, "gl3Event:readSectorHits: %d wrong indexStore\n", 
		     indexStore ,0,0,0,0) ;
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
	//    l3Log ( "hit trackId %d \n", track[index].id  ) ;
	
    }
    nHits += nSectorHits ;
    
    return nSectorHits ;
}


//####################################################################
// readSectorTracks: guess what it does ;)
//        fills some general info and calls addTracks()
//####################################################################
int gl3Event::readSectorTracks ( char* buffer ){

    struct L3_SECTP *head = (struct L3_SECTP *)buffer ;
    
    if ( strncmp(head->bh.bank_type,CHAR_L3_SECTP,8) ) {
      LOG(ERR, "gl3Event::readSectorTracks, wrong bank type %s\n", 
		 head->bh.bank_type,0,0,0,0 ) ;
	return -1 ;
    }

    int sector = head->bh.bank_id ;
    if ( sector < 0 || sector > nSectors ) {
      LOG(ERR," gl3Event::readSector: %d wrong sector \n", sector ,0,0,0,0) ;
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
    sectorP->rVert     = sqrt((double)( sectorP->xVert*sectorP->xVert +
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
	//    l3Log ( "banks[0].len %d\n", head->banks[0].len ) ;
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
// Reconstrucht the vertex and store it in gl3Event::vertex
//####################################################################
int gl3Event::makeVertex (){
  // debug
  //printf ( "doing gl3Vertex process!!!\n");
  
  // init
  //short sector = 0 ;
  gl3Track* gTrack ;
  Ftf3DHit closestHit ;
  
  hVz->Reset();
  hVx->Reset();
  hVy->Reset();
  
  // Comment to use the vertex of the last event
  vertex.Setxyz(0.0,0.0,0.0);
  
  
  for(int iter = 0 ; iter<2; iter++ ) {
    // loop over gtracks
    for(int trkcnt = 0 ; trkcnt<getNTracks(); trkcnt++ ) {
      gTrack = getTrack(trkcnt);
      
      //printf("-----track %d\n", trkcnt);

      // acept only tracks with nHits more then minNoOfHitsOnTrack
      if ( gTrack->nHits > minNoOfHitsOnTrackUsedForVertexCalc &&
	   gTrack->pt > minMomUsedForVertexCalc) {
	// bad bad bad baby! Wouldn't it be nicer to use Vx and Vz!
	closestHit = gTrack->closestApproach(getVertex().Getx(),
					     getVertex().Gety());


	//printf("---------- (%4.2f %4.2f %4.2f)\n",closestHit.x,closestHit.y,closestHit.z);
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
  

  return 0;
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
    if ( hitProcessing ) {
	memset ( trackIndex, 0, maxTracksSector*nSectors*sizeof(int) ) ;
	delete[] hit;
	hit = new gl3Hit[maxHits];
    }

    // Reset trigger data
    for (int i=0; i<16; i++)
	trgData.ZDC[i] = 0;

    for (int i=0; i<240; i++)
	trgData.CTB[i] = 0;

    /*
      for ( int i = 0 ; i < nTracks ; i++ ) {
      track[i].firstHit = 0 ;
      track[i].lastHit = 0 ;
      track[i].nextTrack = 0 ;
      }
    */

    // Reset vertex seed
    vertex.Setxyz(0.0, 0.0, 0.0);

    emc.reset();

    return 0 ;
}

//####################################################################
//
//####################################################################
int gl3Event::setup ( int mxHits, int mxTracks )
{
    
    if ( mxHits < 0 || mxHits > 1000000 ) {
      LOG(ERR, " gl3Event::setup: maxHits %d out of range \n", maxHits,0,0,0,0 ) ;
	mxHits = 500000 ;
    }

    if ( mxTracks < 0 || mxTracks > 1000000 ) {
      LOG(ERR, " gl3Event::setup: maxTracks %d out of range \n", maxTracks,0,0,0,0 );
	mxTracks = 50000 ;
    }


    maxHits    = mxHits ;
    maxTracks  = mxTracks ;
    maxTracksSector = maxTracks*2/ nSectors ;
    hit        = new gl3Hit[maxHits] ;
    track      = new gl3Track[maxTracks] ;
    trackIndex = new int[maxTracksSector*nSectors];
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
      LOG(ERR, "Problem with memory allocation... exiting\n",0,0,0,0,0) ;
	return 1 ;
    }
    para.primaries = 1 ;
    para.ptMinHelixFit = 1.e60 ;
    
    nTracks = 0 ;

    //-----------------------------------------------------------------------
    // instanziate for vertex calc  
    //-----------------------------------------------------------------------
    minNoOfHitsOnTrackUsedForVertexCalc=14; // points
    minMomUsedForVertexCalc=0.25; // GeV

    char hid[50] ;
    char title[100] ;

    strcpy ( hid, "Vertex_Vz" ) ;
    strcpy ( title, "Vertex_Vz" ) ;
    hVz = new gl3Histo ( hid, title, 400, -200., 200. ) ;
  
    strcpy ( hid, "Vertex_Vx" ) ;
    strcpy ( title, "Vertex_Vx" ) ;
    hVx = new gl3Histo ( hid, title, 100,-10,10);

    strcpy ( hid, "Vertex_Vy" ) ;
    strcpy ( title, "Vertex_Vy" ) ;
    hVy = new gl3Histo ( hid, title, 100,-10,10);
    
    // -----------------------------------------------------------------------

    return 0 ;
}

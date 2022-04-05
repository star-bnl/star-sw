//:>------------------------------------------------------------------
//: FILE:       gl3Conductor.cc
//: HISTORY:
//:             1 feb 2000 version 1.00
//:             6 jul 2000 add St_l3_Coordinate_Transformer
//:            27 jul 2000 add print timing
//:<------------------------------------------------------------------
#include "Stl3Util/gl3/gl3Conductor.h"

#include "Stl3Util/base/L3swap.h"
#include "Stl3Util/base/realcc.h"
#include "Stl3Util/base/FtfLog.h"

#include <time.h>
#include <errno.h>
#include <netinet/in.h>
#include <unistd.h>

#ifdef GL3ONLINE
#include <sys/types.h>
#include <sys/socket.h>
#include <fcntl.h>
#endif

//VP extern int errno;


//####################################################################
//
//####################################################################
gl3Conductor::gl3Conductor (void){
    unsigned int start = realcc();
    sleep(1);
    unsigned int stop = realcc();


    ccPerMs = (double)(stop-start)/1000.;

    //ftfLog("gl3Conductor: estimated CPU freq: %7.3f MHz\n", ccPerMs/1000.);

    nAlgorithms = 0;
}

//####################################################################
//
//####################################################################
gl3Conductor::~gl3Conductor ( ) {

    if ( event       != NULL ) delete[] event ;
    clearAlgorithms();
    if ( algorithm   != NULL ) delete   algorithm;
    if ( summary     != NULL ) delete[] summary ;
    if ( summaryData != NULL ) free    (summaryData) ;
    delete []tokenIndex ;
}

//####################################################################
//
//####################################################################
int gl3Conductor::configure (L3_CFG *cfg)
{
    clearAlgorithms(); 

    if(cfg == NULL || cfg == (L3_CFG *)-1) {
	ftfLog("gl3Conductor::configure: No configuration information found.\n");
	return 1;
    }
 
    L3_ALGORITHMS *algos = cfg->l3_setup.gl3_algorithms;

    for (int i=0; i<GL3_ALG_MAX_NUM; i++) {
	if(ntohl(algos[i].alg_id) != 0) {
	    gl3Algorithm *algo = 
		gl3InstantiateAlgorithm(ntohl(algos[i].alg_id));

	    if (algo == NULL) {
		ftfLog("gl3Conductor::configure: Instantiation of algorithm #%d failed.\n", i);
	       
		return 1;
	    }
	    
	    algo->setScaling(ntohl(algos[i].preScale), 
			     ntohl(algos[i].postScale)); 

	    if (algo->setParameters(ntohl(algos[i].GI1), 
				    ntohl(algos[i].GI2), 
				    ntohl(algos[i].GI3), 
				    ntohl(algos[i].GI4), 
				    ntohl(algos[i].GI5), 
				    fswap(algos[i].GF1),
				    fswap(algos[i].GF2),
				    fswap(algos[i].GF3),
				    fswap(algos[i].GF4),
				    fswap(algos[i].GF5))) {
		ftfLog("gl3Conductor::configure: Invalid parameters for algorithm %s (ID %d)", algo->getAlgorithmName(), algo->getAlgorithmID());
		return 1;
	    }		


	    if(add(algo) != 0) {
		ftfLog("gl3Conductor::configure: Appending algorithm %s (ID %i) to list failed.\n", 
		       algo->getAlgorithmName(), algo->getAlgorithmID());
		return 1;
	    }

	    algo->showConfiguration();

	}
    }
    
    //ftfLog("gl3Conductor::configure: Done.\n");
    return 0;
}

//####################################################################
//
//####################################################################
int gl3Conductor::add( gl3Algorithm* module ) {
    if ( nAlgorithms >= maxAlgorithms ) {
	ftfLog ( "gl3Conductor::add: Max. number of algorithms reached\n" ) ;
	return 1;
    }
    algorithm[nAlgorithms] = module ;
    nAlgorithms++ ;

    return 0 ;
}

//####################################################################
//
//####################################################################
void gl3Conductor::clearAlgorithms()
{
    for (int i =0; i<nAlgorithms; i++) {
 	if(algorithm[i]) { 
 	    delete algorithm[i];
 	    algorithm[i] = NULL;
 	}
    }
    
    nAlgorithms = 0;
}

//####################################################################
//
//####################################################################

int gl3Conductor::checkHistoRequest (  ) {
#ifdef GL3ONLINE
    struct sockaddr_in remoteAddress; /* connector's address information */
    socklen_t  sin_size = sizeof(struct sockaddr_in);

    if ((remoteSocket = accept(socketFd, (struct sockaddr *)&remoteAddress, 
			       &sin_size)) == -1) {
	return 0;
    }

    const int maxBytes = 100000 ;
    char* buffer = new char[maxBytes];
   
    int nBytes = writeHistos ( maxBytes, buffer ) ;

    if ( nBytes < 0 ) {
	ftfLog ( "gl3Conductor::checkHistoRequest: buffer too small \n " ) ;
	return 1 ;
    }
    int nSend = send(remoteSocket, buffer, nBytes, 0 ) ;
    ftfLog ( "gl3Conductor: %d out of %d bytes sent\n ", nSend, nBytes ) ;
    if ( nSend == -1) {
	perror("send");
	return 1 ;
    }
    delete []buffer ;
#endif
    return 0 ;
}

//####################################################################
//
//####################################################################
int gl3Conductor::end  (  ){
#ifdef GL3ONLINE
    close ( socketFd ) ; 
#endif
    for ( int i = 0 ; i < nAlgorithms ; i++ ) {
	algorithm[i]->end( );
    }
    return 0 ;
}


//####################################################################
//
//####################################################################
gl3Event* gl3Conductor::getEvent ( int token ) {

    int index = getTokenIndex(token) ;

    if (index < 0)
	return NULL;
    else
	return &(event[index]) ;
}


//####################################################################
//
//####################################################################
int gl3Conductor::init  (  ){

    ftfLog("gl3Conductor::init: %d algos\n", nAlgorithms);
    
#ifdef GL3ONLINE
    l3Rates = new gl3Histo("l3rates", 
			   "L3 trigger rates",
			   nAlgorithms+2, -2.5, (double)nAlgorithms-0.5);

    histoList.push_back(l3Rates); 

    allocateTimingHistos();
#endif

    for ( int i = 0 ; i < nAlgorithms ; i++ ) {
	algorithm[i]->init();
    }

    nReco = 0;
    return 0 ;
}


//####################################################################
//
//####################################################################

int  gl3Conductor::processEvent(EventDescriptor *desc, 
                                L3_P* l3data,
                                TrgSumData* trgSum, 
                                RawTrgDet* trgRaw)
{
    resetTimer();
    
    // If we get a EventDescriptor, it should be for the right token
    if ( desc && desc->token != l3data->bh.token ) {
        ftfLog ("gl3Conductor::readEvent: token mismatch (%d and %d)\n", 
                desc->token, l3data->bh.token);
      return -1;
    }

    int token = l3data->bh.token;

  

    if ( token < 0 || token > maxTokens ){
        ftfLog ("gl3Conductor::readEvent: %d token out of bounds\n", token );
        return -1;
    }

    //   Look for a free event container
    int index = getFreeEventIndex();
    if ( index < 0 ) {
        ftfLog ( "gl3Conductor::readEvent: No free event container \n" ) ;
        return -1;
    }
    tokenIndex[token] = index;

       //  Read Event and check if errors occured

    if (event[index].readEventDescriptor(desc) != 0) {
        ftfLog ( "gl3Conductor::processEvent: error reading EventDescriptor\n" ) ;
        return -1;
    }


    if ( event[index].getTrgCmd() == 0 ||
         event[index].getTrgCmd() == 1 ||
         event[index].getTrgCmd() == 2 ||
         event[index].getTrgCmd() == 3 ) {
      ftfLog ("gl3Conductor::processEvent: Trigger Command 0,1,2 or 3 received\n");
      return 2; // hard trigger
    }

    if ( event[index].getTrgCmd() == 5 ||
         event[index].getTrgCmd() == 6 ||
         event[index].getTrgCmd() == 7 ) {
      ftfLog ("gl3Conductor::processEvent: Trigger Command 5,6 or 7 received - unknown physics run type\n");
      // continue reading data and running algorithms
    }

    // Read L3 data 
    if ( event[index].readL3Data(l3data) != 0 ) {
        ftfLog ( "gl3Conductor::processEvent: error reading L3 data\n" ) ;
        return -1;
    }


    // Do not ontinue (run algorithms) if we have a laser or pulser event.
    // Return decision = 1 to ensure the event is built.
    if ( event[index].getTrgCmd() >= 8 ) {
        // Laser trigger
      return 2; // hard trigger
    }

    timingMark(); // step 1

    if ( event[index].readTrgData(trgSum, trgRaw) != 0 ) {
        ftfLog ( "gl3Conductor::processEvent: error reading TRG data\n" ) ;
        return -1;
    }


    timingMark(); // step 2

    // The preparation is done, now let's run the algorithms.
    int decision = 0;
    
    for ( int i = 0 ; i < nAlgorithms ; i++ ) {
        int alg_decision = algorithm[i]->process( &(event[index]));
        if (alg_decision > decision) {
            decision = alg_decision;
        }
        timingMark();  // step 3...2+nAlgorithms
    }

    // Now increment the counters for the algorithms. Can't be done 
    // earlier, because if an event crashes, we do not want to have 
    // any counters incremented
    nReco++;
    for ( int i = 0 ; i < nAlgorithms ; i++ ) {
        algorithm[i]->incrementCounters();
    }

    collectSummary(token);

    timingMark(); // step 4+nAlgorithms

    fillTimingHistos();

#ifdef GL3ONLINE
    l3Rates->Fill(-2); // event was processed
    if (decision) {
        l3Rates->Fill(-1); // event was triggered for any reason
        
        for (int i=0; i<nAlgorithms;i++) {

            if (summaryData[index].alg[i].accept)
                l3Rates->Fill(i); // accepted because of algorithm #i

        //     cout << "alg #" << i << ": " 
//               << summaryData[index].alg[i].accept 
//               << "   histo: " << l3Rates->GetY(i+2)
//               << endl;


        }
    }

    if ( communicationsFlag ) {
        checkHistoRequest();
    }
#endif

    if (nAlgorithms == 0) decision=1; // no algs -> trigger everything

    return decision ;
}




//####################################################################
// Copy the previously filled summary data to the requested locations
//####################################################################
int gl3Conductor::fillSummary(int token,
			      struct L3_summary* summaryDest, 
			      struct L3_SUMD *summaryDataDest)
{
    int index = getTokenIndex(token);
    if (index < 0) return 1;

    memcpy(summaryDest, &summary[index], sizeof(L3_summary));

    memcpy(summaryDataDest, &summaryData[index], 
	   summaryData[index].bh.length*4);

    return 0;
}

//####################################################################
// Fill summary information for a given token
//####################################################################
int gl3Conductor::collectSummary(int token)
{
    int index = tokenIndex[token];
    if (index < 0) return 1;

    sprintf(summaryData->bh.bank_type, CHAR_L3_SUMD);
    summaryData[index].bh.length = 
	(sizeof(L3_SUMD) + (nAlgorithms-1)*sizeof(algorithm_data))/4;
    summaryData[index].bh.bank_id       = 0;
    summaryData[index].bh.format_ver    = 0;
    summaryData[index].bh.byte_order    = DAQ_RAW_FORMAT_ORDER;
    summaryData[index].bh.format_number = 0;
    summaryData[index].bh.token         = token;
    summaryData[index].bh.w9            = DAQ_RAW_FORMAT_WORD9;
    summaryData[index].bh.crc           = 0;

    summaryData[index].nProcessed       = 0;
    summaryData[index].nReconstructed   = nReco;
    summaryData[index].nAlg             = nAlgorithms;

    summary[index].accept  = 0;
    summary[index].build   = 0;
    summary[index].on      = 0;

    for (int i = 0; i < nAlgorithms; i++) {
	algorithm[i]->fillSummary(&summaryData[index].alg[i]);

	if(summaryData[index].alg[i].on) 
	    summary[index].on |= 1<<i;
	
	if(summaryData[index].alg[i].accept) 
	    summary[index].accept |= 1<<i;
	
	if(summaryData[index].alg[i].build) 
	    summary[index].build |= 1<<i;
 
    }

    summary[index].nTracks = event[index].getNTracks();
    
    return 0;
}

//####################################################################
//
//####################################################################
int gl3Conductor::releaseToken ( int token ) {
    int index = getTokenIndex(token);
    if (index < 0) return 1;

    event[index].resetEvent();
    return 0;
}
//####################################################################
//
//####################################################################
int gl3Conductor::resetHistos (  ) {
    list<gl3Histo*>::iterator histo;
    for(histo=histoList.begin(); histo!=histoList.end(); histo++) {
	(*histo)->Reset();

    }

    return 0 ;
}//####################################################################
//
//####################################################################
int gl3Conductor::runStart ( int _runNumber ) {
    runNumber = _runNumber ;
    for ( int i = 0 ; i < nAlgorithms ; i++ ) {
	algorithm[i]->init();
    }

    nReco = 0;
    return resetHistos ( ) ;
}
//####################################################################
//
//####################################################################
int gl3Conductor::runEnd (  ) {
    for ( int i = 0 ; i < nAlgorithms ; i++ ) {
	algorithm[i]->end( );
    }
    return 0 ;
}
//####################################################################
//
//####################################################################
int gl3Conductor::setCommunications (  ){
    //
#ifdef GL3ONLINE
    struct sockaddr_in gl3Address;    /* my address information */

    gl3Port = 3333 ;
    int backLog = 5 ; // how many pending connections will hold
    
    if ((socketFd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
	ftfLog("setCommunications socket: %s",strerror(errno) );
	return -1;
    }
    fcntl(socketFd, F_SETFL, O_NONBLOCK);


    gl3Address.sin_family = AF_INET;        /* host byte order */
    gl3Address.sin_port = htons(gl3Port);   /* short, network byte order */
    gl3Address.sin_addr.s_addr = INADDR_ANY;/* automatically fill with my IP */
    bzero(&(gl3Address.sin_zero), 8);       /* zero the rest of the struct */

    if (bind(socketFd, (struct sockaddr *)&gl3Address, 
	     sizeof(struct sockaddr)) == -1) {
	ftfLog("setCommunications bind: %s",strerror(errno) );
	return -1;  
    }

    if (listen(socketFd, backLog) == -1) {
	ftfLog("setCommunications listen: %s",strerror(errno) );
	return -1;
    }
#endif
    return 0 ;
}

//####################################################################
//
//####################################################################
void gl3Conductor::setBField ( float bField ){
    for ( int i = 0 ; i < maxEvents ; i++ ) {
	event[i].setBField ( bField ) ;
    }
}
//####################################################################
//
//####################################################################
void gl3Conductor::setHitProcessing ( int hitPro ){
    hitProcessing = hitPro ;
    for ( int i = 0 ; i < maxEvents ; i++ ) {
	event[i].setHitProcessing ( hitPro ) ;
    }
}
//####################################################################
//
//####################################################################
void gl3Conductor::setMaxSectorNForTrackMerging ( int _maxSectorNForTrackMerging ){
    maxSectorNForTrackMerging = _maxSectorNForTrackMerging ;
    for ( int i = 0 ; i < maxEvents ; i++ ) {
	event[i].setMaxSectorNForTrackMerging ( _maxSectorNForTrackMerging ) ;
    }
}
//####################################################################
//
//####################################################################
int gl3Conductor::setup ( St_l3_Coordinate_Transformer* _trans,
                          int maxEventsIn, int maxAlgorithmsIn ) {
    //
    communicationsFlag = 1 ;
    event     = 0 ;
    algorithm  = 0 ;
    runNumber = 0 ;
    maxTokens = 4096 ;
    tokenIndex = new int[maxTokens+1];


    maxEvents         = maxEventsIn ;
    maxAlgorithms     = maxAlgorithmsIn ;

    event             = new gl3Event[maxEvents] ;
    summary           = new L3_summary[maxEvents];

    summaryData = (L3_SUMD*) malloc( maxEvents * (sizeof(L3_SUMD) + 
		     (maxAlgorithms-1)*sizeof(algorithm_data)));

    algorithm         = new pGl3Algorithm[maxAlgorithms] ; 
    for (int i=0; i<maxAlgorithms;i++) algorithm[i]=NULL;

    nEvents           = 0 ;
    nAlgorithms       = 0 ;
    //
    for ( int i = 0 ; i < maxTokens ; i++ ) tokenIndex[i] = 0 ; 

    //
    //   Setup communications
    //
#ifdef GL3ONLINE
    if ( communicationsFlag ){ 
   	if(setCommunications() )
	    return -1;
    }
#endif

    hitProcessing = 0 ;
    for ( int i = 0 ; i < maxEvents ; i++ ) {
	event[i].setHitProcessing ( hitProcessing ) ; 
	event[i].setCoordinateTransformer ( _trans ) ; 
    }
    
    ftfLog("analysis framework set up\n");

    return 0 ;
}
//####################################################################
//
//####################################################################
int gl3Conductor::writeHistos ( int maxBytes, char *buffer )
{

    gl3HistoContainer* container = (gl3HistoContainer *)buffer ;

    ftfLog ( "nHistos %d \n", histoList.size() ) ;
    container->runNumber = runNumber ;
    container->nHistos = histoList.size();
   
    char* pointer = (char *)&(container->buffer) ;
    char* endBuffer = buffer + maxBytes ;
    int nBytes ;
    int nTotalBytes = 6; // offset coming from gl3HistoContainer
    //gl3Histo hTest ;

    list<gl3Histo*>::iterator histo;
    for(histo=histoList.begin(); histo!=histoList.end(); histo++) {

	nBytes = (*histo)->Write ( endBuffer-pointer, pointer ) ;
	if ( nBytes <= 0 ) {
	    ftfLog ( "gl3Container::writeHistos %d byte long buffer is too short \n", maxBytes ) ;
	    return 0 ;
	}
	// ftfLog ( "nBytes written %d pointer %x \n", nBytes, pointer ) ;
	nTotalBytes += nBytes ;
	if ( nTotalBytes > maxBytes ) {
	    ftfLog ( "gl3Conductor::writeHistos: nTotalBytes %d max %d\n", nTotalBytes, maxBytes ) ;
	    return -1 ;
	}
	pointer += nBytes * sizeof(char) ;
    }
    container->nBytes = nTotalBytes ;
    ftfLog ( "gl3Conductor::writeHistos: histos %d Bytes %d \n", 
	     histoList.size(), nTotalBytes ) ;

    return nTotalBytes ;
} 



//####################################################################
//
//####################################################################
int gl3Conductor::getFreeEventIndex()
{
    int freeEventIndex = -1 ;
    for ( int i = 0 ; i < maxEvents ; i++ ) {
	if ( !(event[i].getBusy() ) ) {
	    freeEventIndex = i ;
	    nEvents++ ;
	    break ;
	}
    }
    
    return freeEventIndex;
}

//####################################################################
//
//####################################################################
int gl3Conductor::getTokenIndex(int token)
{
    if (token < 0  ||  token > 4096) {
	ftfLog("gl3Conductor: token [%d] out of bounds \n", token );
	return -1;
    }

    int index = tokenIndex[token];
    
    if (index < 0 || index >= maxTokens) {
	ftfLog("gl3Conductor: event index %d out of bounds \n", token );
	return -2;
    }

    return index;
};



//####################################################################
//
//####################################################################
void gl3Conductor::resetTimer()
{
    lastTimeEntry=0;
    cpuTimes.clear();
    realTimes.clear();
    
    cpuTimes.reserve(5+nAlgorithms);
    realTimes.reserve(5+nAlgorithms);

    timingMark();
}

void gl3Conductor::timingMark()
{
    cpuTimes[lastTimeEntry] =  clock();
    realTimes[lastTimeEntry] =  realcc();

    lastTimeEntry++;
}

void gl3Conductor::allocateTimingHistos()
{
#ifdef GL3ONLINE
    timingHistos.resize(3+nAlgorithms);

    timingHistos[0] = new gl3Histo("time_read_l3", 
				   "Timing: read L3 data",
				   50,0,300); 

    timingHistos[1] = new gl3Histo("time_read_trg", 
				   "Timing: read TRG data",
				   50,0,100); 

    char hid[30], htitle[100];
    for(int i=0; i<nAlgorithms;i++) {
      
      sprintf(hid, "time_alg_%d", i);
      sprintf(htitle, "Timing: algorithms %d", i);
      timingHistos[2+i] = new gl3Histo(hid, htitle, 
				       60,0,30); 
    }
	  
    timingHistos[2+nAlgorithms] = new gl3Histo("time_total", 
				   "Timing: total",
				   50,0,300); 

    

    for(int i=0;i<timingHistos.size();i++) {
	histoList.push_back(timingHistos[i]);
    }
#endif
}

void gl3Conductor::fillTimingHistos()
{
#ifdef GL3ONLINE
    timingHistos[0]->Fill(double(realTimes[1] - realTimes[0])/ccPerMs);
    timingHistos[1]->Fill(double(realTimes[2] - realTimes[1])/ccPerMs);

    for(int i=0; i<nAlgorithms;i++) {
      timingHistos[2+i]->Fill(double(realTimes[3+i] - realTimes[2+i])/ccPerMs);
    }

    timingHistos[2+nAlgorithms]->
      Fill(double(realTimes[lastTimeEntry-1] - realTimes[0])/ccPerMs);

    ftfLog("total time: %f\n", 
	   double(realTimes[lastTimeEntry-1] - realTimes[0])/ccPerMs);
#endif
}

void gl3Conductor::printTiming()
{
    for (int i=1; i<lastTimeEntry;i++) {
	ftfLog("gl3Conductor: timing step %2d: CPU: %8f    real: %8f\n",
	       i, (double)cpuTimes[i]-cpuTimes[i-1]/CLOCKS_PER_SEC, 
	       (double)(realTimes[i]-realTimes[i-1])/ccPerMs);
    }
}




//####################################################################
//
//####################################################################
// void gl3Conductor::printTiming (  ){
//     ftfLog ( "********************************\n" ) ;
//     ftfLog ( "* gl3 timing                   *\n" ) ;
//     ftfLog ( "* Total:  Real  %5.0f (ms) \n", 1000.*totalRealTime ) ;
//     ftfLog ( "*         CPu   %5.0f (ms) \n", 1000.*totalCpuTime ) ;
//     ftfLog ( "* Algorithm     Real     Cpu    *\n" ) ;
//     ftfLog ( "*               ( )     ( )    *\n" ) ;
//     for ( int i = 0 ; i < nAlgorithms ; i++ ) {
// 	ftfLog ( "*    %d      %7.1f  %7.1f   *\n", 
// 		 i, 100.*algorithmRealTime[i]/totalRealTime, 
// 		 100.*algorithmCpuTime[i]/totalCpuTime ) ; 
//     }
//     ftfLog ( "********************************\n" ) ;
// }

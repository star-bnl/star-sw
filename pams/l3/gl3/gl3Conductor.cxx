//:>------------------------------------------------------------------
//: FILE:       gl3Conductor.cc
//: HISTORY:
//:              1feb2000 version 1.00
//:<------------------------------------------------------------------
#include "gl3Conductor.h"


//####################################################################
//
//####################################################################
gl3Conductor::gl3Conductor ( int maxEventsIn, int maxAnalysisIn ) {
   communicationsFlag = 1 ;
   event     = 0 ;
   analysis  = 0 ;
   maxTokens = 4096 ;
   tokenIndex = new int[maxTokens+1];
   setup ( maxEventsIn, maxAnalysisIn ) ;
}
//####################################################################
//
//####################################################################
gl3Conductor::~gl3Conductor ( ) {
   if ( event      != NULL ) delete[] event ;
   if ( analysis   != NULL ) delete[] analysis ;
   delete []tokenIndex ;
}
//####################################################################
//
//####################################################################
int gl3Conductor::add( gl3Analysis* module ) {
   if ( nAnalyses >= maxAnalyses ) {
      fprintf ( stderr, "gl3Conductor::add: Max. number analysis reached\n" ) ;
      return 1;
   }
   analysis[nAnalyses] = module ;
   nAnalyses++ ;

   return 0 ;
}
//####################################################################
//
//####################################################################

int gl3Conductor::checkHistoRequest (  ) {
#ifdef GL3ONLINE
   struct sockaddr_in remoteAddress; /* connector's address information */
    socklen_t  sin_size = sizeof(struct sockaddr_in);

   if ((remoteSocket = accept(socketFd, (struct sockaddr *)&remoteAddress, &sin_size)) == -1) {
      return 0;
   }

   int maxBytes = 100000 ;
   char* buffer = new char[maxBytes];
   
   int nBytes = writeHistos ( maxBytes, buffer ) ;

   if ( nBytes < 0 ) {
      printf ( "gl3Conductor::checkHistoRequest: buffer too small \n " ) ;
      return 1 ;
   }
   int nSend = send(remoteSocket, buffer, nBytes, 0 ) ;
   printf ( " N bytes sent %d send request %d \n ", nSend, nBytes ) ;
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
gl3Event* gl3Conductor::getEvent ( int token ) {
//
//   Check token and store event index in tokenIndex 
//
   gl3Event* eventP = 0 ;
   if ( token < 0 || token > maxTokens ){
      fprintf ( stderr, "gl3Conductor::getEvent: %d token out of bounds \n", token ) ;
      return eventP ;
   }
   int index = tokenIndex[token] ;
//
//   Check index
//
   if ( index < 0 || index >= maxEvents ){
      fprintf ( stderr, "gl3Conductor::getEvent: %d event index out of bounds \n", index ) ;
      return eventP ;
   }
   return &(event[index]) ;
}
//####################################################################
//
//####################################################################
int gl3Conductor::init  (  ){
//
   for ( int i = 0 ; i < nAnalyses ; i++ ) {
      analysis[i]->init( &histoList );
   }

   return 0 ;
}
//####################################################################
//
//####################################################################
gl3EventDecision gl3Conductor::processEvent  ( int maxLength, char* buffer ){
//
//   Look for free event container
//
   int i ;
   gl3EventDecision decision ;

   int freeEventIndex = -1 ;
   for ( i = 0 ; i < maxEvents ; i++ ) {
      if ( !(event[i].getBusy() ) ) {
	 freeEventIndex = i ;
	 nEvents++ ;
	 break ;
      }
   }
//
//    Check there is a free event container
//
   if ( freeEventIndex == -1 ) {
      fprintf ( stderr, "gl3Conductor::readEvent: No free event container \n" ) ;
      return decision ;
   }
//
// Get token
//
   L3_P* header = (L3_P *)buffer ;
   int token    = header->bh.token ;
//
//   Check token and store event index in tokenIndex 
   if ( token < 0 || token > maxTokens ){
      fprintf ( stderr, "gl3Conductor::readEvent: %d token out of bounds \n", token ) ;
      return decision ;
   }
   tokenIndex[token] = freeEventIndex ;
//
//    Read Event
//
   event[freeEventIndex].readEvent ( maxLength, buffer ) ;
//
//    Run analysis modules
//
   int result = 0 ;
   for ( i = 0 ; i < nAnalyses ; i++ ) {
      result = analysis[i]->process( &(event[freeEventIndex]) );
      if ( i > 0 && result ) {
         decision.accept |= mask[i-1] ; 
         decision.build  |= mask[i-1] ; 
      }
//    printf ( " %d result %d decision %x %d\n",i, result, decision.accept, decision.accept ) ;
   }
//
//   Check histo request
//
#ifdef GL3ONLINE
   if ( communicationsFlag ) checkHistoRequest();
#endif

   return decision ;
}
//####################################################################
//
//####################################################################
int gl3Conductor::releaseToken ( int token ) {
//
//   Check token and store event index in tokenIndex 
//
   if ( token < 0 || token > maxTokens ){
      fprintf ( stderr, "gl3Conductor::getEvent: %d token out of bounds \n", token ) ;
      return 1 ;
   }
   int index = tokenIndex[token] ;
//
//   Check index
//
   if ( index < 0 || index >= maxEvents ){
      fprintf ( stderr, "gl3Conductor::getEvent: %d event index out of bounds \n", index ) ;
      return 1 ;
   }
   event[index].resetEvent() ;
   return 0 ;
}
//####################################################################
//
//####################################################################
int gl3Conductor::setCommunications (  ){
//
#ifdef GL3ONLINE
   struct sockaddr_in gl3Address;    /* my address information */

   gl3Port = 9999 ;
   int backLog = 5 ; // how many pending connections will hold
    
   if ((socketFd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
      perror("socket");
      exit(1);
   }
   fcntl(socketFd, F_SETFL, O_NONBLOCK);


   gl3Address.sin_family = AF_INET;         /* host byte order */
   gl3Address.sin_port = htons(gl3Port);     /* short, network byte order */
   gl3Address.sin_addr.s_addr = INADDR_ANY; /* automatically fill with my IP */
   bzero(&(gl3Address.sin_zero), 8);        /* zero the rest of the struct */

   if (bind(socketFd, (struct sockaddr *)&gl3Address, 
                       sizeof(struct sockaddr)) == -1) {
      perror("bind");
      exit(1);
   }

   if (listen(socketFd, backLog) == -1) {
      perror("listen");
      exit(1);
   }
#endif
   return 0 ;
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
int gl3Conductor::setup ( int maxEventsIn, int maxAnalysesIn ) {
//
   maxEvents   = maxEventsIn ;
   maxAnalyses = maxAnalysesIn ;
   event       = new gl3Event[maxEvents] ;
   analysis    = new pGl3Analysis[maxAnalyses] ; 
   nEvents     = 0 ;
   nAnalyses   = 0 ;
//
   int i ;
   for ( i = 0 ; i < maxTokens ; i++ ) tokenIndex[i] = 0 ; 
   printf ( "after event setup \n" ) ;
//
//   Setup masks for bit manipulation
//
   mask[0] = 1 ;
   for ( i = 1 ; i < 16 ; i++ ) mask[i] = mask[i-1]*2 ;
//
//   Setup communications
//
#ifdef GL3ONLINE
   if ( communicationsFlag ) setCommunications();
#endif
   hitProcessing = 0 ;
   for ( i = 0 ; i < maxEvents ; i++ ) event[i].setHitProcessing ( hitProcessing ) ; 

   return 0 ;
}
//####################################################################
//
//####################################################################
int gl3Conductor::writeHistos ( int maxBytes, char *buffer ){
   gl3Histo* hPointer ;

   gl3HistoContainer* container = (gl3HistoContainer *)buffer ;

   printf ( "nHistos %d \n", histoList.size() ) ;
   container->nHistos = histoList.size();
   
   char* pointer = (char *)&(container->buffer) ;
   char* endBuffer = buffer + maxBytes ;
   int nBytes ;
   int nTotalBytes = 0 ;
   gl3Histo hTest ;

   for( hPointer = (gl3Histo *)histoList.first(); 
	 hPointer != 0; 
	 hPointer = (gl3Histo *)histoList.next() ) {

      nBytes = hPointer->Write ( endBuffer-pointer, pointer ) ;
      if ( nBytes <= 0 ) {
         printf ( "gl3Container::writeHistos %d byte long buffer is too short \n", maxBytes ) ;
	 return 0 ;
      }
//    printf ( "nBytes written %d pointer %x \n", nBytes, pointer ) ;
      nTotalBytes += nBytes ;
      if ( nTotalBytes > maxBytes ) {
         fprintf ( stderr, "gl3Conductor::writeHistos: nTotalBytes %d max %d\n",
	           nTotalBytes, maxBytes ) ;
         fprintf ( stderr, "stop \n" ) ;
	 return -1 ;
      }
      pointer += nBytes * sizeof(char) ;
   }
   container->nBytes = nTotalBytes ;
   printf ( " gl3Conductor::writeHistos: histos %d Bytes %d \n", 
              histoList.size(), nTotalBytes ) ;

   return nTotalBytes ;
} 

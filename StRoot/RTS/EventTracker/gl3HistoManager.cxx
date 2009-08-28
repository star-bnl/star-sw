#include "gl3HistoManager.h"

#include <rtsLog.h>

#include <errno.h>
#include <netinet/in.h>
#include <unistd.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <fcntl.h>
#include <string.h>

#include <iostream>

using namespace std;

// ###########################################################
// # Instantiation
// ###########################################################
gl3HistoManager *gl3HistoManager::pinstance = 0; 

gl3HistoManager::gl3HistoManager()
{
    histoList.clear();

    verbosity = warn;
    maxBytes = 300000;
}


gl3HistoManager *gl3HistoManager::instance()
{
    if (pinstance == 0) {  
	pinstance = new gl3HistoManager; // create sole instance
    }
    return pinstance; // address of sole instance
}


// ###########################################################
// # Histogram handling
// ###########################################################
void gl3HistoManager::add(gl3Histo *histo)
{
    histoList.push_back(histo);

    if (verbosity >= dbg) {
	cout << "gl3HistoManager: Histogram '" 
	     << histo->header.title << "' added." << endl;
    }
}

void gl3HistoManager::remove(gl3Histo *histo)
{
    histoList.remove(histo);
}


int gl3HistoManager::resetHistos (  ) {
    list<gl3Histo*>::iterator histo;
    for(histo=histoList.begin(); histo!=histoList.end(); histo++) {
	(*histo)->Reset();
    }
    return 0 ;
}


int  gl3HistoManager::saveHistos(char * filename)
{
    char* buffer = new char[maxBytes];
    
    int nBytes = writeHistos (buffer) ;
    
    if(verbosity >= dbg)
      LOG(ERR, "gl3Conductor::saveHistos: writing to %s\n",
		 filename,0,0,0,0) ;
	

    if ( nBytes < 0 ) {
	if(verbosity >= err)
	    LOG(ERR, "gl3Conductor::saveHistos: buffer too small\n",0,0,0,0,0);
	return 1 ;
    }
    

    int fd = open(filename, O_RDWR|O_CREAT, 00644); 
    if ( fd<0 ) {
	if(verbosity >= err)
	    LOG(ERR, "gl3Conductor::saveHistos: unable to open file %s \n",
		   filename,0,0,0,0) ;
	return 1 ;
    }
    	

    if (write(fd,buffer,nBytes) != nBytes) {
	if(verbosity >= err)
	    LOG(ERR, "gl3Conductor::saveHistos: write to file %s failed\n",
		   filename,0,0,0,0) ;
	return 1 ;
    }
    
    delete[] buffer;

    close(fd);
    
    return 0;
}


// ###########################################################
// # Networking / remote requests
// ###########################################################

int gl3HistoManager::listenAtPort(int port)
{
    if (is_listening) return 0;

    struct sockaddr_in gl3Address;    /* my address information */

    int backLog = 5 ; // how many pending connections will hold
    
    if ((socketFd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
	LOG(ERR, "gl3HistoManager::listen: error opening socket: %s\n",
	       strerror(errno) ,0,0,0,0);
	return -1;
    }
    fcntl(socketFd, F_SETFL, O_NONBLOCK);
    int optval = 1;
    if(setsockopt(socketFd, SOL_SOCKET, 
 		  SO_REUSEADDR, (void *)&optval, sizeof(optval)) == -1) {
 	LOG(ERR, "gl3HistoManager::listen: setsockopt: %s\n", strerror(errno),0,0,0,0);
 	return -1;
    }


    gl3Address.sin_family = AF_INET;         // host byte order
    gl3Address.sin_port = htons(port);       // short, network byte order
    gl3Address.sin_addr.s_addr = INADDR_ANY; // automatically fill with my IP
    bzero(&(gl3Address.sin_zero), 8);        // zero the rest of the struct

    
    if (bind(socketFd, (struct sockaddr *)&gl3Address, 
	     sizeof(struct sockaddr)) == -1) {
	LOG(ERR, "gl3HistoManager::listen: bind: %s\n",strerror(errno) ,0,0,0,0);
	return -1;  
    }

    if (listen(socketFd, backLog) == -1) {
	LOG(ERR, "gl3HistoManager::listen: listen: %s\n",strerror(errno) ,0,0,0,0);
	return -1;
    }

    is_listening = true;
    return 0 ;
}


int gl3HistoManager::checkRequest (  ) {

    struct sockaddr_in remoteAddress; /* connector's address information */
    socklen_t  sin_size = sizeof(struct sockaddr_in);

    if ((remoteSocket = accept(socketFd, (struct sockaddr *)&remoteAddress, 
			       &sin_size)) == -1) {
	return 0;
    }
    
    //const int maxBytes = 200000 ;
    char* buffer = new char[maxBytes];
   
    int nBytes = writeHistos(buffer) ;

    if ( nBytes < 0 ) {
      LOG(ERR, "gl3Conductor::checkHistoRequest: buffer too small \n ",0,0,0,0,0 ); 
	return 1 ;
    }
    int nSend = send(remoteSocket, buffer, nBytes, 0 ) ;
    LOG(ERR, "gl3Conductor: %d out of %d bytes sent\n ", nSend, nBytes ,0,0,0) ;
    if ( nSend == -1) {
	perror("send");
	return 1 ;
    }
    delete []buffer ;

    return 0 ;
}



// ####################################################################
// # write histos to a buffer
// ####################################################################
int gl3HistoManager::writeHistos (char *buffer){

    // I don't know how to get the run number, so I'll set it to a 
    // nonsense value;
    int runNumber = -1;

    //gl3Histo* hPointer ;
    
    gl3HistoContainer* container = (gl3HistoContainer *)buffer ;
    container->runNumber = runNumber ;
    container->nHistos = histoList.size();
   
    char* pointer = (char *)&(container->buffer) ;
    char* endBuffer = buffer + maxBytes ;
    int nBytes ;
    int nTotalBytes = sizeof(gl3HistoContainer) - sizeof(int);

    list<gl3Histo*>::iterator histo;
    for(histo=histoList.begin(); histo!=histoList.end(); histo++) {
	
	if (verbosity >= dbg) {
	    cout << "gl3HistoManager: Histogram '" 
		 << (*histo)->header.title << "' added." << endl;
	}

	nBytes = (*histo)->Write ( endBuffer-pointer, pointer ) ;
	if ( nBytes <= 0 ) {
	    
	    LOG(ERR, "gl3Container::writeHistos: buffer too short (%d bytes)\n",
		   maxBytes,0,0,0,0) ;
	    return 0 ;
	}
	nTotalBytes += nBytes ;
	if ( nTotalBytes > maxBytes ) {
	  LOG(ERR, "gl3HistoManager::writeHistos: nTotalBytes %d max %d\n", 
		     nTotalBytes, maxBytes,0,0,0 ) ;
	    return -1 ;
	}
	pointer += nBytes * sizeof(char) ;
    }
    container->nBytes = nTotalBytes ;
    if(verbosity >= note) {
      LOG(ERR, "gl3HistoManager::writeHistos: histos %d Bytes %d \n", 
		 histoList.size(), nTotalBytes ,0,0,0) ;
    }
    
    return nTotalBytes ;
}

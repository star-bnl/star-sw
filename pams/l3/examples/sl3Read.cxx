#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <errno.h> 
#include <unistd.h>
#include <sys/mman.h>
#include <time.h>
#include "/DAQ/include/daqFormats.h"
#include "FtfSl3.h"
#include "gl3Event.h"
#include "gl3Conductor.h"
#include "gl3Analysis.h"
#include "gl3GeneralHistos.h"
#include "gl3JPsi.h"
#include "gl3GammaGamma.h"
#include "gl3dEdx.h"
#include "gl3HighPt.h"
#include "gl3Svt.h"
#include "gl3Residuals.h"
#include "gl3MomRes.h"


int main( int argc, char** argv)
{
  /*Program reads event from file, retracks the data,
    and feed resulting tracks and hits to a gl3 buffer
    where data can be analyzed using gl3 modules.
    Resulting gl3Histograms are stored in file histos.bin.*/

  if(argc!=2)
    {
      printf("Usage: sl3Read file\n");
      return -1;
    }
  
  /*****************/
  /* setup tracker */
  /*****************/
  
  FtfSl3  tracker ;

  FtfPara* para = &(tracker.para);
  tracker.setup();
  tracker.para.fillTracks = 1 ;

  /* force tracker into type 3 */
  tracker.para.ptMinHelixFit  = 0. ;
  tracker.para.maxChi2Primary = 0. ;

  // Pablo's parameters for cosmics:
  tracker.para.minHitsPerTrack = 5; 
  tracker.para.hitChi2Cut = 50 ;
  tracker.para.trackChi2Cut = 30 ; 
  tracker.para.goodHitChi2 = 10 ; 
  tracker.para.dphi=0.5;
  tracker.para.deta=0.5;
  tracker.xyError = 0.3 ; 
  tracker.zError = 1.0 ; 

  tracker.reset();  

  //Open histofile:
  FILE* histoFile = fopen("histos.bin", "w");
  if ( histoFile == NULL) {
    printf ( " \n Error opening histos file \n " ) ;
    return 1 ;
  }
  

  /***************/
  /* open infile */
  /***************/

  int fd;
  int mode = 0;
  char* fileName;
  
  fd = open (argv[1], S_IREAD, 0777) ;
  //fprintf(stderr, "file: %s\n", argv[1]);
  
  if (fd < 0) {
    perror(fileName) ;
    return -1 ;
  }
  
  int fileSize = lseek(fd,0,SEEK_END);
  
  /**********/
  /* mmap() */
  /**********/
  
  void* filebuff = mmap (0, fileSize, PROT_READ, MAP_PRIVATE, fd, 0) ;

  if (filebuff==NULL) perror("mmap");

  void* evbuff = malloc(fileSize);
  memcpy(evbuff, filebuff, fileSize);
  
  /********/
  /* L3_P */
  /********/
  struct L3_P* l3p = (struct L3_P *)evbuff;  //buffer for L3 contribution
  unsigned int token;

  //intitialize gl3:
  gl3Conductor gl3 ;
  gl3.event[0].bField = 0.5 ;
  gl3GeneralHistos  fillHistoModule ;
  gl3JPsi           jPsiM ;
  gl3GammaGamma     gammaGammaM ;
  gl3dEdx           dEdxM ;
  gl3HighPt         highPtM ;
  gl3Svt            svtM ;
  gl3Residuals       resM;
  gl3MomRes momres;
  
  gl3.add ( &fillHistoModule ) ;
  gl3.add ( &jPsiM   ) ;
  gl3.add ( &gammaGammaM ) ;
  gl3.add ( &dEdxM ) ;
  gl3.add ( &highPtM ) ;
  gl3.add ( &svtM ) ;
  gl3.add ( &resM );
  gl3.add(&momres);

  gl3.init();

  //create a gl3 buffer:
  
  int nBytes;
  int maxBytes  = 9800000 ;
  int maxHitBytes  = 900000 ;
  char* trackDataPointer  ;
 
  char* buffer = new char[maxBytes];
  L3_P* gl3Header = (L3_P *)buffer ;
  int   nBytesHeader = sizeof(L3_P); 
  trackDataPointer = buffer + nBytesHeader ;
  memset ( buffer, 0, nBytesHeader*sizeof(char) ) ;
  char* endTrackBuffer = buffer + maxBytes ;
  /******************/
  /* sector loop    */
  /******************/

  for(int i=0; i<24; i++){
    
    if(l3p->sector[i].len) {
      
      //  fprintf(stderr,"Supersector %i present\n",i+1);
      
      //sectorheader
      struct L3_SECP* l3secp = 
	(struct L3_SECP*) ((char*)l3p + l3p->sector[i].off * 4);
      
      
      if (l3secp->clusterp.len) {
	
	L3_SECP *sectorHeader=(L3_SECP*)trackDataPointer;
	trackDataPointer+=sizeof(L3_SECP);
	
	/************/
	/* TPCSECLP */
	/************/
	
	struct TPCSECLP* tpcseclp = 
	  (struct TPCSECLP*) ((char*)l3secp + l3secp->clusterp.off*4);
	
	token=l3p->bh.token;
	
	tracker.readSector(tpcseclp);
	tracker.processSector();
	
	nBytes=tracker.fillTracks( l3secp->clusterp.len*4, trackDataPointer, token);
	
	//set the track offset and length:
	sectorHeader->trackp.off = (trackDataPointer-(char *)sectorHeader)/4;
	sectorHeader->trackp.len = nBytes/4;

	trackDataPointer+=nBytes;

	nBytes=tracker.fillHits (endTrackBuffer-trackDataPointer, trackDataPointer, token );
	//set cluster offset and lenght
	sectorHeader->sl3clusterp.off = (trackDataPointer-(char *)sectorHeader)/4;
	sectorHeader->sl3clusterp.len = nBytes/4;

	trackDataPointer+=nBytes;
	
	//update gl3header
	gl3Header->bh.token = token;
	gl3Header->sector[i].len = (trackDataPointer-(char*)(sectorHeader))/4;
	gl3Header->sector[i].off = ((char*)(sectorHeader)-buffer)/4;
		
      }
      
    } /* sector[i].len>0 */
    
  } /* i<24 */
  
  //Read event into gl3:
  gl3.processEvent ( nBytesHeader, buffer ) ;
  
  gl3.releaseToken ( token ) ;
  nBytes = gl3.writeHistos ( maxBytes, buffer ) ;
  fwrite ( buffer, sizeof(char), nBytes, histoFile ) ;
}

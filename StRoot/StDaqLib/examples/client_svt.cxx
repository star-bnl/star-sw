/***************************************************************************
 * $Id: client_svt.cxx,v 1.2 2014/06/25 15:33:17 jeromel Exp $
 * Author: J. Schambach
 ***************************************************************************
 * Description: sample top-level code should be used as a tutorial
 *              for any application using the StDaqLib class library
 *      
 *
 ***************************************************************************
 * $Log: client_svt.cxx,v $
 * Revision 1.2  2014/06/25 15:33:17  jeromel
 * Code not used but erradicated use of flush
 *
 * Revision 1.1  2007/01/04 21:27:52  jml
 * zero suppressed reader no longer uses adcx, only seqd.  Fixes bug from early 2005
 *
 * Revision 1.3  2003/09/02 17:55:34  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  2003/01/29 13:55:18  ward
 * Turn off memory mapped operation in the examples, which appears to fail for 2003 daq files.
 *
 * Revision 1.1  2001/04/18 19:49:32  ward
 * Added SVT example program client_zs.cxx from J. Schambach.
 *
 *
 **************************************************************************/


#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <assert.h>


#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/SVT/SVTV1P0.hh"
#include "StDaqLib/SVT/SVTV1P0_Reader.hh"

// the following extracts the filename stripped of its directory path.

int main(int argc, char *argv[])
{
  int fd;
  //client must provide
  //array of pointers to padlist (one for each pad row)
  u_char *(padlist[TPC_PADROWS]); 
  long offset = 0L;
  int histogram[50] = {50*0};
 
  if(argc<2)
  {
    printf("Usage:   %s file.daq\n",argv[0]);
    exit(2);
  }

  fd = open(argv[1],O_RDONLY);

  if (fd == -1)
  { 
    printf("Bad filename %s",argv[1]);
    exit(2);
  }

  int eventNo;
  for (eventNo=0; eventNo < 1; eventNo++ )
    {  
      EventReader *er = getEventReader(fd,offset,"log",0); // Memory-mapped operation fails for 2003 daq files.
      if(!er) 
	{
	  printf("No event reader...\n");
	  close(fd);
	  exit(0);
	  // assert(0); // This may be simply the end of the .daq file.
	}

      er->printEventInfo();
      

      // Prepare for the next event
      if(offset != -1)
	offset = er->NextEventOffset(); 
      else
	offset = -1;

  
      SVTV1P0_Reader *dr = (SVTV1P0_Reader *)getDetectorReader(er, "SVT");
      if(!dr) {
	printf("No SVT reader\n");
	close(fd);
	assert(0);
      } 
      // else printf("created SVT_Reader!!!\n");



      for(int ii=1;ii<=3;ii++) {   // barrel
	int lads[3] = { 8,12,16 };

	for(int jj=1;jj<=lads[ii-1];jj++) {  // ladders
	  int wafs[3] = { 4,6,7 };
	  
	  for(int kk=1;kk<=wafs[ii-1];kk++) {  // wafers
	   
 // 	    printf("Getting zs read (%d %d %d)\n",ii,jj,kk);

	    SVTV1P0_ZS_SR *zsr = 
	      (SVTV1P0_ZS_SR *)dr->getZeroSuppressedReader(ii,jj,kk);
	    
 // 	    printf("Got zs read (%d,%d,%d)\n",ii,jj,kk);

	    if (!zsr) {
	      //printf("zsr creation for (%d,%d,%d) failed\n", ii,jj,kk);
 	      continue;
	    }
	    
	    //else printf("Created zsr for wafer (%d,%d,%d)\n",ii,jj,kk);
	    // do something with it
	    int nSeq, anode, hybrid;
	    int nAnodes, nSpt;
	    u_char *AnodeList;
	    Sequence *Seq;
	    SpacePt *Spt;
	    
	    for(int ll=1;ll<=2;ll++) {  // hybrids
	      
	      //printf("getting padlist\n");
	      nAnodes = zsr->getPadList(ll, &AnodeList);
	      //printf("got padlist %d\n",nAnodes);

	      for(int an = 0;an<nAnodes;an++) {

		//printf("anode[%d] = %d   (0x%x)\n",an,AnodeList[an],zsr);
		int ret = zsr->getSequences(ll, AnodeList[an], &nSeq, &Seq);
		//printf("got seq %d\n",nSeq);

		for(int seq = 0;seq<nSeq;seq++) {
		  
		  for(int iii=0;iii<Seq[seq].Length;iii++) {
		    printf("%d %d %d %d %d %d %d\n",
			   ii,jj,kk,ll,AnodeList[an],
			   iii + Seq[seq].startTimeBin,
			   Seq[seq].FirstAdc[iii]);
		  }	   
		}
	      }  
	    }
	    //  printf("deleting\n");
	    delete zsr; // remove ZeroSuppressedReader
	    //printf("deleted\n");
	  }
	}
      }
      delete dr; // remove SVT reader
      
      delete er;
    }
  close(fd);
}

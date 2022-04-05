/***************************************************************************
 * $Id: client_zs.cxx,v 1.3 2003/09/02 17:55:34 perev Exp $
 * Author: J. Schambach
 ***************************************************************************
 * Description: sample top-level code should be used as a tutorial
 *              for any application using the StDaqLib class library
 *      
 *
 ***************************************************************************
 * $Log: client_zs.cxx,v $
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
#include "Stiostream.h"

#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/SVT/SVTV1P0_Reader.hh"

// the following extracts the filename stripped of its directory path.

char logfile[80];

 
char *convert_name_to_logfile(const char *filename)
{
  static char locfile[80];
  strcpy(locfile,filename);
  strcat(locfile,".log");
  char *t1;
  char *t2 = locfile;
  t1 = strtok(locfile,"/");
  while (t1!=NULL) {
    t2 = t1;
    t1 = strtok(NULL,"/");
  }
  strcpy(locfile,t2);
  return locfile;
}

int main(int argc, char *argv[])
{
  int fd;
  string filename;
  //client must provide
  //array of pointers to padlist (one for each pad row)
  u_char *(padlist[TPC_PADROWS]); 
  long offset = 0L;
  int histogram[50] = {50*0};


  if (argc==3) offset = atol(argv[2]);
 
  if(argc<2)
  {
    printf("Usage:   %s file.daq [offset]\n",argv[0]);
    printf("Example: %s /scratch/2sbWith2Rcv_from_Tape.dat\n",argv[0]);
    exit(2);
  }
  else
  {
    filename = argv[1];
  }

  strcpy(logfile,convert_name_to_logfile(filename.c_str()));

  printf(" opening log file: %s\n",logfile);

  fd = open(filename.c_str(),O_RDONLY);

  if (fd == -1)
  { 
    perror("FormatClient");
  }

  int eventNo;
  //while(offset != -1)
  for (eventNo=0; eventNo < 1; eventNo++ )
    {  
      EventReader *er = getEventReader(fd,offset,(const char *)logfile,0); // Memory-mapped operation fails for 2003 daq files.
      if(!er) 
	{
          cout << "============================================" << endl;
	  cout << "Error creating ER" << endl;
          cout << "This may be simply the end of the .daq file." << endl;
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

#define DOSVT
#define DO_RAW

#ifdef DOSVT      
      DetectorReader *dr = getDetectorReader(er, "SVT");
      if(!dr) {
	cout << "Error creating SVT_Reader: " << er->errstr() << endl;
	if (er->errorNo() == INFO_MISSING_BANK) 
	  cout << "No SVTP bank available" << endl;
	close(fd);
	assert(0);
      } 
      else printf("created SVT_Reader!!!\n");

      
#ifdef DO_RAW
      SVTV1P0_Reader *sdr = (SVTV1P0_Reader *)dr;
      int wafer = 7, barrel = 3, ladder = 1;
      ADCRawReader *adcrr = 
	sdr->getADCRawReader(barrel, ladder, wafer);

      u_char *adcVal;
      int nVal, hybrid=1, anode=241;
      int ret = adcrr->getSequences(hybrid, anode, &nVal, &adcVal); 
      printf("Found %d ADC values for anode %d, hybrid %d\n",
	     nVal, anode, hybrid);

      if (nVal > 0)
	// print the first few
	for (int ii=0; ii<11; ii++) printf("%d ", adcVal[ii]);
      printf("\n");

      delete adcrr;
#endif

#ifdef DO_PEDR
      SVTV1P0_Reader *sdr = (SVTV1P0_Reader *)dr;
      int wafer = 7, barrel = 3, ladder = 1;
      PedestalReader *pedrr = 
	sdr->getPedestalReader(barrel, ladder, wafer);

      u_char *adcVal;
      int nVal, hybrid=1, anode=241;
      int ret = pedrr->getSequences(hybrid, anode, &nVal, &adcVal); 
      printf("Found %d ADC values for anode %d, hybrid %d\n",
	     nVal, anode, hybrid);

      if (nVal > 0)
	// print the first few
	for (int ii=0; ii<11; ii++) printf("%d ", adcVal[ii]);
      printf("\n");

      delete pedrr;
#endif
#ifdef  DO_ZSR
      for(int wafer = 104; wafer < 104+5; wafer++) {
	ZeroSuppressedReader *zsr = 
	  dr->getZeroSuppressedReader(wafer);
	if (!zsr) {
	  printf("zsr creation for wafer %d failed\n", wafer);
	  continue;
	}
	else printf("Created zsr for wafer %d\n", wafer);
	// do something with it
	int nSeq, anode, hybrid;
	int nAnodes, nSpt;
	u_char *AnodeList;
	Sequence *Seq;
	SpacePt *Spt;
	hybrid=1;

	nAnodes = zsr->getPadList(hybrid, &AnodeList);
	printf("Wafer %d, hybrid %d has %d anodes with data:\n",
	       wafer, hybrid, nAnodes);
	if (nAnodes) {
	  for (int ii=0; ii<nAnodes; ii++)
	    printf("%d ", AnodeList[ii]);
	  printf("\n");
	  int ret = zsr->getSequences(hybrid, AnodeList[0], &nSeq, &Seq);
	  printf("Found %d sequences for anode %d, hybrid %d:\n",
		 nSeq, AnodeList[0], hybrid);
	  if (nSeq) { // just print out the first sequence
	    printf("Sequence 0: startTime %d, len %d, ADC values:\n",
		   Seq[0].startTimeBin, Seq[0].Length); 
	    for (int ii=0; ii<Seq[0].Length; ii++)
	      printf("%d ", Seq[0].FirstAdc[ii]);
	    printf("\n");
	  }
	  zsr->getSpacePts(hybrid, &nSpt, &Spt);
	  printf("Found %d spacepoints\n", nSpt);
	}
	
	delete zsr; // remove ZeroSuppressedReader
      }
#endif

      delete dr; // remove SVT reader
#endif

      delete er;
    }
  close(fd);
}

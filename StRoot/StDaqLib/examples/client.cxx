/***************************************************************************
 * $Id: client.cxx,v 1.4 1999/07/21 21:33:10 levine Exp $
 * Author: M.J. LeVine
 ***************************************************************************
 * Description: sample top-level code sould be used as a tutorial
 *              for any application using the StDaqLib class library
 *      
 *
 *   change log
 *  02-Jul-99 MJL remove #include "StDaqLib/TPC/MemMan.hh"
 *  09-Jul-99 MJL add calls to various non_TPC detector readers
 *  12-Jul-99 MJL add calls to getSpacePts()
 *  20-Jul-99 MJL use new EventReader constructor with log file name
 *
 ***************************************************************************
 * $Log: client.cxx,v $
 * Revision 1.4  1999/07/21 21:33:10  levine
 * changes to include error logging to file.
 *
 * There are now 2 constructors for EventReader:
 *
 *  EventReader();
 *  EventReader(const char *logfilename);
 *
 * Constructed with no argument, there is no error logging. Supplying a file name
 * sends all diagnostic output to the named file (N.B. opens in append mode)
 *
 * See example in client.cxx for constructing a log file name based on the
 * datafile name.
 *
 * It is strongly advised to use the log file capability. You can grep it for
 * instances of "ERROR:" to trap anything noteworthy (i.e., corrupted data files).
 *
 * Revision 1.3  1999/07/10 21:31:33  levine
 * Detectors RICH, EMC, TRG now have their own (defined by each detector) interfaces.
 * Existing user code will not have to change any calls to TPC-like detector
 * readers.
 *
 * Revision 1.2  1999/07/07 19:24:37  levine
 * Added comments on using 8-bit to 10-bit translation table by popular request
 * (well, at least one person requested this change)
 *
 * Revision 1.1  1999/07/02 20:40:57  levine
 * Moved to examples directory from top-level
 *
 *
 **************************************************************************/


#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <iostream>

#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/EMC/EMC_Reader.hh"
#include "StDaqLib/RICH/RICH_Reader.hh"
#include "StDaqLib/TRG/TRG_Reader.hh"

#include "StDaqLib/TPC/trans_table.hh"  
// this contains the 8-bit to 10-bit translation table
// for PHYSICS running

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
#define GET_OTHERS

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
 
  if(argc == 1)
  {
    filename = "/scratch/2sbWith2Rcv_from_Tape.dat";
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

  while(offset != -1)
    {  
      EventReader *er = getEventReader(fd,offset,(const char *)logfile,0);
      if(!er) 
	{
	  cout << "Error creating ER" << endl;
	  close(fd);
	  exit(0);
	}

      er->printEventInfo();

      // Prepare for the next event
      if(offset != -1)
	offset = er->NextEventOffset();
      else
	offset = -1;
#ifdef GET_OTHERS
      // try to create RICH reader
      RICH_Reader *drich = getRICHReader(er);
      if(!drich)
	cout << "Error creating RICH_Reader: " << er->errstr() << endl;
      else {
	printf("created RICH_Reader!!!\n");
	// call additional methods defined for RICH
      }

      // try to create TRG reader
      TRG_Reader *dtrg = getTRGReader(er);
      if(!dtrg)
	cout << "Error creating TRG_Reader: " << er->errstr() << endl;
      else {
	printf("created TRG_Reader!!!\n");
	// call additional methods defined for TRG
      }

      // try to create EMC reader
      EMC_Reader *demc = getEMCReader(er);
      if(!demc)
	cout << "Error creating EMC_Reader: " << er->errstr() << endl;
      else {
	printf("created EMC_Reader!!!\n");
	// call additional methods defined for EMC
      }
#endif
      DetectorReader *dr = getDetectorReader(er, "TPCV2P0");
      if(!dr) {
	cout << "Error creating TPC_Reader: " << er->errstr() << endl;
	close(fd);
	exit(0);
      } 
      else printf("created TPC_Reader!!!\n");

      for(int sector=1;sector <= 24;sector++)
	{
	  ZeroSuppressedReader *zsr = dr->getZeroSuppressedReader(sector);
	  if(!zsr)
	    {
	      printf(" error in ZeroSuppressedReader: sector %d\n",sector);
	      continue;  // no banks for this sector
	    }

	  int nSeq[TPC_MAXPADS];
	  Sequence *Seq[TPC_MAXPADS];
	  for(int row=1;row <= TPC_PADROWS; row++) {
	    int count = zsr->getPadList(row, &padlist[row-1]);
	    if (!count) continue; // any pads with data?
	    printf("+++++++ padrow %d +++++++\n", row);
	    for (int padnum = 0; padnum<count; padnum++) {
	      int pad = padlist[row-1][padnum];
	      printf("pad %d:\n",pad);
	      int ret = zsr->getSequences(row, pad, &nSeq[pad-1], &Seq[pad-1]);
	      unsigned short ADC;
	      for (int i=0; i<nSeq[pad-1]; i++){
		int start = Seq[pad-1][i].startTimeBin;
		int len = Seq[pad-1][i].Length;
		printf("\tsequence timebin [%d..%d]\n\t",start,start+len-1);
		fflush(stdout);
		unsigned char *p = Seq[pad-1][i].FirstAdc;

		// Translation from 8-bit to 10-bit ADC values is done as illustrated here
		// The log8to10_table is defined in trans_table.hh
		// NB this table is ONLY applicable for physics runs. Do not try to use it on
		// pedestal or configuration runs. [MJL]

		for (int j=0; j<len; j++) {
		  ADC = log8to10_table[*(p++)];
		  printf("%d ",ADC);
		}
		printf("\n");
	      }
	    }
	    int nSpacePts;
	    struct SpacePt *SpacePts;
	    int iret = zsr->getSpacePts(row, &nSpacePts, &SpacePts); 
	    if (!iret) continue; //test for return status
	    if (!nSpacePts) continue; // test for space points this row
	    printf("--------- Row %d: found %d space pts ----------------\n",row,nSpacePts);
	    printf("\t  <x>\t  <t>\t   Q\t quality\n");
	    for (int isp=0; isp<nSpacePts; isp++) {
	      int tbin = SpacePts[isp].centroids.t/640;
	      if (tbin>49) tbin=49;
	      histogram[tbin]++;
	      printf("\t%5.1f\t%5.1f\t%5d\t%4d ",
		     float(SpacePts[isp].centroids.x)/64.0,
		     float(SpacePts[isp].centroids.t)/64.0,
		     SpacePts[isp].q, 
		     (SpacePts[isp].flags>>3)&0xf );
	     if (SpacePts[isp].flags & 1) printf("\texc pad width ");
	     if (SpacePts[isp].flags & 2) printf("\texc time width ");
	     if (SpacePts[isp].flags & 4) printf("\tsat'd ADC ");
	     printf("\n");
	     //	     printf("0x%X 0x%X 0x%X 0x%X \n",SpacePts[isp].centroids.x,
	     //    SpacePts[isp].centroids.t, SpacePts[isp].flags,
	     //    SpacePts[isp].q);
	    }
	  }

#ifdef TEST_RAW

	  CPPReader *CPPr = dr->getCPPReader(sector);
	  if(!CPPr)
	    {
	      cout << "Error creating CPPr: " << dr->errstr() << endl;
	      close(fd);
	      exit(0);
	    }


	  for(int row=1;row <= TPC_PADROWS; row++)
	    {
	      int count = ADCRr->getPadList(row, &padlist[row-1]);
	      if (count){
		printf("%2d -- pad list\n",row);
		for(int i=0;i<count;i++)
		  {
		    printf("%4d",padlist[row-1][i]);
		  }
		printf("\n");
	      }
	    }

	  for(int row=1;row <= TPC_PADROWS; row++)
	    {
	      int count = ADCRr->getPadList(row, &padlist[row-1]);

	      for(int i=0;i<count;i++)
		{
		  int length;
		  u_char *Array;
		  u_char thispad = padlist[row-1][i];
		  int nseq = ADCRr->getSequences(row, thispad, &length, &Array);
		  //       print non-zero ADC values for this pad (all 512)
		  //        printf("non-zero ADC values for row %d, pad %d:\n",row,thispad);
		  //        for(int j=0; j<length; j++)
		  // 	  {
		  // 	    if (Array[j]) printf("timebin[%d] = %d \n", j, Array[j]);
		  // 	  }
		}
	    }

	  for(int row=1;row <= TPC_PADROWS; row++)
	    {
	      int count = ADCRr->getPadList(row, &padlist[row-1]);

	      for(int i=0;i<count;i++) //loop over pads with data in this row
		{
		  int nClusters;
		  // array of pointers to ASIC_Cluster
		  struct ASIC_Cluster *clusters[TPC_MAXPADS];
		  u_char thispad = padlist[row-1][i];

		  int n = CPPr->getClusters(row, thispad, &nClusters, clusters);
		  if (n) {
		    cout << "Error getting clusters: " << dr->errstr() << endl;
		    close(fd);
		    printf("sector %d row %d   pad %d \n",sector, row, thispad);
		    exit(0);
		  }
		  if (nClusters) printf("found %d clusters sector %d row %d  pad %d\n",
					nClusters,sector,row,thispad);
		  // print all valid CPP values for this pad
		  int jmax = (nClusters>31)? 31:nClusters;
		  for(int j=0; j<jmax; j++)
		    {
		      printf("(%d,%d)[%d] = start %d: stop %d \n", 
			     row,thispad, j, (clusters[thispad-1]+j)->start_time_bin,
			     (clusters[thispad-1]+j)->stop_time_bin);
		    }
		}
	    }

	  delete ADCRr;
	  delete CPPr;
#endif
	  delete zsr;
	}
      EventInfo info=er->getEventInfo();
      printf("===============TIMEBIN HISTOGRAMS: event %d====================\n",
	     info.EventSeqNo);
      float total=0;
      for (int i=0; i<50; i++) {
	total += histogram[i];
      }
      for (int i=0; i<50; i++) {
	printf("%d-%d:  %f clusters\n", i*10, i*10+9, (float)histogram[i]/total);
      }
#ifdef GET_OTHERS
      if (demc) delete demc; // remove EMC_Reader
      if (dtrg) delete dtrg; // remove TRG_Reader
      if (drich) delete drich; // remove RICH_Reader
#endif
      delete dr; // remove TPC reader
      delete er;
    }
  close(fd);
}
  

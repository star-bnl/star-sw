/***************************************************************************
 * $Id: client.cxx,v 1.13 2003/09/02 17:55:34 perev Exp $
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
 * Revision 1.13  2003/09/02 17:55:34  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.12  2003/01/29 20:08:09  geurts
 * disabled usage of TRG_Reader::PrintAllTheData() and ::PrintDataCompact().
 * Both methods are unavailabe in TRG_Reader.
 *
 * Revision 1.11  2003/01/29 13:55:18  ward
 * Turn off memory mapped operation in the examples, which appears to fail for 2003 daq files.
 *
 * Revision 1.10  2000/07/03 16:03:17  ward
 * Minor improvements to client.cxx, eg a usage message.
 *
 * Revision 1.9  2000/06/08 12:45:10  jml
 * Added <assert.h> to fix compile error in offline
 *
 * Revision 1.8  2000/06/07 15:06:26  jml
 * Changed exit() calls to assert(0) to aid in debugging
 *
 * Revision 1.7  2000/01/14 17:54:26  levine
 * example use of TRG_Reader
 *
 * Revision 1.6  2000/01/04 20:55:05  levine
 * Implemented memory-mapped file access in EventReader.cxx. Old method
 * (via seeks) is still possible by setting mmapp=0 in
 *
 * 	getEventReader(fd,offset,(const char *)logfile,mmapp);
 *
 *
 * but memory-mapped access is much more effective.
 *
 * Revision 1.4  1999/07/21 21:33:10  levine
 *
 *
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
#include <assert.h>
#include "Stiostream.h"

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

  while(offset != -1)
    {  
      EventReader *er = getEventReader(fd,offset,(const char *)logfile,0); // Mamory map appears not to work with 2003 daq files.
      if(!er) 
	{
          cout << "============================================" << endl;
	  cout << "Error creating ER" << endl;
          cout << "This may be simply the end of the .daq file." << endl;
	  close(fd);
	  assert(0); // This may be simply the end of the .daq file.
	}

      er->printEventInfo();
      

      // Prepare for the next event
      if(offset != -1)
	offset = er->NextEventOffset(); 
      else
	offset = -1;

      TRG_Reader *tr = getTRGReader(er);
      if(!tr) {
	cout << "Error creating TRG_Reader: " << er->errstr() << endl;
	close(fd);
	assert(0);
      } 
      else printf("created TRG_Reader!!!\n");
      //fprintf(er->logfd,"\n\n================\n\nHerb's formatted TRGD dump:\n\n\n");
      //tr->pBankTRGD->PrintAllTheData(er->logfd);
      //fprintf(er->logfd,"\n\n================\n\nMike's formatted TRGD dump:\n\n\n");
      //tr->pBankTRGD->PrintDataCompact(er->logfd);

#ifdef DOTPC      

      DetectorReader *dr = getDetectorReader(er, "TPCV2P0");
      if(!dr) {
	cout << "Error creating TPC_Reader: " << er->errstr() << endl;
	close(fd);
	assert(0);
      } 
      else printf("created TPC_Reader!!!\n");
#ifdef DO_SECTORS
      for(int sector=1;sector <= 24;sector++)
	{

	  int nSeq[TPC_MAXPADS];
	  Sequence *Seq[TPC_MAXPADS];


	  PedestalReader *pedr = dr->getPedestalReader(sector);
	  if(!pedr)
	    {
	      cout << "Error creating pedestal reader: " << dr->errstr() << endl;
	      close(fd);
	      assert(0);
	    }

	  PedestalRMSReader *rmsr = dr->getPedestalRMSReader(sector);
	  if(!rmsr)
	    {
	      cout << "Error creating pedestal RMS reader: " << dr->errstr() << endl;
	      close(fd);
	      assert(0);
	    }

	  // test for existence of PEDR banks this sector
	  if (pedr->getNumberOfEvents()==0) continue; // no PEDR banks this sector
#ifdef PEDS
	  for(int row=1;row <= TPC_PADROWS; row++)
	    {
	      int count = pedr->getPadList(row, &padlist[row-1]);
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
	      int count = pedr->getPadList(row, &padlist[row-1]);

	      for(int i=0;i<count;i++)
		{
		  int length;
		  u_char  *Array[TPC_MAXPADS];
		  u_char thispad = padlist[row-1][i];
		  int nseq = pedr->getSequences(row, thispad, &length, &Array[thispad-1]);
		  //       print non-zero ADC values for this pad (all 512)
		  if (!nseq) continue; // pad not implemented
		  printf("pedestal values for row %d, pad %d:\n",row,thispad);
		  for(int j=0; j<length; j++)
		    {
		      printf("%d ", Array[thispad-1][j]);
		      if (j%16==0) printf("\n");
		    }
		  printf("\n");
		}
	    }
#endif
	  for(int row=1;row <= TPC_PADROWS; row++)
	    {
	      int count = rmsr->getPadList(row, &padlist[row-1]);

	      for(int i=0;i<count;i++)
		{
		  int length;
		  u_char  *Array[TPC_MAXPADS];
		  u_char thispad = padlist[row-1][i];
		  int nseq = rmsr->getSequences(row, thispad, &length, &Array[thispad-1]);
		  //       print non-zero ADC values for this pad (all 512)
		  if (!nseq) continue; // pad not implemented
		  printf("pedestal RMS values for row %d, pad %d:\n",row,thispad);
		  for(int j=0; j<length; j++)
		    {
		      printf("%4.1f ", (float)Array[thispad-1][j]/16.0);
		      if (j%16==0) printf("\n");
		    }
		  printf("\n");
		}
	    }

	  delete pedr;
	  delete rmsr;
	}
#endif
      delete dr; // remove TPC reader
#endif
      delete tr; // remove TRG reader
      delete er;
    }
  close(fd);
}
  

#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <iostream>

#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/TPC/MemMan.hh"
#include "StDaqLib/TPC/trans_table.hh"



int main(int argc, char *argv[])
{
  int fd;
  string filename;
  //client must provide
  //array of pointers to padlist (one for each pad row)
  u_char *(padlist[TPC_PADROWS]); 
  long offset = 0L;

//   if((argc != 2) && (argc != 1))
//   {
//     cout << "syntax:  client event_filename" << endl;
//     exit(0);
//   }

  if (argc==3) offset = atol(argv[2]);
 
  if(argc == 1)
  {
    filename = "/scratch/2sbWith2Rcv_from_Tape.dat";
  }
  else
  {
    filename = argv[1];
  }

  fd = open(filename.c_str(),O_RDONLY);

  if (fd == -1)
  { 
    perror("FormatClient");
  }


  while(offset != -1)
    {  
      EventReader *er = getEventReader(fd,offset,0);
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
      DetectorReader *dr = getDetectorReader(er, "TPCV2P0");
      if(!dr)
	{
	  cout << "Error creating DR: " << er->errstr() << endl;
	  close(fd);
	  exit(0);
	}

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
		for (int j=0; j<len; j++) {
		  ADC = log8to10_table[*(p++)];
		  printf("%d ",ADC);
		}
		printf("\n");
	      }
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
      delete dr;
      delete er;
    }
  close(fd);
}
  

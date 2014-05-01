#ifndef lint
static char vcid[] = "$Id: pxl_decoder.cxx,v 1.2 2014/05/01 19:46:24 jschamba Exp $";
static const char __attribute__ ((used )) *Get_vcid(){return vcid;}
#endif /* lint */

#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#include <sys/types.h>
#include <stdlib.h>

// C++ header file
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
using namespace std;

#include "pxl_decoder.h"

static int decode16bit(unsigned short val, bool MS16,
		       int sensor,
		       int *row,
		       int *prev_row,
		       int *prev_col,
		       int *error_cnt,
		       int *OVF,
		       bitset2D<NROW,NCOL> *bs,
		       int runlength[][4]
		       )
{
  int ret;
  bool duplicate;
  int column, coding;

  ret = 0;
  if ( (val & 0x1000) == 0x1000 ) { // State0
    int tmpRow = (val>>2) & 0x3ff;
    if (tmpRow >= NROW) {
      DEBUGP("**row %4d", tmpRow);
      ret |= PXLERR_RANGE; (*error_cnt)++;
    }
    else {
      DEBUGP("row %4d", tmpRow);
    }
    if(sensor < NSENSOR) {
      row[sensor] = tmpRow;
      if(tmpRow < (prev_row[sensor]&0xFFF)) {
	if((prev_row[sensor]&0x1000) == 0x1000) {
	  prev_row[sensor] = tmpRow | 0x1000;
	  ret |=  PXLERR_OUTOFORDER; (*error_cnt)++;
	  DEBUGP(" **out-of-order");
	}
	else {
	  prev_row[sensor] = tmpRow | 0x1000;
	}
      }
      else {
	prev_row[sensor] = tmpRow;
      }
    }
    if( (val & 0x2) == 0x2) {
      // overflow:
      DEBUGP(" OVF");
      if(sensor < NSENSOR) OVF[sensor]++;
    }
  }
  else { // StateN
    column = (val>>2) & 0x3ff;
    coding = val & 0x3;
    if (column+coding >= NCOL) {
      if (column == 1023)
	DEBUGP("dummy ");
      else { 
	DEBUGP("**col %4d ", column);
	ret |= PXLERR_RANGE; (*error_cnt)++;
      }
    }
    else {
      DEBUGP("col %4d ", column);
    }
    if(sensor < NSENSOR) {
      if (MS16) {
	if (row[sensor] == -1) {
	  // error: row should already be defined here
	  DEBUGP("(**row %4d) ", row[sensor]);
	  ret |= PXLERR_UNKNOWNROW; (*error_cnt)++;
	}
	else
	  DEBUGP("(row %4d) ", row[sensor]);
      }
      else
	  DEBUGP("(row %4d) ", row[sensor]);
      // valid hits, fill bitset if valid row
      if((row[sensor]>=0) && (row[sensor]<NROW)) {
	duplicate = false;
	runlength[sensor][coding]++;
	for(int c=0; c<coding+1; c++) {
	  if((column+c)<NCOL)  {
	    if (bs[sensor].test(row[sensor],column+c))
	      duplicate = true;
	    bs[sensor].set(row[sensor],column+c);
	  }
	}
	if(duplicate) {
	  DEBUGP("**(duplicate) ");
	  ret |= PXLERR_DUPLICATE; (*error_cnt)++;
	}
      }
    }
    DEBUGP("coding %d", coding);
  }
  return ret;
}

//**************************************************
int pxl_decoder(const u_int *d, const int wordLength,
		bitset2D<NROW,NCOL> *bs,
		int *OVF, struct _pxlHeaderInfo *pxlHeaderInfo,
		float *ave_runlength,
		int *error_cnt)
{
  int i,j;
  int ret; 

  // Sensor variables:
  int sensor;
  int row[NSENSOR]; // 40 sensors per RDO
  unsigned short valLS, valMS;
  int prev_row[NSENSOR], prev_col[NSENSOR];
  int runlength[NSENSOR][4];
#ifdef CRC_CALC
  register unsigned int crc = 0xFFFFFFFF ;
#endif
  ret = 0; // no error

  // clear the row array
  for (i=0; i<NSENSOR; i++) {
    row[i] = -1;
    prev_row[i] = 0;
    prev_col[i] = 0;
    // init the results parameters
    *error_cnt = 0;
    OVF[i] = 0;
    bs[i].reset();
    for(j=0; j<4; j++)
      runlength[i][j] = 0;
  }

  if (wordLength < 20) {
    DEBUGP(" *** Event too short!");
    ret |= PXLERR_LENGTH; (*error_cnt)++;
    return ret;
  }

#ifdef CRC_CALC
#define	 G_CONST  0x04C11DB7 
  for(int i=0; i<(wordLength-2); i++) {
    u_int datum ;
    
    datum = d[i] ;
    register u_int data_j ;
    register u_int crc_31 ;
    
    for(register int j=31;j>=0;j--) {
      data_j = (datum >> j) & 1 ;
      crc_31 = (crc & 0x80000000) ? 1 : 0 ;
      
      if(crc_31 == data_j) {
	crc = (crc<<1) ^ G_CONST ;
      }
      else {
	crc = (crc<<1) ;
      }
    }
  }
#endif

  // dump header
  DEBUGP("\t%4d: 0x%08X",0,d[0]);
  if (d[0] != 0xAAAAAAAA) {
    DEBUGP(" *** wrong header token!");
    ret |= PXLERR_HEADER; (*error_cnt)++;
  }
  DEBUGP("\n");

  pxlHeaderInfo->tcdWord = (unsigned short)(d[1]&0xFFFFF);
  pxlHeaderInfo->rhicStrobeCtr = d[7];
  pxlHeaderInfo->temperature[0][0] = (unsigned short)(d[2]&0x3FF);
  pxlHeaderInfo->temperature[0][1] = (unsigned short)((d[2]&0xFFC00)>>10);
  pxlHeaderInfo->temperature[1][0] = (unsigned short)((d[2]&0x3FF00000)>>20);
  pxlHeaderInfo->temperature[1][1] = (unsigned short)(d[3]&0x3FF);
  pxlHeaderInfo->temperature[2][0] = (unsigned short)((d[3]&0xFFC00)>>10);
  pxlHeaderInfo->temperature[2][1] = (unsigned short)((d[3]&0x3FF00000)>>20);
  pxlHeaderInfo->temperature[3][0] = (unsigned short)(d[4]&0x3FF);
  pxlHeaderInfo->temperature[3][1] = (unsigned short)((d[4]&0xFFC00)>>10);

#ifdef PRINT_DEBUG
  fprintf(stdout, "\t%4d: 0x%08X",1,d[1]);
  fprintf(stdout, " - Trigger word: TRGCMD=0x%x, DAQCMD=0x%x, TOKEN=0x%03X\n",
	 (d[1]&0xf0000)>>16,
	 (d[1]&0x0f000)>>12,
	 d[1]&0x00fff);

  fprintf(stdout, "\t%4d: 0x%08X - ",2,d[2]);
  fprintf(stdout, "L1Temp1 0x%03x L1Temp2 0x%03x L2Temp1 0x%03x\n",
	  d[2]&0x3FF, (d[2]&0xFFC00)>>10, (d[2]&0x3FF00000)>>20);
  
  fprintf(stdout, "\t%4d: 0x%08X - ",3,d[3]);
  fprintf(stdout, "L2Temp2 0x%03x L3Temp1 0x%03x L3Temp2 0x%03x\n",
	  d[3]&0x3FF, (d[3]&0xFFC00)>>10, (d[3]&0x3FF00000)>>20);
  
  fprintf(stdout, "\t%4d: 0x%08X - ",4,d[4]);
  fprintf(stdout, "L4Temp1 0x%03x L4Temp2 0x%03x\n",
	  d[4]&0x3FF, (d[4]&0xFFC00)>>10);
  
  fprintf(stdout, "\t%4d: 0x%08X - Firmware version %d\n",5,d[5],(int)d[5]);
  fprintf(stdout, "\t%4d: 0x%08X - Board Position %d, Serial %d, Configuration %d\n",6,d[6],
	  d[6]&0xF, (d[6]&0x0000FFF0)>>4, (d[6]&0xFFFF0000)>>16);
  fprintf(stdout, "\t%4d: 0x%08X - RHICstrobe Ctr\n",7,d[7]);
  for(i=8;i<15;i++) {
    fprintf(stdout, "\t%4d: 0x%08X - System Status %d\n",i,d[i],i-7);
  }
  fprintf(stdout, "\t%4d: 0x%08X - end of header\n",15,d[15]);
#endif

#ifdef PRINT_STATUS
  if ((d[8] != 0) || (d[9] != 0) || ((d[10]&0xFFFF) != 0)) {
    fprintf(stdout, "\theader_err: ");
    for (i=0; i<16; i++) 
      if((d[8]& (0x3<<(i*2))) != 0) fprintf(stdout, "%d ", i+1);
    for (i=0; i<16; i++) 
      if((d[9]& (0x3<<(i*2))) != 0) fprintf(stdout, "%d ", i+17);
    for (i=0; i<8; i++) 
      if((d[10]& (0x3<<(i*2))) != 0) fprintf(stdout, "%d ", i+33);
    fprintf(stdout, "\n");
  }

  if (((d[10]&0xFFFF0000) != 0) || ((d[11]&0xFFFFFF) != 0)) {
      fprintf(stdout, "\tlength_err: ");
    for (i=0; i<16; i++) 
      if((d[10]& (0x10000<<i)) != 0) fprintf(stdout, "%d ", i+1);
    for (i=0; i<24; i++) 
      if((d[11]& (0x1<<i)) != 0) fprintf(stdout, "%d ", i+17);
    fprintf(stdout, "\n");
  }

  if (((d[11]&0xFF000000) != 0) || (d[12] != 0)) {
      fprintf(stdout, "\tdata_err: ");
    for (i=0; i<8; i++) 
      if((d[11]& (0x1000000<<i)) != 0) fprintf(stdout, "%d ", i+1);
    for (i=0; i<32; i++) 
      if((d[12]& (0x1<<i)) != 0) fprintf(stdout, "%d ", i+9);
    fprintf(stdout, "\n");
  }

  if ((d[13] != 0) || ((d[14]&0xFF) != 0)) {
      fprintf(stdout, "\ttrailer_err: ");
    for (i=0; i<32; i++) 
      if((d[13]& (0x1<<i)) != 0) fprintf(stdout, "%d ", i+1);
    for (i=0; i<8; i++) 
      if((d[14]& (0x1<<i)) != 0) fprintf(stdout, "%d ", i+33);
    fprintf(stdout, "\n");
  }

  if (((d[14]&0xFFFFFF00) != 0) || ((d[15]&0xFFFF) != 0)) {
      fprintf(stdout, "\tsensfull_err: ");
    for (i=0; i<24; i++) 
      if((d[14]& (0x100<<i)) != 0) fprintf(stdout, "%d ", i+1);
    for (i=0; i<16; i++) 
      if((d[15]& (0x1<<i)) != 0) fprintf(stdout, "%d ", i+25);
    fprintf(stdout, "\n");
  }

  if ((d[15]&0x100) == 0x100) fprintf(stdout, "\tFIFO_0_FULL ");
  if ((d[15]&0x200) == 0x200) fprintf(stdout, "\tFIFO_1_FULL");
  if ((d[15]&0x300) != 0) fprintf(stdout, "\n");
#endif
  
  // hit block length
  int hitLength = d[16];
  DEBUGP("\t%4d: 0x%08X - hit block length: %d",16,d[16],d[16]);

  // Now dump hits:
  int endOfHits;
  if ( (hitLength+16) < (wordLength-2) ) {
    endOfHits = hitLength+17;
  }
  else {
    ret |= PXLERR_HITLEN; (*error_cnt)++;
    DEBUGP(" *** too big!!!");
    endOfHits = wordLength - 2;
  }
  DEBUGP("\n");

  for(i=17; i<endOfHits; i++) {
    DEBUGP("\t%4d: 0x%08X",i,d[i]);
    // decode hit data:
    sensor = ((d[i]>>26) & 0x38) | ((d[i]>>13) & 0x7);
    DEBUGP(" - Sensor %2d", sensor);
    // sensor goes  from 1-40, internally use 0-39:
    sensor -= 1;
    if((sensor > (NSENSOR-1)) || (sensor < 0)) {
      ret |= PXLERR_SENSORNUM; (*error_cnt)++;
      DEBUGP("**invalid sensor\n");
      continue;
    }

    valLS = d[i] & 0xffff;
    valMS = (d[i]>>16) & 0xffff;

    // first decode least significant 16 bits
    DEBUGP(", LS16: ");
    ret |= decode16bit(valLS, false, sensor, row, prev_row, prev_col, 
		       error_cnt, OVF, bs, runlength);

    DEBUGP(", MS16: ");
    ret |= decode16bit(valMS, true, sensor, row, prev_row, prev_col,
		       error_cnt, OVF, bs, runlength);

    DEBUGP("\n");
  }
	
#ifdef PRINT_DEBUG
  // next should be the TCD Info block  
  if ( endOfHits < (wordLength-2)) {
    fprintf(stdout, "\t%4d: 0x%08X - start TCD Info\n",endOfHits,d[endOfHits]);
    for (i=endOfHits+1; i<(wordLength-2); i++) {
      fprintf(stdout, "\t%4d: 0x%08X\n",i,d[i]);
    }
  }
  else {
    fprintf(stdout, "\t\tno TCD Info!\n");
  }
#endif

#ifdef PRINT_DEBUG
  // finally, the CRC 
  fprintf(stdout, "\t%4d: 0x%08X - CRC",
	  (wordLength-2),d[wordLength-2]);
#ifdef CRC_CALC
  if (d[wordLength-2] != crc) {
    fprintf(stdout, "*** calculated 0x%08X)",crc);
    ret |= PXLERR_CRC; (*error_cnt)++;
  }
#endif
  fprintf(stdout, "\n");
  // and Ender
  fprintf(stdout, "\t%4d: 0x%08X",(wordLength-1),d[wordLength-1]);

#endif
  if (d[wordLength-1] != 0xBBBBBBBB) {
    ret |= PXLERR_ENDER; (*error_cnt)++;
    DEBUGP(" *** wrong ender token!");
  }
  DEBUGP(" - Ender\n");

  // now calculate the average runlength (coding+1):
  for(i=0; i<NSENSOR; i++) {
    int total=0, N=0;
    for(j=0; j<4; j++) {
      total += (j+1)*runlength[i][j];
      N += runlength[i][j];
    }
    if (N>0)
      ave_runlength[i] = (float)total/((float)N);
    else
      ave_runlength[i] = 0.0;
  }

	
  return ret;
}

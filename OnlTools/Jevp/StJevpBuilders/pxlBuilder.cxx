#include <stdio.h>
#include <stdlib.h>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"

#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1D.h>
#include <TH2F.h>

#include <math.h>
#include "pxlBuilder.h"
#include <RTS/include/rtsLog.h>

//#include "pxlBuilder_helper_funcs.h"


// const int NSENSOR = 40;
// const int NCOL = 960;
// const int NROW = 928;
// const int NRDO = 10;

struct _pxlHeaderInfo {
  unsigned short tcdWord;
  unsigned int rhicStrobeCtr;
  unsigned short temperature[4][2];
};

#ifdef PRINT_DEBUG
#define DEBUGP(...) fprintf(stdout, __VA_ARGS__)
#else
#define DEBUGP(...)
#endif

///////////////////////////////////////////////////////////
///////////////////// PXL /////////////////////////////////
///////////////////////////////////////////////////////////
const int PXLERR_HEADER = 0x1;
const int PXLERR_HITLEN = 0x2;
const int PXLERR_SENSORNUM = 0x4;
const int PXLERR_DUPLICATE = 0x8;
const int PXLERR_ENDER = 0x10;
const int PXLERR_RANGE = 0x20;
const int PXLERR_OUTOFORDER = 0x40;
const int PXLERR_UNKNOWNROW = 0x80;


static int decode16bit(unsigned short val, bool MS16,
		       int sensor,
		       int *row,
		       int *prev_row,
		       int *prev_col,
		       int *error_cnt,
		       int *OVF,
		       bitset<NCOL> bs[][NROW],
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
	    if (bs[sensor][row[sensor]].test(column+c))
	      duplicate = true;
	    bs[sensor][row[sensor]].set(column+c);
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
static int pxl_decoder(const u_int *d, const int wordLength,
		       bitset<NCOL> bs[][NROW],
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
  //register unsigned int crc = 0xFFFFFFFF ;

  ret = 0; // no error

  // clear the row array
  for (i=0; i<NSENSOR; i++) {
    row[i] = -1;
    prev_row[i] = 0;
    prev_col[i] = 0;
    // init the results parameters
    *error_cnt = 0;
    OVF[i] = 0;
    for(j=0; j<NROW; j++)
      bs[i][j].reset();
    for(j=0; j<4; j++)
      runlength[i][j] = 0;
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
  DEBUGP("\t%4d: 0x%08X\n",0,d[0]);
  if (d[0] != 0xAAAAAAAA) {
    DEBUGP(" *** wrong header token!\n");
    ret |= PXLERR_HEADER; (*error_cnt)++;
  }

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
  
  fprintf(stdout, "\t%4d: 0x%08X - Firmware version\n",5,d[5]);
  fprintf(stdout, "\t%4d: 0x%08X - Board Position %d, Serial 0x%x\n",6,d[6],
	  d[6]&0xF, d[6]>>4);
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
  fprintf(stdout, "\t%4d: 0x%08X - CRC (calculated 0x%08X)\n",
	  (wordLength-2),d[wordLength-2],crc);
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





























































_pxlHeaderInfo pxlHeaderInfo;


void pxlBuilder::IncrementMultiplicity(int sensor_number,int row_count){
  (sensor_number < 11) ? multiplicity_inner += row_count : multiplicity_outer += row_count;
}

int pxlBuilder::WhichLadder(int sector_number,int sensor_number){
  //sector_number 1 - 10, sensor 1 - 40, returns ladder number 1 - 40
  int ladder_number = 1+(int)((sensor_number-1)/10)+(4*(sector_number-1));
  return ladder_number;
}

void pxlBuilder::UpdateLadderCount(int sector_number,int sensor_number,int sensor_count){
  int ladder_number = WhichLadder(sector_number,sensor_number);

  if(LadderCount->count(ladder_number) == 0) LadderCount->insert(make_pair(ladder_number,sensor_count));
  else{
    LadderCount->find(ladder_number)->second += sensor_count;
  } 
}

int pxlBuilder::GetLadderCount(int ladder_number){
  return LadderCount->find(ladder_number)->second;
}


void pxlBuilder::FillLadderHistogram(TH1 *hist){
  for(int i=1; i<=40; i++){
    hist->SetBinContent(i,(double)(GetLadderCount(i)));
  }  
}

void pxlBuilder::UpdateLadderHistogram(TH1 *hist, TH1 *hist_single_evt, int number_of_events_old){
  hist->Scale(number_of_events_old);
  hist->Add(hist_single_evt);
  hist->Scale(1.0/(number_of_events_old + 1.0));

}
void pxlBuilder::UpdateLayerHistograms(TH1 *hits_in, TH1 *rl_in, TH1 *hits_out, TH1 *rl_out, int number_of_events)
{
  for(int sec = 1; sec<=10; sec++) {

    for(int sen = 1; sen<=10; sen++) { // Inner Ladder
      int ladder_bin = sec;
      int sen_bin = sen;
      double rl = avg_run_length[sec-1][sen-1];
      int freq_total = sensor_hit_frequency[sec-1][sen-1];
      int hits = sensor_hits[sec-1][sen-1];

      // Run Length Inner
      if (rl > 0.0) {
	double rl_content = (double)rl_in->GetBinContent(ladder_bin, sen_bin);
	(rl_content < 0.0) && (rl_content = 0.0);
	double new_rl_content = (rl_content * (freq_total-1) + rl) / freq_total;
	rl_in->SetBinContent(ladder_bin, sen_bin, new_rl_content);
      }
      // Hits Inner
      double hits_content = (double)hits_in->GetBinContent(ladder_bin, sen_bin);
      (hits_content < 0.0) && (hits_content = 0.0);
      double new_hits_content = ((hits_content * number_of_events) + hits)/(number_of_events + 1);
      hits_in->SetBinContent(ladder_bin, sen_bin, new_hits_content);
    }

    for(int sen = 11; sen<=40; sen++) { // Outer Ladder
      int sen_bin = (sen-1)%10 + 1;
      int ladder_bin = (sec-1)*3 + (int)((sen-1)/10);
      double rl = avg_run_length[sec-1][sen-1];
      int freq_total = sensor_hit_frequency[sec-1][sen-1];
      int hits = sensor_hits[sec-1][sen-1];
      
      // Run Length Outer
      if (rl > 0.0) {
	double rl_content = (double)rl_out->GetBinContent(ladder_bin, sen_bin);
	(rl_content < 0.0) && (rl_content = 0.0);
	double new_rl_content = (rl_content * (freq_total-1) + rl) / freq_total;
	rl_out->SetBinContent(ladder_bin, sen_bin, new_rl_content);
      }
      // Hits Outer
      double hits_content = (double)hits_out->GetBinContent(ladder_bin, sen_bin);
      (hits_content < 0.0) && (hits_content = 0.0);
      double new_hits_content = ((hits_content * number_of_events) + hits)/(number_of_events + 1);
      hits_out->SetBinContent(ladder_bin, sen_bin, new_hits_content);
    }
  }
}


void pxlBuilder::SetRunLength(int sensor_number,double average_run_length){
  if(AverageRunLength->count(sensor_number) == 0) AverageRunLength->insert(make_pair(sensor_number,average_run_length));
  else{
    //This shouldn't happen
    //AverageRunLength->find(sensor_number)->second += average_run_length;
  }
}

int pxlBuilder::IncrementArray(const char* name,int x_bin,int y_bin){
  int count;
  x_bin--;
  y_bin--;

  if(strcmp(name,"hits_inner") == 0){
    count_hits_inner[x_bin][y_bin]++;
    count = count_hits_inner[x_bin][y_bin];
  }

  else if(strcmp(name,"hits_outer") == 0){
    count_hits_outer[x_bin][y_bin]++;
    count = count_hits_outer[x_bin][y_bin];
  }

  else if(strcmp(name,"length_inner") == 0){
    count_length_inner[x_bin][y_bin]++;
    count = count_length_inner[x_bin][y_bin];
  }

  else if(strcmp(name,"length_outer") == 0){
    count_length_outer[x_bin][y_bin]++;
    count = count_length_outer[x_bin][y_bin];
  }

  else{
    cout<<"Error: IncrementArray"<<endl;
    return 1.0;
  }

  return count;
}

bool pxlBuilder::UpdateTH1(TH1 *hist,int bin,double value){
  double bin_content = 0.0;
  bin_content += (double)hist->GetBinContent(bin);
  ((!bin_content) || (bin_content < 0.0)) && (bin_content = 0.0);
  double new_content = bin_content+value;

  if(new_content <= 0.0) return true;
  else{

    hist->SetBinContent(bin,new_content);
    return ((double)hist->GetBinContent(bin) == new_content) ? true : false;
  }
}

bool pxlBuilder::UpdateTH1_Scale(TH1 *hist,int bin,double value, int number_of_events_old){
  double bin_content = 0.0;
  bin_content += (double)hist->GetBinContent(bin);
  ((!bin_content) || (bin_content < 0.0)) && (bin_content = 0.0);
  double new_content;
  if(number_of_events_old > 0 && bin_content > 0)
    new_content = (double)((bin_content * number_of_events_old) + value)/(number_of_events_old + 1.0);
  else new_content = bin_content+value;

  if(new_content <= 0.0) return true;
  else{

    hist->SetBinContent(bin,new_content);
    return ((double)hist->GetBinContent(bin) == new_content) ? true : false;
  }
}

bool pxlBuilder::UpdateTH2(TH1 *hist,int x_bin,int y_bin,double value){
  if(value <= 0.0) return true;
  else{

    double bin_content = 0.0;
    bin_content += (double)hist->GetBinContent(x_bin,y_bin);
    ((!bin_content) || (bin_content < 0.0)) && (bin_content = 0.0);
    
    double new_content = bin_content+value;
    if(new_content < 0.0) cout<<"Less than 0!"<<endl;

    hist->SetBinContent(x_bin,y_bin,new_content);
    return (hist->GetBinContent(x_bin,y_bin) == new_content) ? true : false;
  }
}

bool pxlBuilder::UpdateTH2_Scale(TH1 *hist,int x_bin,int y_bin,double value, int number_of_events_old){
  if(value <= 0.0) return true;
  else{

    double bin_content = 0.0;
    bin_content += (double)hist->GetBinContent(x_bin,y_bin);
    ((!bin_content) || (bin_content < 0.0)) && (bin_content = 0.0);
    
    double new_content;
    if(number_of_events_old > 0 && bin_content > 0)
      new_content = (double)((bin_content * number_of_events_old) + value)/(number_of_events_old + 1.0);
    else new_content = bin_content+value;

    if(new_content < 0.0) cout<<"Less than 0!"<<endl;

    hist->SetBinContent(x_bin,y_bin,new_content);
    return (hist->GetBinContent(x_bin,y_bin) == new_content) ? true : false;
  }
}

bool pxlBuilder::UpdateTH2_Scale2(const char* name,TH1 *hist,int x_bin,int y_bin,double value, int number_of_events_old){
  if(value <= 0.0) return true;
  else{

    double bin_content = 0.0;
    bin_content += (double)hist->GetBinContent(x_bin,y_bin);
    ((!bin_content) || (bin_content < 0)) && (bin_content = 0.0);
    
    double new_content;
    int count = IncrementArray(name,x_bin,y_bin);
    if(count == 1) new_content = value;
    else{
      if(number_of_events_old > 0 && bin_content > 0)
	new_content = (double)((bin_content*(count-1)*number_of_events_old)+value)/((double)(count)*(number_of_events_old + 1.0));
      else new_content = bin_content+value;
    }
    if(new_content < 0.0) cout<<"Less than 0!"<<endl;

    hist->SetBinContent(x_bin,y_bin,new_content);
    return (hist->GetBinContent(x_bin,y_bin) == new_content) ? true : false;
  }
}



void pxlBuilder::SetLadderMap(){
  for(int i=1; i<41; i++){
    int value = ((i-1)/4)+1;
    LadderMap->insert(make_pair(i,value));
  }
}


void pxlBuilder::UpdateSectorErrorTypeTH2(TH1 *hist, int ret, int sector_number){
  //cout<<ret<<" | ";
  if(ret & 0x01) UpdateTH2(hist,sector_number,1,1.0);
  if(ret & 0x02) UpdateTH2(hist,sector_number,2,1.0);
  if(ret & 0x04) UpdateTH2(hist,sector_number,3,1.0);
  if(ret & 0x08) UpdateTH2(hist,sector_number,4,1.0);
  if(ret & 0x10) UpdateTH2(hist,sector_number,5,1.0);
  if(ret & 0x20) UpdateTH2(hist,sector_number,6,1.0);
  if(ret & 0x40) UpdateTH2(hist,sector_number,7,1.0);
  if(ret & 0x80) UpdateTH2(hist,sector_number,8,1.0);
}



ClassImp(pxlBuilder);
  

pxlBuilder::pxlBuilder(JevpServer *parent) : JevpPlotSet(parent) {
  plotsetname = (char *)"pxl";

  // start with histograms undefined...
  memset(&contents, 0, sizeof(contents));

  event_multiplicity = 0;
  multiplicity_inner = 0;
  multiplicity_outer = 0;
  sensor_count = 0;
  number_of_events = 0;

  max_count = 0;
  for(int i=0; i<NRDO; i++) max_count_sector[i] = 0;

  max_count_inner = 0;
  max_count_outer = 0;

  min_count = 1000000;
  for(int i=0; i<NRDO; i++) min_count_sector[i] = 1000000;

  min_count_inner = 1000000;
  min_count_outer = 1000000;

  memset(count_hits_inner, 0, sizeof(count_hits_inner[0][0]) * 10 * 10);
  memset(count_hits_outer, 0, sizeof(count_hits_outer[0][0]) * 30 * 10);
  memset(count_length_inner, 0, sizeof(count_length_inner[0][0]) * 10 * 10);
  memset(count_length_outer, 0, sizeof(count_length_outer[0][0]) * 30 * 10);

  memset(sensor_hits, 0, sizeof(sensor_hits[0][0])*400);
  memset(sensor_hit_frequency, 0, sizeof(sensor_hit_frequency[0][0])*400);
  memset(sensor_hit_frequency_SE, 0, sizeof(sensor_hit_frequency_SE[0][0])*400);
  memset(avg_run_length, 0.0, sizeof(avg_run_length[0][0])*400);

}

pxlBuilder::~pxlBuilder() {

  // Delete any existing histograms...
  int n = sizeof(contents) / sizeof(TH1 *);
  for(int i=0;i<n;i++) {
    if(contents.array[i]) delete contents.array[i];
  }
}

void pxlBuilder::initialize(int argc, char *argv[]) {
  // Initialization of histograms.

  char tmpchr[255];
  //%%%%%%%%%%%%%%Creating Histograms%%%%%%%%%%%%%%

  //Tab 1: PXL GlobalMultiplicity Plots
  contents.GlobalHitMultiplicity = new TH1D("GlobalHitMultiplicity","",1000,0,150000);
  
  for (int i=0; i<NRDO; i++) {
    sprintf(tmpchr, "GlobalHitMultiplicitySector%d", i+1);
    contents.GlobalHitMultiplicitySector[i] = new TH1D(tmpchr,"",1000,0,50000);
  }

  //Tab 2: PXL Hit Multiplicity
  contents.HitMultiplicityPerEvent = new TH1D("HitMultiplicityPerEvent","",500,0,5000);
  
  contents.HitsPerLadder = new TH1D("HitsPerLadder","",40,1,41);
  contents.HitsPerLadderPerEvent = new TH1D("HitsPerLadderPerEvent","",40,1,41);
  
  contents.HitCorrelation = new TH2D("HitCorrelation","",100,0.,80000.,100,0.,100000.);
  
  
  //Tab 3: PXL Hit Map Plots
  contents.SensorHitsInnerLayer = new TH2D("SensorHitsInnerLayer","",10,1,11,10,1,11);
  
  contents.SensorHitsOuterLayer = new TH2D("SensorHitsOuterLayer","",30,1,31,10,1,11);
  
  contents.AverageRunLengthInnerLayer = new TH2D("AverageRunLengthInnerLayer","",10,1,11,10,1,11);
  contents.AverageRunLengthOuterLayer = new TH2D("AverageRunLengthOuterLayer","",30,1,31,10,1,11);

  //Tab 4: PXL Error Plots
  for (int i=0; i<NRDO; i++) {
    sprintf(tmpchr, "ErrorCountSector%d", i+1);
    contents.ErrorCountSector[i] = new TH1D(tmpchr,"",500,0,5000);
  }
  contents.SectorErrorType = new TH2I("SectorErrorType","",10,1,11,8,1,9);

  contents.SectorErrorType->SetTitle("[PXL] Sector Error Type;Sector ID;Bit");
  contents.SectorErrorType->SetMinimum(0.0);
  TAxis *set_x = (TAxis*)contents.SectorErrorType->GetXaxis();
  set_x->SetBinLabel(1,"1");
  set_x->SetBinLabel(2,"2");
  set_x->SetBinLabel(3,"3");
  set_x->SetBinLabel(4,"4");
  set_x->SetBinLabel(5,"5");
  set_x->SetBinLabel(6,"6");
  set_x->SetBinLabel(7,"7");
  set_x->SetBinLabel(8,"8");
  set_x->SetBinLabel(9,"9");
  set_x->SetBinLabel(10,"10");

  TAxis *set_y = (TAxis*)contents.SectorErrorType->GetYaxis();
  set_y->SetBinLabel(1,"L");
  set_y->SetBinLabel(8,"H");
  

  //%%%%%%%%%%%%%%Histogram Attributes%%%%%%%%%%%%%%
  //%%%%%%%%%%%%%%Histogram Attributes%%%%%%%%%%%%%%
  sprintf(tmpchr,"[PXL] Total Number of Hits per Event;Global Hit Multiplicity per Event;Counts"); 
  contents.GlobalHitMultiplicity->SetTitle(tmpchr);
  contents.GlobalHitMultiplicity->SetFillColor(kMagenta+3);

  for(int i=0; i<NRDO; i++) {
    sprintf(tmpchr,"[PXL] Total Number of Hits per Event (Sector %d);Global Hit Multiplicity per Event;Counts", i+1); 
    contents.GlobalHitMultiplicitySector[i]->SetTitle(tmpchr);
    contents.GlobalHitMultiplicitySector[i]->SetFillColor(kMagenta+3);
    contents.GlobalHitMultiplicitySector[i]->SetFillStyle(3001);

    sprintf(tmpchr, "[PXL] Number of Errors per Event - Sector %d;Event Counter;Error Count", i+1);
    contents.ErrorCountSector[i]->SetTitle(tmpchr);
    contents.ErrorCountSector[i]->SetFillColor(kOrange+7);
    contents.ErrorCountSector[i]->SetFillStyle(3001);
  }

  sprintf(tmpchr,"[PXL] Global Hit Multiplicity per Event;Event Counter;Global Hit Multiplicity"); 
  contents.HitMultiplicityPerEvent->SetTitle(tmpchr);
  contents.HitMultiplicityPerEvent->SetFillColor(kGreen+2);
  
  sprintf(tmpchr,"[PXL] Number of Hits per Ladder;Ladder ID;Hit Multiplicity"); 
  contents.HitsPerLadder->SetTitle(tmpchr);
  contents.HitsPerLadder->SetFillColor(kGreen+2);

  TAxis *hpl_x = (TAxis*)contents.HitsPerLadder->GetXaxis();
  hpl_x->SetBinLabel(1,"1");
  hpl_x->SetBinLabel(5,"2");
  hpl_x->SetBinLabel(9,"3");
  hpl_x->SetBinLabel(13,"4");
  hpl_x->SetBinLabel(17,"5");
  hpl_x->SetBinLabel(21,"6");
  hpl_x->SetBinLabel(25,"7");
  hpl_x->SetBinLabel(29,"8");
  hpl_x->SetBinLabel(33,"9");
  hpl_x->SetBinLabel(37,"10");


  sprintf(tmpchr,"[PXL] Number of Hits per Ladder Event-by-Event;Ladder ID;Hit Multiplicity"); 
  contents.HitsPerLadderPerEvent->SetTitle(tmpchr);
  contents.HitsPerLadderPerEvent->SetFillColor(kGreen+2);
  contents.HitsPerLadderPerEvent->SetFillStyle(3001);

  TAxis *hplpe_x = (TAxis*)contents.HitsPerLadderPerEvent->GetXaxis();
  hplpe_x->SetBinLabel(1,"1");
  hplpe_x->SetBinLabel(5,"2");
  hplpe_x->SetBinLabel(9,"3");
  hplpe_x->SetBinLabel(13,"4");
  hplpe_x->SetBinLabel(17,"5");
  hplpe_x->SetBinLabel(21,"6");
  hplpe_x->SetBinLabel(25,"7");
  hplpe_x->SetBinLabel(29,"8");
  hplpe_x->SetBinLabel(33,"9");
  hplpe_x->SetBinLabel(37,"10");
  
  sprintf(tmpchr,"%s%s%s",
	  "[PXL] Hit Correlation Inner-Outer Layer;",
	  "Outer Layer Hit Multiplicity per Event;",
	  "Inner Layer Hit Multiplicity per Event"); 
  contents.HitCorrelation->SetTitle(tmpchr);
  contents.HitCorrelation->SetMinimum(0.0);


  sprintf(tmpchr,"[PXL] Number of Hits per Sensor (Inner Layer);Sector ID;Sensor ID"); 
  contents.SensorHitsInnerLayer->SetTitle(tmpchr);
  contents.SensorHitsInnerLayer->SetMinimum(0.0);
  TAxis *shi_x = (TAxis*)contents.SensorHitsInnerLayer->GetXaxis();
  shi_x->SetBinLabel(1,"1");
  shi_x->SetBinLabel(2,"2");
  shi_x->SetBinLabel(3,"3");
  shi_x->SetBinLabel(4,"4");
  shi_x->SetBinLabel(5,"5");
  shi_x->SetBinLabel(6,"6");
  shi_x->SetBinLabel(7,"7");
  shi_x->SetBinLabel(8,"8");
  shi_x->SetBinLabel(9,"9");
  shi_x->SetBinLabel(10,"10");
  
  sprintf(tmpchr,"[PXL] Number of Hits per Sensor (Outer Layer);Sector ID;Sensor ID"); 
  contents.SensorHitsOuterLayer->SetTitle(tmpchr);
  contents.SensorHitsOuterLayer->SetMinimum(0.0);
  TAxis *sho_x = (TAxis*)contents.SensorHitsOuterLayer->GetXaxis();
  sho_x->SetBinLabel(1,"1");
  sho_x->SetBinLabel(4,"2");
  sho_x->SetBinLabel(7,"3");
  sho_x->SetBinLabel(10,"4");
  sho_x->SetBinLabel(13,"5");
  sho_x->SetBinLabel(16,"6");
  sho_x->SetBinLabel(19,"7");
  sho_x->SetBinLabel(22,"8");
  sho_x->SetBinLabel(25,"9");
  sho_x->SetBinLabel(28,"10");
 

  sprintf(tmpchr,"[PXL] Intensity Plot of Average Run Length (Inner Layer);Sector ID;Sensor ID"); 
  contents.AverageRunLengthInnerLayer->SetTitle(tmpchr);
  contents.AverageRunLengthInnerLayer->SetMinimum(0.0);
  contents.AverageRunLengthInnerLayer->SetMaximum(4.0);
  TAxis *rli_x = (TAxis*)contents.AverageRunLengthInnerLayer->GetXaxis();
  rli_x->SetBinLabel(1,"1");
  rli_x->SetBinLabel(2,"2");
  rli_x->SetBinLabel(3,"3");
  rli_x->SetBinLabel(4,"4");
  rli_x->SetBinLabel(5,"5");
  rli_x->SetBinLabel(6,"6");
  rli_x->SetBinLabel(7,"7");
  rli_x->SetBinLabel(8,"8");
  rli_x->SetBinLabel(9,"9");
  rli_x->SetBinLabel(10,"10");


  sprintf(tmpchr,"[PXL] Intensity Plot of Average Run Length (Outer Layer);Sector ID;Sensor ID"); 
  contents.AverageRunLengthOuterLayer->SetTitle(tmpchr);
  contents.AverageRunLengthOuterLayer->SetMinimum(0.0);
  contents.AverageRunLengthOuterLayer->SetMaximum(4.0);
  TAxis *rlo_x = (TAxis*)contents.AverageRunLengthOuterLayer->GetXaxis();
  rlo_x->SetBinLabel(1,"1");
  rlo_x->SetBinLabel(4,"2");
  rlo_x->SetBinLabel(7,"3");
  rlo_x->SetBinLabel(10,"4");
  rlo_x->SetBinLabel(13,"5");
  rlo_x->SetBinLabel(16,"6");
  rlo_x->SetBinLabel(19,"7");
  rlo_x->SetBinLabel(22,"8");
  rlo_x->SetBinLabel(25,"9");
  rlo_x->SetBinLabel(28,"10");


  //%%%%%%%%%%%%%%Add root histograms to Plots%%%%%%%%%%%%%%
  //%%%%%%%%%%%%%%Add root histograms to Plots%%%%%%%%%%%%%%
  int np = sizeof(contents) / sizeof(TH1 *);
  JevpPlot *plots[np];

  int n=0;

  plots[n] = new JevpPlot(contents.GlobalHitMultiplicity);
  for (int i=0; i<NRDO; i++) {
    plots[++n] = new JevpPlot(contents.GlobalHitMultiplicitySector[i]);
  }

  plots[++n] = new JevpPlot(contents.HitMultiplicityPerEvent);
  plots[n]->optstat = 10;

  plots[++n] = new JevpPlot(contents.HitsPerLadder);
  plots[n]->logy = 1;
  plots[n]->optstat = 10;

  plots[++n] = new JevpPlot(contents.HitsPerLadderPerEvent);
  plots[n]->logy = 1;
  plots[n]->optstat = 10;

  plots[++n] = new JevpPlot(contents.HitCorrelation);
  plots[n]->optstat = 10;

  plots[++n] = new JevpPlot(contents.SensorHitsInnerLayer);
  plots[n]->optstat = 10;

  plots[++n] = new JevpPlot(contents.SensorHitsOuterLayer);
  plots[n]->optstat = 10;

  plots[++n] = new JevpPlot(contents.AverageRunLengthInnerLayer);
  plots[n]->optstat = 10;
  
  plots[++n] = new JevpPlot(contents.AverageRunLengthOuterLayer);
  plots[n]->optstat = 10;

  for (int i=0; i<NRDO; i++) {
    plots[++n] = new JevpPlot(contents.ErrorCountSector[i]);
    plots[n]->optstat = 10;
  }

  plots[++n] = new JevpPlot(contents.SectorErrorType);
  plots[n]->optstat = 10;

  //plots[++n] = new JevpPlot(contents.h0_evt_size);
  //plots[n]->optstat = 0;

  // Add Plots to plot set...
  for(int i=0;i<=n;i++) {
    LOG(DBG, "Adding plot %d",i);
    addPlot(plots[i]);
  }

}
  

void pxlBuilder::startrun(daqReader *rdr) {
  LOG(NOTE, "pxlBuilder starting run #%d",rdr->run);
  resetAllPlots();

  event_multiplicity = 0;
  multiplicity_inner = 0;
  multiplicity_outer = 0;
  sensor_count = 0;
  number_of_events = 0;

  max_count = 0;
  for(int i=0; i<NRDO; i++) max_count_sector[i] = 0;

  max_count_inner = 0;
  max_count_outer = 0;

  min_count = 1000000;
  for(int i=0; i<NRDO; i++) min_count_sector[i] = 1000000;

  min_count_inner = 1000000;
  min_count_outer = 1000000;

  memset(count_hits_inner, 0, sizeof(count_hits_inner[0][0]) * 10 * 10);
  memset(count_hits_outer, 0, sizeof(count_hits_outer[0][0]) * 30 * 10);
  memset(count_length_inner, 0, sizeof(count_length_inner[0][0]) * 10 * 10);
  memset(count_length_outer, 0, sizeof(count_length_outer[0][0]) * 30 * 10);

  memset(sensor_hits, 0, sizeof(sensor_hits[0][0])*400);
  memset(sensor_hit_frequency, 0, sizeof(sensor_hit_frequency[0][0])*400);
  memset(sensor_hit_frequency_SE, 0, sizeof(sensor_hit_frequency_SE[0][0])*400);
  memset(avg_run_length, 0.0, sizeof(avg_run_length[0][0])*400);

  LadderMap = new map<int,int>();
  SetLadderMap();
}

#define safelog(x) ((x > 0) ? log10(x) : 0)

void pxlBuilder::event(daqReader *rdr){
  //int pxl_size = rdr->getDetectorSize("pxl");
  //contents.myhisto->Fill(safelog(pxl_size));

  //Reset Variables
  event_multiplicity = 0;
  multiplicity_inner = 0;
  multiplicity_outer = 0;
  sensor_count = 0;
  
  memset(sensor_hits, 0, sizeof(sensor_hits[0][0])*400);
  memset(avg_run_length, 0.0, sizeof(avg_run_length[0][0])*400);
  memset(sensor_hit_frequency_SE, 0, sizeof(sensor_hit_frequency_SE[0][0])*400);
  AverageRunLength = new map<int,double>();
  LadderCount = new map<int,int>();


  //+++++++++++++++++++++++Per Event Decoding+++++++++++++++++++++++
  //+++++++++++++++++++++++Per Event Decoding+++++++++++++++++++++++
  //+++++++++++++++++++++++Per Event Decoding+++++++++++++++++++++++
  
  /*************************** PXL **************************/
  daq_dta *dd ;
  dd = rdr->det("pxl")->get("raw") ;
  if(dd) { // "pxl" was found
    //Reset Histos
    contents.HitsPerLadderPerEvent->Reset("ICESM");
    
    // iterate over RDO's (each RDO has 4 ladders)
    int event_count = 0;

    while(dd->iterate()) {
      int OVF[NSENSOR];
      int error_cnt;
      error_cnt = 0;
      
      
      int pxl_sector = (dd->sec-1)*5 + dd->rdo;
      //printf("DAQ Sector %d, RDO %d (PXL Sector %d): %d words\n",
      //dd->sec, dd->rdo,pxl_sector, dd->ncontent/4) ;
       if((pxl_sector>10) || (pxl_sector<1)) {
	 cout<<"pxl_sector error: "<< pxl_sector<< " is not a valid PXL sector. Skipping..."<<endl;
	 continue;
      }
     
      for(int i=0; i<NSENSOR; i++) {
	OVF[i] = 0;
	for(int j=0; j<NROW; j++) {
	  bs[i][j].reset();
	}
      }
      
      u_int *d = (u_int *)dd->Void ; // point to the start of the DDL raw data	
      int wordLength = dd->ncontent/4; // number of 32bit words

      int ret = pxl_decoder(d, wordLength, bs, OVF, &pxlHeaderInfo, ave_runlength, &error_cnt);
      // if (ret != 0){
      // 	//printf("pxl_decoder returned 0x%x, error_cnt = %d\n", ret, error_cnt);
      // }
      // else{
      // 	//printf("pxl_decoder returned no errors\n");
      // }
      
      
      int sensor_count = 0;
      int row_count = 0;
      int sector_count = 0;
      int sensor_id = 0;

      for(int i=0; i<NSENSOR; i++){
	sensor_count = 0;
	sensor_id = i+1;

	for(int j=0; j<NROW; j++) {
	  row_count = bs[i][j].count();

	  //if(row_count != 0) cout<<row_count<<endl;

	  IncrementMultiplicity(i+1, row_count);
	  sensor_count += row_count;
	  event_count +=  row_count;
	  sector_count +=  row_count;
	  
	}


	UpdateLadderCount(pxl_sector, sensor_id, sensor_count);
	avg_run_length[pxl_sector - 1][i] = ave_runlength[i];
	sensor_hits[pxl_sector - 1][i] = sensor_count;

	if(ave_runlength[i] != 0.0){
	  sensor_hit_frequency[pxl_sector - 1][i]++;
	  sensor_hit_frequency_SE[pxl_sector - 1][i]++;
	}

      } //Loop over sensors
  
      int k = pxl_sector-1;
      ((sector_count > max_count_sector[k]) && (max_count_sector[k] = sector_count));
      ((sector_count < min_count_sector[k]) && (min_count_sector[k] = sector_count));
	
      contents.GlobalHitMultiplicitySector[k]->Fill(sector_count);
      if(!(number_of_events % 30))
	contents.GlobalHitMultiplicitySector[k]->SetAxisRange(min_count_sector[k]-10,
							      max_count_sector[k]+10);
      
      if((number_of_events < 5000) && (error_cnt > 0))
	contents.ErrorCountSector[k]->Fill(number_of_events, error_cnt);

      UpdateSectorErrorTypeTH2(contents.SectorErrorType,ret,pxl_sector);
      
    }
    //After dd loop


    FillLadderHistogram(contents.HitsPerLadderPerEvent);

    UpdateLadderHistogram(contents.HitsPerLadder, contents.HitsPerLadderPerEvent, number_of_events);

    UpdateLayerHistograms(contents.SensorHitsInnerLayer, contents.AverageRunLengthInnerLayer,
			  contents.SensorHitsOuterLayer, contents.AverageRunLengthOuterLayer,
			  number_of_events);



    //^^^HIST^^^::GlobalHitMultiplicity
    (event_count > max_count) && (max_count = event_count);
    (event_count < min_count) && (min_count = event_count);
    //cout<<event_count<<endl;
    contents.GlobalHitMultiplicity->Fill(event_count);
    if(!(number_of_events % 30)){
      //contents.GlobalHitMultiplicity->SetAxisRange(1,number_of_events+10);
      for(int i=0; i<NRDO; i++) contents.ErrorCountSector[i]->SetAxisRange(1,number_of_events+10);
    }

    //^^^HIST^^^::HitCorrelation
    (multiplicity_inner > max_count_inner) && (max_count_inner = multiplicity_inner);
    (multiplicity_inner < min_count_inner) && (min_count_inner = multiplicity_inner);

    (multiplicity_outer > max_count_outer) && (max_count_outer = multiplicity_outer);
    (multiplicity_outer < min_count_outer) && (min_count_outer = multiplicity_outer);

    contents.HitCorrelation->Fill(multiplicity_outer,multiplicity_inner);
    /*
      if(!(number_of_events % 30)){
      contents.HitCorrelation->SetAxisRange(min_count_outer+10,max_count_outer+10);
      contents.HitCorrelation->SetAxisRange(min_count_inner+10,max_count_inner+10,"Y");
      }
    */

    //^^^HIST^^^::HitMultiplicityPerEvent
    if(number_of_events < 5000){
      int event_bin = (number_of_events/10)+1;
      if(!(number_of_events % 30)) contents.HitMultiplicityPerEvent->SetAxisRange(0,number_of_events+30);
      if(!UpdateTH1(contents.HitMultiplicityPerEvent,event_bin,(double)(event_count/10)))
	cout << "Something is very wrong with HitMultiplicityPerEvent!" << endl;
    }
    else {
      cout<<"FYI: Number of events in evp > 5000"<<endl;
    }

    
    number_of_events++;
  } // ...if(dd)
  
  else {
    //printf("no PXL found\n");
  }
  
  //number_of_events++;
  // End Fill Histograms...
  delete AverageRunLength;
  delete LadderCount;
}

void pxlBuilder::stoprun(daqReader *rdr) {
}

void pxlBuilder::main(int argc, char *argv[]){
  pxlBuilder me;
  
  me.Main(argc, argv);
}

#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"

#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1D.h>
#include <TH2F.h>

#include <math.h>
#include <RTS/include/rtsLog.h>

#include "pxlBuilder.h"



void pxlBuilder::IncrementMultiplicity(int sensor_number,int count)
{
  (sensor_number < 11) ? multiplicity_inner += count : multiplicity_outer += count;
}

int pxlBuilder::WhichLadder(int sector_number,int sensor_number)
{
  //sector_number 1 - 10, sensor 1 - 40, returns ladder number 1 - 40
  int ladder_number = 1+(int)((sensor_number-1)/10)+(4*(sector_number-1));
  return ladder_number;
}

void pxlBuilder::UpdateLadderCount(int sector_number,int sensor_number,int sensor_count)
{
  int ladder_number = WhichLadder(sector_number,sensor_number);

  if(LadderCount->count(ladder_number) == 0)
    LadderCount->insert(make_pair(ladder_number,sensor_count));
  else
    LadderCount->find(ladder_number)->second += sensor_count;
}

int pxlBuilder::GetLadderCount(int ladder_number)
{
  if(LadderCount->count(ladder_number) == 0)
    return (0);
  else
    return LadderCount->find(ladder_number)->second;
}


void pxlBuilder::FillLadderHistogram(TH1 *hist)
{
  for(int i=1; i<=40; i++) {
    hist->SetBinContent(i,(double)(GetLadderCount(i)));
  }  
}

void pxlBuilder::UpdateLadderHistogram(TH1 *hist, TH1 *hist_single_evt, int number_of_events_old)
{
  hist->Scale(number_of_events_old);
  hist->Add(hist_single_evt);
  hist->Scale(1.0/(number_of_events_old + 1.0));
}

void pxlBuilder::UpdateLayerHistograms(TH1 *hits_in, TH1 *rl_in,
				       TH1 *hits_out, TH1 *rl_out,
				       int number_of_events)
{
  for(int sec = 1; sec<=10; sec++) {
    for(int sen = 1; sen<=10; sen++) { // Inner Ladder
      int ladder_bin = sec;
      int sen_bin = sen;
      double rl = avg_run_length[sec-1][sen-1];
      int freq_total = sensor_hit_frequency[sec-1][sen-1];
      int hits = sensor_hits[sec-1][sen-1];

      // Run Length
      if (rl > 0.0) {
	double rl_content = (double)rl_in->GetBinContent(ladder_bin, sen_bin);
	(rl_content < 0.0) && (rl_content = 0.0);
	double new_rl_content = (rl_content * (freq_total-1) + rl) / freq_total;
	rl_in->SetBinContent(ladder_bin, sen_bin, new_rl_content);
      }
      // Hits
      double hits_content = (double)hits_in->GetBinContent(ladder_bin, sen_bin);
      (hits_content < 0.0) && (hits_content = 0.0);
      double new_hits_content = ((hits_content * number_of_events) + hits)/(number_of_events + 1);
      hits_in->SetBinContent(ladder_bin, sen_bin, new_hits_content);
    }

    for(int sen = 11; sen<=40; sen++) { // Outer Ladders
      int sen_bin = (sen-1)%10 + 1;
      int ladder_bin = (sec-1)*3 + (int)((sen-1)/10);
      double rl = avg_run_length[sec-1][sen-1];
      int freq_total = sensor_hit_frequency[sec-1][sen-1];
      int hits = sensor_hits[sec-1][sen-1];
      
      // Run Length
      if (rl > 0.0) {
	double rl_content = (double)rl_out->GetBinContent(ladder_bin, sen_bin);
	(rl_content < 0.0) && (rl_content = 0.0);
	double new_rl_content = (rl_content * (freq_total-1) + rl) / freq_total;
	rl_out->SetBinContent(ladder_bin, sen_bin, new_rl_content);
      }
      // Hits
      double hits_content = (double)hits_out->GetBinContent(ladder_bin, sen_bin);
      (hits_content < 0.0) && (hits_content = 0.0);
      double new_hits_content = ((hits_content * number_of_events) + hits)/(number_of_events + 1);
      hits_out->SetBinContent(ladder_bin, sen_bin, new_hits_content);
    }
  }
}


int pxlBuilder::IncrementArray(const char* name,int x_bin,int y_bin)
{
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

bool pxlBuilder::UpdateTH1(TH1 *hist,int bin,double value)
{
  double bin_content = (double)hist->GetBinContent(bin);
  (bin_content < 0.0) && (bin_content = 0.0);
  bin_content += value;
  if(bin_content < 0.0)
    return false;
  else {
    hist->SetBinContent(bin,bin_content);
    return ((double)hist->GetBinContent(bin) == bin_content) ? true : false;
  }
}

bool pxlBuilder::UpdateTH1_Scale(TH1 *hist, int bin, double value,
				 int number_of_events_old)
{
  double bin_content = (double)hist->GetBinContent(bin);
  (bin_content < 0.0) && (bin_content = 0.0);
  double new_content;
  if(number_of_events_old >= 0)
    new_content = (bin_content * (double)number_of_events_old + value)/((double)number_of_events_old + 1.0);

  if(new_content < 0.0)
    return false;
  else {
    hist->SetBinContent(bin,new_content);
    return ((double)hist->GetBinContent(bin) == new_content) ? true : false;
  }
}

bool pxlBuilder::UpdateTH2(TH1 *hist,int x_bin,int y_bin,double value)
{
  if(value < 0.0)
    return false;
  else {
    double bin_content = 0.0;
    bin_content += (double)hist->GetBinContent(x_bin,y_bin);
    ((!bin_content) || (bin_content < 0.0)) && (bin_content = 0.0);
    
    double new_content = bin_content+value;
    if(new_content < 0.0) 
      return false;
    else {
      hist->SetBinContent(x_bin,y_bin,new_content);
      return (hist->GetBinContent(x_bin,y_bin) == new_content) ? true : false;
    }
  }
}

bool pxlBuilder::UpdateTH2_Scale(TH1 *hist,int x_bin,int y_bin,double value, int number_of_events_old)
{
  if(value < 0.0)
    return false;
  else {
    double bin_content = 0.0;
    bin_content += (double)hist->GetBinContent(x_bin,y_bin);
    ((!bin_content) || (bin_content < 0.0)) && (bin_content = 0.0);
    
    double new_content;
    if(number_of_events_old > 0 && bin_content > 0)
      new_content = (double)((bin_content * number_of_events_old) + value)/(number_of_events_old + 1.0);
    else new_content = bin_content+value;

    if(new_content < 0.0)
      return false;
    else {
      hist->SetBinContent(x_bin,y_bin,new_content);
      return (hist->GetBinContent(x_bin,y_bin) == new_content) ? true : false;
    }
  }
}

bool pxlBuilder::UpdateTH2_Scale2(const char* name,
				  TH1 *hist, int x_bin, int y_bin, double value,
				  int number_of_events_old)
{
  if(value < 0.0)
    return false;
  else {
    double bin_content = 0.0;
    bin_content += (double)hist->GetBinContent(x_bin,y_bin);
    ((!bin_content) || (bin_content < 0)) && (bin_content = 0.0);
    
    double new_content;
    int count = IncrementArray(name,x_bin,y_bin);
    if(count == 1) new_content = value;
    else{
      if((number_of_events_old > 0) && (bin_content > 0))
	new_content = (double)((bin_content*(count-1)*number_of_events_old)+value) /
	  ((double)(count)*(number_of_events_old + 1.0));
      else new_content = bin_content+value;
    }
    if(new_content < 0.0) 
      return false;
    else {
      hist->SetBinContent(x_bin,y_bin,new_content);
      return (hist->GetBinContent(x_bin,y_bin) == new_content) ? true : false;
    }
  }
}

void pxlBuilder::UpdateSectorErrorTypeTH2(TH1 *hist, int ret, int sector_number)
{
  for(int i=0; i<10; i++) {
    int errbit = 0x001<<i;
    if((ret & errbit) == errbit)  UpdateTH2(hist,sector_number,i+1,1.0);
  }
}


//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Class Implementation %%%%%%%%%%
ClassImp(pxlBuilder);
  

pxlBuilder::pxlBuilder(JevpServer *parent) : JevpBuilder(parent)
{
  plotsetname = (char *)"pxl";

  // start with histograms undefined...
  memset(&contents, 0, sizeof(contents));

  // allocate memory for bitset
  bs = new bitset2D<NROW,NCOL>[NSENSOR];

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
  memset(avg_run_length, 0.0, sizeof(avg_run_length[0][0])*400);
}

pxlBuilder::~pxlBuilder()
{
  // Delete any existing histograms...
  int n = sizeof(contents) / sizeof(TH1 *);
  for(int i=0;i<n;i++) {
    if(contents.array[i]) delete contents.array[i];
  }

  // Delete memory for the bitset array
  delete bs;
}

void pxlBuilder::initialize(int argc, char *argv[])
{
  // Initialization of histograms.

  char tmpchr[255];
  //%%%%%%%%%%%%%%Creating Histograms%%%%%%%%%%%%%%

  //Tab 1: PXL GlobalMultiplicity Plots
  contents.GlobalHitMultiplicity = new TH1D("GlobalHitMultiplicity","",1000,0,150000);
  
  for (int i=0; i<NRDO; i++) {
    sprintf(tmpchr, "GlobalHitMultiplicitySector%d", i+1);
    contents.GlobalHitMultiplicitySector[i] = new TH1D(tmpchr,"",1000,0,20000);
  }

  //Tab 2: PXL Hit Multiplicity
  contents.HitMultiplicityPerEvent = new TH1D("HitMultiplicityPerEvent","",500,0,5000);
  //contents.HitMultiplicityPerEvent = new TH1D("HitMultiplicityPerEvent","",500,0,500);

  contents.HitsPerLadder = new TH1D("HitsPerLadder","",40,1,41);
  contents.HitsPerLadderPerEvent = new TH1D("HitsPerLadderPerEvent","",40,1,41);
  
  contents.HitCorrelation = new TH2D("HitCorrelation","", 100,0.,80000., 100,0.,100000.);
  
  
  //Tab 3: PXL Hit Map Plots
  contents.SensorHitsInnerLayer = new TH2D("SensorHitsInnerLayer", "",  10,1,11, 10,1,11);
  contents.SensorHitsOuterLayer = new TH2D("SensorHitsOuterLayer", "", 30,1,31, 10,1,11);
  contents.AverageRunLengthInnerLayer = new TH2D("AverageRunLengthInnerLayer","", 10,1,11, 10,1,11);
  contents.AverageRunLengthOuterLayer = new TH2D("AverageRunLengthOuterLayer","", 30,1,31, 10,1,11);

  //Tab 4: PXL Error Plots
  for (int i=0; i<NRDO; i++) {
    sprintf(tmpchr, "ErrorCountSector%d", i+1);
    contents.ErrorCountSector[i] = new TH1D(tmpchr,"",500,0,5000);
  }
  contents.SectorErrorType = new TH2I("SectorErrorType","",10,1,11, 10,1,11);
  
  contents.SerdesErrors = new TH2D("SerdesErrors", "", 40,1,41, 10,1,11);

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
  set_y->SetBinLabel(10,"H");
  

  //%%%%%%%%%%%%%%Histogram Attributes%%%%%%%%%%%%%%
  sprintf(tmpchr,"%s%s", 
	  "[PXL] Total Number of Hits per Event;",
	  "Global Hit Multiplicity per Event;Counts"); 
  contents.GlobalHitMultiplicity->SetTitle(tmpchr);
  contents.GlobalHitMultiplicity->SetFillColor(kMagenta+3);

  for(int i=0; i<NRDO; i++) {
    sprintf(tmpchr,"%s%d%s", 
	    "[PXL] Total Number of Hits per Event (Sector ",
	    i+1, ");Global Hit Multiplicity per Event;Counts"); 
    contents.GlobalHitMultiplicitySector[i]->SetTitle(tmpchr);
    contents.GlobalHitMultiplicitySector[i]->SetFillColor(kMagenta+3);
    contents.GlobalHitMultiplicitySector[i]->SetFillStyle(3001);

    sprintf(tmpchr, "%s%d%s",
	    "[PXL] Number of Errors per Event - Sector ",
	    i+1, ";Event Counter;Error Count");
    contents.ErrorCountSector[i]->SetTitle(tmpchr);
    contents.ErrorCountSector[i]->SetFillColor(kOrange+7);
    contents.ErrorCountSector[i]->SetFillStyle(3001);
  }

  sprintf(tmpchr,"%s%s",
	  "[PXL] Global Hit Multiplicity per Event;",
	  "Event Counter;Global Hit Multiplicity"); 
  contents.HitMultiplicityPerEvent->SetTitle(tmpchr);
  contents.HitMultiplicityPerEvent->SetFillColor(kGreen+2);
  //contents.HitMultiplicityPerEvent->SetBit(TH1::kCanRebin);

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

  sprintf(tmpchr,"%s%s%s",
	  "[PXL] Number of Hits per Ladder Event-by-Event;",
	  "Ladder ID;",
	  "Hit Multiplicity"); 
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
 

  sprintf(tmpchr,"%s%s",
	  "[PXL] Intensity Plot of Average Run Length (Inner Layer);",
	  "Sector ID;Sensor ID"); 
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

  sprintf(tmpchr,"%s%s",
	  "[PXL] Intensity Plot of Average Run Length (Outer Layer);",
	  "Sector ID;Sensor ID"); 
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

  sprintf(tmpchr,"%s%s",
	  "[PXL] Total Serdes Errors;",
	  "Sector ID;Sensor ID"); 
  contents.SerdesErrors->SetTitle(tmpchr);

  TAxis *ser_x = (TAxis*)contents.SerdesErrors->GetXaxis();
  ser_x->SetBinLabel(1,"1");
  ser_x->SetBinLabel(5,"2");
  ser_x->SetBinLabel(9,"3");
  ser_x->SetBinLabel(13,"4");
  ser_x->SetBinLabel(17,"5");
  ser_x->SetBinLabel(21,"6");
  ser_x->SetBinLabel(25,"7");
  ser_x->SetBinLabel(29,"8");
  ser_x->SetBinLabel(33,"9");
  ser_x->SetBinLabel(37,"10");


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

  plots[++n] = new JevpPlot(contents.SerdesErrors);
  plots[n]->optstat = 10;

  //plots[++n] = new JevpPlot(contents.h0_evt_size);
  //plots[n]->optstat = 0;

  // Add Plots to plot set...
  for(int i=0;i<=n;i++) {
    LOG(DBG, "Adding plot %d",i);
    addPlot(plots[i]);
  }
}
  

void pxlBuilder::startrun(daqReader *rdr)
{
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
  memset(avg_run_length, 0.0, sizeof(avg_run_length[0][0])*400);
}


void pxlBuilder::event(daqReader *rdr)
{
  int OVF[NSENSOR];
  float ave_runlength[NSENSOR];
  _pxlHeaderInfo pxlHeaderInfo;
  int error_cnt;
  int sector_count;
  int sensor_id;
  //int row_count;

  //Reset Variables
  event_multiplicity = 0;
  multiplicity_inner = 0;
  multiplicity_outer = 0;
  event_count = 0;
  
  memset(sensor_hits, 0, sizeof(sensor_hits[0][0])*400);
  memset(avg_run_length, 0.0, sizeof(avg_run_length[0][0])*400);
  LadderCount = new map<int,int>();

  //+++++++++++++++++++++++Per Event Decoding+++++++++++++++++++++++
  daq_dta *dd ;
  dd = rdr->det("pxl")->get("raw") ;
  if(dd) { // "pxl" was found
    //Reset Histos
    contents.HitsPerLadderPerEvent->Reset("ICESM");
    
    // iterate over RDO's (each RDO has 4 ladders)
    while(dd->iterate()) {
      int pxl_sector = (dd->sec-1)*5 + dd->rdo;
      //printf("DAQ Sector %d, RDO %d (PXL Sector %d): %d words\n",
      //dd->sec, dd->rdo,pxl_sector, dd->ncontent/4) ;
       if((pxl_sector>10) || (pxl_sector<1)) {
	 cout<<"pxl_sector error: "<< pxl_sector<< " is not a valid PXL sector. Skipping..."<<endl;
	 continue;
      }
     
      u_int *d = (u_int *)dd->Void ; // point to the start of the DDL raw data	
      int wordLength = dd->ncontent/4; // number of 32bit words

      // decode the sector
      error_cnt = 0;
      int ret = pxl_decoder(d, wordLength, bs, OVF, &pxlHeaderInfo, ave_runlength, &error_cnt);
      // if (ret != 0){printf("pxl_decoder returned 0x%x, error_cnt = %d\n", ret, error_cnt);}
      // else {printf("pxl_decoder returned no errors\n");}
      
      sector_count = 0;
      for(int i=0; i<NSENSOR; i++){
	sensor_id = i+1;
	sensor_count = bs[i].count();
	
	IncrementMultiplicity(i+1, sensor_count);
	event_count += sensor_count;
	sector_count += sensor_count;

	UpdateLadderCount(pxl_sector, sensor_id, sensor_count);
	avg_run_length[pxl_sector - 1][i] = ave_runlength[i];
	sensor_hits[pxl_sector - 1][i] = sensor_count;

	if(ave_runlength[i] != 0.0){
	  sensor_hit_frequency[pxl_sector-1][i]++;
	}

      } //Loop over sensors
  
      int k = pxl_sector-1;

      u_int serdesErr1 = d[8]; // sensors 1 - 32
      u_int serdesErr2 = d[9]; // sensors 33 - 40
      for (int i=0; i<32; i++)
	if(((0x1<<i) & serdesErr1) == (u_int)(0x1<<i)) {
	  contents.SerdesErrors->Fill(k*4+(int)(i/10)+1, i%10+1);
	}
      for (int i=0; i<8; i++)
	if(((0x1<<i) & serdesErr2) == (u_int)(0x1<<i)) {
	  contents.SerdesErrors->Fill(k*4+4, i+3);
	}

      ((sector_count > max_count_sector[k]) && (max_count_sector[k] = sector_count));
      ((sector_count < min_count_sector[k]) && (min_count_sector[k] = sector_count));
	
      // if((number_of_events % 100) == 0) {
      // 	// Double_t xmin = (Double_t)(min_count_sector[k]-0.05*min_count_sector[k]);
      // 	// Double_t xmax = (Double_t)(max_count_sector[k]+0.05*max_count_sector[k]);
      // 	Double_t xmin = (Double_t)(min_count_sector[k]-100);
      // 	Double_t xmax = (Double_t)(max_count_sector[k]+100);
      // 	if (xmin < 0.0) xmin = 0.0;
      // 	if (xmax < (xmin + 1000.)) xmax = xmin+1000.0;
      // 	contents.GlobalHitMultiplicitySector[k]->SetAxisRange(xmin,xmax);
      // }
      contents.GlobalHitMultiplicitySector[k]->Fill(sector_count);
      
      if(number_of_events < 5000) {
	int event_bin = (int)(number_of_events/10)+1;
	if(!UpdateTH1_Scale(contents.ErrorCountSector[k],event_bin,(double)error_cnt,
			    number_of_events%10))
	  cout << "Something is very wrong with ErrorCountSector" << k << endl;
	//contents.ErrorCountSector[k]->Fill(number_of_events, error_cnt);
      }
      UpdateSectorErrorTypeTH2(contents.SectorErrorType,ret,pxl_sector);
    } // end of loop over RDOs (= sectors)

    FillLadderHistogram(contents.HitsPerLadderPerEvent);
    UpdateLadderHistogram(contents.HitsPerLadder, contents.HitsPerLadderPerEvent, number_of_events);
    UpdateLayerHistograms(contents.SensorHitsInnerLayer, contents.AverageRunLengthInnerLayer,
			  contents.SensorHitsOuterLayer, contents.AverageRunLengthOuterLayer,
			  number_of_events);

    //^^^HIST^^^::GlobalHitMultiplicity
    (event_count > max_count) && (max_count = event_count);
    (event_count < min_count) && (min_count = event_count);
    contents.GlobalHitMultiplicity->Fill(event_count);
    // if((number_of_events % 100) == 0) {
    //   // Double_t xmin = (Double_t)(min_count-0.05*min_count);
    //   // Double_t xmax = (Double_t)(max_count+0.05*max_count);
    //   Double_t xmin = (Double_t)(min_count-100);
    //   Double_t xmax = (Double_t)(max_count+100);
    //   if (xmin < 0.0) xmin = 0.0;
    //   if (xmax < (xmin + 1000.)) xmax = xmin+1000.0;
    //   contents.GlobalHitMultiplicity->SetAxisRange(xmin,xmax);
    // }

    //^^^HIST^^^::HitCorrelation
    (multiplicity_inner > max_count_inner) && (max_count_inner = multiplicity_inner);
    (multiplicity_inner < min_count_inner) && (min_count_inner = multiplicity_inner);

    (multiplicity_outer > max_count_outer) && (max_count_outer = multiplicity_outer);
    (multiplicity_outer < min_count_outer) && (min_count_outer = multiplicity_outer);

    contents.HitCorrelation->Fill(multiplicity_outer,multiplicity_inner);
    //   if((number_of_events % 100) == 0) {
    //   contents.HitCorrelation->SetAxisRange(min_count_outer+10,max_count_outer+10);
    //   contents.HitCorrelation->SetAxisRange(min_count_inner+10,max_count_inner+10,"Y");
    //   }

    //^^^HIST^^^::HitMultiplicityPerEvent
    if(number_of_events < 5000) {
      int event_bin = (int)(number_of_events/10)+1;
      if(!UpdateTH1_Scale(contents.HitMultiplicityPerEvent,event_bin,(double)event_count,
    			  number_of_events%10))
    	cout << "Something is very wrong with HitMultiplicityPerEvent!" << endl;

      if((number_of_events % 100) == 0) {
	// every 100 events rescale histos 
	//contents.HitMultiplicityPerEvent->SetAxisRange(0,number_of_events+100);
    	contents.HitMultiplicityPerEvent->GetXaxis()->SetRangeUser(0,number_of_events+100);
	for(int i=0; i<NRDO; i++)
	  contents.ErrorCountSector[i]->GetXaxis()->SetRangeUser(0,number_of_events+100);
      }
    }
    else {
      cout<<"FYI: Number of events in evp > 5000"<<endl;
    }

    number_of_events++;
  } // ...if(dd)
  
  else {
    //printf("no PXL found\n");
  }
  
  // End Fill Histograms...
  delete LadderCount;
}

void pxlBuilder::stoprun(daqReader *rdr) {}

void pxlBuilder::main(int argc, char *argv[])
{
  pxlBuilder me;
  me.Main(argc, argv);
}

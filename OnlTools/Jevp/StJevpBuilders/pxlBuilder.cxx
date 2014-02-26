#include <stdio.h>
#include <stdlib.h>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"

#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include "pxlBuilder.h"
#include <RTS/include/rtsLog.h>

#include "pxlBuilder_helper_funcs.h"


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
  max_count_sector1 = 0;
  max_count_sector2 = 0;
  max_count_sector3 = 0;
  max_count_sector4 = 0;
  max_count_sector5 = 0;
  max_count_sector6 = 0;
  max_count_sector7 = 0;
  max_count_sector8 = 0;
  max_count_sector9 = 0;
  max_count_sector10 = 0;

  max_count_inner = 0;
  max_count_outer = 0;

  min_count = 1000000;
  min_count_sector1 = 1000000;
  min_count_sector2 = 1000000;
  min_count_sector3 = 1000000;
  min_count_sector4 = 1000000;
  min_count_sector5 = 1000000;
  min_count_sector6 = 1000000;
  min_count_sector7 = 1000000;
  min_count_sector8 = 1000000;
  min_count_sector9 = 1000000;
  min_count_sector10 = 1000000;

  min_count_inner = 1000000;
  min_count_outer = 1000000;

  memset(count_hits_inner, 0, sizeof(count_hits_inner[0][0]) * 10 * 10);
  memset(count_hits_outer, 0, sizeof(count_hits_outer[0][0]) * 30 * 10);
  memset(count_length_inner, 0, sizeof(count_length_inner[0][0]) * 10 * 10);
  memset(count_length_outer, 0, sizeof(count_length_outer[0][0]) * 30 * 10);

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
  //contents.myhisto = new TH1F("myhisto","My Histogram",50,0,10);
  //contents.GlobalHitMultiplicity =  new TH1F("GlobalHitMultiplicity","Distribution of the Total Number of Hits per Event",400,1,8001);

  //%%%%%%%%%%%%%%Creating Histograms%%%%%%%%%%%%%%
  //%%%%%%%%%%%%%%Creating Histograms%%%%%%%%%%%%%%
  //Tab 1: PXL Multiplicity Plots
  contents.GlobalHitMultiplicity = new TH1I("GlobalHitMultiplicity","",2500,0,75000);
  

  contents.GlobalHitMultiplicitySector1 = new TH1I("GlobalHitMultiplicitySector1","",2500,0,50000);
  contents.GlobalHitMultiplicitySector2 = new TH1I("GlobalHitMultiplicitySector2","",2500,0,50000);
  contents.GlobalHitMultiplicitySector3 = new TH1I("GlobalHitMultiplicitySector3","",2500,0,50000);
  contents.GlobalHitMultiplicitySector4 = new TH1I("GlobalHitMultiplicitySector4","",2500,0,50000);
  contents.GlobalHitMultiplicitySector5 = new TH1I("GlobalHitMultiplicitySector5","",2500,0,50000);
  contents.GlobalHitMultiplicitySector6 = new TH1I("GlobalHitMultiplicitySector6","",2500,0,50000);
  contents.GlobalHitMultiplicitySector7 = new TH1I("GlobalHitMultiplicitySector7","",2500,0,50000);
  contents.GlobalHitMultiplicitySector8 = new TH1I("GlobalHitMultiplicitySector8","",2500,0,50000);
  contents.GlobalHitMultiplicitySector9 = new TH1I("GlobalHitMultiplicitySector9","",2500,0,50000);
  contents.GlobalHitMultiplicitySector10 = new TH1I("GlobalHitMultiplicitySector10","",2500,0,50000);

  contents.HitMultiplicityPerEvent = new TH1I("HitMultiplicityPerEvent","",500,0,5000);
  
  contents.HitsPerLadder = new TH1I("HitsPerLadder","",40,1,41);
  contents.HitsPerLadderPerEvent = new TH1I("HitsPerLadderPerEvent","",40,1,41);
  
  contents.HitCorrelation = new TH2D("HitCorrelation","",100,0.,10000.,100,0.,10000.);
  
  
  //Tab 2: PXL Hit Maps
  contents.SensorHitsInnerLayer = new TH2D("SensorHitsInnerLayer","",10,1,11,10,1,11);
  
  contents.SensorHitsOuterLayer = new TH2D("SensorHitsOuterLayer","",30,1,31,10,1,11);
  
  contents.AverageRunLengthInnerLayer = new TH2D("AverageRunLengthInnerLayer","",10,1,11,10,1,11);
  contents.AverageRunLengthOuterLayer = new TH2D("AverageRunLengthOuterLayer","",30,1,31,10,1,11);

  /*
  contents.ErrorCountSector1 = new TH1I("ErrorCountSector1","",500,0,5000);
  contents.ErrorCountSector2 = new TH1I("ErrorCountSector2","",500,0,5000);
  contents.ErrorCountSector3 = new TH1I("ErrorCountSector3","",500,0,5000);
  contents.ErrorCountSector4 = new TH1I("ErrorCountSector4","",500,0,5000);
  contents.ErrorCountSector5 = new TH1I("ErrorCountSector5","",500,0,5000);
  contents.ErrorCountSector6 = new TH1I("ErrorCountSector6","",500,0,5000);
  contents.ErrorCountSector7 = new TH1I("ErrorCountSector7","",500,0,5000);
  contents.ErrorCountSector8 = new TH1I("ErrorCountSector8","",500,0,5000);
  contents.ErrorCountSector9 = new TH1I("ErrorCountSector9","",500,0,5000);
  contents.ErrorCountSector10 = new TH1I("ErrorCountSector10","",500,0,5000);
  */

  //contents.->SetTitle("Number of Errors per Event;Global Hit Multiplicity per Event;Counts");
  //contents.->SetFillColor(kOrange+7);
  

  //%%%%%%%%%%%%%%Histogram Attributes%%%%%%%%%%%%%%
  //%%%%%%%%%%%%%%Histogram Attributes%%%%%%%%%%%%%%
  contents.GlobalHitMultiplicity->SetTitle("Distribution of the Total Number of Hits per Event;Global Hit Multiplicity per Event;Counts");
  contents.GlobalHitMultiplicity->SetFillColor(kMagenta+3);

  contents.GlobalHitMultiplicitySector1->SetTitle("Distribution of the Total Number of Hits per Event (Sector 1);Global Hit Multiplicity per Event;Counts");
  contents.GlobalHitMultiplicitySector1->SetFillColor(kMagenta+3);
  contents.GlobalHitMultiplicitySector1->SetFillStyle(3001);

  contents.GlobalHitMultiplicitySector2->SetTitle("Distribution of the Total Number of Hits per Event (Sector 2);Global Hit Multiplicity per Event;Counts");
  contents.GlobalHitMultiplicitySector2->SetFillColor(kMagenta+3);
  contents.GlobalHitMultiplicitySector2->SetFillStyle(3001);

  contents.GlobalHitMultiplicitySector3->SetTitle("Distribution of the Total Number of Hits per Event (Sector 3);Global Hit Multiplicity per Event;Counts");
  contents.GlobalHitMultiplicitySector3->SetFillColor(kMagenta+3);
  contents.GlobalHitMultiplicitySector3->SetFillStyle(3001);

  contents.GlobalHitMultiplicitySector4->SetTitle("Distribution of the Total Number of Hits per Event (Sector 4);Global Hit Multiplicity per Event;Counts");
  contents.GlobalHitMultiplicitySector4->SetFillColor(kMagenta+3);
  contents.GlobalHitMultiplicitySector4->SetFillStyle(3001);

  contents.GlobalHitMultiplicitySector5->SetTitle("Distribution of the Total Number of Hits per Event (Sector 5);Global Hit Multiplicity per Event;Counts");
  contents.GlobalHitMultiplicitySector5->SetFillColor(kMagenta+3);
  contents.GlobalHitMultiplicitySector5->SetFillStyle(3001);

  contents.GlobalHitMultiplicitySector6->SetTitle("Distribution of the Total Number of Hits per Event (Sector 6);Global Hit Multiplicity per Event;Counts");
  contents.GlobalHitMultiplicitySector6->SetFillColor(kMagenta+3);
  contents.GlobalHitMultiplicitySector6->SetFillStyle(3001);

  contents.GlobalHitMultiplicitySector7->SetTitle("Distribution of the Total Number of Hits per Event (Sector 7);Global Hit Multiplicity per Event;Counts");
  contents.GlobalHitMultiplicitySector7->SetFillColor(kMagenta+3);
  contents.GlobalHitMultiplicitySector7->SetFillStyle(3001);

  contents.GlobalHitMultiplicitySector8->SetTitle("Distribution of the Total Number of Hits per Event (Sector 8);Global Hit Multiplicity per Event;Counts");
  contents.GlobalHitMultiplicitySector8->SetFillColor(kMagenta+3);
  contents.GlobalHitMultiplicitySector8->SetFillStyle(3001);

  contents.GlobalHitMultiplicitySector9->SetTitle("Distribution of the Total Number of Hits per Event (Sector 9);Global Hit Multiplicity per Event;Counts");
  contents.GlobalHitMultiplicitySector9->SetFillColor(kMagenta+3);
  contents.GlobalHitMultiplicitySector9->SetFillStyle(3001);

  contents.GlobalHitMultiplicitySector10->SetTitle("Distribution of the Total Number of Hits per Event (Sector 10);Global Hit Multiplicity per Event;Counts");
  contents.GlobalHitMultiplicitySector10->SetFillColor(kMagenta+3);
  contents.GlobalHitMultiplicitySector10->SetFillStyle(3001);


  contents.HitMultiplicityPerEvent->SetTitle("Global Hit Multiplicity per Event;Event Counter;Global Hit Multiplicity");
  contents.HitMultiplicityPerEvent->SetFillColor(kRed+2);
  
  contents.HitsPerLadder->SetTitle("Number of Hits per Ladder;Ladder ID;Hit Multiplicity");
  contents.HitsPerLadder->SetFillColor(kGreen+2);
  TAxis *hpl_x = (TAxis*)contents.HitsPerLadder->GetXaxis();
  hpl_x->SetBinLabel(1,"1");
  hpl_x->SetBinLabel(5,"5");
  hpl_x->SetBinLabel(9,"9");
  hpl_x->SetBinLabel(13,"13");
  hpl_x->SetBinLabel(17,"17");
  hpl_x->SetBinLabel(21,"21");
  hpl_x->SetBinLabel(25,"25");
  hpl_x->SetBinLabel(29,"29");
  hpl_x->SetBinLabel(33,"33");
  hpl_x->SetBinLabel(37,"37");


  contents.HitsPerLadderPerEvent->SetTitle("Number of Hits per Ladder Event-by-Event;Ladder ID;Hit Multiplicity");
  contents.HitsPerLadderPerEvent->SetFillColor(kGreen+2);
  contents.HitsPerLadderPerEvent->SetFillStyle(3001);

  TAxis *hplpe_x = (TAxis*)contents.HitsPerLadderPerEvent->GetXaxis();
  hplpe_x->SetBinLabel(1,"1");
  hplpe_x->SetBinLabel(5,"5");
  hplpe_x->SetBinLabel(9,"9");
  hplpe_x->SetBinLabel(13,"13");
  hplpe_x->SetBinLabel(17,"17");
  hplpe_x->SetBinLabel(21,"21");
  hplpe_x->SetBinLabel(25,"25");
  hplpe_x->SetBinLabel(29,"29");
  hplpe_x->SetBinLabel(33,"33");
  hplpe_x->SetBinLabel(37,"37");
  
  contents.HitCorrelation->SetTitle("Distribution Hit Correlation Inner-Outer Layer;Outer Layer Hit Multiplicity per Event;Inner Layer Hit Multiplicity per Event");
  contents.HitCorrelation->SetMinimum(0.0);



  contents.SensorHitsInnerLayer->SetTitle("Number of Hits per Sensor (Inner Layer);Sector ID;Sensor ID");
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
  
  contents.SensorHitsOuterLayer->SetTitle("Number of Hits per Sensor (Outer Layer);Sector ID;Sensor NumberID");
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
 

  contents.AverageRunLengthInnerLayer->SetTitle("Intensity Plot of Average Run Length (Inner Layer);Sector ID;Sensor ID");
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


  contents.AverageRunLengthOuterLayer->SetTitle("Intensity Plot of Average Run Length (Outer Layer);Sector ID;Sensor ID");
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
  //plots[n] = new JevpPlot(contents.myhisto);
  //plots[n]->setDrawOpts("col");
  //plots[n]->optstat = 0;

  plots[n] = new JevpPlot(contents.GlobalHitMultiplicity);
  plots[++n] = new JevpPlot(contents.GlobalHitMultiplicitySector1);
  plots[++n] = new JevpPlot(contents.GlobalHitMultiplicitySector2);
  plots[++n] = new JevpPlot(contents.GlobalHitMultiplicitySector3);
  plots[++n] = new JevpPlot(contents.GlobalHitMultiplicitySector4);
  plots[++n] = new JevpPlot(contents.GlobalHitMultiplicitySector5);
  plots[++n] = new JevpPlot(contents.GlobalHitMultiplicitySector6);
  plots[++n] = new JevpPlot(contents.GlobalHitMultiplicitySector7);
  plots[++n] = new JevpPlot(contents.GlobalHitMultiplicitySector8);
  plots[++n] = new JevpPlot(contents.GlobalHitMultiplicitySector9);
  plots[++n] = new JevpPlot(contents.GlobalHitMultiplicitySector10);

  plots[++n] = new JevpPlot(contents.HitMultiplicityPerEvent);
  plots[++n] = new JevpPlot(contents.HitsPerLadder);
  plots[++n] = new JevpPlot(contents.HitsPerLadderPerEvent);

  plots[++n] = new JevpPlot(contents.HitCorrelation);
  //plots[n]->setDrawOpts("colz");

  plots[++n] = new JevpPlot(contents.SensorHitsInnerLayer);
  //plots[n]->setDrawOpts("colz");

  plots[++n] = new JevpPlot(contents.SensorHitsOuterLayer);
  //plots[n]->setDrawOpts("colz");

  plots[++n] = new JevpPlot(contents.AverageRunLengthInnerLayer);
  //plots[n]->setDrawOpts("colz");
  
  plots[++n] = new JevpPlot(contents.AverageRunLengthOuterLayer);
  //plots[n]->setDrawOpts("colz");

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
  max_count_sector1 = 0;
  max_count_sector2 = 0;
  max_count_sector3 = 0;
  max_count_sector4 = 0;
  max_count_sector5 = 0;
  max_count_sector6 = 0;
  max_count_sector7 = 0;
  max_count_sector8 = 0;
  max_count_sector9 = 0;
  max_count_sector10 = 0;

  max_count_inner = 0;
  max_count_outer = 0;

  min_count = 1000000;
  min_count_sector1 = 1000000;
  min_count_sector2 = 1000000;
  min_count_sector3 = 1000000;
  min_count_sector4 = 1000000;
  min_count_sector5 = 1000000;
  min_count_sector6 = 1000000;
  min_count_sector7 = 1000000;
  min_count_sector8 = 1000000;
  min_count_sector9 = 1000000;
  min_count_sector10 = 1000000;

  min_count_inner = 1000000;
  min_count_outer = 1000000;

  memset(count_hits_inner, 0, sizeof(count_hits_inner[0][0]) * 10 * 10);
  memset(count_hits_outer, 0, sizeof(count_hits_outer[0][0]) * 30 * 10);
  memset(count_length_inner, 0, sizeof(count_length_inner[0][0]) * 10 * 10);
  memset(count_length_outer, 0, sizeof(count_length_outer[0][0]) * 30 * 10);

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
  
  
  AverageRunLength = new map<int,double>();
  LadderCount = new map<int,int>();

  //Reset Histos
  contents.HitsPerLadderPerEvent->Reset("ICESM");

  //+++++++++++++++++++++++Per Event Decoding+++++++++++++++++++++++
  //+++++++++++++++++++++++Per Event Decoding+++++++++++++++++++++++
  //+++++++++++++++++++++++Per Event Decoding+++++++++++++++++++++++
  
  /*************************** PXL **************************/
  daq_dta *dd ;
  dd = rdr->det("pxl")->get("raw") ;
  if(dd){ // "pxl" was found
    // iterate over RDO's (each RDO has 4 ladders)
    int event_count = 0;

    while(dd->iterate()){
      int OVF[_NSENSOR];
      int error_cnt;
      error_cnt = 0;
      
      
      int pxl_sector = (dd->sec-1)*5 + dd->rdo;
      //printf("DAQ Sector %d, RDO %d (PXL Sector %d): %d words\n", dd->sec, dd->rdo,pxl_sector, dd->ncontent/4) ;
      
      for(int i=0; i<_NSENSOR; i++){
	OVF[i] = 0;
	for(int j=0; j<_NROW; j++){
	  bs[i][j].reset();
	}
      }
      
      u_int *d = (u_int *)dd->Void ; // point to the start of the DDL raw data	
      int wordLength = dd->ncontent/4; // number of 32bit words

      int ret = pxl_decoder(d, wordLength, bs, OVF, &pxlHeaderInfo, ave_runlength, &error_cnt);
      if (ret != 0){
	//printf("pxl_decoder returned 0x%x, error_cnt = %d\n", ret, error_cnt);
      }
      else{
	//printf("pxl_decoder returned no errors\n");
      }
      
      
      int sensor_count = 0;
      int row_count = 0;
      int sector_count = 0;
      int sensor_id = 0;

      for(int i=0; i<_NSENSOR; i++){
	sensor_count = 0;
	sensor_id = i+1;

	for(int j=0; j<_NROW; j++) {
	  row_count = bs[i][j].count();

	  //if(row_count != 0) cout<<row_count<<endl;

	  IncrementMultiplicity(i+1, row_count);
	  sensor_count += row_count;
	  event_count +=  row_count;
	  sector_count +=  row_count;
	  
	  //if (bs[i][j].any()) printf("found %4d hits in sensor %2d row %3d\n", bs[i][j].count(), i+1, j);
	}

	//Filling Sensor histograms
	int ladder_number = WhichLadder(pxl_sector,sensor_id);
	int ladder_bin = LadderMap->find(ladder_number)->second;

	if(!UpdateTH1(contents.HitsPerLadder,ladder_number,(double)sensor_count)) cout<<"Something is very wrong with HitsPerLadder!"<<endl;
	if(!UpdateTH1(contents.HitsPerLadderPerEvent,ladder_number,(double)sensor_count)) cout<<"Something is very wrong with HitsPerLadder!"<<endl;

	if(sensor_id < 11){
	  if(!UpdateTH2(contents.SensorHitsInnerLayer,ladder_bin,sensor_id,(double)sensor_count)) cout<<"Something is very wrong with SensorHitsInnerLayer!"<<endl;
	  if(!UpdateTH2("length_inner",contents.AverageRunLengthInnerLayer,ladder_bin,sensor_id,(double)ave_runlength[i])) cout<<"Something is very wrong with AverageRunLengthInnerLayer!"<<endl;

	}
	else{
	  if(sensor_id < 21){
	    if(!UpdateTH2(contents.SensorHitsOuterLayer,ladder_bin,sensor_id-10,(double)sensor_count)) cout<<"Something is very wrong with SensorHitsOuterLayer!"<<endl;
	    if(!UpdateTH2("length_outer",contents.AverageRunLengthOuterLayer,ladder_bin,sensor_id-10,(double)ave_runlength[i])) cout<<"Something is very wrong with AverageRunLengthOuterLayer!"<<endl;

	  }
	  else if(sensor_id < 31){
	    if(!UpdateTH2(contents.SensorHitsOuterLayer,ladder_bin+10,sensor_id-20,(double)sensor_count)) cout<<"Something is very wrong with SensorHitsOuterLayer!"<<endl;
	    if(!UpdateTH2("length_outer",contents.AverageRunLengthOuterLayer,ladder_bin+10,sensor_id-20,(double)ave_runlength[i])) cout<<"Something is very wrong with AverageRunLengthOuterLayer!"<<endl;

	  }
	  else if(sensor_id < 41){
	    if(!UpdateTH2(contents.SensorHitsOuterLayer,ladder_bin+20,sensor_id-30,(double)sensor_count)) cout<<"Something is very wrong with SensorHitsOuterLayer!"<<endl;
	    if(!UpdateTH2("length_outer",contents.AverageRunLengthOuterLayer,ladder_bin+20,sensor_id-30,(double)ave_runlength[i])) cout<<"Something is very wrong with AverageRunLengthOuterLayer!"<<endl;

	  }
	  else cout<<"Something is extremely wrong!"<<endl;
	}

      }
      
  

      if(pxl_sector == 1){
	((sector_count > max_count_sector1) && (max_count_sector1 = sector_count));
	((sector_count < min_count_sector1) && (min_count_sector1 = sector_count));

	contents.GlobalHitMultiplicitySector1->Fill(sector_count);
	if(!(number_of_events % 30)) contents.GlobalHitMultiplicitySector1->SetAxisRange(min_count_sector1-10,max_count_sector1+10);
      }
      else if(pxl_sector == 2){
	((sector_count > max_count_sector2) && (max_count_sector2 = sector_count));
	((sector_count < min_count_sector2) && (min_count_sector2 = sector_count));

	contents.GlobalHitMultiplicitySector2->Fill(sector_count);
	if(!(number_of_events % 30)) contents.GlobalHitMultiplicitySector2->SetAxisRange(min_count_sector2-10,max_count_sector2+10);
      }
      else if(pxl_sector == 3){
	((sector_count > max_count_sector3) && (max_count_sector3 = sector_count));
	((sector_count < min_count_sector3) && (min_count_sector3 = sector_count));

	contents.GlobalHitMultiplicitySector3->Fill(sector_count);
	if(!(number_of_events % 30)) contents.GlobalHitMultiplicitySector3->SetAxisRange(min_count_sector3-10,max_count_sector3+10);
      }
      else if(pxl_sector == 4){
	((sector_count > max_count_sector4) && (max_count_sector4 = sector_count));
	((sector_count < min_count_sector4) && (min_count_sector4 = sector_count));

	contents.GlobalHitMultiplicitySector4->Fill(sector_count);
	if(!(number_of_events % 30)) contents.GlobalHitMultiplicitySector4->SetAxisRange(min_count_sector4-10,max_count_sector4+10);
      }
      else if(pxl_sector == 5){
	((sector_count > max_count_sector5) && (max_count_sector5 = sector_count));
	((sector_count < min_count_sector5) && (min_count_sector5 = sector_count));

	contents.GlobalHitMultiplicitySector5->Fill(sector_count);
	if(!(number_of_events % 30)) contents.GlobalHitMultiplicitySector5->SetAxisRange(min_count_sector5-10,max_count_sector5+10);
      }
      else if(pxl_sector == 6){
	((sector_count > max_count_sector6) && (max_count_sector6 = sector_count));
	((sector_count < min_count_sector6) && (min_count_sector6 = sector_count));

	contents.GlobalHitMultiplicitySector6->Fill(sector_count);
	if(!(number_of_events % 30)) contents.GlobalHitMultiplicitySector6->SetAxisRange(min_count_sector6-10,max_count_sector6+10);
      }
      else if(pxl_sector == 7){
	((sector_count > max_count_sector7) && (max_count_sector7 = sector_count));
	((sector_count < min_count_sector7) && (min_count_sector7 = sector_count));

	contents.GlobalHitMultiplicitySector7->Fill(sector_count);
	if(!(number_of_events % 30)) contents.GlobalHitMultiplicitySector7->SetAxisRange(min_count_sector7-10,max_count_sector7+10);
      }
      else if(pxl_sector == 8){
	((sector_count > max_count_sector8) && (max_count_sector8 = sector_count));
	((sector_count < min_count_sector8) && (min_count_sector8 = sector_count));

	contents.GlobalHitMultiplicitySector8->Fill(sector_count);
	if(!(number_of_events % 30)) contents.GlobalHitMultiplicitySector8->SetAxisRange(min_count_sector8-10,max_count_sector8+10);
      }
      else if(pxl_sector == 9){
	((sector_count > max_count_sector9) && (max_count_sector9 = sector_count));
	((sector_count < min_count_sector9) && (min_count_sector9 = sector_count));

	contents.GlobalHitMultiplicitySector9->Fill(sector_count);
	if(!(number_of_events % 30)) contents.GlobalHitMultiplicitySector9->SetAxisRange(min_count_sector9-10,max_count_sector9+10);
      }
      else if(pxl_sector == 10){
	((sector_count > max_count_sector10) && (max_count_sector10 = sector_count));
	((sector_count < min_count_sector10) && (min_count_sector10 = sector_count));

	contents.GlobalHitMultiplicitySector10->Fill(sector_count);
	if(!(number_of_events % 30)) contents.GlobalHitMultiplicitySector10->SetAxisRange(min_count_sector10-10,max_count_sector10+10);
      }
      else cout<<"pxl_sector error"<<endl;


     
      
    }
    //After dd loop
    //^^^HIST^^^::GlobalHitMultiplicity
    (event_count > max_count) && (max_count = event_count);
    (event_count < min_count) && (min_count = event_count);
    contents.GlobalHitMultiplicity->Fill(event_count);
    if(!(number_of_events % 30)) contents.GlobalHitMultiplicity->SetAxisRange(min_count+10,max_count+10);

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
      if(!UpdateTH1(contents.HitMultiplicityPerEvent,event_bin,(double)(event_count/10))) cout<<"Something is very wrong with HitMultiplicityPerEvent!"<<endl;
    }
    else{
      cout<<"FYI: Number of events in evp > 5000"<<endl;
    }

    
    double scale_factor = (double)((double)number_of_events/(double)(number_of_events+1));
    (scale_factor == 0.0) && (scale_factor = 1.0);

    contents.SensorHitsInnerLayer->Scale(scale_factor);
    contents.SensorHitsOuterLayer->Scale(scale_factor);
    contents.HitsPerLadder->Scale(scale_factor);
    contents.AverageRunLengthInnerLayer->Scale(scale_factor);
    contents.AverageRunLengthOuterLayer->Scale(scale_factor);
    
    
  }
  
  else {
    //printf("no PXL found\n");
  }
  
  // End Fill Histograms...
  number_of_events++;
  delete AverageRunLength;
  delete LadderCount;
}

void pxlBuilder::stoprun(daqReader *rdr) {
}

void pxlBuilder::main(int argc, char *argv[]){
  pxlBuilder me;
  
  me.Main(argc, argv);
}

#include <stdio.h>
#include <stdlib.h>

#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include "DAQ_FCS/daq_fcs.h"

#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include "fcsBuilder.h"
#include <RTS/include/rtsLog.h>

// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//


ClassImp(fcsBuilder);
  

fcsBuilder::fcsBuilder(JevpServer *parent) : JevpBuilder(parent) {
  plotsetname = (char *)"fcs";

  // start with histograms undefined...
  memset(&contents, 0, sizeof(contents));
}

fcsBuilder::~fcsBuilder() {

  // Delete any existing histograms...
  int n = sizeof(contents) / sizeof(TH1 *);
  for(int i=0;i<n;i++) {
    if(contents.array[i]) delete contents.array[i];
  }
}

void fcsBuilder::initialize(int argc, char *argv[]) {

    for(int ch=0;ch<16;ch++) {
	char nm[64];
	sprintf(nm,"adcsum_vs_time_%02d", ch);
	contents.adcsum_vs_time[ch] = new TH1D(nm, "Adc Sum vs TB", 201, -.5, 200.5);
	contents.adcsum_vs_time[ch]->SetXTitle("Time (RHIC CROSSINGS/8)");
	
	sprintf(nm, "peakpos_all_%02d", ch);
	contents.peakpos_all[ch] = new TH1D(nm, "PeakPos vs TB (all)", 2000, 0, 200);	
	contents.peakpos_all[ch]->SetXTitle("Peak Position (RHIC CROSSINGS/8)");

	sprintf(nm, "peakpos_trg_%02d", ch);
	contents.peakpos_trg[ch] = new TH1D(nm, "PeakPos vs TB (trigger)", 200, 36*13.375, 43*13.375);
	contents.peakpos_trg[ch]->SetXTitle("Peak Position (ns)");

	sprintf(nm, "peakpos_trg_cog_%02d", ch);
	contents.peakpos_trg_cog[ch] = new TH1D(nm, "COG PeakPos vs TB (trigger)", 200, 36*13.375, 43*13.375);
	contents.peakpos_trg_cog[ch]->SetXTitle("Peak Position (ns)");

	sprintf(nm, "peakpos_trg_gaus_%02d", ch);
	contents.peakpos_trg_gauss[ch] = new TH1D(nm, "GAUSS PeakPos vs TB (trigger)", 200, 36*13.375, 43*13.375);
	contents.peakpos_trg_gauss[ch]->SetXTitle("Peak Position (ns)");

	sprintf(nm, "peakcharge_%02d", ch);
	contents.peakcharge[ch] = new TH1D(nm, "Peak Charge", 1000, 0, 1000);
	contents.peakcharge[ch]->SetXTitle("Peak Charge (ADC counts)");

	sprintf(nm, "peakmaxadc_%02d", ch);
	contents.peakmaxadc[ch] = new TH1D(nm, "Max Adc", 201, -.5, 200.5);
	contents.peakmaxadc[ch]->SetXTitle("Peak Maximum (ADC counts)");

	sprintf(nm, "pp_%02d", ch);
	contents.pp[ch] = new TH1D(nm, "delta", 300, 20, 50);	
	contents.pp[ch]->SetXTitle("Peak Position (RHIC CROSSINGS/8)");
    }

    // Add root histograms to Plots
    int np = sizeof(contents) / sizeof(TH1 *);
    JevpPlot *plots[np];

    int n=-1;
    for(int ch=0;ch<16;ch++) {
	plots[++n] = new JevpPlot(contents.adcsum_vs_time[ch]);
	plots[++n] = new JevpPlot(contents.peakpos_all[ch]);
	plots[++n] = new JevpPlot(contents.peakpos_trg[ch]);
	plots[++n] = new JevpPlot(contents.peakpos_trg_cog[ch]);
	plots[++n] = new JevpPlot(contents.peakpos_trg_gauss[ch]);
	plots[++n] = new JevpPlot(contents.peakcharge[ch]);
	plots[++n] = new JevpPlot(contents.peakmaxadc[ch]); 
	plots[++n] = new JevpPlot(contents.pp[ch]);
    }
 
    //plots[++n] = new JevpPlot(contents.h155_time_size_2min);
    // plots[n]->setDrawOpts("col");
    //plots[n]->optstat = 0;

    // Add Plots to plot set...
    for(int i=0;i<=n;i++) {
	LOG(DBG, "Adding plot %d",i);
	addPlot(plots[i]);
    }
}
  
void fcsBuilder::readPedestals() {
    char line[256];
    char *ped_fn = "/star/u/jml/FCS/fcs_pedestals.txt";
    FILE *file = fopen(ped_fn, "r");

    if(!file) {
	LOG(OPER, "No pedestal file (%s) for fcsBuilder", ped_fn);
	memset(pedestal_mean, 0, sizeof(pedestal_mean));
	memset(pedestal_rms, 0, sizeof(pedestal_rms));
	return;
    }
    while(fgets(line, 256, file)) {
	if(line[0] == '#') continue;
	
	int ch;
	double mean, rms, t1, t2, t3;

	if(sscanf(line, "%d %lf %lf %lf %lf %lf", &ch, &mean, &rms, &t1, &t2, &t3) != 6) continue;

	pedestal_mean[ch] = mean;
	pedestal_rms[ch] = rms;
    }

    for(int i=0;i<16;i++) {
	printf("pedestals[%d]:  [%f %f]\n", i, pedestal_mean[i], pedestal_rms[i]);
    }

    fclose(file);
}

bool fcsBuilder::readEvent(daqReader *rdr) {
    daq_dta *dd = rdr->det("fcs")->get("adc");
    if(!dd) return false;


    //printf("evt: %d\n", rdr->seq);
    memset(currEvent, 0, sizeof(currEvent));

    while(dd->iterate())  {
	u_short *d16 = (u_short *)dd->Void ;
	int ch = dd->pad;
	
	int ntb = dd->ncontent;
	if(ntb != 201) {
	    printf("FCS has %d rather than 201 timebins this time [ch=%d, evt=%d]!\n", ntb, ch, rdr->seq);
	    LOG(ERR, "FCS has %d rather than 201 timebins this time [ch=%d, evt=%d]!", ntb, ch, rdr->seq);
	    if(ntb > 201) ntb = 201;
	}

	int first_rhic_clock = -1;
	int first_trg_crossing = -1;

	for(int i=0;i<ntb;i++) {
	    int tb = i;
	    u_int flags = d16[i] >> 12 ;
	    u_int data = d16[i] & 0xFFF ;
	    
	    double adc = (double)data;
	    adc -= pedestal_mean[ch];
	    
	    if(0) {
		adc -= 3*pedestal_rms[ch];
		if(adc < 0) adc = 0;
		else adc += 3*pedestal_rms[ch];
	    }
			    
	    currEvent[ch][tb] = adc;

	    if((flags & 0x1) && (first_rhic_clock == -1)) {
		first_rhic_clock = i;
	    }
	    
	    if((flags & 0x8) && (first_trg_crossing == -1)) {
		first_trg_crossing = i;
	    }
	}

	//printf("evt=%6d ch=%02d frc=%d ftc=%d\n", rdr->seq, ch, first_rhic_clock, first_trg_crossing);
    }

    return true;
}

// Hits using Tonko's 16 time bin sum
//
int fcsBuilder::calculateHits(double *adcs, Hit *hits, int ch) {
    double window_sum[201];
    double window_mom[201];
    double window_max[201];
    int ws = 16;
    memset(window_sum, 0, sizeof(window_sum));
    memset(window_mom, 0, sizeof(window_mom));
    memset(window_max, 0, sizeof(window_max));
    for(int i=0;i<201;i++) {
	for(int j=i;j<(i+ws);j++) {
	    if(j >= 201) continue;

	    window_sum[i] += adcs[j];
	    window_mom[i] += j*adcs[j];

	    if(adcs[j] > window_max[i]) {
		window_max[i] = adcs[j];
	    }
	}	
    }

    int nhits = 0;
    for(int i=1;i<200;i++) {
	if(window_sum[i] < 0) continue;

	double s = window_sum[i];
	double sp = window_sum[i-1];
	double sn = window_sum[i+1];
	if((s > sp) &&
	   (s > sn) && 
	   (window_max[i] > 2)) {
	    
	    hits[nhits].pos = window_mom[i]/window_sum[i];
	    hits[nhits].charge = window_sum[i];
	    hits[nhits].maxadc = window_max[i];

	    nhits++;

	    if(nhits >= MAX_FCS_HITS) {
		LOG("JEFF", "Too many hits!");
		break;
	    }
	}
    }
	
    return nhits;
}

// Gets the hit array assuming:
//
// 1. A hit must have 3 time bins in a row > sigma
// 2. A hit must have all adc values > 0
//
int fcsBuilder::idHits(double *adcs, Hit *hits, int ch) {
    int found = 0;

    int totoversigma = 0;
    for(int i=0;i<201;i++) {
	if(adcs[i] > 3 * pedestal_rms[ch]) totoversigma++;
	else totoversigma = 0;

	if(totoversigma >= 3) {  // we have a peak!
	    int first = i;
	    int last = i;

	    while(first >= 0 && adcs[first]>0) first--;
	    while(last <= 200 && adcs[last]>0) last++;
		
	    if(first < 0) first = 0;
	    if(last > 200) last = 200;

	    hits[found].first = first;
	    hits[found].last = last;

	    found++;

	    if(found >= MAX_FCS_HITS) {
		LOG("JEFF", "Too many hits");
		break;
	    }

	    totoversigma = 0;
	    i = last+1;
	}
    }
    return found;
}

int fcsBuilder::calculateHits_cog(double *adcs, Hit *hits, int ch) {
    int nhits = idHits(adcs, hits, ch);
    
    
    for(int i=0;i<nhits;i++) {
	double sum = 0;
	double mom = 0;
	double max = 0;
	for(int j=hits[i].first; j<=hits[i].last; j++) {
	    sum += adcs[j];
	    mom += j*adcs[j];
	    if(max < adcs[j]) max = adcs[j];
	}
	hits[i].pos = mom / sum;
	hits[i].charge = sum;
	hits[i].maxadc = max;
    }

    //printf("%d: ",nhits);

    //for(int i=0;i<nhits;i++) {
    //	printf("%d %lf %lf %lf\n", ch, hits[i].pos, hits[i].charge, hits[i].maxadc);
    //}
    return nhits;
}

int fcsBuilder::calculateHits_gauss(double *adcs, Hit *hits, int ch) {
    return 0;
}

void fcsBuilder::startrun(daqReader *rdr) {
    LOG(NOTE, "fcsBuilder starting run #%d",rdr->run);
    resetAllPlots();

    readPedestals();
}

#define safelog(x) ((x > 0) ? log10(x) : 0)

void fcsBuilder::event(daqReader *rdr)
{
    // Fill Histograms...
  
#ifdef PROVOKE_ERROR
    if(rdr->seq > 500) {
	int *xxx = NULL;
	*xxx = 10;
    }
#endif

    if(!readEvent(rdr)) return;
    
    for(int ch=4;ch<12;ch++) {
	for(int tb=0;tb<201;tb++) {
	    contents.adcsum_vs_time[ch]->Fill(tb, currEvent[ch][tb]);
	}
	
	Hit hits[MAX_FCS_HITS];
	Hit hits_cog[MAX_FCS_HITS];
	Hit hits_gauss[MAX_FCS_HITS];

	int nhits = calculateHits(currEvent[ch], hits, ch);
	int nhits_cog = calculateHits_cog(currEvent[ch], hits_cog, ch);
	int nhits_gauss = calculateHits_gauss(currEvent[ch], hits_gauss, ch);

	if(ch != 0) {
	    double ppeak = 0;

	    for(int i=0;i<nhits;i++) {
		double cpeak = hits[i].pos;

		contents.peakpos_all[ch]->Fill(hits[i].pos);
		contents.peakpos_trg[ch]->Fill(hits[i].pos*13.375);

		contents.peakcharge[ch]->Fill(hits[i].charge);
		contents.peakmaxadc[ch]->Fill(hits[i].maxadc);
		if(i>0) contents.pp[ch]->Fill(cpeak - ppeak);
	     
		ppeak = cpeak;
	    }

	    for(int i=0;i<nhits_cog;i++) {
		contents.peakpos_trg_cog[ch]->Fill(hits_cog[i].pos*13.375);
	    }

	    for(int i=0;i<nhits_gauss;i++) {
		contents.peakpos_trg_gauss[ch]->Fill(hits_gauss[i].pos*13.375);
	    }
	}
    }
}

void fcsBuilder::stoprun(daqReader *rdr) {
  
    for(int ch=4;ch<12;ch++) {
	contents.peakpos_trg[ch]->Fit("gaus");
	contents.peakpos_trg_cog[ch]->Fit("gaus");
	contents.peakpos_trg_gauss[ch]->Fit("gaus");
	contents.pp[ch]->Fit("gaus","","",37.5,42.5);
    }

}


void fcsBuilder::main(int argc, char *argv[])
{
  fcsBuilder me;
  
  me.Main(argc, argv);
}


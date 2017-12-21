#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include "DAQ_FCS/daq_fcs.h"


#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1I.h>
#include <TH2F.h>
#include <TF1.h>
#include <TFitter.h>

#include <math.h>
#include "fcsBuilder.h"
#include <RTS/include/rtsLog.h>

#include "fft.C"    // BAD BAD USAGE!   Temporary!

// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//

// RHIC crossing time divided by 8
#define RHIC8 (107.0/8.0)
//#define RHIC8 1

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

void fcsBuilder::complex_mult(double *rres, double *ires, double r1, double i1, double r2, double i2) 
{
    *rres = r1 * r2 - i1 * i2;
    *ires = r1 * i2 + i1 * r2;
}

void fcsBuilder::fourier(double *rres, double *ires, double *real, double *im, int N, int reverse)
{
    for(int k=0;k<N;k++) {
	rres[k] = 0;
	ires[k] = 0;
       
	for(int n=0;n<N;n++) {
	    double theta = 2 * 3.14159 * k * n / N;
	    
	    double costheta = cos(theta) / sqrt(N);
	    double sintheta = sin(theta) / sqrt(N);
	    if(reverse == 0) sintheta = -sintheta;

	    double r, i;

	    complex_mult(&r, &i, real[n], im[n], costheta, sintheta);

	    rres[k] += r;
	    ires[k] += i;
	}
    }		
}

void fcsBuilder::initialize(int argc, char *argv[]) {
    fbin = 1/(107e-9*16001);   // size of frequency bins for pedestals

    readPedestals();  // We do this at the start of the run, but also now!

    for(int ch=0;ch<16;ch++) {
	char nm[64];
	char title[64];
	sprintf(nm,"adcsum_vs_time_%02d", ch);
	sprintf(title, "Channel %02d: ADC Sum vs TB", ch);
	contents.adcsum_vs_time[ch] = new TH1D(nm, title, 201, -.5, 200.5);
	contents.adcsum_vs_time[ch]->SetXTitle("Time (RHIC CROSSINGS/8)");
	
	sprintf(nm, "peakpos_all_%02d", ch);
	sprintf(title, "Channel %02d: PeakPos vs TB", ch);
	contents.peakpos_all[ch] = new TH1D(nm, title, 2000, 0, 200);	
	contents.peakpos_all[ch]->SetXTitle("Peak Position (RHIC CROSSINGS/8)");

	sprintf(nm, "peakpos_trg_%02d", ch);
	sprintf(title, "Channel %02d: Trigger Peak Position", ch);
	contents.peakpos_trg[ch] = new TH1D(nm, title, 200, 36 * RHIC8, 43* RHIC8);
	contents.peakpos_trg[ch]->SetXTitle("Peak Position (ns)");

	sprintf(nm, "peakpos_trg_cog_%02d", ch);
	contents.peakpos_trg_cog[ch] = new TH1D(nm, "COG PeakPos vs TB (trigger)", 200, 36*RHIC8, 43*RHIC8);
	contents.peakpos_trg_cog[ch]->SetXTitle("Peak Position (ns)");

	sprintf(nm, "peakpos_trg_gauss_%02d", ch);
	contents.peakpos_trg_gauss[ch] = new TH1D(nm, "GAUSS PeakPos vs TB (trigger)", 200, 36*RHIC8, 43*RHIC8);
	contents.peakpos_trg_gauss[ch]->SetXTitle("Peak Position (ns)");

	sprintf(nm, "peakcharge_%02d", ch);
	sprintf(title, "Channel %02d: Peak Charge", ch);
	contents.peakcharge[ch] = new TH1D(nm, title, 1000, 0, 1000);
	contents.peakcharge[ch]->SetXTitle("Peak Charge (ADC counts)");

	sprintf(nm, "peakmaxadc_%02d", ch);
	sprintf(title, "Channel %02d: Peak Maximum", ch);
	contents.peakmaxadc[ch] = new TH1D(nm, title, 201, -.5, 200.5);
	contents.peakmaxadc[ch]->SetXTitle("Peak Maximum (ADC counts)");

	sprintf(nm, "pp_%02d", ch);
	sprintf(title, "Channel %02d: Peak Position (Delta from previous peak)", ch);
	contents.pp[ch] = new TH1D(nm, title, 300, 20, 50);	
	contents.pp[ch]->SetXTitle("Peak Position (RHIC CROSSINGS/8)");

	sprintf(nm, "peak_width_%02d", ch);
	sprintf(title, "Channel %02d: Peak Width (COG & GAUSS)", ch);
	contents.peak_width[ch] = new TH1D(nm, title, 101, -.5, 100.5);


	sprintf(title, "Channel %02d: Sample Events", ch);
	for(int i=0;i<10;i++) {
	    sprintf(nm, "sample_events_%02d_%d", ch,i);
	    contents.sample_events[i][ch] = new TH1D(nm, title, 201, -.5, 200.5);
	    //contents.sample_events[i][ch]->GetYaxis()->SetRangeUser(-30,30);
	}
    }

    contents.xpad_n[0] = new TH1D("xpad_n", "XPAD Trigger N", 8, -.5, 8.5);
    contents.xpad_mean[0] = new TH1D("xpad_mean", "XPAD Position", 100, -4*RHIC8, 4*RHIC8);
    contents.xpad_sigma[0] = new TH1D("xpad_sigma", "XPAD Sigma", 200, 0, 4*RHIC8);    
    contents.xpad_sigma2[0] = new TH1D("xpad_sigma2", "XPAD Sigma 2 Hits", 200, 0, 3*RHIC8);
    contents.xpad_n_plus[0] = new TH1D("xpad_n_plus", "XPAD Trigger N (Uncorrelated)", 8, -.5, 8.5);
    contents.xpad_mean_plus[0] = new TH1D("xpad_mean_plus", "XPAD Position (Uncorrelated)", 100, -4*RHIC8, 4*RHIC8);
    contents.xpad_sigma_plus[0] = new TH1D("xpad_sigma_plus", "XPAD Sigma (Uncorrelated)", 200, 0, 4*RHIC8);
    contents.xpad_sigma2_plus[0] = new TH1D("xpad_sigma2_plus", "XPAD Sigma (Uncorrelated) 2 Hits", 200, 0, 3*RHIC8);

    contents.xpad_n[1] = new TH1D("xpad_n_cog", "XPAD Trigger N", 8, -.5, 8.5);
    contents.xpad_mean[1] = new TH1D("xpad_mean_cog", "XPAD Position", 100, -4*RHIC8, 4*RHIC8);
    contents.xpad_sigma[1] = new TH1D("xpad_sigma_cog", "XPAD Sigma", 200, 0, 4*RHIC8);    
    contents.xpad_sigma2[1] = new TH1D("xpad_sigma2_cog", "XPAD Sigma", 200, 0, 3*RHIC8);
    contents.xpad_n_plus[1] = new TH1D("xpad_n_plus_cog", "XPAD Trigger N (Uncorrelated)", 8, -.5, 8.5);
    contents.xpad_mean_plus[1] = new TH1D("xpad_mean_plus_cog", "XPAD Position (Uncorrelated)", 100, -4*RHIC8, 4*RHIC8);
    contents.xpad_sigma_plus[1] = new TH1D("xpad_sigma_plus_cog", "XPAD Sigma (Uncorrelated)", 200, 0, 4*RHIC8);
    contents.xpad_sigma2_plus[1] = new TH1D("xpad_sigma2_plus_cog", "XPAD Sigma (Uncorrelated)", 200, 0, 3*RHIC8);

    contents.xpad_n[2] = new TH1D("xpad_n_gaus", "XPAD Trigger N", 8, -.5, 8.5);
    contents.xpad_mean[2] = new TH1D("xpad_mean_guas", "XPAD Position", 100, -4*RHIC8, 4*RHIC8);
    contents.xpad_sigma[2] = new TH1D("xpad_sigma_gaus", "XPAD Sigma", 200, 0, 4*RHIC8);  
    contents.xpad_sigma2[2] = new TH1D("xpad_sigma2_gaus", "XPAD Sigma", 200, 0, 3*RHIC8);
    contents.xpad_n_plus[2] = new TH1D("xpad_n_plus_gaus", "XPAD Trigger N (Uncorrelated)", 8, -.5, 8.5);
    contents.xpad_mean_plus[2] = new TH1D("xpad_mean_plus_gaus", "XPAD Position (Uncorrelated)", 100, -4*RHIC8, 4*RHIC8);
    contents.xpad_sigma_plus[2] = new TH1D("xpad_sigma_plus_gaus", "XPAD Sigma (Uncorrelated)", 200, 0, 4*RHIC8);
    contents.xpad_sigma2_plus[2] = new TH1D("xpad_sigma2_plus_gaus", "XPAD Sigma (Uncorrelated)", 200, 0, 3*RHIC8);

    contents.corr_pos_vs_charge = new TH2D("corr_pos_vs_charge", "Corrected Slewing", 200, 0, 10000, 200, -4*RHIC8, 4*RHIC8);    
    contents.corr_pos_vs_charge_prof = new TProfile("corr_pos_vs_charge_prof", "Corrected Slewing (Profile)", 200, 0, 10000);
    contents.corr_pos_vs_charge_prof->Sumw2();

    contents.corr_width_vs_charge = new TH2D("corr_width_vs_charge", "Corrected Peak Width vs Charge", 200, 0, 10000, 61, -0.5, 60.5);
    contents.corr_max_vs_charge = new TH2D("corr_max_vs_charge", "Corrected Peak Max vs Charge", 200, 0, 10000, 200, 0, 1100);

   
    for(int ch=0;ch<16;ch++) {	
	int npeds=41;
	double npedsdiff = 20.5;
	double min = (int)(pedestal_mean[ch]+.5) - npedsdiff;
	double max = (int)(pedestal_mean[ch]+.5) + npedsdiff;

	if(pedestal_mean[ch] < 5) {
	    npeds = 4096;
	    min=-.5;
	    max = 4095.5;
	}
      

   
	char nm[64];
	char title[64];

	for(int i=0;i<10;i++) {
	    sprintf(nm, "ped_adc_vs_timem_%02d_%d", ch, i);
	    sprintf(title, "Channel %02d: Pedestal ADC (Magnified)", ch);
	    contents.ped_adc_vs_timem[i][ch] = new TH1D(nm, title, 301, 999.5, 1300.5);	
	    contents.ped_adc_vs_timem[i][ch]->SetXTitle("(RHIC CROSSINGS/8)");

	    sprintf(nm, "ped_adc_vs_time_%02d_%d", ch, i);
	    sprintf(title, "Channel %02d: Pedestal ADC", ch);
	    contents.ped_adc_vs_time[i][ch] = new TH1D(nm, title, 16001, -.5, 16000.5);	
	    contents.ped_adc_vs_time[i][ch]->SetXTitle("(RHIC CROSSINGS/8)");

	    sprintf(nm, "ped_power_spectrum_%02d_%d", ch, i);
	    sprintf(title, "Channel %02d: Power Spectrum", ch);
	    contents.ped_power_spectrum[i][ch] = new TH1D(nm, title, 1000, 0, 5);	
	    contents.ped_power_spectrum[i][ch]->SetXTitle("Frequency (MHz)");
	}
	
	sprintf(nm, "ped_vals_%02d", ch);
	sprintf(title, "Channel %02d: Pedestal Values", ch);
 
	
	contents.ped_vals[ch] = new TH1D(nm,title, npeds, min, max);
    }

    for(int i=0;i<10;i++) {
	char nm[64];
	char title[64];
       
	sprintf(nm, "canonical_peak_%02d", i);
	sprintf(title, "Simulated Peak Position (%d adc)", (i+1)*80);

	contents.canonical_peak[i] = new TH1D(nm, title, 200, 34*RHIC8, 46*RHIC8);
    }
	
    contents.test_a = new TH1D("test_a", "test_a", 16000, -.5, 16000.5);
    contents.test_b = new TH1D("test_b", "test_b", 16000, -.5, 16000.5);
    contents.test_c = new TH1D("test_c", "test_c", 16000, -.5, 16000.5);
    contents.test_d = new TH1D("test_d", "test_d", 16000, -.5, 16000.5);

    // Add root histograms to Plots
    int np = sizeof(contents) / sizeof(TH1 *);
    JevpPlot *plots[np];

    int n=-1;
    for(int ch=0;ch<16;ch++) {
	plots[++n] = new JevpPlot(contents.adcsum_vs_time[ch]);
	plots[++n] = new JevpPlot(contents.peakpos_all[ch]);
	//plots[++n] = new JevpPlot(contents.peakpos_trg[ch]);
	//plots[++n] = new JevpPlot(contents.peakpos_trg_cog[ch]);
     
	// Trigger Plots
	ph_gaus[ch] = new PlotHisto(contents.peakpos_trg_gauss[ch]);
	ph_gaus[ch]->histo->SetLineColor(1);
	ph_gaus[ch]->setLegText("GAUSS");
	ph_gaus[ch]->setLegArgs("l");
      
	ph_cog[ch] = new PlotHisto(contents.peakpos_trg_cog[ch]);
	ph_cog[ch]->histo->SetLineColor(2);
	ph_cog[ch]->setLegText("COG");
	ph_cog[ch]->setLegArgs("l");
	
	ph_trg[ch] = new PlotHisto(contents.peakpos_trg[ch]);
	ph_trg[ch]->histo->SetLineColor(3);
	ph_trg[ch]->setLegText("SUM16");
	ph_trg[ch]->setLegArgs("l");
	
	plots[++n] = new JevpPlot();
	plots[n]->addHisto(ph_trg[ch]);
	plots[n]->addHisto(ph_cog[ch]);
	//plots[n]->addHisto(ph_gaus[ch]);
	
	plots[n]->setLegend(.1, .6, .35, .90);
	plots[n]->setOptStat(0);
	plots[n]->setFitStat(0);
     
	plots[++n] = new JevpPlot(contents.peakcharge[ch]);
	plots[++n] = new JevpPlot(contents.peakmaxadc[ch]); 
	plots[++n] = new JevpPlot(contents.pp[ch]);
	plots[++n] = new JevpPlot(contents.peak_width[ch]);
	plots[++n] = new JevpPlot();
	for(int i=0;i<5;i++) {
	    PlotHisto *ph = new PlotHisto(contents.sample_events[i][ch]);
	    ph->histo->SetLineColor(i+1);
	    
	    plots[n]->addHisto(ph);
	    plots[n]->setOptStat(0);
	    plots[n]->setMaxY(30);
	}
    }

    char *types[] = { (char *)"SUM16", (char *)"COG", (char *)"GAUSS" };

    for(int type=0;type<3;type++) {
	ph_xpad_n[type] = new PlotHisto(contents.xpad_n[type]);
	ph_xpad_mean[type] = new PlotHisto(contents.xpad_mean[type]);
	ph_xpad_sigma[type] = new PlotHisto(contents.xpad_sigma[type]);
	ph_xpad_sigma2[type] = new PlotHisto(contents.xpad_sigma2[type]);
	ph_xpad_n_plus[type] = new PlotHisto(contents.xpad_n_plus[type]);
	ph_xpad_mean_plus[type] = new PlotHisto(contents.xpad_mean_plus[type]);
	ph_xpad_sigma_plus[type] = new PlotHisto(contents.xpad_sigma_plus[type]);
	ph_xpad_sigma2_plus[type] = new PlotHisto(contents.xpad_sigma2_plus[type]);

	ph_xpad_n[type]->histo->SetLineColor(type+1);
	ph_xpad_mean[type]->histo->SetLineColor(type+1);
	ph_xpad_sigma[type]->histo->SetLineColor(type+1);
	ph_xpad_sigma2[type]->histo->SetLineColor(type+1);
 	ph_xpad_n_plus[type]->histo->SetLineColor(type+1);
	ph_xpad_mean_plus[type]->histo->SetLineColor(type+1);
	ph_xpad_sigma_plus[type]->histo->SetLineColor(type+1);
	ph_xpad_sigma2_plus[type]->histo->SetLineColor(type+1);

	ph_xpad_n[type]->setLegText(types[type]);
	ph_xpad_mean[type]->setLegText(types[type]);
	ph_xpad_sigma[type]->setLegText(types[type]);
	ph_xpad_sigma2[type]->setLegText(types[type]);
 	ph_xpad_n_plus[type]->setLegText(types[type]);
	ph_xpad_mean_plus[type]->setLegText(types[type]);
	ph_xpad_sigma_plus[type]->setLegText(types[type]);
	ph_xpad_sigma2_plus[type]->setLegText(types[type]);

	ph_xpad_n[type]->setLegArgs("l");
	ph_xpad_mean[type]->setLegArgs("l");
	ph_xpad_sigma[type]->setLegArgs("l");
 	ph_xpad_sigma2[type]->setLegArgs("l");
	ph_xpad_n_plus[type]->setLegArgs("l");
	ph_xpad_mean_plus[type]->setLegArgs("l");
	ph_xpad_sigma_plus[type]->setLegArgs("l");	
	ph_xpad_sigma2_plus[type]->setLegArgs("l");
    }
	
    plots[++n] = new JevpPlot();
    for(int i=0;i<3;i++) plots[n]->addHisto(ph_xpad_n[i]);
    plots[n]->setLegend(.7, .7, .95, .90);
    plots[n]->setOptStat(0);
    plots[n]->setFitStat(0);
    
    plots[++n] = new JevpPlot();
    for(int i=0;i<3;i++) plots[n]->addHisto(ph_xpad_mean[i]);
    plots[n]->setLegend(.7, .7, .95, .90);
    plots[n]->setOptStat(0);
    plots[n]->setFitStat(0);

    plots[++n] = new JevpPlot();
    for(int i=0;i<3;i++) plots[n]->addHisto(ph_xpad_sigma[i]);
    plots[n]->setLegend(.7, .7, .95, .90);
    plots[n]->setOptStat(0);
    plots[n]->setFitStat(0);   

    plots[++n] = new JevpPlot();
    for(int i=0;i<3;i++) plots[n]->addHisto(ph_xpad_sigma2[i]);
    plots[n]->setLegend(.5, .7, .95, .90);
    plots[n]->setOptStat(0);
    plots[n]->setFitStat(0);

    plots[++n] = new JevpPlot();
    for(int i=0;i<3;i++) plots[n]->addHisto(ph_xpad_n_plus[i]);
    plots[n]->setLegend(.7, .7, .95, .90);
    plots[n]->setOptStat(0);
    plots[n]->setFitStat(0);

    plots[++n] = new JevpPlot();
    for(int i=0;i<3;i++) plots[n]->addHisto(ph_xpad_mean_plus[i]);
    plots[n]->setLegend(.7, .7, .95, .90);
    plots[n]->setOptStat(0);
    plots[n]->setFitStat(0);

    plots[++n] = new JevpPlot();
    for(int i=0;i<3;i++) plots[n]->addHisto(ph_xpad_sigma_plus[i]);
    plots[n]->setLegend(.7, .7, .95, .90);
    plots[n]->setOptStat(0);
    plots[n]->setFitStat(0);  

    plots[++n] = new JevpPlot();
    for(int i=0;i<3;i++) plots[n]->addHisto(ph_xpad_sigma2_plus[i]);
    plots[n]->setLegend(.5, .7, .95, .90);
    plots[n]->setOptStat(0);
    plots[n]->setFitStat(0);

    plots[++n] = new JevpPlot(contents.corr_pos_vs_charge);
    plots[n]->setDrawOpts("COLZ");
    plots[n]->setOptStat(11);
    plots[++n] = new JevpPlot(contents.corr_pos_vs_charge_prof);
    plots[n]->setDrawOpts("prof");
    plots[n]->setOptStat(11);
    plots[++n] = new JevpPlot(contents.corr_width_vs_charge);
    plots[n]->setDrawOpts("COLZ"); 
    plots[n]->setOptStat(11);
    plots[++n] = new JevpPlot(contents.corr_max_vs_charge);
    plots[n]->setDrawOpts("COLZ");
    plots[n]->setOptStat(11);

    for(int i=0;i<16;i++) {
	plots[++n] = new JevpPlot();
	for(int j=0;j<10;j++) {
	    PlotHisto *ph = new PlotHisto(contents.ped_adc_vs_time[j][i]);
	    ph->histo->SetLineColor(j+1);
	    char tmp[100];
	    sprintf(tmp, "event %d", j+1);
	    ph->setLegText(tmp);
	    ph->setLegArgs("l");
	    plots[n]->addHisto(ph);
	}
	plots[n]->setOptStat(11);
	plots[n]->setLegend(.7, .7, .95, .90);

	double min = (int)(pedestal_mean[i]+.5) - 20.5;
	double max = (int)(pedestal_mean[i]+.5) + 20.5;
	if(pedestal_mean[i] < 5) {
	    min = 0;
	    max = 4095;
	}

	plots[n]->setMaxY(max);
	plots[n]->setMinY(min);	

	plots[++n] = new JevpPlot();
	for(int j=0;j<10;j++) {
	    PlotHisto *ph = new PlotHisto(contents.ped_adc_vs_timem[j][i]);
	    ph->histo->SetLineColor(j+1);
	    char tmp[100];
	    sprintf(tmp, "event %d", j+1);
	    ph->setLegText(tmp);
	    ph->setLegArgs("l");
	    plots[n]->addHisto(ph);
	}
	plots[n]->setOptStat(11);
	plots[n]->setLegend(.7, .7, .95, .90);
	plots[n]->setMaxY(max);
	plots[n]->setMinY(min);
	
	plots[++n] = new JevpPlot(contents.ped_vals[i]);

	plots[++n] = new JevpPlot();
	for(int j=0;j<10;j++) {
	    PlotHisto *ph = new PlotHisto(contents.ped_power_spectrum[j][i]);
	    ph->histo->SetLineColor(j+1);
	    char tmp[100];
	    sprintf(tmp, "event %d", j+1);
	    ph->setLegText(tmp);
	    ph->setLegArgs("l");
	    plots[n]->addHisto(ph);
	}
	plots[n]->setOptStat(11);
	plots[n]->setLegend(.7, .7, .95, .90);
	//plots[n]->setMaxY(max);
	//plots[n]->setMinY(min);
	

    }

    for(int i=0;i<10;i++) {
	plots[++n] = new JevpPlot(contents.canonical_peak[i]);
    }

    plots[++n] = new JevpPlot(contents.test_a);
    plots[++n] = new JevpPlot(contents.test_b);
    plots[++n] = new JevpPlot(contents.test_c);
    plots[++n] = new JevpPlot(contents.test_d);
  
    // Add Plots to plot set...
    for(int i=0;i<=n;i++) {
	LOG(DBG, "Adding plot %d",i);
	addPlot(plots[i]);
    }
}
  
void fcsBuilder::readT0() {
    char line[256];
    char fn[256];
    LOG("OPER", "Read T0 file");

    sprintf(fn, "%s/t0.txt", clientdatadir);
    FILE *file = fopen(fn, "r");

    memset(t0_vals, 0, sizeof(t0_vals));
    memset(gain_corr, 0, sizeof(gain_corr));

    if(!file) {
	for(int i=0;i<3;i++) {
	    for(int j=0;j<16;j++) {
		t0_vals[i][j] = 40 * RHIC8;
	    }
	}

	for(int ch=4;ch<12;ch++) {
	    printf("nft0: (%lf %lf %lf) gain: %lf\n", t0_vals[0][ch], t0_vals[1][ch], t0_vals[2][ch], gain_corr[ch]);
	}

	return;
    }
   
    while(fgets(line, 256, file)) {
	if(line[0] == '#') continue;
	
	int run, ch;
	double t0_s, t0_c, t0_g, gc;

	if(sscanf(line, "%d %d %lf %lf %lf %lf", &run, &ch, &t0_s, &t0_c, &t0_g, &gc) != 6) continue;

	t0_vals[0][ch] = t0_s;
	t0_vals[1][ch] = t0_c;
	t0_vals[2][ch] = t0_g;
	gain_corr[ch] = gc;
    }

    for(int ch=4;ch<12;ch++) {
	printf("t0: (%lf %lf %lf) gain: %lf\n", t0_vals[0][ch], t0_vals[1][ch], t0_vals[2][ch], gain_corr[ch]);
    }

}

void fcsBuilder::readPedestals() {
    char line[256];
    char ped_fn[256];
    sprintf(ped_fn, "%s/fcs_pedestals.txt", clientdatadir);

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

    for(int i=4;i<12;i++) {
    	printf("pedestals[%d]:  [%f %f]\n", i, pedestal_mean[i], pedestal_rms[i]);
    }

    fclose(file);
}

bool fcsBuilder::readEvent(daqReader *rdr) {

    //printf("Reader: trigger type: %d %d 0x%llx %s\n", rdr->trgcmd, rdr->daqcmd, rdr->daqbits64, daqfile);



    daq_dta *dd = rdr->det("fcs")->get("adc");
    if(!dd) {
	return false;
    }

    
    //printf("evt: %d\n", rdr->seq);
    memset(currEvent, 0, sizeof(currEvent));
    
    //LOG("JEFF", "HERE");
    while(dd->iterate())  {
	u_short *d16 = (u_short *)dd->Void ;
	int ch = dd->pad;
	
	ntb = dd->ncontent;
	//if(ntb != 201) {
	//  printf("FCS has %d rather than 201 timebins this time [ch=%d, evt=%d]!\n", ntb, ch, rdr->seq);
	//  LOG(ERR, "FCS has %d rather than 201 timebins this time [ch=%d, evt=%d]!", ntb, ch, rdr->seq);
	//  if(ntb > 201) ntb = 201;
	//}

	int first_rhic_clock = -1;
	int first_trg_crossing = -1;

	for(int i=0;i<ntb;i++) {
	    int tb = i;
	    u_int flags = d16[i] >> 12 ;
	    u_int data = d16[i] & 0xFFF ;
	    
	    double adc = (double)data;

	    if(pedestal_run == 0) {
		adc -= pedestal_mean[ch];
	    }
	    
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

    //LOG("JEFF", "HERE");

    return true;
}

#define NSIDE 8
// Hits using Tonko's 16 time bin sum
//
int fcsBuilder::calculateHits(double *adcs, Hit *hits, int ch) {
    double window_sum[201];
    double window_mom[201];
    double window_max[201];
    int ws = NSIDE*2;
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
	   (window_max[i] > 1)) {
	    
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


#ifdef NSIDEPEAK
// Gets the hit array assuming:
//
// 1. A hit is a peak > 3sigma
// 2. A hit is n adc on each side of peak
//
int fcsBuilder::idHits(double *adcs, Hit *hits, int ch) {
    int found = 0;
    int nside = NSIDE;

    for(int i=1;i<200;i++) {
	
	if((adcs[i] > 3 * pedestal_rms[ch]) &&
	   (adcs[i] > adcs[i-1]) &&
	   (adcs[i] >= adcs[i+1])) {
	    // potential hit!
	    
	    int first = i-nside;
	    int last = i+nside;

	    if(first < 0) first = 0;
	    if(first > 200) first = 200;
	    
	    int goodhit = 1;
	    for(int j=first;j<=last;j++) {
		if(adcs[i] < adcs[j])
		    goodhit = 0;
	    }
	    
	    if(goodhit == 0) continue;
	    
	    hits[found].first = first;
	    hits[found].last = last;
	    found++;

	    if(found >= MAX_FCS_HITS) {
		LOG("JEFF", "Too many hits");
		break;
	    }
	}
    }
    return found;
}

#else

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

	    //while(first >= 0 && adcs[first] > 3*pedestal_rms[ch]) first--;
	    //while(last <= 200 && adcs[last] > 3*pedestal_rms[ch]) last++;
	    while(first >= 0 && adcs[first] > 0) first--;
	    while(last <= 200 && adcs[last] > 0) last++;
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
#endif

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
    int nhits = idHits(adcs, hits, ch);
    
    int jgh = 0;

    for(int i=0; i<nhits; i++) {
	
	TH1D *hist = new TH1D("tmp", "tmp",hits[i].last - hits[i].first + 1, hits[i].first - .5, hits[i].last + .5);

	// Only do guas fit for hits within the triggered peak!
	double cent = (double)(hits[i].first + hits[i].last)/ 2.0;
	if((cent < 30) ||(cent > 50)) {
	    delete hist;
	    continue;    
	}

	for(int j=hits[i].first; j<=hits[i].last; j++) {
	    hist->Fill((double)j, adcs[j]);
	}
	
	//TF1 *myfunc = new TF1("myfunc", "gaus");
	hist->Fit("gaus","Q");
	TVirtualFitter *fitter = TVirtualFitter::GetFitter();

	double a = fitter->GetParameter(0);
	double p = fitter->GetParameter(1);
	//double sig = fitter->GetParameter(2);

	hits[jgh].pos = p;
	hits[jgh].charge = a;
	hits[jgh].maxadc = 0;
	     
	jgh++;

	//delete myfunc;
	delete hist;
    }			      
    
    return jgh;
}

void fcsBuilder::startrun(daqReader *rdr) {
    LOG(NOTE, "fcsBuilder starting run #%d",rdr->run);
    resetAllPlots();

    if(strstr(daqfile, "ped") == NULL) {
	addServerTags((char *)"|fcs|fcsdat|");
	pedestal_run = 0;
    }
    else {
	addServerTags((char *)"|fcs|ped|");
	pedestal_run = 1;
    }
    
    readPedestals();
    readT0();
}

#define safelog(x) ((x > 0) ? log10(x) : 0)

void fcsBuilder::event(daqReader *rdr)
{
    //LOG("JEFF", "Evt");
    // Fill Histograms...
#ifdef PROVOKE_ERROR
    if(rdr->seq > 500) {
	int *xxx = NULL;
	*xxx = 10;
    }
#endif

    if(!readEvent(rdr)) return;


    int nch_triggered[3];
    double sum_triggered[3];
    double sum2_triggered[3];

    int nch_triggered_plus[3];
    double sum_triggered_plus[3];
    double sum2_triggered_plus[3];

    memset(nch_triggered, 0, sizeof(nch_triggered));
    memset(sum_triggered, 0, sizeof(sum_triggered));
    memset(sum2_triggered, 0, sizeof(sum2_triggered));

    memset(nch_triggered_plus, 0, sizeof(nch_triggered_plus));
    memset(sum_triggered_plus, 0, sizeof(sum_triggered_plus));
    memset(sum2_triggered_plus, 0, sizeof(sum2_triggered_plus));

    //LOG("JEFF", "In event");
    if(pedestal_run) {
	for(int ch=4;ch<12;ch++) {
	    double r_fft[16001];
	    double i_fft[16001];
		
	    for(int tb=0;tb<ntb;tb++) {
		if(rdr->seq < 10) {
		    contents.ped_adc_vs_time[rdr->seq][ch]->Fill(tb, currEvent[ch][tb]);
		    contents.ped_adc_vs_timem[rdr->seq][ch]->Fill(tb, currEvent[ch][tb]);
		    
		    r_fft[tb] = currEvent[ch][tb];
		    i_fft[tb] = 0;
		}
		contents.ped_vals[ch]->Fill(currEvent[ch][tb]);
	    }

	    if(rdr->seq < 10) {
		//LOG("JEFF", "Transform");
		Fft_transform(r_fft, i_fft, 16001);
		//LOG("JEFF", "Done");
		for(int i=1;i<8000;i++) {
		    int ri = 16001-i;
		    double mag = (sqrt(r_fft[i] * r_fft[i] + i_fft[i] * i_fft[i]) + sqrt(r_fft[ri] * r_fft[ri] + i_fft[ri] * i_fft[ri]))/2;
		    contents.ped_power_spectrum[rdr->seq][ch]->Fill(i*fbin / 1e6, mag);
		}
	    }
	}
    }
    else {
	for(int ch=4;ch<12;ch++) {
	    for(int tb=0;tb<201;tb++) {
		contents.adcsum_vs_time[ch]->Fill(tb, currEvent[ch][tb]);
		
		if((rdr->seq == 1000)||(rdr->seq == 2000)|| (rdr->seq == 3000) || (rdr->seq == 4000) || (rdr->seq == 5000) ||
		   (rdr->seq == 6000)||(rdr->seq == 7000)|| (rdr->seq == 8000) || (rdr->seq == 9000) || (rdr->seq == 10000)){
		    int i = rdr->seq/1000 - 1;
		    contents.sample_events[i][ch]->Fill(tb, currEvent[ch][tb]);
		}
	    }
	    
	    
	    
	    Hit hits[3][MAX_FCS_HITS];
	
	    Hit *hits_sum = hits[0];
	    Hit *hits_cog = hits[1];
	    Hit *hits_gauss = hits[2];
	    
	    int nhits[3];
	    
	    int &nhits_sum = nhits[0];
	    int &nhits_cog = nhits[1];
	    int &nhits_gauss = nhits[2];
	    
	    nhits_sum = calculateHits(currEvent[ch], hits_sum, ch);
	    nhits_cog = calculateHits_cog(currEvent[ch], hits_cog, ch);
	    nhits_gauss = calculateHits_gauss(currEvent[ch], hits_gauss, ch);
	    
	    double ppeak = 0;
	    
	    for(int i=0;i<nhits_sum;i++) {
		double cpeak = hits_sum[i].pos;	   
		
		contents.peakpos_all[ch]->Fill(hits_sum[i].pos);
		contents.peakpos_trg[ch]->Fill(hits_sum[i].pos*RHIC8);
		
		contents.peakcharge[ch]->Fill(hits_sum[i].charge);
		contents.peakmaxadc[ch]->Fill(hits_sum[i].maxadc);
		if(i>0) contents.pp[ch]->Fill(cpeak - ppeak);
		
		ppeak = cpeak;
	    }
	    
	    for(int i=0;i<nhits_cog;i++) {
		contents.peakpos_trg_cog[ch]->Fill(hits_cog[i].pos*RHIC8);
		contents.peak_width[ch]->Fill(hits_cog[i].last - hits_cog[i].first);
		
		
		double cpeak = hits_cog[i].pos;
		double t0corr_pos = cpeak * RHIC8 - t0_vals[1][ch];
		double gaincorr_charge = hits_cog[i].charge * gain_corr[ch];
		double gaincorr_max = hits_cog[i].maxadc * gain_corr[ch];
		// triggered peak!
		if((cpeak>36) && (cpeak < 44)) {
		    contents.corr_pos_vs_charge->Fill(gaincorr_charge, t0corr_pos);
		    contents.corr_pos_vs_charge_prof->Fill(gaincorr_charge, t0corr_pos);
		    //printf("corr_pos:  %lf %lf\n", gaincorr_charge, t0corr_pos);
		    contents.corr_width_vs_charge->Fill(gaincorr_charge, hits_cog[i].last - hits_cog[i].first);
		    contents.corr_max_vs_charge->Fill(gaincorr_charge, gaincorr_max);
		}
		
	    }
	    
	    for(int i=0;i<nhits_gauss;i++) {
		contents.peakpos_trg_gauss[ch]->Fill(hits_gauss[i].pos*RHIC8);
	    }
	    
	    for(int type=0;type<3;type++) {
		Hit *hh = hits[type];
		int nh = nhits[type];
		double *t0 = t0_vals[type];
		
		//printf("evt[%d] type[%d] nhits[%d] ch[%d]: ",rdr->seq, type, nh, ch);

		// Cross pad correlations	
		int second = 0;
		for(int i=0;i<nh;i++) {

		    //printf("%4.1lf ", hh[i].pos);

		    if((hh[i].pos > 34) && (hh[i].pos < 46)) {
			
			if(second == 0) {
			    nch_triggered[type]++;
			    
			    double pos = hh[i].pos*RHIC8 - t0[ch];
			    sum_triggered[type] += pos;
			    sum2_triggered[type] += pos*pos;
			    
			    second++;
			}
			else {
			    //printf("We got a second one? %d\n",type);
			}
			
		    }
		    
		    if((hh[i].pos > 34 + ch*8) && (hh[i].pos < 46 + ch*8)) {
			nch_triggered_plus[type]++;
			
			double pos = (hh[i].pos - ch*8)*RHIC8 - t0[ch];
			sum_triggered_plus[type] += pos;
			sum2_triggered_plus[type] += pos*pos;
		    }
		}
		
		//printf(" [%d]\n", nch_triggered[type]);
	    }
	}

	for(int type=0; type<3; type++) {
	    contents.xpad_n[type]->Fill(nch_triggered[type]);
	    contents.xpad_n_plus[type]->Fill(nch_triggered_plus[type]);
	    
	    if(nch_triggered[type] > 1) {
		sum_triggered[type] /= nch_triggered[type];
		sum2_triggered[type] /= nch_triggered[type];
		
		//printf("asdfasdfasdf %lf\n",sum_triggered[type]);
		contents.xpad_mean[type]->Fill(sum_triggered[type]);
		contents.xpad_sigma[type]->Fill(sqrt(sum2_triggered[type] - sum_triggered[type]*sum_triggered[type]));
		if(nch_triggered[type] == 2) contents.xpad_sigma2[type]->Fill(sqrt(sum2_triggered[type] - sum_triggered[type]*sum_triggered[type]));
	    }
	    
	    if(nch_triggered_plus[type] >1) {
		sum_triggered_plus[type] /= nch_triggered_plus[type];
		sum2_triggered_plus[type] /= nch_triggered_plus[type];
		
		contents.xpad_mean_plus[type]->Fill(sum_triggered_plus[type]);
		contents.xpad_sigma_plus[type]->Fill(sqrt(sum2_triggered_plus[type] - sum_triggered_plus[type]*sum_triggered_plus[type]));	 
		if(nch_triggered_plus[type] == 2) contents.xpad_sigma2_plus[type]->Fill(sqrt(sum2_triggered_plus[type] - sum_triggered_plus[type]*sum_triggered_plus[type]));
	    }
	}
    }
    //LOG("JEFF", "EVT");
}

void fcsBuilder::saveT0File(daqReader *rdr) {    
    TF1 *fit;
    char tmp[256];
    char tmp2[256];
    char tmp3[256];

    sprintf(tmp, "%s/t0.txt", clientdatadir);
    sprintf(tmp2, "%s/exdata.txt", clientdatadir);
    sprintf(tmp3, "%s/peakshape.txt", clientdatadir);
    FILE *psf = fopen(tmp3, "w");
    if(!psf) {
	LOG(ERR, "Error opening filename: %s", tmp3);
    }
    // Get avg peak value!
    double peakavg = 0;
    int avgn = 0;
    for(int ch=4;ch<12;ch++) {
	TH1D *h = contents.adcsum_vs_time[ch];
	double max = h->GetMaximum();
	int bin = h->GetMaximumBin();
	double x = h->GetXaxis()->GetBinCenter(bin);
	int n = h->GetEntries();
	if(!((x > 35) && (x < 45))) {
	    LOG("JEFF", "No Valid Peaks... don't write pdf");
	    exit(0);
	}

	peakavg +=  max / n;
	avgn++;
    }
    peakavg /= (double)avgn;


    FILE *t0file = fopen(tmp, "w");
    FILE *exdatafile = fopen(tmp2, "a");

    fprintf(t0file, "# run channel sum16_t0 cog_t0 gaus_t0 gain_corr sum16_sigma cog_sigma gaus_sigma\n");  
    fprintf(exdatafile, "# run channel sum16_t0 cog_t0 gaus_t0 gain_corr sum16_sigma cog_sigma gaus_sigma\n");
    double sum_t0, cog_t0, gaus_t0, sum_sig, cog_sig, gaus_sig;

    for(int ch=4;ch<12;ch++) {

	// Calculate T0's...
	contents.peakpos_trg[ch]->Fit("gaus","Q");
	fit = contents.peakpos_trg[ch]->GetFunction("gaus");
	//printf("sum16:  %lf %lf %lf\n", fit->GetParameter(0), fit->GetParameter(1), fit->GetParameter(2));
	fit->SetLineColor(3); fit->SetLineWidth(1);
	sprintf(tmp, "SUM16:  mean=%.2f, sig=%.2f", fit->GetParameter(1), fit->GetParameter(2));
	sum_t0 = fit->GetParameter(1);
	sum_sig = fit->GetParameter(2);
	ph_trg[ch]->setLegText(tmp);

	//LOG("JEFF", "a ch=%d", ch);

	contents.peakpos_trg_cog[ch]->Fit("gaus","Q");	
	//LOG("JEFF", "a");
	fit = contents.peakpos_trg_cog[ch]->GetFunction("gaus");	
	//LOG("JEFF", "a");
	if(fit) {
	    fit->SetLineColor(2); fit->SetLineWidth(1);
	    //LOG("JEFF", "a");
	    sprintf(tmp, "  COG:  mean=%.2f, sig=%.2f", fit->GetParameter(1), fit->GetParameter(2));
	    //LOG("JEFF", "a");
	    cog_t0 = fit->GetParameter(1);
	    cog_sig = fit->GetParameter(2);
	    //LOG("JEFF", "a");
	    ph_cog[ch]->setLegText(tmp);

	    //LOG("JEFF", "a");
	}
	
	
	contents.peakpos_trg_gauss[ch]->Fit("gaus","Q");
	fit = contents.peakpos_trg_gauss[ch]->GetFunction("gaus");
	if(fit) {
	    //printf("gaus:  %lf %lf %lf\n", fit->GetParameter(0), fit->GetParameter(1), fit->GetParameter(2));
	    fit->SetLineColor(1); fit->SetLineWidth(1);
	    sprintf(tmp, "GAUSS:  mean=%.2f, sig=%.2f", fit->GetParameter(1), fit->GetParameter(2));
	    gaus_t0 = fit->GetParameter(1);
	    gaus_sig = fit->GetParameter(2);
	    ph_gaus[ch]->setLegText(tmp);
	    
	}
	//LOG("JEFF", "a");

	// Calculate Gains
	TH1D *h = contents.adcsum_vs_time[ch];

	double max = h->GetMaximum();
	int bin = h->GetMaximumBin();
	double x = h->GetXaxis()->GetBinCenter(bin);
	int n = h->GetEntries();
	assert((x > 35) && (x < 45));
	double mypeak = max / n;
	double gain_corr = peakavg/mypeak;
	

	// Save T0 file
	if(rdr->run >0) local_run = rdr->run;

	fprintf(t0file, "%d %d %lf %lf %lf %lf %lf %lf %lf\n", local_run, ch, sum_t0, cog_t0, gaus_t0, gain_corr, sum_sig, cog_sig, gaus_sig);
	fprintf(exdatafile, "%d %d %lf %lf %lf %lf %lf %lf %lf\n", local_run, ch, sum_t0, cog_t0, gaus_t0, gain_corr, sum_sig, cog_sig, gaus_sig);

	contents.pp[ch]->Fit("gaus","Q","",37.5,42.5);
    }


    for(int i=0;i<201;i++) {
	fprintf(psf, "%d %f\n",i,(float)contents.adcsum_vs_time[11]->GetBinContent(i));
    }

    fclose(psf);
    fclose(t0file);
    fclose(exdatafile);
}

void fcsBuilder::do_canonical_peaks() {
    // read canonical peak file:
    char tmp[256];
    sprintf(tmp, "%s/peakshape_canonical.txt", clientdatadir);
    FILE *f = fopen(tmp, "r");
    if(!f) {
	LOG(ERR, "Error opening: %s", tmp);
	return;
    }
    double dat[201];
    int x;
    for(int i=0;i<201;i++) {
	fscanf(f, "%d %lf", &x, &dat[i]);
    }
    fclose(f);
    
    double mean = contents.ped_vals[11]->GetMean();


    // normalize
    // Positive values in range from 31 - 55  (24 timebins)
    double sum = 0;
    for(int i=31;i<=55;i++) sum += dat[i];
    for(int i=31;i<=55;i++) dat[i] /= sum;

    // Go through first pedestal event on channel 11
    double peds[16001];
    for(int i=0;i<16001;i++) peds[i] = (double)contents.ped_adc_vs_time[1][11]->GetBinContent(i);
    
    for(int i=0;i<16001;i++) {
	printf("peds[%d] = %lf\n", i, peds[i] - mean);
    }
    // Fill the histogram
    for(int offset=0;offset<16000;offset++) {
	for(int scale=0;scale<10;scale++) {
	    double mult = (scale+1)*80;
	    double sx = 0;
	    double s = 0;

	    int max = 0;
	    int pp = 0;
	    int first = 0;
	    int last = 0;
	    
	    for(int i=31;i< 55;i++) {
		int pi = i + offset;
		if(pi > 16000) pi = pi - 16000;
		
		int y = (int)(dat[i] * mult + peds[pi] - mean +.5);
		if(y > max) {
		    max = y;
		    pp = i;
		}
	    }

	    for(int i=31;i<pp;i++) {
		int pi = i + offset;
		if(pi > 16000) pi = pi - 16000;
		int y = (int)(dat[i] * mult + peds[pi] - mean +.5);
		if(y < 0) first = i+1;
	    }
	    
	    for(int i=55;i>pp;i--) {
		int pi = i + offset;
		if(pi > 16000) pi = pi - 16000;
		int y = (int)(dat[i] * mult + peds[pi] - mean +.5);
		if(y < 0) last = i-1;
	    }

	    first = 31;
	    last = 55;

	    for(int i=first;i<=last;i++) {
		int pi = i + offset;
		if(pi > 16000) pi = pi - 16000;
		
		int y = (int)(dat[i] * mult + peds[pi] - mean +.5);
		sx += i*y;
		s += y;
	    }
	    contents.canonical_peak[scale]->Fill(RHIC8*sx/s);
	}
    }
}

void fcsBuilder::stoprun(daqReader *rdr) {
    //LOG("JEFF", "Stop");
    if(pedestal_run == 0) {
	saveT0File(rdr);
    }

    if(pedestal_run == 1) {
	// Write pedestals
	char ped_fn[256];
	sprintf(ped_fn, "%s/fcs_pedestals.txt", clientdatadir);
	FILE *f = fopen(ped_fn, "w");
	if(!f) {
	    LOG(ERR, "Error opening filename: %s", ped_fn);
	}
	sprintf(ped_fn, "%s/expedestals.txt", clientdatadir);
	FILE *exf = fopen(ped_fn, "a");
	if(!exf) {
	    LOG(ERR, "Error opening filename: %s", ped_fn);
	}
	else {
	    //LOG("JEFF", "Writing!");

	    fprintf(f, "# Run %d\n", local_run);
	    fprintf(f, "# ch mean rms blah blah blah\n");
	    
	    for(int i=0;i<16;i++) {
		double mean = contents.ped_vals[i]->GetMean();
		double rms = contents.ped_vals[i]->GetRMS();

		//LOG("JEFF", "HEre: %d %lf %lf", i, mean, rms);
		fprintf(f, "%d %lf %lf 0 0 0\n",i, mean, rms);
		fprintf(exf, "%d %d %lf %lf\n",local_run, i, mean, rms);
	    }

	    
	    fclose(f);
	    fclose(exf);
	}

	do_canonical_peaks();

	
    }

    //LOG("JEFF", "Stop");
    /* 
    char *types[] = { "SUM", "COG", "GAUSS" };
    for(int i=0;i<3;i++) {
	double mean = contents.xpad_sigma2[i]->GetMean();
	sprintf(tmp, "%s: mean=%.2f", types[i], mean);
	ph_xpad_sigma2[i]->setLegText(tmp);
    }
    */

    //LOG("JEFF", "Do Fourier");
    // tests!
    double rdat[16001];
    double idat[16001];
    double rr[16001];
    double ir[16001];
    double mag[16001];
    double phase[16001];
    double r_rev[16001];
    double i_rev[16001];

    double f = 9000;

    //LOG("JEFF", "Freq = %f",f);
    for(int i=0;i<16001;i++) {
	double x = i * 107e-9;
       
	rdat[i] = sin(x*2*3.14159*f/3) + 2 * cos(x*2*3.14159*f);
	idat[i] = 0;
    }

    //LOG("JEFF", "a");

    for(int i=0;i<16001;i++) {
	rr[i] = rdat[i];
	ir[i] = idat[i];
    }
    //fourier(rr, ir, rdat, idat, 16001, 0);
    Fft_transform(rr, ir, 16001);



    //LOG("JEFF", "b");
    for(int i=0;i<16001;i++) {
	r_rev[i] = rr[i];
	i_rev[i] = ir[i];
    }

    //fourier(r_rev, i_rev, rr, ir, 16001, 1);
    Fft_inverseTransform(r_rev, i_rev, 16001);
    //LOG("JEFF", "c");
    
	//double fbin = 2/(107e-9);
    contents.test_a->SetBins(16001,-107e-9*.5,107e-9 * 16000.5);
    contents.test_b->SetBins(1000, 0, 12000);

  

    for(int i=0;i<16001;i++) {
	mag[i] = (sqrt(rr[i]*rr[i]+ir[i]*ir[i]) + sqrt(rr[16001-i]*rr[16001-i] + ir[16001-i]*ir[16001-i]))/2;

	if(rr[i] == 0) phase[i] = (3.14159/2) * ((ir[i] > 0) ? 1 : -1);
	else phase[i] = atan(ir[i] / rr[i]);
    
	contents.test_a->Fill(i*107e-9, rdat[i]);
	contents.test_d->Fill(i, r_rev[i]);
	contents.test_b->Fill(i*fbin, mag[i]);
	contents.test_c->Fill(i, phase[i]);
    }

    contents.test_a->SetXTitle("Seconds");
    contents.test_b->SetXTitle("Hz");
	
    //LOG("JEFF", "Done fourier");
}


void fcsBuilder::main(int argc, char *argv[])
{
    fcsBuilder me;
  
   
    me.Main(argc, argv);
}


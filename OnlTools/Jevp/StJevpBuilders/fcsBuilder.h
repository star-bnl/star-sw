#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>
#include <TProfile.h>

#include <math.h>

#define MAX_FCS_HITS 50

struct Hit {
    double pos;
    double charge;
    double maxadc;

    // Used by ID
    int first;
    int last;
    
    // Used by GAUSS
    double width;
    double sigma;
};

class fcsBuilder : public JevpBuilder {
public:
    int run;

    int pedestal_run;
    
    fcsBuilder(JevpServer *parent=NULL); 
    ~fcsBuilder();
    
    void initialize(int argc, char *argv[]);
    void startrun(daqReader *rdr);
    void stoprun(daqReader *rdr);
    void event(daqReader *rdr);
    
    static void main(int argc, char *argv[]);
    
    void readT0();
    void readPedestals();
    bool readEvent(daqReader *rdr);
    int calculateHits(double *adcs, Hit *hits, int ch);
    int calculateHits_cog(double *adcs, Hit *hits, int ch);
    int calculateHits_gauss(double *adcs, Hit *hits, int ch);
    int idHits(double *adcs, Hit *hits, int ch);
    void do_canonical_peaks();

 private:
    void complex_mult(double *rres, double *ires, double r1, double i1, double r2, double i2);
    void fourier(double *rres, double *ires, double *real, double *im, int n, int reverse=0);

    void saveT0File(daqReader *rdr);
    
    double pedestal_mean[16];
    double pedestal_rms[16];
    double t0_vals[3][16];    // 0 = sum16, 1 = cog,  2 = gaus
    double gain_corr[16];

    double fbin;

    int ntb;
    double currEvent[16][16000];

    PlotHisto *ph_trg[16];
    PlotHisto *ph_cog[16];
    PlotHisto *ph_gaus[16];

    PlotHisto *ph_xpad_n[3];
    PlotHisto *ph_xpad_mean[3];
    PlotHisto *ph_xpad_sigma[3];
    PlotHisto *ph_xpad_sigma2[3];
    PlotHisto *ph_xpad_n_plus[3];
    PlotHisto *ph_xpad_mean_plus[3];
    PlotHisto *ph_xpad_sigma_plus[3];
    PlotHisto *ph_xpad_sigma2_plus[3];


    //*** Histogram Declarations...
    //*** Use the union to be able to treat in bulk
    //*** As well as by name...
    union {
	TH1 *array[];
	struct {
	    TH1D *adcsum_vs_time[16];
	    TH1D *peakpos_all[16];
	    TH1D *peakpos_trg[16];	    
	    TH1D *peakpos_trg_cog[16];	    
	    TH1D *peakpos_trg_gauss[16];
	    TH1D *peakcharge[16];
	    TH1D *pp[16];
	    TH1D *peakmaxadc[16];
	    TH1D *peak_width[16];
	    TH1D *sample_events[10][16];

	    TH1D *xpad_n[3];
	    TH1D *xpad_mean[3];
	    TH1D *xpad_sigma[3];
	    TH1D *xpad_sigma2[3];
	    TH1D *xpad_n_plus[3];
	    TH1D *xpad_mean_plus[3];
	    TH1D *xpad_sigma_plus[3];
	    TH1D *xpad_sigma2_plus[3];

	    TH2D *corr_pos_vs_charge;
	    TProfile *corr_pos_vs_charge_prof;
	    TH2D *corr_width_vs_charge;
	    TH2D *corr_max_vs_charge;

	    // Pedestal files...
	    TH1D *ped_adc_vs_time[10][16];	  
	    TH1D *ped_adc_vs_timem[10][16];
	    TH1D *ped_power_spectrum[10][16];
	    TH1D *ped_vals[16];

	    TH1D *canonical_peak[10];
	    
	    TH1D *test_a;
	    TH1D *test_b;
	    TH1D *test_c;
	    TH1D *test_d;
	};

       

    } contents;
    
    //*** End Histogram Declarations...
    
    ClassDef(fcsBuilder, 1);
};

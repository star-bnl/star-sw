#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include <TH1F.h>
#include <TH2F.h>

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
    
    fcsBuilder(JevpServer *parent=NULL); 
    ~fcsBuilder();
    
    
    void initialize(int argc, char *argv[]);
    void startrun(daqReader *rdr);
    void stoprun(daqReader *rdr);
    void event(daqReader *rdr);
    
    static void main(int argc, char *argv[]);
    
    void readPedestals();
    bool readEvent(daqReader *rdr);
    int calculateHits(double *adcs, Hit *hits, int ch);
    int calculateHits_cog(double *adcs, Hit *hits, int ch);
    int calculateHits_gauss(double *adcs, Hit *hits, int ch);
    int idHits(double *adcs, Hit *hits, int ch);

 private:

    double pedestal_mean[16];
    double pedestal_rms[16];
    double currEvent[16][201];

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
	};
    } contents;
    
    //*** End Histogram Declarations...
    
    ClassDef(fcsBuilder, 1);
};

#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
class daqReader;
//#include "DAQ_READER/daq_dta.h"
struct daq_dta;
#include "Jevp/StJevpPlot/JevpPlot.h"
#include <math.h>
#include <TH1F.h>
#include <TH2F.h>

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
    
private:
    
    daq_dta *dd;
    JevpPlot **mPlots;
    int mEvt=-1;
    int mPhyLed=-1;

    //*** Histogram Declarations...
    //*** Use the union to be able to treat in bulk
    //*** As well as by name...
    union {
	TH1 *array[1];
	struct {
	    TH1 *h_evt_size;	    
	    TH2 *h_fcs_crt_depch_tbin[5];
	    TH2 *h_fcs_det_hitmap[3];
	    TH2 *h_fcs_ehpns_id_adcsum[3][2];
  	    TH2 *h_fcs_det_tbin_adc[3];
	}; 
    } contents;
    
    //*** End Histogram Declarations...
        
    ClassDef(fcsBuilder, 2);
};

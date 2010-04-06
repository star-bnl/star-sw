#ifndef BESHistogramGroup_h
#define BESHistogramGroup_h

#define BEMCTOW 4800

#include "Rtypes.h"
#include "HistogramGroup.h"
#include <TLine.h>
#include <TLatex.h>

class BESHistogramGroup : public HistogramGroup {

public:
    BESHistogramGroup();
    BESHistogramGroup(const char* group, const char* subGroup="vpd", const char* trigger="any", const char* detector="vpd");

    ~BESHistogramGroup();

    virtual void reset();
    virtual bool fill(evpReader* evp, char* datap);
    virtual void draw(TCanvas* cc);

private:

    TH2* h_bbc_adc_west_vs_east;  // bbc adc west vs adc east
    TH2* h_bemc_energy_west_vs_east; // bemc energy west vs bemc energy east
    TH2* h_bemc_adc_west_vs_east; // bemc adc west vs bemc adc east
    TH1* h_bbc_adc_east;  // bbc adc east
    TH1* h_bbc_adc_west;  // bbc adc west
    TH1* h_bemc_adc_east;  // bemc adc east
    TH1* h_bemc_adc_west;  // bemc adc west
    TH1* h_event_type;    //
    TH1* h_bemc_adc_all;
    const char *bemcStatus;
    float towerPed[BEMCTOW];
    float towerGain[BEMCTOW];
    float towerUnmask[BEMCTOW];
    float towerUnmaskHT[BEMCTOW];
    TLatex text1,text2,text3,text4,text5,text6;
    TLine  line1,line2,line3,line4,line5,line6,line7,line8,line9,lin10;

    ClassDef(BESHistogramGroup,1) ;
};


#endif

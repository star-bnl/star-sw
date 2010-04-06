#include "BESHistogramGroup.h"
#include <iostream>
#include <fstream>
using namespace std;
#include <TH1F.h>
#include <TH2F.h>
#include <TFile.h>
#include <TCanvas.h>
#include <TArrow.h>
#include <TString.h>
#include <TBox.h>
#include <TObjArray.h>
#include <TDatime.h>

#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_BSMD/daq_bsmd.h"
#include "DAQ_BTOW/daq_btow.h"
#include "DAQ_EMC/daq_emc.h"
#include "DAQ_TRG/daq_trg.h"
#include <RTS/include/daqFormats.h>

#include "DAQ_TRG/trgReader.h"
#include "DAQ_READER/cfgutil.h"
#include "StEvent/StTriggerData.h"
#include "DAQ_L3/l3Reader.h"
#include "TriggerData.h"

#include <StEmcUtil/database/StEmcDecoder.h>
#include <StEmcUtil/geometry/StEmcGeom.h>

#include "StEmcPool/StBEMCPlots/BEMC_DSM_decoder.h"
#include <TEnv.h>


StEmcDecoder *BEMCDecoder = 0;

#define BEMCOK 1
#define BEMCNOTINSTALLED 2
#define BEMCCORRUPTED 3


#include <sstream>
#include <stdlib.h>
#include "TVirtualPad.h"
#include "TStyle.h"
#include "TMapFile.h"
#include "EvpUtil.h"

#include "StEmcUtil/database/StBemcTablesWriter.h"


static Float_t BEMC_energy_diff     = 20.0;
static Float_t BEMC_low_min_energy  = 50.0;
static Float_t BEMC_min_energy      = 2.0;
static Float_t BBC_min_adc          = 150.0;

void plotTopLegend(char* label,Float_t x=-1,Float_t y=-1,Float_t size=0.06,Int_t color=1,Float_t angle=0.0,TLatex* text = 0)
{
    // coordinates in NDC!
    // plots the string label in position x and y in NDC coordinates
    // size is the text size
    // color is the text color

    if(x<0||y<0)
    {   // defaults
      x=gPad->GetLeftMargin()*1.15;
      y=(1-gPad->GetTopMargin())*1.04;
    }
    text->SetText(x,y,label);
    text->SetTextSize(size);
    text->SetNDC();
    text->SetTextColor(color);
    text->SetTextAngle(angle);
    text->Draw();
}


void PlotLine(Double_t x1_val, Double_t x2_val, Double_t y1_val, Double_t y2_val, Int_t Line_Col, Int_t LineWidth, Int_t LineStyle, TLine* Zero_line)
{
    Zero_line -> SetX1(x1_val);
    Zero_line -> SetX2(x2_val);
    Zero_line -> SetY1(y1_val);
    Zero_line -> SetY2(y2_val);
    Zero_line -> SetLineWidth(LineWidth);
    Zero_line -> SetLineStyle(LineStyle);
    Zero_line -> SetLineColor(Line_Col);
    Zero_line -> Draw();
}


using namespace std;


ClassImp(BESHistogramGroup) ;

BESHistogramGroup::BESHistogramGroup()
{
    // For ROOT I/O
    h_bbc_adc_west_vs_east     = 0;
    h_bbc_adc_east             = 0;
    h_bbc_adc_west             = 0;
    h_bemc_adc_east            = 0;
    h_bemc_adc_west            = 0;
    h_event_type               = 0;
    h_bemc_energy_west_vs_east = 0;
    h_bemc_adc_west_vs_east    = 0;
    h_bemc_adc_all             = 0;
}

BESHistogramGroup::BESHistogramGroup(const char* group, const char* subGroup, const char* trigger, const char* detector)
: HistogramGroup(group,subGroup,trigger,detector)
{

    h_bbc_adc_west_vs_east     = new TH2D("h_bbc_adc_west_vs_east","h_bbc_adc_west_vs_east",100,0,5000,100,0,5000);
    h_bbc_adc_east             = new TH1D("h_bbc_adc_east","h_bbc_adc_east",100,0,5000);
    h_bbc_adc_west             = new TH1D("h_bbc_adc_west","h_bbc_adc_west",100,0,5000);
    h_bemc_adc_east            = new TH1D("h_bemc_adc_east","h_bemc_adc_east",100,0,30000);
    h_bemc_adc_west            = new TH1D("h_bemc_adc_west","h_bemc_adc_west",100,0,30000);
    h_event_type               = new TH1D("h_event_type","h_event_type",24,0,6);
    h_bemc_adc_west_vs_east    = new TH2D("h_bemc_adc_west_vs_east","h_bemc_adc_west_vs_east",100,0,30000,100,0,30000);
    h_bemc_energy_west_vs_east = new TH2D("h_bemc_energy_west_vs_east","h_bemc_energy_west_vs_east",100,0,600,100,0,600);
    h_bemc_adc_all             = new TH1D("h_bemc_adc_all","h_bemc_adc_all",200,-50,300);

    h_bbc_adc_east ->SetLineColor(1);
    h_bbc_adc_west ->SetLineColor(2);
    h_bemc_adc_east->SetLineColor(1);
    h_bemc_adc_west->SetLineColor(2);
    h_bbc_adc_east->GetXaxis()->SetTitle("BBC East Adc");
    h_bbc_adc_east->GetYaxis()->SetTitle("counts");
    h_bbc_adc_west->GetXaxis()->SetTitle("BBC East Adc");
    h_bbc_adc_west->GetYaxis()->SetTitle("counts");
    h_bbc_adc_west_vs_east->GetXaxis()->SetTitle("BBC East Adc");
    h_bbc_adc_west_vs_east->GetYaxis()->SetTitle("BBC West Adc");
    h_bemc_adc_east->GetXaxis()->SetTitle("BEMC East Adc");
    h_bemc_adc_east->GetYaxis()->SetTitle("counts");
    h_bemc_adc_west->GetXaxis()->SetTitle("BEMC West Adc");
    h_bemc_adc_west->GetYaxis()->SetTitle("counts");
    h_event_type->GetXaxis()->SetTitle("event type");
    h_event_type->GetYaxis()->SetTitle("counts");
    h_bemc_energy_west_vs_east->GetXaxis()->SetTitle("BEMC East Energy [GeV]");
    h_bemc_energy_west_vs_east->GetYaxis()->SetTitle("BEMC West Energy [GeV]");
    h_bemc_adc_all->GetXaxis()->SetTitle("BEMC Adc sum");
    h_bemc_adc_all->GetYaxis()->SetTitle("counts");
    h_bemc_adc_west_vs_east->GetXaxis()->SetTitle("BEMC East Energy [GeV]");
    h_bemc_adc_west_vs_east->GetYaxis()->SetTitle("BEMC West Energy [GeV]");
}


BESHistogramGroup::~BESHistogramGroup() {

    delete  h_bbc_adc_west_vs_east;
    delete  h_bbc_adc_east;
    delete  h_bbc_adc_west;
    delete  h_bemc_adc_east;
    delete  h_bemc_adc_west;
    delete  h_event_type;
    delete  h_bemc_energy_west_vs_east;
    delete  h_bemc_adc_west_vs_east;
    delete  h_bemc_adc_all;
}


void BESHistogramGroup::reset() {
    cout << "Reset was called " << endl;
    h_bbc_adc_west_vs_east     ->Reset();
    h_bbc_adc_east             ->Reset();
    h_bbc_adc_west             ->Reset();
    h_bemc_adc_east            ->Reset();
    h_bemc_adc_west            ->Reset();
    h_event_type               ->Reset();
    h_bemc_energy_west_vs_east ->Reset();
    h_bemc_adc_west_vs_east    ->Reset();
    h_bemc_adc_all             ->Reset();
    bemcStatus = gEnv->GetValue("Online.bemcStatus", "bemcStatus.txt");

    if (!BEMCDecoder) BEMCDecoder = new StEmcDecoder();
    if(bemcStatus) cout << "bemcStatus approved" << endl;
    else cout << "bemcStatus not approved" << endl;

    //**************************** Read BEMC pedestals **************************************************
    if(bemcStatus)
    {
        ifstream ifstr(bemcStatus);
        if (ifstr.good()) {
            cout << "Reading BEMC trigger status file " << bemcStatus << endl;
        } else {
            cout << "Cannot open BEMC trigger status file! " << bemcStatus << endl;
        }
        while(ifstr.good())
        {
            string token;
            do
            {
                if(token == "#")
                {
                    char dummy[4096];
                    ifstr.getline(dummy, sizeof(dummy));
                }
                ifstr >> token;
            } while(ifstr.good() && (token != "SoftId") && (token != "triggerPatch") && (token != "TriggerPedestalShift"));
            if(ifstr.good())
            {
                if(token == "SoftId")
                {
                    int softId, crate, crateSeq, unmaskTower, unmaskHT, unmaskPA, triggerPatch;
                    float ped;
                    ifstr >> softId >> crate >> crateSeq >> unmaskTower >> unmaskHT >> unmaskPA >> ped >> triggerPatch;
                    if ((softId >= 1) && (softId <= 4800)) {
                        towerPed[softId-1]=ped;
                        towerUnmask[softId-1]=unmaskTower;
                        towerUnmaskHT[softId-1]=unmaskHT;
                    }
                } else if (token == "triggerPatch")
                {
                    int triggerPatch, crate, crateSeq, unmaskHT, unmaskPA, bitConv, formula, formulaParam0, formulaParam1, formulaParam2, formulaParam3, formulaParam4, formulaParam5;
                    ifstr >> triggerPatch >> crate >> crateSeq >> unmaskHT >> unmaskPA >> bitConv >> formula >> formulaParam0 >> formulaParam1 >> formulaParam2 >> formulaParam3 >> formulaParam4 >> formulaParam5;
                }
            }
        }
        ifstr.close();
    }
    //********************************************************************************************


    //**************************** Read BEMC gains **************************************************
    cout << "Read in Bemc tables from database" << endl;
    StBemcTablesWriter* BemcTables = new StBemcTablesWriter();
    BemcTables->loadTables("2036-01-01 00:00:00");
    Float_t calib;
    for(Int_t SoftId = 1; SoftId < 4801; SoftId++)
    {
        calib = BemcTables->calib(1,SoftId,1);
        towerGain[SoftId-1]=calib;
    }
    //********************************************************************************************


}


void BESHistogramGroup::draw(TCanvas* cc) {

    TLatex label;
    label.SetTextAlign(23);  // center, top
    label.SetTextSize(0.06);
    label.SetTextColor(16);

    gStyle->SetPalette(1);
    gStyle->SetOptStat(1);
    gStyle->SetLabelSize(0.06,"y");
    gStyle->SetLabelSize(0.06,"x");
    gStyle->SetTitleX(0.1); gStyle->SetTitleY(1.);
    gStyle->SetTitleW(0.8); gStyle->SetTitleH(0.088);
    gStyle->SetOptTitle(1);

    cc->cd();
    cc->Clear();
    cc->Divide(3,2);

    h_event_type ->SetStats(0);

    cc->cd(4);
    h_bbc_adc_west_vs_east->Draw("colz");
    PlotLine(0.0,5000.0,0.0,5000.0,1,1,2,&line4); // x1,x2,y1,y2,Line_Col,LineWidth,LineStyle
    PlotLine(BBC_min_adc,BBC_min_adc,BBC_min_adc,5000.0,2,1,2,&line7); // x1,x2,y1,y2,Line_Col,LineWidth,LineStyle
    PlotLine(BBC_min_adc,5000.0,BBC_min_adc,BBC_min_adc,2,1,2,&line8); // x1,x2,y1,y2,Line_Col,LineWidth,LineStyle
    cc->cd(1);
    h_bbc_adc_east ->Draw("h");
    cc->cd(1);
    h_bbc_adc_west ->Draw("same h");
    cc->cd(2);
    h_bemc_adc_east ->Draw("h");
    cc->cd(2);
    h_bemc_adc_west ->Draw("same h");
    cc->cd(5);
    h_bemc_energy_west_vs_east->Draw("colz");
    PlotLine(0.0,600.0,0.0,600.0,1,1,2,&line1); // x1,x2,y1,y2,Line_Col,LineWidth,LineStyle
    PlotLine(BEMC_energy_diff,BEMC_low_min_energy+BEMC_energy_diff,0.0,BEMC_low_min_energy,2,1,2,&line2); // x1,x2,y1,y2,Line_Col,LineWidth,LineStyle
    PlotLine(0.0,BEMC_low_min_energy,BEMC_energy_diff,BEMC_low_min_energy+BEMC_energy_diff,2,1,2,&line3); // x1,x2,y1,y2,Line_Col,LineWidth,LineStyle
    PlotLine(BEMC_low_min_energy+BEMC_energy_diff,600.0,BEMC_low_min_energy,BEMC_low_min_energy,2,1,2,&line5); // x1,x2,y1,y2,Line_Col,LineWidth,LineStyle
    PlotLine(BEMC_low_min_energy,BEMC_low_min_energy,BEMC_low_min_energy+BEMC_energy_diff,600.0,2,1,2,&line6); // x1,x2,y1,y2,Line_Col,LineWidth,LineStyle
    cc->cd(3);
    cc->cd(3)->SetLogy(1);
    h_bemc_adc_all ->Draw("h");
    cc->cd(6);
    h_event_type   ->Draw("h");
    plotTopLegend("All",0.13,0.3,0.05,1,90.0,&text1);
    plotTopLegend("BBC",0.268,0.3,0.05,1,90.0,&text2);
    plotTopLegend("BEMC min. energy",0.4,0.3,0.05,1,90.0,&text3);
    plotTopLegend("BEMC all cuts",0.54,0.3,0.05,1,90.0,&text4);
    plotTopLegend("BBC+BEMC",0.67,0.3,0.05,1,90.0,&text5);
    plotTopLegend("Bad",0.8,0.3,0.05,1,90.0,&text6);

    cc->Update();



}


bool BESHistogramGroup::fill(evpReader* evp, char* datap)
{

    //cout << "Fill was called " << endl;
    //***************************** BBC data *******************************************************
    StTriggerData* trgd = TriggerData::Instance(datap);
    if(!trgd) return false;

    short bbcEastADCSum = trgd->bbcADCSum((StBeamDirection)0,0);
    short bbcWestADCSum = trgd->bbcADCSum((StBeamDirection)1,0);

    h_bbc_adc_west_vs_east->Fill(bbcEastADCSum,bbcWestADCSum);
    h_bbc_adc_east        ->Fill(bbcEastADCSum);
    h_bbc_adc_west        ->Fill(bbcWestADCSum);
    //cout << "bbcEastADCSum = " << bbcEastADCSum << ", bbcWestADCSum = " << bbcWestADCSum << endl;
    //***************************** BEMC data ******************************************************
    if(!BEMCDecoder) BEMCDecoder = new StEmcDecoder();

    daqReader *rdr = (daqReader*)(datap);

    TDatime evt_time(rdr->evt_time); // time in unix seconds
    if (BEMCDecoder) BEMCDecoder->SetDateTime(evt_time.GetDate(),evt_time.GetTime());

    int STATUS = BEMCNOTINSTALLED; //NOT PRESENT

    float adcsumEast = 0.0;
    float adcsumWest = 0.0;
    float bemc_energy_sumWest = 0.0;
    float bemc_energy_sumEast = 0.0;

    daq_dta *dd_btow = rdr ? (rdr->det("btow")->get("adc")) : 0;
    if (dd_btow) while (dd_btow->iterate())
    {
        btow_t *d = (btow_t *) dd_btow->Void;
        if (d)
        {
            int TDCStatus[BTOW_MAXFEE];
            memset(TDCStatus, BEMCNOTINSTALLED, sizeof(TDCStatus));
            int TDCTotal = 0;
            STATUS = BEMCOK; //OK
            for (int tdc = 0; tdc < BTOW_MAXFEE;tdc++)
            {
                int count = d->preamble[tdc][0];
                int error = d->preamble[tdc][1];

                if ((error == 0) && (count == (BTOW_PRESIZE + BTOW_DATSIZE))) TDCStatus[tdc] = BEMCOK; // OK
                else if ((error == 4095) && (count == 4095)) TDCStatus[tdc] = BEMCNOTINSTALLED; // NOT INSTALLED
                else TDCStatus[tdc] = BEMCCORRUPTED; //CORRUPTED
                if (TDCStatus[tdc] == BEMCCORRUPTED) STATUS = BEMCCORRUPTED; // if any crate is corrupted, mark event as corrupted
            }
            adcsumEast = 0.0;
            adcsumWest = 0.0;
            bemc_energy_sumWest = 0.0;
            bemc_energy_sumEast = 0.0;
            float adc_at     = 0.0;
            float bemc_energy = 0.0;
            for(int i = 0; i < (BTOW_MAXFEE * BTOW_DATSIZE);i++)
            {
                int tdc = i % BTOW_MAXFEE;

                int tdc_channel = i / BTOW_MAXFEE;
                int count = d->preamble[tdc][0];
                int error = d->preamble[tdc][1];

                if((error==0) && (count == (BTOW_PRESIZE + BTOW_DATSIZE)))
                {
                    // OK
                    int adc = d->adc[tdc][tdc_channel];

                    TDCTotal += adc;
                    int daqid = ((tdc * BTOW_DATSIZE) + tdc_channel);
                    int softId = -1;
                    float iphi, eta, adcped;

                    if(BEMCDecoder && BEMCDecoder->GetTowerIdFromDaqId(i, softId))
                    {
                        adcped     =  adc-towerPed[softId-1];
                        bemc_energy = adcped*towerGain[softId-1];

                        StEmcGeom *BEMCGeom = StEmcGeom::instance("bemc");

                        BEMCGeom->getEta(softId, eta);
                        BEMCGeom->getPhi(softId, iphi);
 
                        adc_at = adc - towerPed[softId-1];
                        h_bemc_adc_all ->Fill(adc_at);
                        if(
                           adc_at > 6.0
                           && adc_at < 1000.0
                          )
                        {
                            if(eta > 0.0)
                            {
                                adcsumWest = adcsumWest + adc_at;
                                bemc_energy_sumWest = bemc_energy_sumWest + bemc_energy;
                            }
                            else
                            {
                                adcsumEast = adcsumEast + adc_at;
                                bemc_energy_sumEast = bemc_energy_sumEast + bemc_energy;
                            }
                        }
                        //cout << "i = " << i << ", tdc = " << tdc << ", softId = " << softId
                         //   << ", eta = " << eta << ", iphi = " << iphi << ", adcped = " << adcped << endl;
                    }
                }
            }
            h_bemc_adc_west_vs_east    ->Fill(adcsumEast,adcsumWest);
            h_bemc_energy_west_vs_east ->Fill(bemc_energy_sumEast,bemc_energy_sumWest);
            h_bemc_adc_east            ->Fill(adcsumEast);
            h_bemc_adc_west            ->Fill(adcsumWest);
            //cout << "adcsumEast = " << adcsumEast << ", adcsumWest = " << adcsumWest << endl;
        }
    }

    h_event_type ->Fill(0); // All events
    // Good BBC events
    if(
       bbcEastADCSum    > BBC_min_adc
       && bbcWestADCSum > BBC_min_adc
      )
    {
        h_event_type ->Fill(1);
    }
    // Good BEMC events - only minimum energy cut applied
    if(
       bemc_energy_sumEast    > BEMC_min_energy
       && bemc_energy_sumWest > BEMC_min_energy
      )
    {
        h_event_type ->Fill(2);
    }
    // Good BEMC events - all cuts applied
    if(
       (bemc_energy_sumEast > BEMC_min_energy && bemc_energy_sumWest > BEMC_min_energy)
       && (fabs(bemc_energy_sumEast-bemc_energy_sumWest) < BEMC_energy_diff
       || (bemc_energy_sumEast > BEMC_low_min_energy && bemc_energy_sumWest > BEMC_low_min_energy))
      )
    {
        h_event_type ->Fill(3);
    }
    // Good BBC and BEMC events
    if(
       (bbcEastADCSum > BBC_min_adc
       && bbcWestADCSum > BBC_min_adc)
       && (bemc_energy_sumEast > BEMC_min_energy && bemc_energy_sumWest > BEMC_min_energy)
       && (fabs(bemc_energy_sumEast-bemc_energy_sumWest) < BEMC_energy_diff
       || (bemc_energy_sumEast > BEMC_low_min_energy && bemc_energy_sumWest > BEMC_low_min_energy))
      )
    {
        h_event_type ->Fill(4);
    }
    else // not good at all
    {
        h_event_type ->Fill(5);
    }

    return true;

}

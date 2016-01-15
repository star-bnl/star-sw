#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"

#include "StRoot/StEEmcPool/muEztPanitkin/EEMCPlots.h"

#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1I.h>
#include <TH2F.h>
#include <TLine.h>
#include <TLatex.h>
#include <TArrow.h>

#include <math.h>
#include "eemcBuilder.h"
#include <RTS/include/rtsLog.h>
#include <TMath.h>

ClassImp(eemcBuilder);

#define min(A, B) (((A) < (B)) ? (A) : (B))
#define max(A, B) (((A) > (B)) ? (A) : (B))

eemcBuilder::eemcBuilder(JevpServer *parent) : JevpBuilder(parent) {
  plotsetname = (char *)"eemc";
}

eemcBuilder::~eemcBuilder() {
}





void eemcBuilder::eeJpQaMinMax(TH1 *hh) {
    if (!hh) return;
    hh->SetAxisRange(0.5, 6.4);
    hh->SetMinimum(0.);
    int ib = hh->GetMaximumBin();
    float yMax = hh->GetBinContent(ib);
    ib = hh->GetMinimumBin();
    float yMin = hh->GetBinContent(ib);
    float r = 0, er = 999;
    if (yMin <= 0) yMin = 1;
    if (yMax <= 0) yMax = 1;    
    if ((yMin > 0) && (yMax > 0)) {
	r = yMin / yMax;
	er = r * TMath::Sqrt((1 / yMax) + (1 / yMin));
    }
}


static Def defs[] = {
  // 11 - 1
  { "JPpedZoom", "colz", 0, 0, 1, 11 },
  { "JPtotCor", NULL, 0, 0, 0, 11 },
  { "JPtotFreq", NULL, 0, 0, 0, 11 },
  { "JPsumTh3", "b", 0, 0, 0, 11 }, // swapped in for JPtotCor depending on eemcTwMask?
  { "JPpedHot", NULL, 0, 0, 0, 11 },
  // 11 - 2, 12 -1
  { "ETowHealth", NULL, 0,0,0,11 },
  { "ETowHeadCorr", NULL, 0,0,0,11 },
  { "ETowOFF", NULL, 0,0,0,11 },
  { "ETowN256", NULL, 0,0,0,11 },
  { "ETowOFFid", NULL, 0,0,0,11 },
  { "ETowGhost", NULL, 0,0,0,11 },
  { "ETowCorrBit", NULL, 0,0,0,11 },
  { "ESmdHealth", NULL, 0,0,0,11 },
  { "ESmdHeadCorr", NULL, 0,0,0,11 },
  { "ESmdOFF", NULL, 0,0,0,11 },
  { "ESmdN256", NULL, 0,0,0,11 },
  { "ESmdOFFid", NULL, 0,0,0,11 },
  // { "ESmdGhost", NULL, 0,0,0,11 },
  { "ESmdCorrBit", NULL, 0,0,0,11 },
  // 11 - 3, 
  { "cr1Hot", "b", 0,1,0,11 },
  { "cr2Hot", "b", 0,1,0,11 },
  { "cr3Hot", "b", 0,1,0,11 }, 
  { "cr4Hot", "b", 0,1,0,11 },
  { "cr5Hot", "b", 0,1,0,11 },
  { "cr6Hot", "b", 0,1,0,11 },
  // 11 - 4
  { "HTow", "", 0,0,0,11 }, 
  { "HPre1", "", 0,0,0,11 }, 
  { "HPre2", "", 0,0,0,11 }, 
  { "HPost", "", 0,0,0,11 },
  // 12 - 2
  { "MAPMHits", "colz", 0,0,0,11 },
  { "MAPMHitsCopy", "colz", 0,0,0,11 },  // this is a special one...
  // The EEqaPresenter draws same histo twice with different bounds
  // This doesn't work for JevpPlot class, so needs to hack around it
  // I copy the first histo each event..
  // Ugly...
  //
  // 12 - 3,4,5,6
  { "SmdA1U", "", 0, 1, 0, 11},
  { "SmdA2U", "", 0, 1, 0, 11},
  { "SmdA3U", "", 0, 1, 0, 11},
  { "SmdA4U", "", 0, 1, 0, 11},
  { "SmdA5U", "", 0, 1, 0, 11},
  { "SmdA6U", "", 0, 1, 0, 11},
  { "SmdA7U", "", 0, 1, 0, 11},
  { "SmdA8U", "", 0, 1, 0, 11},
  { "SmdA9U", "", 0, 1, 0, 11},
  { "SmdA10U", "", 0, 1, 0, 11},
  { "SmdA11U", "", 0, 1, 0, 11},
  { "SmdA12U", "", 0, 1, 0, 11},
  { "SmdA1V", "", 0, 1, 0, 11},
  { "SmdA2V", "", 0, 1, 0, 11},
  { "SmdA3V", "", 0, 1, 0, 11},
  { "SmdA4V", "", 0, 1, 0, 11},
  { "SmdA5V", "", 0, 1, 0, 11},
  { "SmdA6V", "", 0, 1, 0, 11},
  { "SmdA7V", "", 0, 1, 0, 11},
  { "SmdA8V", "", 0, 1, 0, 11},
  { "SmdA9V", "", 0, 1, 0, 11},
  { "SmdA10V", "", 0, 1, 0, 11},
  { "SmdA11V", "", 0, 1, 0, 11},
  { "SmdA12V", "", 0, 1, 0, 11},

  { "HSmd1U", "", 0, 1, 0, 11},
  { "HSmd2U", "", 0, 1, 0, 11},
  { "HSmd3U", "", 0, 1, 0, 11},
  { "HSmd4U", "", 0, 1, 0, 11},
  { "HSmd5U", "", 0, 1, 0, 11},
  { "HSmd6U", "", 0, 1, 0, 11},
  { "HSmd7U", "", 0, 1, 0, 11},
  { "HSmd8U", "", 0, 1, 0, 11},
  { "HSmd9U", "", 0, 1, 0, 11},
  { "HSmd10U", "", 0, 1, 0, 11},
  { "HSmd11U", "", 0, 1, 0, 11},
  { "HSmd12U", "", 0, 1, 0, 11},
  { "HSmd1V", "", 0, 1, 0, 11},
  { "HSmd2V", "", 0, 1, 0, 11},
  { "HSmd3V", "", 0, 1, 0, 11},
  { "HSmd4V", "", 0, 1, 0, 11},
  { "HSmd5V", "", 0, 1, 0, 11},
  { "HSmd6V", "", 0, 1, 0, 11},
  { "HSmd7V", "", 0, 1, 0, 11},
  { "HSmd8V", "", 0, 1, 0, 11},
  { "HSmd9V", "", 0, 1, 0, 11},
  { "HSmd10V", "", 0, 1, 0, 11},
  { "HSmd11V", "", 0, 1, 0, 11},
  { "HSmd12V", "", 0, 1, 0, 11},
  // 13 - 1,2
  { "dsm0inJP1_HT", "colz", 0,0,1,11},
  { "dsm0inJP2_HT", "colz", 0,0,1,11},
  { "dsm0inJP3_HT", "colz", 0,0,1,11},
  { "dsm0inJP4_HT", "colz", 0,0,1,11},
  { "dsm0inJP5_HT", "colz", 0,0,1,11},
  { "dsm0inJP6_HT", "colz", 0,0,1,11},
  { "dsm0inJP1_TP", "colz", 0,0,1,11},
  { "dsm0inJP2_TP", "colz", 0,0,1,11},
  { "dsm0inJP3_TP", "colz", 0,0,1,11},
  { "dsm0inJP4_TP", "colz", 0,0,1,11},
  { "dsm0inJP5_TP", "colz", 0,0,1,11},
  { "dsm0inJP6_TP", "colz", 0,0,1,11},
  // 13 - 3,4
  { "dsm1HJP1_HT", "colz", 0,0,1,11},
  { "dsm1HJP2_HT", "colz", 0,0,1,11},
  { "dsm1HJP3_HT", "colz", 0,0,1,11},
  { "dsm1HJP4_HT", "colz", 0,0,1,11},
  { "dsm1HJP5_HT", "colz", 0,0,1,11},
  { "dsm1HJP6_HT", "colz", 0,0,1,11},
  { "dsm1HJP7_HT", "colz", 0,0,1,11},
  { "dsm1HJP8_HT", "colz", 0,0,1,11},
  { "dsm1HJP9_HT", "colz", 0,0,1,11},
  { "dsm1HJP10_HT", "colz", 0,0,1,11},
  { "dsm1HJP11_HT", "colz", 0,0,1,11},
  { "dsm1HJP12_HT", "colz", 0,0,1,11},
  { "dsm1HJP1_TP", "colz", 0,0,1,11},
  { "dsm1HJP2_TP", "colz", 0,0,1,11},
  { "dsm1HJP3_TP", "colz", 0,0,1,11},
  { "dsm1HJP4_TP", "colz", 0,0,1,11},
  { "dsm1HJP5_TP", "colz", 0,0,1,11},
  { "dsm1HJP6_TP", "colz", 0,0,1,11},
  { "dsm1HJP7_TP", "colz", 0,0,1,11},
  { "dsm1HJP8_TP", "colz", 0,0,1,11},
  { "dsm1HJP9_TP", "colz", 0,0,1,11},
  { "dsm1HJP10_TP", "colz", 0,0,1,11},
  { "dsm1HJP11_TP", "colz", 0,0,1,11},
  { "dsm1HJP12_TP", "colz", 0,0,1,11},
  // 13 - 5
  { "dsm2Half1_HTTP", "colz", 0,0,1,11},
  { "dsm2Half2_HTTP", "colz", 0,0,1,11},
  { "dsm3_HTTP", "colz", 0,0,1,11},
  // 13 - 6
  { "JP1_sum", "", 0,1,0,11},
  { "JP2_sum", "", 0,1,0,11},
  { "JP3_sum", "", 0,1,0,11},
  { "JP4_sum", "", 0,1,0,11},
  { "JP5_sum", "", 0,1,0,11},
  { "JP6_sum", "", 0,1,0,11},
  // 13 -7
  { "JPsumTh0", "", 0,0,0,11},
  { "JPsumTh1", "", 0,0,0,11},
  { "JPsumTh2", "", 0,0,0,11},
  { "JPsumTh3", "", 0,0,0,11},
  { "dsm2Half1_Etot", "", 0,0,0,11},
  { "dsm2Half2_Etot", "", 0,0,0,11},
  // 13 - 8
  { "JP12_sum","", 0,1,0,11},
  { "JP23_sum","", 0,1,0,11},
  { "JP34_sum","", 0,1,0,11},
  { "JP45_sum","", 0,1,0,11},  
  { "JP56_sum","", 0,1,0,11},  
  { "JP61_sum","", 0,1,0,11},
  // 13 - 9
  { "JP12_cor", "colz", 0,0,1,11},
  { "JP23_cor", "colz", 0,0,1,11},
  { "JP34_cor", "colz", 0,0,1,11},
  { "JP45_cor", "colz", 0,0,1,11},  
  { "JP56_cor", "colz", 0,0,1,11},  
  { "JP61_cor", "colz", 0,0,1,11},
  // 13 - 10
  { "dsm2E_etot0", "", 0,0,0,11 },
  { "dsm2B_etot0", 0,0,0,11 },
  { "dsm2BE_etot0", 0,0,0, 11},
  // 14-1
  { "cr1","colz",0,0,1,11 },
  { "cr2","colz",0,0,1,11 },
  { "cr3","colz",0,0,1,11 },
  { "cr4","colz",0,0,1,11 },
  { "cr5","colz",0,0,1,11 },
  { "cr6","colz",0,0,1,11 },
  // 14-2
  { "TowHits", "colz",0,0,1,11 },
  { "Pre1Hits", "colz", 0,0,1,11},
  { "Pre2Hits", "colz",0,0,1,11},
  { "PostHits", "colz",0,0,1,11},
  // 14-3...14-8
  { "cr64", "colz",0,0,1,11},
  { "cr65", "colz",0,0,1,11},
  { "cr66", "colz",0,0,1,11},
  { "cr67", "colz",0,0,1,11},
  { "cr68", "colz",0,0,1,11},
  { "cr69", "colz",0,0,1,11},
  { "cr70", "colz",0,0,1,11},
  { "cr71", "colz",0,0,1,11},
  { "cr72", "colz",0,0,1,11},
  { "cr73", "colz",0,0,1,11},
  { "cr74", "colz",0,0,1,11},
  { "cr75", "colz",0,0,1,11},
  { "cr76", "colz",0,0,1,11},
  { "cr77", "colz",0,0,1,11},
  { "cr78", "colz",0,0,1,11},
  { "cr79", "colz",0,0,1,11},
  { "cr80", "colz",0,0,1,11},
  { "cr81", "colz",0,0,1,11},
  { "cr82", "colz",0,0,1,11},
  { "cr83", "colz",0,0,1,11},
  { "cr84", "colz",0,0,1,11},
  { "cr85", "colz",0,0,1,11},
  { "cr86", "colz",0,0,1,11},
  { "cr87", "colz",0,0,1,11},
  { "cr88", "colz",0,0,1,11},
  { "cr89", "colz",0,0,1,11},
  { "cr90", "colz",0,0,1,11},
  { "cr91", "colz",0,0,1,11},
  { "cr92", "colz",0,0,1,11},
  { "cr93", "colz",0,0,1,11},
  { "cr94", "colz",0,0,1,11},
  { "cr95", "colz",0,0,1,11},
  { "cr96", "colz",0,0,1,11},
  { "cr97", "colz",0,0,1,11},
  { "cr98", "colz",0,0,1,11},
  { "cr99", "colz",0,0,1,11},
  { "cr100", "colz",0,0,1,11},
  { "cr101", "colz",0,0,1,11},
  { "cr102", "colz",0,0,1,11},
  { "cr103", "colz",0,0,1,11},
  { "cr104", "colz",0,0,1,11},
  { "cr105", "colz",0,0,1,11},
  { "cr106", "colz",0,0,1,11},
  { "cr107", "colz",0,0,1,11},
  { "cr108", "colz",0,0,1,11},
  { "cr109", "colz",0,0,1,11},
  { "cr110", "colz",0,0,1,11},
  { "cr111", "colz",0,0,1,11},
  // 14-9
  { "dsm0inJPall_HT", "colz",0,0,1,11},  
  { "dsm0inJPall_TP", "colz",0,0,1,11},
  // 15-10
  //{ "HighTowerTriggerCorruption", "colz",0,0,1,11},
  //{ "PatchSumTriggerCorruption", "colz",0,0,1,11},

  { NULL, "",0,0,0, 11 }, 
};

void eemcBuilder::initialize(int argc, char *argv[]) {
  char dumpfile[256];
  char pathin[256];
  char pathout[256];
  
  sprintf(dumpfile, "%s/eemc/eemcDbDump.dat", confdatadir);
  sprintf(pathin, "%s/eemc/", confdatadir);
  sprintf(pathout, "%s/eemc/", confdatadir);
 
  EEMCPlots::initHisto(0, dumpfile, pathin, pathout);

  TH1 *hist;

  // Add root histograms to Plots

  int n=0;


  //  gROOT->GetObject(Hist_TDC_statusName, hist);
  //   if(!hist) LOG("ERR", "couldn't find %s",Hist_TDC_statusName);
  //   plots[n] = new JevpPlot(hist);
  //   plots[n]->setDrawOpts((char *)"H COLZ");
  
 
  while(defs[n].name) {
    if(strcmp(defs[n].name,"MAPMHitsCopy") == 0) {
      hist = new TH1F("MAPMHits2","blah",10,0,10);
      // LOG("JEFF", "hist 0x%x",hist);
      plots[n] = new JevpPlot(hist);
      addPlot(plots[n]);
      MAPMHitsCopy = n;
      plots[n]->setDrawOpts("colz");
      plots[n]->optstat = 11;
      n++;
      continue;
    }

    gROOT->GetObject(defs[n].name, hist);
    if(!hist) LOG("ERR", "couldn't find %s",defs[n].name);
    plots[n] = new JevpPlot(hist);
    plots[n]->gridx = 0;
    plots[n]->gridy = 0;
    if(defs[n].opts) plots[n]->setDrawOpts(defs[n].opts);
    plots[n]->logx = defs[n].logx;
    plots[n]->logy = defs[n].logy;
    //plots[n]->logz = defs[n].logz;
    plots[n]->optstat = defs[n].optstat;
    
    // mods based on specific histos...
    if((strcmp("JPtotCor", defs[n].name) == 0) ||
       (strcmp("JPtotFreq", defs[n].name) == 0) ||
       (strcmp("JPsumTh3", defs[n].name) == 0)) {
      //eeJpQaMinMax(hist); 
    }
      
    if((strcmp("cr1Hot", defs[n].name) == 0) ||
       (strcmp("cr2Hot", defs[n].name) == 0) ||
       (strcmp("cr3Hot", defs[n].name) == 0) ||
       (strcmp("cr4Hot", defs[n].name) == 0) ||
       (strcmp("cr5Hot", defs[n].name) == 0) ||
       (strcmp("cr6Hot", defs[n].name) == 0) ) {
      if((hist->Integral() > 10) &&
	 (hist->GetRMS() > 2)) {
	hist->Fit("pol0");
	//TF1 *ff = hist->GetFunction("pol0");
// 	if (twMask) {
// 	  TGraph &gr = twMask->crG[i];
// 	  if (gr.GetN() > 0) gr.Draw("P");
// 	}
      }
    }
       
    if(strcmp(defs[n].name, "MAPMHits") == 0) {
      MAPMHits = n;
      plots[n]->gridx = 1;
      plots[n]->gridy = 1;
      TAxis *xaxis = hist->GetXaxis();
      xaxis->SetRange(63,87);
      xaxis->SetTitle("Crate ID     12S1=64,  1S1=68,  2S1=72,  3S1=76,  4S1=80,  5S1=84"); 
      xaxis->SetTitleSize(.03);
      xaxis->SetTitleOffset(-.01);
      
    }

    if(strcmp(defs[n].name, "dsm2E_etot0") == 0) {
      TH1 *hh;
      gROOT->GetObject("dsm2E_etot1", hh);
      if(!hist) LOG(ERR, "couldnt find %s",defs[n].name);
      plots[n]->addHisto(hh);
    }
    
    if(strcmp(defs[n].name, "dsm2B_etot0") == 0) {   
      TH1 *hh;
      gROOT->GetObject("dsm2B_etot1", hh);
      if(!hist) LOG("ERR", "couldn't find %s",defs[n].name);
      plots[n]->addHisto(hh);
    }
	
    if(strcmp(defs[n].name, "dsm2BE_etot0") == 0) {
      TH1 *hh;
      gROOT->GetObject("dsm2BE_etot1", hh);
      if(!hist) LOG("ERR", "couldn't find %s",defs[n].name);
      plots[n]->addHisto(hh);
    }
    
    addPlot(plots[n]);
    n++;
  }
}
  
void eemcBuilder::startrun(daqReader *rdr) {
  LOG("JEFF", "eemcBuilder starting run #%d",rdr->run);
 
  EEMCPlots::resetHisto();
}

#define safelog(x) ((x > 0) ? log10(x) : 0)

void eemcBuilder::event(daqReader *rdr)
{
  // Find trigger data...
  static int eventn;
  eventn++;
  
  StTriggerData *trgd = getStTriggerData(rdr);
  // if(!trgd) return;

  //LOG("JEFF", "FillHisto");
  EEMCPlots::fillHisto( (char *)rdr,
			trgd ? trgd->getDsm0_EEMC() : 0,
			trgd ? trgd->getDsm1_EEMC() : 0,
			trgd ? trgd->getDsm2_EMC() : 0,
			trgd ? trgd->getDsm3() : 0);

  // LOG("JEFF", "Done with fillHisto");
  // Hack to double up MAPMHits histo 2!  
  // Delete and recreate each time...

  // can this possibly work?
  TH2F *oh = (TH2F *)plots[MAPMHits]->getHisto(0)->histo;
  TH2F *h = (TH2F *)oh->Clone("MAPMHits2");
  PlotHisto *ph = plots[MAPMHitsCopy]->getHisto(0);
  delete(ph->histo);
  ph->histo = h;
  h->SetAxisRange(88,120);
  h->SetXTitle("Crate ID     6S1=88,  7S1=92,  8S1=96,  9S1=100, 10S1=104,  11S1=108");


  if(trgd) {
    delete trgd;
    trgd = NULL;
  }
 
  //LOG("JEFF", "Done with event");
}

void eemcBuilder::stoprun(daqReader *rdr) {
  
}

void eemcBuilder::main(int argc, char *argv[])
{
  eemcBuilder me;
  
  me.Main(argc, argv);
}


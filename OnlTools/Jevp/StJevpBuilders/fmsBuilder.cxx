#include "fmsBuilder.h"


#include <RTS/include/rtsLog.h>


/* TEST COMMANDS used for run17 implementation
 * tcsh
 * starver SL16l
 * cd ~/JEVP
 * cons
 * OnlTools/Jevp/launch fmsBuilder \
   -file /star/u/dilks/scratch/dilks/st_fms_17128032_raw_2000014.daq \
   -pdf ~/protected/dilks/sandbox/new.pdf -maxevts 111 -loglevel C
 */


ClassImp(fmsBuilder)

namespace {
  enum StFmsQtCrateNumber {kQt1 = 1, kQt2, kQt3, kQt4, kFpd, kQtError};
  enum StFmsSignalType {kADC, kLED};
  enum large_small {kLarge,kSmall};

  /*
   Basic QT crate geometry.
   Note that physically there are 16 slots per crate, but only 11
   at most are currently actually used.
   */
  const int kNQtSlotsPerCrate = 11;
  const int kNQtChannelsPerSlot = 32;
  const int kNChannels = kNQtSlotsPerCrate * kNQtChannelsPerSlot;
  const int kNAdc = 4096;
  
  /* plot aesthetics */
  const float kAxisLabelSize = 0.02; // axis label size (default=0.04)
  const int kNLargeDivisions = kNQtSlotsPerCrate*2; // number of vertical grid lines
  const int kNMediumDivisions = 2; // number of medium tick marks per vertical grid line
  const int kNSmallDivisions = 4; // number of small tick marks per medium tick mark

  /*
   Returns a string giving the position of a given QT crate.
   For example QT 1 is "south-top" and QT 4 is "north-bottom".
   Returns a blank string for an invalid QT crate number.
   */
  std::string position(int qt) {
    std::string s;
    switch(qt) {
      case kQt1:
        s = "south-top";
        break;
      case kQt2:
        s = "south-bottom";
        break;
      case kQt3:
        s = "north-top";
        break;
      case kQt4:
        s = "north-bottom";
        break;
      case kFpd:
        s = "FPD";
        break;
    } // switch
    return s;
  }

  /*
   Composes the name and title corresponding to a given QT crate number.
   */
  std::pair<std::string, std::string> composeQtNameTitle(int qt, int evtype, int dim) {
    std::string name, title;
    std::stringstream stream;
    if(qt >= kQt1 and qt <= kQt4 and dim == 2) {
      if(evtype == kADC) {
        stream << "fms_qt_channel_adc_crate_" << qt;
        name = stream.str();
        stream.str("");
        stream.clear();
        stream << "ADC Input to FMS QT crate " << qt << " (" << position(qt) << ")";
        title = stream.str();
      } // if
      else if(evtype == kLED) {
        stream << "fms_qt_channel_led_crate_" << qt;
        name = stream.str();
        stream.str("");
        stream.clear();
        stream << "LED Input to FMS QT crate " << qt << " (" << position(qt) << ")";
        title = stream.str();
      } // if
    } // if
    else if(qt >= kQt1 and qt <= kQt4 and dim == 1) {
      stream << "adc_critical_crate_" << qt;
      name = stream.str();
      stream.str("");
      stream.clear();
      stream << "Overflow (ADC>4094) for QT crate " << qt << " (" << position(qt) << ")";
      title = stream.str();
    } // else if
    else if(kFpd == qt) {
      name = "fpd_channel_adc";
      title = "Input to FPD QT crate";
    } // else if
    return std::make_pair(name, title);
  }

  /*
   Discussing with Akio and Pibero, they says that LED
   events can be identified via StTriggerData, using the 
   least-significant bit in channel four of the DSM.
   */
  bool isLedEvent(const StTriggerData& trigger) {
    const unsigned short dsm = trigger.lastDSM(4);
    return dsm & 0x1; // Checks least-significant bit
  }

} // namespace

fmsBuilder::fmsBuilder(JevpServer* parent)
: JevpBuilder(parent) {
  // Set plotsetname inherited from JevpBuilder.
  plotsetname = (char*)"fms";
}

fmsBuilder::~fmsBuilder() {
  // Delete plots but not histograms, as the histograms
  // are owned by the plots.
  JevpPlotPtrList::iterator i;
  for(i = mPlots.begin(); i not_eq mPlots.end(); ++i) {
    delete *i;
  } // for
}

void fmsBuilder::initialize(int /* unused */, char** /* unused */) {
  LOG(DBG, "fmsBuilder::initialize");

  initMaps();

  Int_t count=1;
  //fmsdb = new StFmsDbMaker(); // disabled... causes symbol lookup error?


  // Create histograms for each QT crate:
  // mPlots list entries:
  //  - 1-4 = ADC vs. channel for QT 1-4
  //  - 5-8 = LED ADC vs. channel for QT 1-4
  //  - 9-12 = ADC overflow plots for QT 1-4

  // -- ADC vs channel & LED ADC vs. channel
  for(int evtype = kADC; evtype <= kLED; ++evtype) {
    for(int qt = kQt1; qt <= kQt4; ++qt) {
      // Create the histogram.
      std::pair<std::string, std::string> nameTitle
        = composeQtNameTitle(qt,evtype,2);
      TH2F* h = new TH2F(nameTitle.first.c_str(),
                         nameTitle.second.c_str(),
                         kNChannels, 0., kNChannels,  // Channel axis bins
                         200, 0., kNAdc);           // ADC axis bins
                         
      h->SetBit(TH1::kCanRebin);
      h->SetXTitle("slot * 32 + channel");
      h->SetYTitle("ADC");
      h->GetXaxis()->SetNdivisions(kNLargeDivisions,
                                   kNMediumDivisions,
                                   kNSmallDivisions,0);
      h->GetYaxis()->SetNdivisions(8,4,4,0);
      h->GetXaxis()->SetLabelSize(kAxisLabelSize);
      h->GetYaxis()->SetLabelSize(kAxisLabelSize);

      mHists.insert(std::make_pair(count++, h));
      JevpPlot * j = new JevpPlot(h); // create JevpPlot
      j->optlogz=1;
      j->optstat=0;
      mPlots.push_back(j);
      addPlot(mPlots.back()); // Registers the plot with this Jevp
    } // for
  } // for

  
  // -- overflow plots
  for(int qt = kQt1; qt <= kQt4; ++qt) {
    std::pair<std::string, std::string> nameTitle
      = composeQtNameTitle(qt,kADC,1);
    TH1F* h = new TH1F(nameTitle.first.c_str(),
                       nameTitle.second.c_str(),
                       kNChannels, 0., kNChannels);
    h->SetBit(TH1::kCanRebin);
    h->SetXTitle("slot * 32 + channel");
    h->SetLineWidth(5);
    h->SetLineColor(kRed);
    h->GetXaxis()->SetNdivisions(kNLargeDivisions,
                                 kNMediumDivisions,
                                 kNSmallDivisions,0);
    h->GetXaxis()->SetLabelSize(kAxisLabelSize);
    h->GetYaxis()->SetLabelSize(kAxisLabelSize);

    mHists.insert(std::make_pair(count++, h));
    mPlots.push_back(new JevpPlot(h));
    (mPlots.back())->optstat=0;
    addPlot(mPlots.back()); // Registers the plot with this Jevp
  } // for


  // -- hit map
  hitmap[kLarge][kADC] = new TH2D("large_adc_hitmap","ADC Large Cells Hit Map",
                                   34,-17,17,34,-34,0);
  hitmap[kSmall][kADC] = new TH2D("small_adc_hitmap","ADC Small Cells Hit Map",
                                   24,-12,12,24,-24,0);
  hitmap[kLarge][kLED] = new TH2D("large_led_hitmap","LED Large Cells Hit Map",
                                   34,-17,17,34,-34,0);
  hitmap[kSmall][kLED] = new TH2D("small_led_hitmap","LED Small Cells Hit Map",
                                   24,-12,12,24,-24,0);
  /*
   * DEPRECATED version: used to fit small cells inside
   * large cells and uses methods xPos and yPos to convert
   * cell coordinates into histogram coordinates
  hitmap[kLarge] = new TH2D("large_hm","ADC Hit Map",
                             34,-98.6,98.6,34,-98.6,98.6);
  hitmap[kSmall] = new TH2D("small_hm","ADC Hit Map",
                             52,-98.6,98.6,52,-98.6,98.6);
  */
  for(int ee=0; ee<2; ee++) {
    for(int ls=0; ls<2; ls++) {
      hitmap[ls][ee]->SetBit(TH1::kCanRebin);
      hitmap[ls][ee]->SetXTitle("column");
      hitmap[ls][ee]->SetYTitle("row");
      hitmap[ls][ee]->GetXaxis()->SetLabelSize(kAxisLabelSize);
      hitmap[ls][ee]->GetYaxis()->SetLabelSize(kAxisLabelSize);
    };
    hitmap[kLarge][ee]->GetXaxis()->SetNdivisions(34,2,1,0);
    hitmap[kLarge][ee]->GetYaxis()->SetNdivisions(34,2,1,0);
    hitmap[kSmall][ee]->GetXaxis()->SetNdivisions(24,2,1,0);
    hitmap[kSmall][ee]->GetYaxis()->SetNdivisions(24,2,1,0);
  };

  JevpPlot * jhm[2][2]; // [kLarge/kSmall] [kADC/kLED]
  for(int ee=0; ee<2; ee++) {
    for(int ls=0; ls<2; ls++) {
      jhm[ls][ee] = new JevpPlot(hitmap[ls][ee]);
      jhm[ls][ee]->optlogz=1;
      jhm[ls][ee]->optstat=0;
      //jhm[ls][ee]->setDrawOpts("colz x+ y+");
      addPlot(jhm[ls][ee]);
    };
  };


  // -- fms channel map
  // DEPRECATED -- idea was to put channel number on hit map,
  // but I can't get JevpPlot to draw a histogram with one 
  // draw option (fmschan with option "text") over another histogram
  // with another draw option (hitmap with option "colz")
  /*
  fmschan[kLarge] = new TH2D("large_fmschan","ADC Channel Map",
                             34,-98.6,98.6,34,-98.6,98.6);
  fmschan[kSmall] = new TH2D("small_fmschan","ADC Channel Map",
                             52,-98.6,98.6,52,-98.6,98.6);
  for(int ls=0; ls<2; ls++) {
    fmschan[ls]->SetBit(TH1::kCanRebin);
    fmschan[ls]->SetXTitle("x position");
    fmschan[ls]->SetYTitle("y position");
    fmschan[ls]->GetXaxis()->SetLabelSize(kAxisLabelSize);
    fmschan[ls]->GetYaxis()->SetLabelSize(kAxisLabelSize);
    fmschan[ls]->SetDrawOption("box");
  };
  // fill channel map
  Int_t binn,size;
  for(int qqc=0; qqc<N_QTCR; qqc++) {
    for(int qqi=0; qqi<N_QTIDX; qqi++) {
      nstb = nstb_map[qqc][qqi];
      row = row_map[qqc][qqi];
      col = col_map[qqc][qqi];
      if(nstb>0 && row>=0 && col>=0) {
        size = nstb<3 ? kLarge:kSmall;
        binn = fmschan[size]->FindBin(
          xPos(nstb,row,col),
          yPos(nstb,row,col)
        );
        fmschan[size]->SetBinContent(binn,chan_map[qqc][qqi]);
      };
    };
  };
  */

}

void fmsBuilder::startrun(daqReader* reader) {
  LOG(DBG, "fmsBuilder::startrun %d", reader->run);
  // Clear existing histogram contents.
  TH1PtrMap::iterator i;
  for(i = mHists.begin(); i not_eq mHists.end(); ++i) {
    i->second->Reset();
  } // for
  for(int ee=0; ee<2; ee++) {
    for(int ls=0; ls<2; ls++) {
      hitmap[ls][ee]->Reset();
    };
  };
}

void fmsBuilder::stoprun(daqReader* reader) {
  LOG(DBG, "fmsBuilder::stoprun %d", reader->run);
}

void fmsBuilder::event(daqReader* reader) {

  LOG(DBG, "fmsBuilder::event %d", reader->event_number);

  // Get trigger data for the current event.
  // getStTriggerData is inherited from Jevp.
  std::auto_ptr<StTriggerData> trigger(getStTriggerData(reader));
  if(not trigger.get()) {
    return;
  } // if


  // select event type
  int evtype;
  if(isLedEvent(*trigger)) {
    evtype = kLED;
  } // if
  else evtype = kADC;


  // Loop over map of histograms (indexed by crate+evtype*4).
  TH1PtrMap::iterator i;
  for(i = mHists.begin(); i not_eq mHists.end(); ++i) {
    int ii = i->first;

    // if evtype=0 then use 1<=ii<=4;  if evtype=1 then use 5<=ii<=8
    if(ii>evtype*4 && ii<=(evtype*4+4)) {

      TH1* histogram = i->second;

      // Fill the histogram for each channel by looping over
      // all slot and channel-in-slot values.
      crate = ii - evtype*4;
      for(int slot(0); slot < kNQtSlotsPerCrate; ++slot) {
        for(int qtchan(0); qtchan < kNQtChannelsPerSlot; ++qtchan) {

          // fill ADC vs. channel plot
          index = slot * kNQtChannelsPerSlot + qtchan;
          adc = trigger->fmsADC(crate, slot, qtchan, 0);
          histogram->Fill(index, adc);

          // fill critical plot
          if(adc>4094 and evtype==kADC) (mHists.at(ii+8))->Fill(index);

          /* DEPRECATED -- map using StFmsDbMaker (not working and
           * consequently, never tested)
          fmsdb->getReverseMap(crate,slot,qtchan,&detid,&cellchan);
          nstb = detid-7; // 1<=nstb<=4 given by detectorid - 7
          row = fmsdb->getRowNumber(detid,cellchan);
          col = fmsdb->getColumnNumber(detid,cellchan);
          */

          // get nstb,row,col from QT crate and QT channel index
          nstb = nstb_map[crate][index];
          row = row_map[crate][index];
          col = col_map[crate][index];

          if(nstb>0 && row>=0 && col>=0) {

            /* for debugging
            printf("nstb=%d row=%d col=%d -- crate=%d idx=%d adc=%d\n",
              nstb,row,col,crate,index,adc);
            */

            hitmap[nstb<3?kLarge:kSmall][evtype]->Fill(
                (nstb==1||nstb==3) ? -1*col - 1 : col,
                -1 * row - 1,
                adc
            );

            /* DEPRECATED version's fill call
            hitmap[nstb<3?kLarge:kSmall]->Fill(
                xPos(nstb,row,col),
                yPos(nstb,row,col),
                adc
            );
            */

          }; // if
        } // for
      } // for
    } // if
  } // for
}



// MAIN
void fmsBuilder::main(int argc, char *argv[]) {
  fmsBuilder self;
  self.Main(argc, argv);
}



// convert nstb,row,col to x,y position for hit maps
// DEPRECATED -- these were used to fit small cells inside
// large cells, but I've ultimately decided to keep large
// cells and small cells seperate
/*
Float_t fmsBuilder::xPos(Int_t nstb_,Int_t row_,Int_t col_) {
  if(nstb_==1 || nstb_==2) {
    return 5.8*2*(nstb_-1.5)*(col_+0.5);
  }
  else if(nstb_==3 || nstb_==4) {
    return 3.8*2*(nstb_-3.5)*(col_+0.5);
  }
  else return 0;
};
Float_t fmsBuilder::yPos(Int_t nstb_,Int_t row_,Int_t col_) {
  if(nstb_==1 || nstb_==2) {
    return -5.8*(row_+0.5-17);
  }
  else if(nstb_==3 || nstb_==4) {
    return -3.8*(row_-11.5);
  }
  else return 0;
};
*/
  



// initialize fms channel maps; this map was generated by
// run17tools/fms_map/make_map_for_jevp_code.C by chris dilks
// 
// really one should use StFmsDbMaker to get these maps, but I had
// trouble getting it working here... instead from here on is a long list
// of hard-coded numbers!
void fmsBuilder::initMaps() {

  for(int qqc=0; qqc<N_QTCR; qqc++) {
    for(int qqi=0; qqi<N_QTIDX; qqi++) {
      nstb_map[qqc][qqi]=-1;
      chan_map[qqc][qqi]=-1;
      row_map[qqc][qqi]=-1;
      col_map[qqc][qqi]=-1;
    };
  };

  nstb_map[3][342]=1; chan_map[3][342]=1; row_map[3][342]=0; col_map[3][342]=0;
  nstb_map[3][343]=1; chan_map[3][343]=2; row_map[3][343]=0; col_map[3][343]=1;
  nstb_map[3][328]=1; chan_map[3][328]=3; row_map[3][328]=0; col_map[3][328]=2;
  nstb_map[3][329]=1; chan_map[3][329]=4; row_map[3][329]=0; col_map[3][329]=3;
  nstb_map[3][330]=1; chan_map[3][330]=5; row_map[3][330]=0; col_map[3][330]=4;
  nstb_map[3][331]=1; chan_map[3][331]=6; row_map[3][331]=0; col_map[3][331]=5;
  nstb_map[3][332]=1; chan_map[3][332]=7; row_map[3][332]=0; col_map[3][332]=6;
  nstb_map[3][333]=1; chan_map[3][333]=8; row_map[3][333]=0; col_map[3][333]=7;
  nstb_map[3][334]=1; chan_map[3][334]=9; row_map[3][334]=0; col_map[3][334]=8;
  nstb_map[3][335]=1; chan_map[3][335]=10; row_map[3][335]=0; col_map[3][335]=9;
  nstb_map[3][135]=1; chan_map[3][135]=18; row_map[3][135]=1; col_map[3][135]=0;
  nstb_map[3][143]=1; chan_map[3][143]=19; row_map[3][143]=1; col_map[3][143]=1;
  nstb_map[3][151]=1; chan_map[3][151]=20; row_map[3][151]=1; col_map[3][151]=2;
  nstb_map[3][159]=1; chan_map[3][159]=21; row_map[3][159]=1; col_map[3][159]=3;
  nstb_map[3][167]=1; chan_map[3][167]=22; row_map[3][167]=1; col_map[3][167]=4;
  nstb_map[3][175]=1; chan_map[3][175]=23; row_map[3][175]=1; col_map[3][175]=5;
  nstb_map[3][183]=1; chan_map[3][183]=24; row_map[3][183]=1; col_map[3][183]=6;
  nstb_map[3][191]=1; chan_map[3][191]=25; row_map[3][191]=1; col_map[3][191]=7;
  nstb_map[3][194]=1; chan_map[3][194]=26; row_map[3][194]=1; col_map[3][194]=8;
  nstb_map[3][193]=1; chan_map[3][193]=27; row_map[3][193]=1; col_map[3][193]=9;
  nstb_map[3][192]=1; chan_map[3][192]=28; row_map[3][192]=1; col_map[3][192]=10;
  nstb_map[3][134]=1; chan_map[3][134]=35; row_map[3][134]=2; col_map[3][134]=0;
  nstb_map[3][142]=1; chan_map[3][142]=36; row_map[3][142]=2; col_map[3][142]=1;
  nstb_map[3][150]=1; chan_map[3][150]=37; row_map[3][150]=2; col_map[3][150]=2;
  nstb_map[3][158]=1; chan_map[3][158]=38; row_map[3][158]=2; col_map[3][158]=3;
  nstb_map[3][166]=1; chan_map[3][166]=39; row_map[3][166]=2; col_map[3][166]=4;
  nstb_map[3][174]=1; chan_map[3][174]=40; row_map[3][174]=2; col_map[3][174]=5;
  nstb_map[3][182]=1; chan_map[3][182]=41; row_map[3][182]=2; col_map[3][182]=6;
  nstb_map[3][190]=1; chan_map[3][190]=42; row_map[3][190]=2; col_map[3][190]=7;
  nstb_map[3][203]=1; chan_map[3][203]=43; row_map[3][203]=2; col_map[3][203]=8;
  nstb_map[3][202]=1; chan_map[3][202]=44; row_map[3][202]=2; col_map[3][202]=9;
  nstb_map[3][201]=1; chan_map[3][201]=45; row_map[3][201]=2; col_map[3][201]=10;
  nstb_map[3][200]=1; chan_map[3][200]=46; row_map[3][200]=2; col_map[3][200]=11;
  nstb_map[3][133]=1; chan_map[3][133]=52; row_map[3][133]=3; col_map[3][133]=0;
  nstb_map[3][141]=1; chan_map[3][141]=53; row_map[3][141]=3; col_map[3][141]=1;
  nstb_map[3][149]=1; chan_map[3][149]=54; row_map[3][149]=3; col_map[3][149]=2;
  nstb_map[3][157]=1; chan_map[3][157]=55; row_map[3][157]=3; col_map[3][157]=3;
  nstb_map[3][165]=1; chan_map[3][165]=56; row_map[3][165]=3; col_map[3][165]=4;
  nstb_map[3][173]=1; chan_map[3][173]=57; row_map[3][173]=3; col_map[3][173]=5;
  nstb_map[3][181]=1; chan_map[3][181]=58; row_map[3][181]=3; col_map[3][181]=6;
  nstb_map[3][189]=1; chan_map[3][189]=59; row_map[3][189]=3; col_map[3][189]=7;
  nstb_map[3][211]=1; chan_map[3][211]=60; row_map[3][211]=3; col_map[3][211]=8;
  nstb_map[3][210]=1; chan_map[3][210]=61; row_map[3][210]=3; col_map[3][210]=9;
  nstb_map[3][209]=1; chan_map[3][209]=62; row_map[3][209]=3; col_map[3][209]=10;
  nstb_map[3][208]=1; chan_map[3][208]=63; row_map[3][208]=3; col_map[3][208]=11;
  nstb_map[3][341]=1; chan_map[3][341]=64; row_map[3][341]=3; col_map[3][341]=12;
  nstb_map[3][132]=1; chan_map[3][132]=69; row_map[3][132]=4; col_map[3][132]=0;
  nstb_map[3][140]=1; chan_map[3][140]=70; row_map[3][140]=4; col_map[3][140]=1;
  nstb_map[3][148]=1; chan_map[3][148]=71; row_map[3][148]=4; col_map[3][148]=2;
  nstb_map[3][156]=1; chan_map[3][156]=72; row_map[3][156]=4; col_map[3][156]=3;
  nstb_map[3][164]=1; chan_map[3][164]=73; row_map[3][164]=4; col_map[3][164]=4;
  nstb_map[3][172]=1; chan_map[3][172]=74; row_map[3][172]=4; col_map[3][172]=5;
  nstb_map[3][180]=1; chan_map[3][180]=75; row_map[3][180]=4; col_map[3][180]=6;
  nstb_map[3][188]=1; chan_map[3][188]=76; row_map[3][188]=4; col_map[3][188]=7;
  nstb_map[3][219]=1; chan_map[3][219]=77; row_map[3][219]=4; col_map[3][219]=8;
  nstb_map[3][218]=1; chan_map[3][218]=78; row_map[3][218]=4; col_map[3][218]=9;
  nstb_map[3][217]=1; chan_map[3][217]=79; row_map[3][217]=4; col_map[3][217]=10;
  nstb_map[3][216]=1; chan_map[3][216]=80; row_map[3][216]=4; col_map[3][216]=11;
  nstb_map[3][340]=1; chan_map[3][340]=81; row_map[3][340]=4; col_map[3][340]=12;
  nstb_map[3][339]=1; chan_map[3][339]=82; row_map[3][339]=4; col_map[3][339]=13;
  nstb_map[3][131]=1; chan_map[3][131]=86; row_map[3][131]=5; col_map[3][131]=0;
  nstb_map[3][139]=1; chan_map[3][139]=87; row_map[3][139]=5; col_map[3][139]=1;
  nstb_map[3][147]=1; chan_map[3][147]=88; row_map[3][147]=5; col_map[3][147]=2;
  nstb_map[3][155]=1; chan_map[3][155]=89; row_map[3][155]=5; col_map[3][155]=3;
  nstb_map[3][163]=1; chan_map[3][163]=90; row_map[3][163]=5; col_map[3][163]=4;
  nstb_map[3][171]=1; chan_map[3][171]=91; row_map[3][171]=5; col_map[3][171]=5;
  nstb_map[3][179]=1; chan_map[3][179]=92; row_map[3][179]=5; col_map[3][179]=6;
  nstb_map[3][187]=1; chan_map[3][187]=93; row_map[3][187]=5; col_map[3][187]=7;
  nstb_map[3][230]=1; chan_map[3][230]=94; row_map[3][230]=5; col_map[3][230]=8;
  nstb_map[3][229]=1; chan_map[3][229]=95; row_map[3][229]=5; col_map[3][229]=9;
  nstb_map[3][228]=1; chan_map[3][228]=96; row_map[3][228]=5; col_map[3][228]=10;
  nstb_map[3][227]=1; chan_map[3][227]=97; row_map[3][227]=5; col_map[3][227]=11;
  nstb_map[3][226]=1; chan_map[3][226]=98; row_map[3][226]=5; col_map[3][226]=12;
  nstb_map[3][225]=1; chan_map[3][225]=99; row_map[3][225]=5; col_map[3][225]=13;
  nstb_map[3][224]=1; chan_map[3][224]=100; row_map[3][224]=5; col_map[3][224]=14;
  nstb_map[3][130]=1; chan_map[3][130]=103; row_map[3][130]=6; col_map[3][130]=0;
  nstb_map[3][138]=1; chan_map[3][138]=104; row_map[3][138]=6; col_map[3][138]=1;
  nstb_map[3][146]=1; chan_map[3][146]=105; row_map[3][146]=6; col_map[3][146]=2;
  nstb_map[3][154]=1; chan_map[3][154]=106; row_map[3][154]=6; col_map[3][154]=3;
  nstb_map[3][162]=1; chan_map[3][162]=107; row_map[3][162]=6; col_map[3][162]=4;
  nstb_map[3][170]=1; chan_map[3][170]=108; row_map[3][170]=6; col_map[3][170]=5;
  nstb_map[3][178]=1; chan_map[3][178]=109; row_map[3][178]=6; col_map[3][178]=6;
  nstb_map[3][186]=1; chan_map[3][186]=110; row_map[3][186]=6; col_map[3][186]=7;
  nstb_map[3][232]=1; chan_map[3][232]=111; row_map[3][232]=6; col_map[3][232]=8;
  nstb_map[3][233]=1; chan_map[3][233]=112; row_map[3][233]=6; col_map[3][233]=9;
  nstb_map[3][234]=1; chan_map[3][234]=113; row_map[3][234]=6; col_map[3][234]=10;
  nstb_map[3][235]=1; chan_map[3][235]=114; row_map[3][235]=6; col_map[3][235]=11;
  nstb_map[3][236]=1; chan_map[3][236]=115; row_map[3][236]=6; col_map[3][236]=12;
  nstb_map[3][237]=1; chan_map[3][237]=116; row_map[3][237]=6; col_map[3][237]=13;
  nstb_map[3][238]=1; chan_map[3][238]=117; row_map[3][238]=6; col_map[3][238]=14;
  nstb_map[3][239]=1; chan_map[3][239]=118; row_map[3][239]=6; col_map[3][239]=15;
  nstb_map[3][129]=1; chan_map[3][129]=120; row_map[3][129]=7; col_map[3][129]=0;
  nstb_map[3][137]=1; chan_map[3][137]=121; row_map[3][137]=7; col_map[3][137]=1;
  nstb_map[3][145]=1; chan_map[3][145]=122; row_map[3][145]=7; col_map[3][145]=2;
  nstb_map[3][153]=1; chan_map[3][153]=123; row_map[3][153]=7; col_map[3][153]=3;
  nstb_map[3][161]=1; chan_map[3][161]=124; row_map[3][161]=7; col_map[3][161]=4;
  nstb_map[3][169]=1; chan_map[3][169]=125; row_map[3][169]=7; col_map[3][169]=5;
  nstb_map[3][177]=1; chan_map[3][177]=126; row_map[3][177]=7; col_map[3][177]=6;
  nstb_map[3][185]=1; chan_map[3][185]=127; row_map[3][185]=7; col_map[3][185]=7;
  nstb_map[3][240]=1; chan_map[3][240]=128; row_map[3][240]=7; col_map[3][240]=8;
  nstb_map[3][241]=1; chan_map[3][241]=129; row_map[3][241]=7; col_map[3][241]=9;
  nstb_map[3][242]=1; chan_map[3][242]=130; row_map[3][242]=7; col_map[3][242]=10;
  nstb_map[3][243]=1; chan_map[3][243]=131; row_map[3][243]=7; col_map[3][243]=11;
  nstb_map[3][244]=1; chan_map[3][244]=132; row_map[3][244]=7; col_map[3][244]=12;
  nstb_map[3][245]=1; chan_map[3][245]=133; row_map[3][245]=7; col_map[3][245]=13;
  nstb_map[3][246]=1; chan_map[3][246]=134; row_map[3][246]=7; col_map[3][246]=14;
  nstb_map[3][247]=1; chan_map[3][247]=135; row_map[3][247]=7; col_map[3][247]=15;
  nstb_map[3][338]=1; chan_map[3][338]=136; row_map[3][338]=7; col_map[3][338]=16;
  nstb_map[3][128]=1; chan_map[3][128]=137; row_map[3][128]=8; col_map[3][128]=0;
  nstb_map[3][136]=1; chan_map[3][136]=138; row_map[3][136]=8; col_map[3][136]=1;
  nstb_map[3][144]=1; chan_map[3][144]=139; row_map[3][144]=8; col_map[3][144]=2;
  nstb_map[3][152]=1; chan_map[3][152]=140; row_map[3][152]=8; col_map[3][152]=3;
  nstb_map[3][160]=1; chan_map[3][160]=141; row_map[3][160]=8; col_map[3][160]=4;
  nstb_map[3][168]=1; chan_map[3][168]=142; row_map[3][168]=8; col_map[3][168]=5;
  nstb_map[3][176]=1; chan_map[3][176]=143; row_map[3][176]=8; col_map[3][176]=6;
  nstb_map[3][184]=1; chan_map[3][184]=144; row_map[3][184]=8; col_map[3][184]=7;
  nstb_map[3][248]=1; chan_map[3][248]=145; row_map[3][248]=8; col_map[3][248]=8;
  nstb_map[3][249]=1; chan_map[3][249]=146; row_map[3][249]=8; col_map[3][249]=9;
  nstb_map[3][250]=1; chan_map[3][250]=147; row_map[3][250]=8; col_map[3][250]=10;
  nstb_map[3][251]=1; chan_map[3][251]=148; row_map[3][251]=8; col_map[3][251]=11;
  nstb_map[3][252]=1; chan_map[3][252]=149; row_map[3][252]=8; col_map[3][252]=12;
  nstb_map[3][253]=1; chan_map[3][253]=150; row_map[3][253]=8; col_map[3][253]=13;
  nstb_map[3][254]=1; chan_map[3][254]=151; row_map[3][254]=8; col_map[3][254]=14;
  nstb_map[3][255]=1; chan_map[3][255]=152; row_map[3][255]=8; col_map[3][255]=15;
  nstb_map[3][337]=1; chan_map[3][337]=153; row_map[3][337]=8; col_map[3][337]=16;
  nstb_map[3][256]=1; chan_map[3][256]=162; row_map[3][256]=9; col_map[3][256]=8;
  nstb_map[3][257]=1; chan_map[3][257]=163; row_map[3][257]=9; col_map[3][257]=9;
  nstb_map[3][258]=1; chan_map[3][258]=164; row_map[3][258]=9; col_map[3][258]=10;
  nstb_map[3][259]=1; chan_map[3][259]=165; row_map[3][259]=9; col_map[3][259]=11;
  nstb_map[3][260]=1; chan_map[3][260]=166; row_map[3][260]=9; col_map[3][260]=12;
  nstb_map[3][261]=1; chan_map[3][261]=167; row_map[3][261]=9; col_map[3][261]=13;
  nstb_map[3][262]=1; chan_map[3][262]=168; row_map[3][262]=9; col_map[3][262]=14;
  nstb_map[3][263]=1; chan_map[3][263]=169; row_map[3][263]=9; col_map[3][263]=15;
  nstb_map[3][336]=1; chan_map[3][336]=170; row_map[3][336]=9; col_map[3][336]=16;
  nstb_map[3][264]=1; chan_map[3][264]=179; row_map[3][264]=10; col_map[3][264]=8;
  nstb_map[3][265]=1; chan_map[3][265]=180; row_map[3][265]=10; col_map[3][265]=9;
  nstb_map[3][266]=1; chan_map[3][266]=181; row_map[3][266]=10; col_map[3][266]=10;
  nstb_map[3][267]=1; chan_map[3][267]=182; row_map[3][267]=10; col_map[3][267]=11;
  nstb_map[3][268]=1; chan_map[3][268]=183; row_map[3][268]=10; col_map[3][268]=12;
  nstb_map[3][269]=1; chan_map[3][269]=184; row_map[3][269]=10; col_map[3][269]=13;
  nstb_map[3][270]=1; chan_map[3][270]=185; row_map[3][270]=10; col_map[3][270]=14;
  nstb_map[3][271]=1; chan_map[3][271]=186; row_map[3][271]=10; col_map[3][271]=15;
  nstb_map[3][320]=1; chan_map[3][320]=187; row_map[3][320]=10; col_map[3][320]=16;
  nstb_map[3][272]=1; chan_map[3][272]=196; row_map[3][272]=11; col_map[3][272]=8;
  nstb_map[3][273]=1; chan_map[3][273]=197; row_map[3][273]=11; col_map[3][273]=9;
  nstb_map[3][274]=1; chan_map[3][274]=198; row_map[3][274]=11; col_map[3][274]=10;
  nstb_map[3][275]=1; chan_map[3][275]=199; row_map[3][275]=11; col_map[3][275]=11;
  nstb_map[3][276]=1; chan_map[3][276]=200; row_map[3][276]=11; col_map[3][276]=12;
  nstb_map[3][277]=1; chan_map[3][277]=201; row_map[3][277]=11; col_map[3][277]=13;
  nstb_map[3][278]=1; chan_map[3][278]=202; row_map[3][278]=11; col_map[3][278]=14;
  nstb_map[3][279]=1; chan_map[3][279]=203; row_map[3][279]=11; col_map[3][279]=15;
  nstb_map[3][321]=1; chan_map[3][321]=204; row_map[3][321]=11; col_map[3][321]=16;
  nstb_map[3][280]=1; chan_map[3][280]=213; row_map[3][280]=12; col_map[3][280]=8;
  nstb_map[3][281]=1; chan_map[3][281]=214; row_map[3][281]=12; col_map[3][281]=9;
  nstb_map[3][282]=1; chan_map[3][282]=215; row_map[3][282]=12; col_map[3][282]=10;
  nstb_map[3][283]=1; chan_map[3][283]=216; row_map[3][283]=12; col_map[3][283]=11;
  nstb_map[3][284]=1; chan_map[3][284]=217; row_map[3][284]=12; col_map[3][284]=12;
  nstb_map[3][285]=1; chan_map[3][285]=218; row_map[3][285]=12; col_map[3][285]=13;
  nstb_map[3][286]=1; chan_map[3][286]=219; row_map[3][286]=12; col_map[3][286]=14;
  nstb_map[3][287]=1; chan_map[3][287]=220; row_map[3][287]=12; col_map[3][287]=15;
  nstb_map[3][322]=1; chan_map[3][322]=221; row_map[3][322]=12; col_map[3][322]=16;
  nstb_map[3][288]=1; chan_map[3][288]=230; row_map[3][288]=13; col_map[3][288]=8;
  nstb_map[3][289]=1; chan_map[3][289]=231; row_map[3][289]=13; col_map[3][289]=9;
  nstb_map[3][290]=1; chan_map[3][290]=232; row_map[3][290]=13; col_map[3][290]=10;
  nstb_map[3][291]=1; chan_map[3][291]=233; row_map[3][291]=13; col_map[3][291]=11;
  nstb_map[3][292]=1; chan_map[3][292]=234; row_map[3][292]=13; col_map[3][292]=12;
  nstb_map[3][293]=1; chan_map[3][293]=235; row_map[3][293]=13; col_map[3][293]=13;
  nstb_map[3][294]=1; chan_map[3][294]=236; row_map[3][294]=13; col_map[3][294]=14;
  nstb_map[3][295]=1; chan_map[3][295]=237; row_map[3][295]=13; col_map[3][295]=15;
  nstb_map[3][323]=1; chan_map[3][323]=238; row_map[3][323]=13; col_map[3][323]=16;
  nstb_map[3][296]=1; chan_map[3][296]=247; row_map[3][296]=14; col_map[3][296]=8;
  nstb_map[3][297]=1; chan_map[3][297]=248; row_map[3][297]=14; col_map[3][297]=9;
  nstb_map[3][298]=1; chan_map[3][298]=249; row_map[3][298]=14; col_map[3][298]=10;
  nstb_map[3][299]=1; chan_map[3][299]=250; row_map[3][299]=14; col_map[3][299]=11;
  nstb_map[3][300]=1; chan_map[3][300]=251; row_map[3][300]=14; col_map[3][300]=12;
  nstb_map[3][301]=1; chan_map[3][301]=252; row_map[3][301]=14; col_map[3][301]=13;
  nstb_map[3][302]=1; chan_map[3][302]=253; row_map[3][302]=14; col_map[3][302]=14;
  nstb_map[3][303]=1; chan_map[3][303]=254; row_map[3][303]=14; col_map[3][303]=15;
  nstb_map[3][325]=1; chan_map[3][325]=255; row_map[3][325]=14; col_map[3][325]=16;
  nstb_map[3][304]=1; chan_map[3][304]=264; row_map[3][304]=15; col_map[3][304]=8;
  nstb_map[3][305]=1; chan_map[3][305]=265; row_map[3][305]=15; col_map[3][305]=9;
  nstb_map[3][306]=1; chan_map[3][306]=266; row_map[3][306]=15; col_map[3][306]=10;
  nstb_map[3][307]=1; chan_map[3][307]=267; row_map[3][307]=15; col_map[3][307]=11;
  nstb_map[3][308]=1; chan_map[3][308]=268; row_map[3][308]=15; col_map[3][308]=12;
  nstb_map[3][309]=1; chan_map[3][309]=269; row_map[3][309]=15; col_map[3][309]=13;
  nstb_map[3][310]=1; chan_map[3][310]=270; row_map[3][310]=15; col_map[3][310]=14;
  nstb_map[3][311]=1; chan_map[3][311]=271; row_map[3][311]=15; col_map[3][311]=15;
  nstb_map[3][326]=1; chan_map[3][326]=272; row_map[3][326]=15; col_map[3][326]=16;
  nstb_map[3][312]=1; chan_map[3][312]=281; row_map[3][312]=16; col_map[3][312]=8;
  nstb_map[3][313]=1; chan_map[3][313]=282; row_map[3][313]=16; col_map[3][313]=9;
  nstb_map[3][314]=1; chan_map[3][314]=283; row_map[3][314]=16; col_map[3][314]=10;
  nstb_map[3][315]=1; chan_map[3][315]=284; row_map[3][315]=16; col_map[3][315]=11;
  nstb_map[3][316]=1; chan_map[3][316]=285; row_map[3][316]=16; col_map[3][316]=12;
  nstb_map[3][317]=1; chan_map[3][317]=286; row_map[3][317]=16; col_map[3][317]=13;
  nstb_map[3][318]=1; chan_map[3][318]=287; row_map[3][318]=16; col_map[3][318]=14;
  nstb_map[3][319]=1; chan_map[3][319]=288; row_map[3][319]=16; col_map[3][319]=15;
  nstb_map[3][324]=1; chan_map[3][324]=289; row_map[3][324]=16; col_map[3][324]=16;
  nstb_map[4][312]=1; chan_map[4][312]=298; row_map[4][312]=17; col_map[4][312]=8;
  nstb_map[4][313]=1; chan_map[4][313]=299; row_map[4][313]=17; col_map[4][313]=9;
  nstb_map[4][314]=1; chan_map[4][314]=300; row_map[4][314]=17; col_map[4][314]=10;
  nstb_map[4][315]=1; chan_map[4][315]=301; row_map[4][315]=17; col_map[4][315]=11;
  nstb_map[4][316]=1; chan_map[4][316]=302; row_map[4][316]=17; col_map[4][316]=12;
  nstb_map[4][317]=1; chan_map[4][317]=303; row_map[4][317]=17; col_map[4][317]=13;
  nstb_map[4][318]=1; chan_map[4][318]=304; row_map[4][318]=17; col_map[4][318]=14;
  nstb_map[4][319]=1; chan_map[4][319]=305; row_map[4][319]=17; col_map[4][319]=15;
  nstb_map[4][324]=1; chan_map[4][324]=306; row_map[4][324]=17; col_map[4][324]=16;
  nstb_map[4][304]=1; chan_map[4][304]=315; row_map[4][304]=18; col_map[4][304]=8;
  nstb_map[4][305]=1; chan_map[4][305]=316; row_map[4][305]=18; col_map[4][305]=9;
  nstb_map[4][306]=1; chan_map[4][306]=317; row_map[4][306]=18; col_map[4][306]=10;
  nstb_map[4][307]=1; chan_map[4][307]=318; row_map[4][307]=18; col_map[4][307]=11;
  nstb_map[4][308]=1; chan_map[4][308]=319; row_map[4][308]=18; col_map[4][308]=12;
  nstb_map[4][309]=1; chan_map[4][309]=320; row_map[4][309]=18; col_map[4][309]=13;
  nstb_map[4][310]=1; chan_map[4][310]=321; row_map[4][310]=18; col_map[4][310]=14;
  nstb_map[4][311]=1; chan_map[4][311]=322; row_map[4][311]=18; col_map[4][311]=15;
  nstb_map[4][326]=1; chan_map[4][326]=323; row_map[4][326]=18; col_map[4][326]=16;
  nstb_map[4][296]=1; chan_map[4][296]=332; row_map[4][296]=19; col_map[4][296]=8;
  nstb_map[4][297]=1; chan_map[4][297]=333; row_map[4][297]=19; col_map[4][297]=9;
  nstb_map[4][298]=1; chan_map[4][298]=334; row_map[4][298]=19; col_map[4][298]=10;
  nstb_map[4][299]=1; chan_map[4][299]=335; row_map[4][299]=19; col_map[4][299]=11;
  nstb_map[4][300]=1; chan_map[4][300]=336; row_map[4][300]=19; col_map[4][300]=12;
  nstb_map[4][301]=1; chan_map[4][301]=337; row_map[4][301]=19; col_map[4][301]=13;
  nstb_map[4][302]=1; chan_map[4][302]=338; row_map[4][302]=19; col_map[4][302]=14;
  nstb_map[4][303]=1; chan_map[4][303]=339; row_map[4][303]=19; col_map[4][303]=15;
  nstb_map[4][323]=1; chan_map[4][323]=340; row_map[4][323]=19; col_map[4][323]=16;
  nstb_map[4][288]=1; chan_map[4][288]=349; row_map[4][288]=20; col_map[4][288]=8;
  nstb_map[4][289]=1; chan_map[4][289]=350; row_map[4][289]=20; col_map[4][289]=9;
  nstb_map[4][290]=1; chan_map[4][290]=351; row_map[4][290]=20; col_map[4][290]=10;
  nstb_map[4][291]=1; chan_map[4][291]=352; row_map[4][291]=20; col_map[4][291]=11;
  nstb_map[4][292]=1; chan_map[4][292]=353; row_map[4][292]=20; col_map[4][292]=12;
  nstb_map[4][293]=1; chan_map[4][293]=354; row_map[4][293]=20; col_map[4][293]=13;
  nstb_map[4][294]=1; chan_map[4][294]=355; row_map[4][294]=20; col_map[4][294]=14;
  nstb_map[4][295]=1; chan_map[4][295]=356; row_map[4][295]=20; col_map[4][295]=15;
  nstb_map[4][325]=1; chan_map[4][325]=357; row_map[4][325]=20; col_map[4][325]=16;
  nstb_map[4][280]=1; chan_map[4][280]=366; row_map[4][280]=21; col_map[4][280]=8;
  nstb_map[4][281]=1; chan_map[4][281]=367; row_map[4][281]=21; col_map[4][281]=9;
  nstb_map[4][282]=1; chan_map[4][282]=368; row_map[4][282]=21; col_map[4][282]=10;
  nstb_map[4][283]=1; chan_map[4][283]=369; row_map[4][283]=21; col_map[4][283]=11;
  nstb_map[4][284]=1; chan_map[4][284]=370; row_map[4][284]=21; col_map[4][284]=12;
  nstb_map[4][285]=1; chan_map[4][285]=371; row_map[4][285]=21; col_map[4][285]=13;
  nstb_map[4][286]=1; chan_map[4][286]=372; row_map[4][286]=21; col_map[4][286]=14;
  nstb_map[4][287]=1; chan_map[4][287]=373; row_map[4][287]=21; col_map[4][287]=15;
  nstb_map[4][322]=1; chan_map[4][322]=374; row_map[4][322]=21; col_map[4][322]=16;
  nstb_map[4][272]=1; chan_map[4][272]=383; row_map[4][272]=22; col_map[4][272]=8;
  nstb_map[4][273]=1; chan_map[4][273]=384; row_map[4][273]=22; col_map[4][273]=9;
  nstb_map[4][274]=1; chan_map[4][274]=385; row_map[4][274]=22; col_map[4][274]=10;
  nstb_map[4][275]=1; chan_map[4][275]=386; row_map[4][275]=22; col_map[4][275]=11;
  nstb_map[4][276]=1; chan_map[4][276]=387; row_map[4][276]=22; col_map[4][276]=12;
  nstb_map[4][277]=1; chan_map[4][277]=388; row_map[4][277]=22; col_map[4][277]=13;
  nstb_map[4][278]=1; chan_map[4][278]=389; row_map[4][278]=22; col_map[4][278]=14;
  nstb_map[4][279]=1; chan_map[4][279]=390; row_map[4][279]=22; col_map[4][279]=15;
  nstb_map[4][321]=1; chan_map[4][321]=391; row_map[4][321]=22; col_map[4][321]=16;
  nstb_map[4][264]=1; chan_map[4][264]=400; row_map[4][264]=23; col_map[4][264]=8;
  nstb_map[4][265]=1; chan_map[4][265]=401; row_map[4][265]=23; col_map[4][265]=9;
  nstb_map[4][266]=1; chan_map[4][266]=402; row_map[4][266]=23; col_map[4][266]=10;
  nstb_map[4][267]=1; chan_map[4][267]=403; row_map[4][267]=23; col_map[4][267]=11;
  nstb_map[4][268]=1; chan_map[4][268]=404; row_map[4][268]=23; col_map[4][268]=12;
  nstb_map[4][269]=1; chan_map[4][269]=405; row_map[4][269]=23; col_map[4][269]=13;
  nstb_map[4][270]=1; chan_map[4][270]=406; row_map[4][270]=23; col_map[4][270]=14;
  nstb_map[4][271]=1; chan_map[4][271]=407; row_map[4][271]=23; col_map[4][271]=15;
  nstb_map[4][320]=1; chan_map[4][320]=408; row_map[4][320]=23; col_map[4][320]=16;
  nstb_map[4][256]=1; chan_map[4][256]=417; row_map[4][256]=24; col_map[4][256]=8;
  nstb_map[4][257]=1; chan_map[4][257]=418; row_map[4][257]=24; col_map[4][257]=9;
  nstb_map[4][258]=1; chan_map[4][258]=419; row_map[4][258]=24; col_map[4][258]=10;
  nstb_map[4][259]=1; chan_map[4][259]=420; row_map[4][259]=24; col_map[4][259]=11;
  nstb_map[4][260]=1; chan_map[4][260]=421; row_map[4][260]=24; col_map[4][260]=12;
  nstb_map[4][261]=1; chan_map[4][261]=422; row_map[4][261]=24; col_map[4][261]=13;
  nstb_map[4][262]=1; chan_map[4][262]=423; row_map[4][262]=24; col_map[4][262]=14;
  nstb_map[4][263]=1; chan_map[4][263]=424; row_map[4][263]=24; col_map[4][263]=15;
  nstb_map[4][336]=1; chan_map[4][336]=425; row_map[4][336]=24; col_map[4][336]=16;
  nstb_map[4][128]=1; chan_map[4][128]=426; row_map[4][128]=25; col_map[4][128]=0;
  nstb_map[4][136]=1; chan_map[4][136]=427; row_map[4][136]=25; col_map[4][136]=1;
  nstb_map[4][144]=1; chan_map[4][144]=428; row_map[4][144]=25; col_map[4][144]=2;
  nstb_map[4][152]=1; chan_map[4][152]=429; row_map[4][152]=25; col_map[4][152]=3;
  nstb_map[4][160]=1; chan_map[4][160]=430; row_map[4][160]=25; col_map[4][160]=4;
  nstb_map[4][168]=1; chan_map[4][168]=431; row_map[4][168]=25; col_map[4][168]=5;
  nstb_map[4][176]=1; chan_map[4][176]=432; row_map[4][176]=25; col_map[4][176]=6;
  nstb_map[4][184]=1; chan_map[4][184]=433; row_map[4][184]=25; col_map[4][184]=7;
  nstb_map[4][248]=1; chan_map[4][248]=434; row_map[4][248]=25; col_map[4][248]=8;
  nstb_map[4][249]=1; chan_map[4][249]=435; row_map[4][249]=25; col_map[4][249]=9;
  nstb_map[4][250]=1; chan_map[4][250]=436; row_map[4][250]=25; col_map[4][250]=10;
  nstb_map[4][251]=1; chan_map[4][251]=437; row_map[4][251]=25; col_map[4][251]=11;
  nstb_map[4][252]=1; chan_map[4][252]=438; row_map[4][252]=25; col_map[4][252]=12;
  nstb_map[4][253]=1; chan_map[4][253]=439; row_map[4][253]=25; col_map[4][253]=13;
  nstb_map[4][254]=1; chan_map[4][254]=440; row_map[4][254]=25; col_map[4][254]=14;
  nstb_map[4][255]=1; chan_map[4][255]=441; row_map[4][255]=25; col_map[4][255]=15;
  nstb_map[4][337]=1; chan_map[4][337]=442; row_map[4][337]=25; col_map[4][337]=16;
  nstb_map[4][129]=1; chan_map[4][129]=443; row_map[4][129]=26; col_map[4][129]=0;
  nstb_map[4][137]=1; chan_map[4][137]=444; row_map[4][137]=26; col_map[4][137]=1;
  nstb_map[4][145]=1; chan_map[4][145]=445; row_map[4][145]=26; col_map[4][145]=2;
  nstb_map[4][153]=1; chan_map[4][153]=446; row_map[4][153]=26; col_map[4][153]=3;
  nstb_map[4][161]=1; chan_map[4][161]=447; row_map[4][161]=26; col_map[4][161]=4;
  nstb_map[4][169]=1; chan_map[4][169]=448; row_map[4][169]=26; col_map[4][169]=5;
  nstb_map[4][177]=1; chan_map[4][177]=449; row_map[4][177]=26; col_map[4][177]=6;
  nstb_map[4][185]=1; chan_map[4][185]=450; row_map[4][185]=26; col_map[4][185]=7;
  nstb_map[4][240]=1; chan_map[4][240]=451; row_map[4][240]=26; col_map[4][240]=8;
  nstb_map[4][241]=1; chan_map[4][241]=452; row_map[4][241]=26; col_map[4][241]=9;
  nstb_map[4][242]=1; chan_map[4][242]=453; row_map[4][242]=26; col_map[4][242]=10;
  nstb_map[4][243]=1; chan_map[4][243]=454; row_map[4][243]=26; col_map[4][243]=11;
  nstb_map[4][244]=1; chan_map[4][244]=455; row_map[4][244]=26; col_map[4][244]=12;
  nstb_map[4][245]=1; chan_map[4][245]=456; row_map[4][245]=26; col_map[4][245]=13;
  nstb_map[4][246]=1; chan_map[4][246]=457; row_map[4][246]=26; col_map[4][246]=14;
  nstb_map[4][247]=1; chan_map[4][247]=458; row_map[4][247]=26; col_map[4][247]=15;
  nstb_map[4][338]=1; chan_map[4][338]=459; row_map[4][338]=26; col_map[4][338]=16;
  nstb_map[4][130]=1; chan_map[4][130]=460; row_map[4][130]=27; col_map[4][130]=0;
  nstb_map[4][138]=1; chan_map[4][138]=461; row_map[4][138]=27; col_map[4][138]=1;
  nstb_map[4][146]=1; chan_map[4][146]=462; row_map[4][146]=27; col_map[4][146]=2;
  nstb_map[4][154]=1; chan_map[4][154]=463; row_map[4][154]=27; col_map[4][154]=3;
  nstb_map[4][162]=1; chan_map[4][162]=464; row_map[4][162]=27; col_map[4][162]=4;
  nstb_map[4][170]=1; chan_map[4][170]=465; row_map[4][170]=27; col_map[4][170]=5;
  nstb_map[4][178]=1; chan_map[4][178]=466; row_map[4][178]=27; col_map[4][178]=6;
  nstb_map[4][186]=1; chan_map[4][186]=467; row_map[4][186]=27; col_map[4][186]=7;
  nstb_map[4][232]=1; chan_map[4][232]=468; row_map[4][232]=27; col_map[4][232]=8;
  nstb_map[4][233]=1; chan_map[4][233]=469; row_map[4][233]=27; col_map[4][233]=9;
  nstb_map[4][234]=1; chan_map[4][234]=470; row_map[4][234]=27; col_map[4][234]=10;
  nstb_map[4][235]=1; chan_map[4][235]=471; row_map[4][235]=27; col_map[4][235]=11;
  nstb_map[4][236]=1; chan_map[4][236]=472; row_map[4][236]=27; col_map[4][236]=12;
  nstb_map[4][237]=1; chan_map[4][237]=473; row_map[4][237]=27; col_map[4][237]=13;
  nstb_map[4][238]=1; chan_map[4][238]=474; row_map[4][238]=27; col_map[4][238]=14;
  nstb_map[4][239]=1; chan_map[4][239]=475; row_map[4][239]=27; col_map[4][239]=15;
  nstb_map[4][131]=1; chan_map[4][131]=477; row_map[4][131]=28; col_map[4][131]=0;
  nstb_map[4][139]=1; chan_map[4][139]=478; row_map[4][139]=28; col_map[4][139]=1;
  nstb_map[4][147]=1; chan_map[4][147]=479; row_map[4][147]=28; col_map[4][147]=2;
  nstb_map[4][155]=1; chan_map[4][155]=480; row_map[4][155]=28; col_map[4][155]=3;
  nstb_map[4][163]=1; chan_map[4][163]=481; row_map[4][163]=28; col_map[4][163]=4;
  nstb_map[4][171]=1; chan_map[4][171]=482; row_map[4][171]=28; col_map[4][171]=5;
  nstb_map[4][179]=1; chan_map[4][179]=483; row_map[4][179]=28; col_map[4][179]=6;
  nstb_map[4][187]=1; chan_map[4][187]=484; row_map[4][187]=28; col_map[4][187]=7;
  nstb_map[4][230]=1; chan_map[4][230]=485; row_map[4][230]=28; col_map[4][230]=8;
  nstb_map[4][229]=1; chan_map[4][229]=486; row_map[4][229]=28; col_map[4][229]=9;
  nstb_map[4][228]=1; chan_map[4][228]=487; row_map[4][228]=28; col_map[4][228]=10;
  nstb_map[4][227]=1; chan_map[4][227]=488; row_map[4][227]=28; col_map[4][227]=11;
  nstb_map[4][226]=1; chan_map[4][226]=489; row_map[4][226]=28; col_map[4][226]=12;
  nstb_map[4][225]=1; chan_map[4][225]=490; row_map[4][225]=28; col_map[4][225]=13;
  nstb_map[4][224]=1; chan_map[4][224]=491; row_map[4][224]=28; col_map[4][224]=14;
  nstb_map[4][132]=1; chan_map[4][132]=494; row_map[4][132]=29; col_map[4][132]=0;
  nstb_map[4][140]=1; chan_map[4][140]=495; row_map[4][140]=29; col_map[4][140]=1;
  nstb_map[4][148]=1; chan_map[4][148]=496; row_map[4][148]=29; col_map[4][148]=2;
  nstb_map[4][156]=1; chan_map[4][156]=497; row_map[4][156]=29; col_map[4][156]=3;
  nstb_map[4][164]=1; chan_map[4][164]=498; row_map[4][164]=29; col_map[4][164]=4;
  nstb_map[4][172]=1; chan_map[4][172]=499; row_map[4][172]=29; col_map[4][172]=5;
  nstb_map[4][180]=1; chan_map[4][180]=500; row_map[4][180]=29; col_map[4][180]=6;
  nstb_map[4][188]=1; chan_map[4][188]=501; row_map[4][188]=29; col_map[4][188]=7;
  nstb_map[4][219]=1; chan_map[4][219]=502; row_map[4][219]=29; col_map[4][219]=8;
  nstb_map[4][218]=1; chan_map[4][218]=503; row_map[4][218]=29; col_map[4][218]=9;
  nstb_map[4][217]=1; chan_map[4][217]=504; row_map[4][217]=29; col_map[4][217]=10;
  nstb_map[4][216]=1; chan_map[4][216]=505; row_map[4][216]=29; col_map[4][216]=11;
  nstb_map[4][340]=1; chan_map[4][340]=506; row_map[4][340]=29; col_map[4][340]=12;
  nstb_map[4][339]=1; chan_map[4][339]=507; row_map[4][339]=29; col_map[4][339]=13;
  nstb_map[4][133]=1; chan_map[4][133]=511; row_map[4][133]=30; col_map[4][133]=0;
  nstb_map[4][141]=1; chan_map[4][141]=512; row_map[4][141]=30; col_map[4][141]=1;
  nstb_map[4][149]=1; chan_map[4][149]=513; row_map[4][149]=30; col_map[4][149]=2;
  nstb_map[4][157]=1; chan_map[4][157]=514; row_map[4][157]=30; col_map[4][157]=3;
  nstb_map[4][165]=1; chan_map[4][165]=515; row_map[4][165]=30; col_map[4][165]=4;
  nstb_map[4][173]=1; chan_map[4][173]=516; row_map[4][173]=30; col_map[4][173]=5;
  nstb_map[4][181]=1; chan_map[4][181]=517; row_map[4][181]=30; col_map[4][181]=6;
  nstb_map[4][189]=1; chan_map[4][189]=518; row_map[4][189]=30; col_map[4][189]=7;
  nstb_map[4][211]=1; chan_map[4][211]=519; row_map[4][211]=30; col_map[4][211]=8;
  nstb_map[4][210]=1; chan_map[4][210]=520; row_map[4][210]=30; col_map[4][210]=9;
  nstb_map[4][209]=1; chan_map[4][209]=521; row_map[4][209]=30; col_map[4][209]=10;
  nstb_map[4][208]=1; chan_map[4][208]=522; row_map[4][208]=30; col_map[4][208]=11;
  nstb_map[4][341]=1; chan_map[4][341]=523; row_map[4][341]=30; col_map[4][341]=12;
  nstb_map[4][134]=1; chan_map[4][134]=528; row_map[4][134]=31; col_map[4][134]=0;
  nstb_map[4][142]=1; chan_map[4][142]=529; row_map[4][142]=31; col_map[4][142]=1;
  nstb_map[4][150]=1; chan_map[4][150]=530; row_map[4][150]=31; col_map[4][150]=2;
  nstb_map[4][158]=1; chan_map[4][158]=531; row_map[4][158]=31; col_map[4][158]=3;
  nstb_map[4][166]=1; chan_map[4][166]=532; row_map[4][166]=31; col_map[4][166]=4;
  nstb_map[4][174]=1; chan_map[4][174]=533; row_map[4][174]=31; col_map[4][174]=5;
  nstb_map[4][182]=1; chan_map[4][182]=534; row_map[4][182]=31; col_map[4][182]=6;
  nstb_map[4][190]=1; chan_map[4][190]=535; row_map[4][190]=31; col_map[4][190]=7;
  nstb_map[4][203]=1; chan_map[4][203]=536; row_map[4][203]=31; col_map[4][203]=8;
  nstb_map[4][202]=1; chan_map[4][202]=537; row_map[4][202]=31; col_map[4][202]=9;
  nstb_map[4][201]=1; chan_map[4][201]=538; row_map[4][201]=31; col_map[4][201]=10;
  nstb_map[4][200]=1; chan_map[4][200]=539; row_map[4][200]=31; col_map[4][200]=11;
  nstb_map[4][135]=1; chan_map[4][135]=545; row_map[4][135]=32; col_map[4][135]=0;
  nstb_map[4][143]=1; chan_map[4][143]=546; row_map[4][143]=32; col_map[4][143]=1;
  nstb_map[4][151]=1; chan_map[4][151]=547; row_map[4][151]=32; col_map[4][151]=2;
  nstb_map[4][159]=1; chan_map[4][159]=548; row_map[4][159]=32; col_map[4][159]=3;
  nstb_map[4][167]=1; chan_map[4][167]=549; row_map[4][167]=32; col_map[4][167]=4;
  nstb_map[4][175]=1; chan_map[4][175]=550; row_map[4][175]=32; col_map[4][175]=5;
  nstb_map[4][183]=1; chan_map[4][183]=551; row_map[4][183]=32; col_map[4][183]=6;
  nstb_map[4][191]=1; chan_map[4][191]=552; row_map[4][191]=32; col_map[4][191]=7;
  nstb_map[4][194]=1; chan_map[4][194]=553; row_map[4][194]=32; col_map[4][194]=8;
  nstb_map[4][193]=1; chan_map[4][193]=554; row_map[4][193]=32; col_map[4][193]=9;
  nstb_map[4][192]=1; chan_map[4][192]=555; row_map[4][192]=32; col_map[4][192]=10;
  nstb_map[4][342]=1; chan_map[4][342]=562; row_map[4][342]=33; col_map[4][342]=0;
  nstb_map[4][343]=1; chan_map[4][343]=563; row_map[4][343]=33; col_map[4][343]=1;
  nstb_map[4][328]=1; chan_map[4][328]=564; row_map[4][328]=33; col_map[4][328]=2;
  nstb_map[4][329]=1; chan_map[4][329]=565; row_map[4][329]=33; col_map[4][329]=3;
  nstb_map[4][330]=1; chan_map[4][330]=566; row_map[4][330]=33; col_map[4][330]=4;
  nstb_map[4][331]=1; chan_map[4][331]=567; row_map[4][331]=33; col_map[4][331]=5;
  nstb_map[4][332]=1; chan_map[4][332]=568; row_map[4][332]=33; col_map[4][332]=6;
  nstb_map[4][333]=1; chan_map[4][333]=569; row_map[4][333]=33; col_map[4][333]=7;
  nstb_map[4][334]=1; chan_map[4][334]=570; row_map[4][334]=33; col_map[4][334]=8;
  nstb_map[4][335]=1; chan_map[4][335]=571; row_map[4][335]=33; col_map[4][335]=9;
  nstb_map[1][342]=2; chan_map[1][342]=1; row_map[1][342]=0; col_map[1][342]=0;
  nstb_map[1][343]=2; chan_map[1][343]=2; row_map[1][343]=0; col_map[1][343]=1;
  nstb_map[1][328]=2; chan_map[1][328]=3; row_map[1][328]=0; col_map[1][328]=2;
  nstb_map[1][329]=2; chan_map[1][329]=4; row_map[1][329]=0; col_map[1][329]=3;
  nstb_map[1][330]=2; chan_map[1][330]=5; row_map[1][330]=0; col_map[1][330]=4;
  nstb_map[1][331]=2; chan_map[1][331]=6; row_map[1][331]=0; col_map[1][331]=5;
  nstb_map[1][332]=2; chan_map[1][332]=7; row_map[1][332]=0; col_map[1][332]=6;
  nstb_map[1][333]=2; chan_map[1][333]=8; row_map[1][333]=0; col_map[1][333]=7;
  nstb_map[1][334]=2; chan_map[1][334]=9; row_map[1][334]=0; col_map[1][334]=8;
  nstb_map[1][335]=2; chan_map[1][335]=10; row_map[1][335]=0; col_map[1][335]=9;
  nstb_map[1][135]=2; chan_map[1][135]=18; row_map[1][135]=1; col_map[1][135]=0;
  nstb_map[1][143]=2; chan_map[1][143]=19; row_map[1][143]=1; col_map[1][143]=1;
  nstb_map[1][151]=2; chan_map[1][151]=20; row_map[1][151]=1; col_map[1][151]=2;
  nstb_map[1][159]=2; chan_map[1][159]=21; row_map[1][159]=1; col_map[1][159]=3;
  nstb_map[1][167]=2; chan_map[1][167]=22; row_map[1][167]=1; col_map[1][167]=4;
  nstb_map[1][175]=2; chan_map[1][175]=23; row_map[1][175]=1; col_map[1][175]=5;
  nstb_map[1][183]=2; chan_map[1][183]=24; row_map[1][183]=1; col_map[1][183]=6;
  nstb_map[1][191]=2; chan_map[1][191]=25; row_map[1][191]=1; col_map[1][191]=7;
  nstb_map[1][194]=2; chan_map[1][194]=26; row_map[1][194]=1; col_map[1][194]=8;
  nstb_map[1][193]=2; chan_map[1][193]=27; row_map[1][193]=1; col_map[1][193]=9;
  nstb_map[1][192]=2; chan_map[1][192]=28; row_map[1][192]=1; col_map[1][192]=10;
  nstb_map[1][134]=2; chan_map[1][134]=35; row_map[1][134]=2; col_map[1][134]=0;
  nstb_map[1][142]=2; chan_map[1][142]=36; row_map[1][142]=2; col_map[1][142]=1;
  nstb_map[1][150]=2; chan_map[1][150]=37; row_map[1][150]=2; col_map[1][150]=2;
  nstb_map[1][158]=2; chan_map[1][158]=38; row_map[1][158]=2; col_map[1][158]=3;
  nstb_map[1][166]=2; chan_map[1][166]=39; row_map[1][166]=2; col_map[1][166]=4;
  nstb_map[1][174]=2; chan_map[1][174]=40; row_map[1][174]=2; col_map[1][174]=5;
  nstb_map[1][182]=2; chan_map[1][182]=41; row_map[1][182]=2; col_map[1][182]=6;
  nstb_map[1][190]=2; chan_map[1][190]=42; row_map[1][190]=2; col_map[1][190]=7;
  nstb_map[1][203]=2; chan_map[1][203]=43; row_map[1][203]=2; col_map[1][203]=8;
  nstb_map[1][202]=2; chan_map[1][202]=44; row_map[1][202]=2; col_map[1][202]=9;
  nstb_map[1][201]=2; chan_map[1][201]=45; row_map[1][201]=2; col_map[1][201]=10;
  nstb_map[1][200]=2; chan_map[1][200]=46; row_map[1][200]=2; col_map[1][200]=11;
  nstb_map[1][133]=2; chan_map[1][133]=52; row_map[1][133]=3; col_map[1][133]=0;
  nstb_map[1][141]=2; chan_map[1][141]=53; row_map[1][141]=3; col_map[1][141]=1;
  nstb_map[1][149]=2; chan_map[1][149]=54; row_map[1][149]=3; col_map[1][149]=2;
  nstb_map[1][157]=2; chan_map[1][157]=55; row_map[1][157]=3; col_map[1][157]=3;
  nstb_map[1][165]=2; chan_map[1][165]=56; row_map[1][165]=3; col_map[1][165]=4;
  nstb_map[1][173]=2; chan_map[1][173]=57; row_map[1][173]=3; col_map[1][173]=5;
  nstb_map[1][181]=2; chan_map[1][181]=58; row_map[1][181]=3; col_map[1][181]=6;
  nstb_map[1][189]=2; chan_map[1][189]=59; row_map[1][189]=3; col_map[1][189]=7;
  nstb_map[1][211]=2; chan_map[1][211]=60; row_map[1][211]=3; col_map[1][211]=8;
  nstb_map[1][210]=2; chan_map[1][210]=61; row_map[1][210]=3; col_map[1][210]=9;
  nstb_map[1][209]=2; chan_map[1][209]=62; row_map[1][209]=3; col_map[1][209]=10;
  nstb_map[1][208]=2; chan_map[1][208]=63; row_map[1][208]=3; col_map[1][208]=11;
  nstb_map[1][341]=2; chan_map[1][341]=64; row_map[1][341]=3; col_map[1][341]=12;
  nstb_map[1][132]=2; chan_map[1][132]=69; row_map[1][132]=4; col_map[1][132]=0;
  nstb_map[1][140]=2; chan_map[1][140]=70; row_map[1][140]=4; col_map[1][140]=1;
  nstb_map[1][148]=2; chan_map[1][148]=71; row_map[1][148]=4; col_map[1][148]=2;
  nstb_map[1][156]=2; chan_map[1][156]=72; row_map[1][156]=4; col_map[1][156]=3;
  nstb_map[1][164]=2; chan_map[1][164]=73; row_map[1][164]=4; col_map[1][164]=4;
  nstb_map[1][172]=2; chan_map[1][172]=74; row_map[1][172]=4; col_map[1][172]=5;
  nstb_map[1][180]=2; chan_map[1][180]=75; row_map[1][180]=4; col_map[1][180]=6;
  nstb_map[1][188]=2; chan_map[1][188]=76; row_map[1][188]=4; col_map[1][188]=7;
  nstb_map[1][219]=2; chan_map[1][219]=77; row_map[1][219]=4; col_map[1][219]=8;
  nstb_map[1][218]=2; chan_map[1][218]=78; row_map[1][218]=4; col_map[1][218]=9;
  nstb_map[1][217]=2; chan_map[1][217]=79; row_map[1][217]=4; col_map[1][217]=10;
  nstb_map[1][216]=2; chan_map[1][216]=80; row_map[1][216]=4; col_map[1][216]=11;
  nstb_map[1][340]=2; chan_map[1][340]=81; row_map[1][340]=4; col_map[1][340]=12;
  nstb_map[1][339]=2; chan_map[1][339]=82; row_map[1][339]=4; col_map[1][339]=13;
  nstb_map[1][131]=2; chan_map[1][131]=86; row_map[1][131]=5; col_map[1][131]=0;
  nstb_map[1][139]=2; chan_map[1][139]=87; row_map[1][139]=5; col_map[1][139]=1;
  nstb_map[1][147]=2; chan_map[1][147]=88; row_map[1][147]=5; col_map[1][147]=2;
  nstb_map[1][155]=2; chan_map[1][155]=89; row_map[1][155]=5; col_map[1][155]=3;
  nstb_map[1][163]=2; chan_map[1][163]=90; row_map[1][163]=5; col_map[1][163]=4;
  nstb_map[1][171]=2; chan_map[1][171]=91; row_map[1][171]=5; col_map[1][171]=5;
  nstb_map[1][179]=2; chan_map[1][179]=92; row_map[1][179]=5; col_map[1][179]=6;
  nstb_map[1][187]=2; chan_map[1][187]=93; row_map[1][187]=5; col_map[1][187]=7;
  nstb_map[1][230]=2; chan_map[1][230]=94; row_map[1][230]=5; col_map[1][230]=8;
  nstb_map[1][229]=2; chan_map[1][229]=95; row_map[1][229]=5; col_map[1][229]=9;
  nstb_map[1][228]=2; chan_map[1][228]=96; row_map[1][228]=5; col_map[1][228]=10;
  nstb_map[1][227]=2; chan_map[1][227]=97; row_map[1][227]=5; col_map[1][227]=11;
  nstb_map[1][226]=2; chan_map[1][226]=98; row_map[1][226]=5; col_map[1][226]=12;
  nstb_map[1][225]=2; chan_map[1][225]=99; row_map[1][225]=5; col_map[1][225]=13;
  nstb_map[1][224]=2; chan_map[1][224]=100; row_map[1][224]=5; col_map[1][224]=14;
  nstb_map[1][130]=2; chan_map[1][130]=103; row_map[1][130]=6; col_map[1][130]=0;
  nstb_map[1][138]=2; chan_map[1][138]=104; row_map[1][138]=6; col_map[1][138]=1;
  nstb_map[1][146]=2; chan_map[1][146]=105; row_map[1][146]=6; col_map[1][146]=2;
  nstb_map[1][154]=2; chan_map[1][154]=106; row_map[1][154]=6; col_map[1][154]=3;
  nstb_map[1][162]=2; chan_map[1][162]=107; row_map[1][162]=6; col_map[1][162]=4;
  nstb_map[1][170]=2; chan_map[1][170]=108; row_map[1][170]=6; col_map[1][170]=5;
  nstb_map[1][178]=2; chan_map[1][178]=109; row_map[1][178]=6; col_map[1][178]=6;
  nstb_map[1][186]=2; chan_map[1][186]=110; row_map[1][186]=6; col_map[1][186]=7;
  nstb_map[1][232]=2; chan_map[1][232]=111; row_map[1][232]=6; col_map[1][232]=8;
  nstb_map[1][233]=2; chan_map[1][233]=112; row_map[1][233]=6; col_map[1][233]=9;
  nstb_map[1][234]=2; chan_map[1][234]=113; row_map[1][234]=6; col_map[1][234]=10;
  nstb_map[1][235]=2; chan_map[1][235]=114; row_map[1][235]=6; col_map[1][235]=11;
  nstb_map[1][236]=2; chan_map[1][236]=115; row_map[1][236]=6; col_map[1][236]=12;
  nstb_map[1][237]=2; chan_map[1][237]=116; row_map[1][237]=6; col_map[1][237]=13;
  nstb_map[1][238]=2; chan_map[1][238]=117; row_map[1][238]=6; col_map[1][238]=14;
  nstb_map[1][239]=2; chan_map[1][239]=118; row_map[1][239]=6; col_map[1][239]=15;
  nstb_map[1][129]=2; chan_map[1][129]=120; row_map[1][129]=7; col_map[1][129]=0;
  nstb_map[1][137]=2; chan_map[1][137]=121; row_map[1][137]=7; col_map[1][137]=1;
  nstb_map[1][145]=2; chan_map[1][145]=122; row_map[1][145]=7; col_map[1][145]=2;
  nstb_map[1][153]=2; chan_map[1][153]=123; row_map[1][153]=7; col_map[1][153]=3;
  nstb_map[1][161]=2; chan_map[1][161]=124; row_map[1][161]=7; col_map[1][161]=4;
  nstb_map[1][169]=2; chan_map[1][169]=125; row_map[1][169]=7; col_map[1][169]=5;
  nstb_map[1][177]=2; chan_map[1][177]=126; row_map[1][177]=7; col_map[1][177]=6;
  nstb_map[1][185]=2; chan_map[1][185]=127; row_map[1][185]=7; col_map[1][185]=7;
  nstb_map[1][240]=2; chan_map[1][240]=128; row_map[1][240]=7; col_map[1][240]=8;
  nstb_map[1][241]=2; chan_map[1][241]=129; row_map[1][241]=7; col_map[1][241]=9;
  nstb_map[1][242]=2; chan_map[1][242]=130; row_map[1][242]=7; col_map[1][242]=10;
  nstb_map[1][243]=2; chan_map[1][243]=131; row_map[1][243]=7; col_map[1][243]=11;
  nstb_map[1][244]=2; chan_map[1][244]=132; row_map[1][244]=7; col_map[1][244]=12;
  nstb_map[1][245]=2; chan_map[1][245]=133; row_map[1][245]=7; col_map[1][245]=13;
  nstb_map[1][246]=2; chan_map[1][246]=134; row_map[1][246]=7; col_map[1][246]=14;
  nstb_map[1][247]=2; chan_map[1][247]=135; row_map[1][247]=7; col_map[1][247]=15;
  nstb_map[1][338]=2; chan_map[1][338]=136; row_map[1][338]=7; col_map[1][338]=16;
  nstb_map[1][128]=2; chan_map[1][128]=137; row_map[1][128]=8; col_map[1][128]=0;
  nstb_map[1][136]=2; chan_map[1][136]=138; row_map[1][136]=8; col_map[1][136]=1;
  nstb_map[1][144]=2; chan_map[1][144]=139; row_map[1][144]=8; col_map[1][144]=2;
  nstb_map[1][152]=2; chan_map[1][152]=140; row_map[1][152]=8; col_map[1][152]=3;
  nstb_map[1][160]=2; chan_map[1][160]=141; row_map[1][160]=8; col_map[1][160]=4;
  nstb_map[1][168]=2; chan_map[1][168]=142; row_map[1][168]=8; col_map[1][168]=5;
  nstb_map[1][176]=2; chan_map[1][176]=143; row_map[1][176]=8; col_map[1][176]=6;
  nstb_map[1][184]=2; chan_map[1][184]=144; row_map[1][184]=8; col_map[1][184]=7;
  nstb_map[1][248]=2; chan_map[1][248]=145; row_map[1][248]=8; col_map[1][248]=8;
  nstb_map[1][249]=2; chan_map[1][249]=146; row_map[1][249]=8; col_map[1][249]=9;
  nstb_map[1][250]=2; chan_map[1][250]=147; row_map[1][250]=8; col_map[1][250]=10;
  nstb_map[1][251]=2; chan_map[1][251]=148; row_map[1][251]=8; col_map[1][251]=11;
  nstb_map[1][252]=2; chan_map[1][252]=149; row_map[1][252]=8; col_map[1][252]=12;
  nstb_map[1][253]=2; chan_map[1][253]=150; row_map[1][253]=8; col_map[1][253]=13;
  nstb_map[1][254]=2; chan_map[1][254]=151; row_map[1][254]=8; col_map[1][254]=14;
  nstb_map[1][255]=2; chan_map[1][255]=152; row_map[1][255]=8; col_map[1][255]=15;
  nstb_map[1][337]=2; chan_map[1][337]=153; row_map[1][337]=8; col_map[1][337]=16;
  nstb_map[1][256]=2; chan_map[1][256]=162; row_map[1][256]=9; col_map[1][256]=8;
  nstb_map[1][257]=2; chan_map[1][257]=163; row_map[1][257]=9; col_map[1][257]=9;
  nstb_map[1][258]=2; chan_map[1][258]=164; row_map[1][258]=9; col_map[1][258]=10;
  nstb_map[1][259]=2; chan_map[1][259]=165; row_map[1][259]=9; col_map[1][259]=11;
  nstb_map[1][260]=2; chan_map[1][260]=166; row_map[1][260]=9; col_map[1][260]=12;
  nstb_map[1][261]=2; chan_map[1][261]=167; row_map[1][261]=9; col_map[1][261]=13;
  nstb_map[1][262]=2; chan_map[1][262]=168; row_map[1][262]=9; col_map[1][262]=14;
  nstb_map[1][263]=2; chan_map[1][263]=169; row_map[1][263]=9; col_map[1][263]=15;
  nstb_map[1][336]=2; chan_map[1][336]=170; row_map[1][336]=9; col_map[1][336]=16;
  nstb_map[1][264]=2; chan_map[1][264]=179; row_map[1][264]=10; col_map[1][264]=8;
  nstb_map[1][265]=2; chan_map[1][265]=180; row_map[1][265]=10; col_map[1][265]=9;
  nstb_map[1][266]=2; chan_map[1][266]=181; row_map[1][266]=10; col_map[1][266]=10;
  nstb_map[1][267]=2; chan_map[1][267]=182; row_map[1][267]=10; col_map[1][267]=11;
  nstb_map[1][268]=2; chan_map[1][268]=183; row_map[1][268]=10; col_map[1][268]=12;
  nstb_map[1][269]=2; chan_map[1][269]=184; row_map[1][269]=10; col_map[1][269]=13;
  nstb_map[1][270]=2; chan_map[1][270]=185; row_map[1][270]=10; col_map[1][270]=14;
  nstb_map[1][271]=2; chan_map[1][271]=186; row_map[1][271]=10; col_map[1][271]=15;
  nstb_map[1][320]=2; chan_map[1][320]=187; row_map[1][320]=10; col_map[1][320]=16;
  nstb_map[1][272]=2; chan_map[1][272]=196; row_map[1][272]=11; col_map[1][272]=8;
  nstb_map[1][273]=2; chan_map[1][273]=197; row_map[1][273]=11; col_map[1][273]=9;
  nstb_map[1][274]=2; chan_map[1][274]=198; row_map[1][274]=11; col_map[1][274]=10;
  nstb_map[1][275]=2; chan_map[1][275]=199; row_map[1][275]=11; col_map[1][275]=11;
  nstb_map[1][276]=2; chan_map[1][276]=200; row_map[1][276]=11; col_map[1][276]=12;
  nstb_map[1][277]=2; chan_map[1][277]=201; row_map[1][277]=11; col_map[1][277]=13;
  nstb_map[1][278]=2; chan_map[1][278]=202; row_map[1][278]=11; col_map[1][278]=14;
  nstb_map[1][279]=2; chan_map[1][279]=203; row_map[1][279]=11; col_map[1][279]=15;
  nstb_map[1][321]=2; chan_map[1][321]=204; row_map[1][321]=11; col_map[1][321]=16;
  nstb_map[1][280]=2; chan_map[1][280]=213; row_map[1][280]=12; col_map[1][280]=8;
  nstb_map[1][281]=2; chan_map[1][281]=214; row_map[1][281]=12; col_map[1][281]=9;
  nstb_map[1][282]=2; chan_map[1][282]=215; row_map[1][282]=12; col_map[1][282]=10;
  nstb_map[1][283]=2; chan_map[1][283]=216; row_map[1][283]=12; col_map[1][283]=11;
  nstb_map[1][284]=2; chan_map[1][284]=217; row_map[1][284]=12; col_map[1][284]=12;
  nstb_map[1][285]=2; chan_map[1][285]=218; row_map[1][285]=12; col_map[1][285]=13;
  nstb_map[1][286]=2; chan_map[1][286]=219; row_map[1][286]=12; col_map[1][286]=14;
  nstb_map[1][287]=2; chan_map[1][287]=220; row_map[1][287]=12; col_map[1][287]=15;
  nstb_map[1][322]=2; chan_map[1][322]=221; row_map[1][322]=12; col_map[1][322]=16;
  nstb_map[1][288]=2; chan_map[1][288]=230; row_map[1][288]=13; col_map[1][288]=8;
  nstb_map[1][289]=2; chan_map[1][289]=231; row_map[1][289]=13; col_map[1][289]=9;
  nstb_map[1][290]=2; chan_map[1][290]=232; row_map[1][290]=13; col_map[1][290]=10;
  nstb_map[1][291]=2; chan_map[1][291]=233; row_map[1][291]=13; col_map[1][291]=11;
  nstb_map[1][292]=2; chan_map[1][292]=234; row_map[1][292]=13; col_map[1][292]=12;
  nstb_map[1][293]=2; chan_map[1][293]=235; row_map[1][293]=13; col_map[1][293]=13;
  nstb_map[1][294]=2; chan_map[1][294]=236; row_map[1][294]=13; col_map[1][294]=14;
  nstb_map[1][295]=2; chan_map[1][295]=237; row_map[1][295]=13; col_map[1][295]=15;
  nstb_map[1][323]=2; chan_map[1][323]=238; row_map[1][323]=13; col_map[1][323]=16;
  nstb_map[1][296]=2; chan_map[1][296]=247; row_map[1][296]=14; col_map[1][296]=8;
  nstb_map[1][297]=2; chan_map[1][297]=248; row_map[1][297]=14; col_map[1][297]=9;
  nstb_map[1][298]=2; chan_map[1][298]=249; row_map[1][298]=14; col_map[1][298]=10;
  nstb_map[1][299]=2; chan_map[1][299]=250; row_map[1][299]=14; col_map[1][299]=11;
  nstb_map[1][300]=2; chan_map[1][300]=251; row_map[1][300]=14; col_map[1][300]=12;
  nstb_map[1][301]=2; chan_map[1][301]=252; row_map[1][301]=14; col_map[1][301]=13;
  nstb_map[1][302]=2; chan_map[1][302]=253; row_map[1][302]=14; col_map[1][302]=14;
  nstb_map[1][303]=2; chan_map[1][303]=254; row_map[1][303]=14; col_map[1][303]=15;
  nstb_map[1][325]=2; chan_map[1][325]=255; row_map[1][325]=14; col_map[1][325]=16;
  nstb_map[1][304]=2; chan_map[1][304]=264; row_map[1][304]=15; col_map[1][304]=8;
  nstb_map[1][305]=2; chan_map[1][305]=265; row_map[1][305]=15; col_map[1][305]=9;
  nstb_map[1][306]=2; chan_map[1][306]=266; row_map[1][306]=15; col_map[1][306]=10;
  nstb_map[1][307]=2; chan_map[1][307]=267; row_map[1][307]=15; col_map[1][307]=11;
  nstb_map[1][308]=2; chan_map[1][308]=268; row_map[1][308]=15; col_map[1][308]=12;
  nstb_map[1][309]=2; chan_map[1][309]=269; row_map[1][309]=15; col_map[1][309]=13;
  nstb_map[1][310]=2; chan_map[1][310]=270; row_map[1][310]=15; col_map[1][310]=14;
  nstb_map[1][311]=2; chan_map[1][311]=271; row_map[1][311]=15; col_map[1][311]=15;
  nstb_map[1][326]=2; chan_map[1][326]=272; row_map[1][326]=15; col_map[1][326]=16;
  nstb_map[1][312]=2; chan_map[1][312]=281; row_map[1][312]=16; col_map[1][312]=8;
  nstb_map[1][313]=2; chan_map[1][313]=282; row_map[1][313]=16; col_map[1][313]=9;
  nstb_map[1][314]=2; chan_map[1][314]=283; row_map[1][314]=16; col_map[1][314]=10;
  nstb_map[1][315]=2; chan_map[1][315]=284; row_map[1][315]=16; col_map[1][315]=11;
  nstb_map[1][316]=2; chan_map[1][316]=285; row_map[1][316]=16; col_map[1][316]=12;
  nstb_map[1][317]=2; chan_map[1][317]=286; row_map[1][317]=16; col_map[1][317]=13;
  nstb_map[1][318]=2; chan_map[1][318]=287; row_map[1][318]=16; col_map[1][318]=14;
  nstb_map[1][319]=2; chan_map[1][319]=288; row_map[1][319]=16; col_map[1][319]=15;
  nstb_map[1][324]=2; chan_map[1][324]=289; row_map[1][324]=16; col_map[1][324]=16;
  nstb_map[2][312]=2; chan_map[2][312]=298; row_map[2][312]=17; col_map[2][312]=8;
  nstb_map[2][313]=2; chan_map[2][313]=299; row_map[2][313]=17; col_map[2][313]=9;
  nstb_map[2][314]=2; chan_map[2][314]=300; row_map[2][314]=17; col_map[2][314]=10;
  nstb_map[2][315]=2; chan_map[2][315]=301; row_map[2][315]=17; col_map[2][315]=11;
  nstb_map[2][316]=2; chan_map[2][316]=302; row_map[2][316]=17; col_map[2][316]=12;
  nstb_map[2][317]=2; chan_map[2][317]=303; row_map[2][317]=17; col_map[2][317]=13;
  nstb_map[2][318]=2; chan_map[2][318]=304; row_map[2][318]=17; col_map[2][318]=14;
  nstb_map[2][319]=2; chan_map[2][319]=305; row_map[2][319]=17; col_map[2][319]=15;
  nstb_map[2][324]=2; chan_map[2][324]=306; row_map[2][324]=17; col_map[2][324]=16;
  nstb_map[2][304]=2; chan_map[2][304]=315; row_map[2][304]=18; col_map[2][304]=8;
  nstb_map[2][305]=2; chan_map[2][305]=316; row_map[2][305]=18; col_map[2][305]=9;
  nstb_map[2][306]=2; chan_map[2][306]=317; row_map[2][306]=18; col_map[2][306]=10;
  nstb_map[2][307]=2; chan_map[2][307]=318; row_map[2][307]=18; col_map[2][307]=11;
  nstb_map[2][308]=2; chan_map[2][308]=319; row_map[2][308]=18; col_map[2][308]=12;
  nstb_map[2][309]=2; chan_map[2][309]=320; row_map[2][309]=18; col_map[2][309]=13;
  nstb_map[2][310]=2; chan_map[2][310]=321; row_map[2][310]=18; col_map[2][310]=14;
  nstb_map[2][311]=2; chan_map[2][311]=322; row_map[2][311]=18; col_map[2][311]=15;
  nstb_map[2][326]=2; chan_map[2][326]=323; row_map[2][326]=18; col_map[2][326]=16;
  nstb_map[2][296]=2; chan_map[2][296]=332; row_map[2][296]=19; col_map[2][296]=8;
  nstb_map[2][297]=2; chan_map[2][297]=333; row_map[2][297]=19; col_map[2][297]=9;
  nstb_map[2][298]=2; chan_map[2][298]=334; row_map[2][298]=19; col_map[2][298]=10;
  nstb_map[2][299]=2; chan_map[2][299]=335; row_map[2][299]=19; col_map[2][299]=11;
  nstb_map[2][300]=2; chan_map[2][300]=336; row_map[2][300]=19; col_map[2][300]=12;
  nstb_map[2][301]=2; chan_map[2][301]=337; row_map[2][301]=19; col_map[2][301]=13;
  nstb_map[2][302]=2; chan_map[2][302]=338; row_map[2][302]=19; col_map[2][302]=14;
  nstb_map[2][303]=2; chan_map[2][303]=339; row_map[2][303]=19; col_map[2][303]=15;
  nstb_map[2][323]=2; chan_map[2][323]=340; row_map[2][323]=19; col_map[2][323]=16;
  nstb_map[2][288]=2; chan_map[2][288]=349; row_map[2][288]=20; col_map[2][288]=8;
  nstb_map[2][289]=2; chan_map[2][289]=350; row_map[2][289]=20; col_map[2][289]=9;
  nstb_map[2][290]=2; chan_map[2][290]=351; row_map[2][290]=20; col_map[2][290]=10;
  nstb_map[2][291]=2; chan_map[2][291]=352; row_map[2][291]=20; col_map[2][291]=11;
  nstb_map[2][292]=2; chan_map[2][292]=353; row_map[2][292]=20; col_map[2][292]=12;
  nstb_map[2][293]=2; chan_map[2][293]=354; row_map[2][293]=20; col_map[2][293]=13;
  nstb_map[2][294]=2; chan_map[2][294]=355; row_map[2][294]=20; col_map[2][294]=14;
  nstb_map[2][295]=2; chan_map[2][295]=356; row_map[2][295]=20; col_map[2][295]=15;
  nstb_map[2][325]=2; chan_map[2][325]=357; row_map[2][325]=20; col_map[2][325]=16;
  nstb_map[2][280]=2; chan_map[2][280]=366; row_map[2][280]=21; col_map[2][280]=8;
  nstb_map[2][281]=2; chan_map[2][281]=367; row_map[2][281]=21; col_map[2][281]=9;
  nstb_map[2][282]=2; chan_map[2][282]=368; row_map[2][282]=21; col_map[2][282]=10;
  nstb_map[2][283]=2; chan_map[2][283]=369; row_map[2][283]=21; col_map[2][283]=11;
  nstb_map[2][284]=2; chan_map[2][284]=370; row_map[2][284]=21; col_map[2][284]=12;
  nstb_map[2][285]=2; chan_map[2][285]=371; row_map[2][285]=21; col_map[2][285]=13;
  nstb_map[2][286]=2; chan_map[2][286]=372; row_map[2][286]=21; col_map[2][286]=14;
  nstb_map[2][287]=2; chan_map[2][287]=373; row_map[2][287]=21; col_map[2][287]=15;
  nstb_map[2][322]=2; chan_map[2][322]=374; row_map[2][322]=21; col_map[2][322]=16;
  nstb_map[2][272]=2; chan_map[2][272]=383; row_map[2][272]=22; col_map[2][272]=8;
  nstb_map[2][273]=2; chan_map[2][273]=384; row_map[2][273]=22; col_map[2][273]=9;
  nstb_map[2][274]=2; chan_map[2][274]=385; row_map[2][274]=22; col_map[2][274]=10;
  nstb_map[2][275]=2; chan_map[2][275]=386; row_map[2][275]=22; col_map[2][275]=11;
  nstb_map[2][276]=2; chan_map[2][276]=387; row_map[2][276]=22; col_map[2][276]=12;
  nstb_map[2][277]=2; chan_map[2][277]=388; row_map[2][277]=22; col_map[2][277]=13;
  nstb_map[2][278]=2; chan_map[2][278]=389; row_map[2][278]=22; col_map[2][278]=14;
  nstb_map[2][279]=2; chan_map[2][279]=390; row_map[2][279]=22; col_map[2][279]=15;
  nstb_map[2][321]=2; chan_map[2][321]=391; row_map[2][321]=22; col_map[2][321]=16;
  nstb_map[2][264]=2; chan_map[2][264]=400; row_map[2][264]=23; col_map[2][264]=8;
  nstb_map[2][265]=2; chan_map[2][265]=401; row_map[2][265]=23; col_map[2][265]=9;
  nstb_map[2][266]=2; chan_map[2][266]=402; row_map[2][266]=23; col_map[2][266]=10;
  nstb_map[2][267]=2; chan_map[2][267]=403; row_map[2][267]=23; col_map[2][267]=11;
  nstb_map[2][268]=2; chan_map[2][268]=404; row_map[2][268]=23; col_map[2][268]=12;
  nstb_map[2][269]=2; chan_map[2][269]=405; row_map[2][269]=23; col_map[2][269]=13;
  nstb_map[2][270]=2; chan_map[2][270]=406; row_map[2][270]=23; col_map[2][270]=14;
  nstb_map[2][271]=2; chan_map[2][271]=407; row_map[2][271]=23; col_map[2][271]=15;
  nstb_map[2][320]=2; chan_map[2][320]=408; row_map[2][320]=23; col_map[2][320]=16;
  nstb_map[2][256]=2; chan_map[2][256]=417; row_map[2][256]=24; col_map[2][256]=8;
  nstb_map[2][257]=2; chan_map[2][257]=418; row_map[2][257]=24; col_map[2][257]=9;
  nstb_map[2][258]=2; chan_map[2][258]=419; row_map[2][258]=24; col_map[2][258]=10;
  nstb_map[2][259]=2; chan_map[2][259]=420; row_map[2][259]=24; col_map[2][259]=11;
  nstb_map[2][260]=2; chan_map[2][260]=421; row_map[2][260]=24; col_map[2][260]=12;
  nstb_map[2][261]=2; chan_map[2][261]=422; row_map[2][261]=24; col_map[2][261]=13;
  nstb_map[2][262]=2; chan_map[2][262]=423; row_map[2][262]=24; col_map[2][262]=14;
  nstb_map[2][263]=2; chan_map[2][263]=424; row_map[2][263]=24; col_map[2][263]=15;
  nstb_map[2][336]=2; chan_map[2][336]=425; row_map[2][336]=24; col_map[2][336]=16;
  nstb_map[2][128]=2; chan_map[2][128]=426; row_map[2][128]=25; col_map[2][128]=0;
  nstb_map[2][136]=2; chan_map[2][136]=427; row_map[2][136]=25; col_map[2][136]=1;
  nstb_map[2][144]=2; chan_map[2][144]=428; row_map[2][144]=25; col_map[2][144]=2;
  nstb_map[2][152]=2; chan_map[2][152]=429; row_map[2][152]=25; col_map[2][152]=3;
  nstb_map[2][160]=2; chan_map[2][160]=430; row_map[2][160]=25; col_map[2][160]=4;
  nstb_map[2][168]=2; chan_map[2][168]=431; row_map[2][168]=25; col_map[2][168]=5;
  nstb_map[2][176]=2; chan_map[2][176]=432; row_map[2][176]=25; col_map[2][176]=6;
  nstb_map[2][184]=2; chan_map[2][184]=433; row_map[2][184]=25; col_map[2][184]=7;
  nstb_map[2][248]=2; chan_map[2][248]=434; row_map[2][248]=25; col_map[2][248]=8;
  nstb_map[2][249]=2; chan_map[2][249]=435; row_map[2][249]=25; col_map[2][249]=9;
  nstb_map[2][250]=2; chan_map[2][250]=436; row_map[2][250]=25; col_map[2][250]=10;
  nstb_map[2][251]=2; chan_map[2][251]=437; row_map[2][251]=25; col_map[2][251]=11;
  nstb_map[2][252]=2; chan_map[2][252]=438; row_map[2][252]=25; col_map[2][252]=12;
  nstb_map[2][253]=2; chan_map[2][253]=439; row_map[2][253]=25; col_map[2][253]=13;
  nstb_map[2][254]=2; chan_map[2][254]=440; row_map[2][254]=25; col_map[2][254]=14;
  nstb_map[2][255]=2; chan_map[2][255]=441; row_map[2][255]=25; col_map[2][255]=15;
  nstb_map[2][337]=2; chan_map[2][337]=442; row_map[2][337]=25; col_map[2][337]=16;
  nstb_map[2][129]=2; chan_map[2][129]=443; row_map[2][129]=26; col_map[2][129]=0;
  nstb_map[2][137]=2; chan_map[2][137]=444; row_map[2][137]=26; col_map[2][137]=1;
  nstb_map[2][145]=2; chan_map[2][145]=445; row_map[2][145]=26; col_map[2][145]=2;
  nstb_map[2][153]=2; chan_map[2][153]=446; row_map[2][153]=26; col_map[2][153]=3;
  nstb_map[2][161]=2; chan_map[2][161]=447; row_map[2][161]=26; col_map[2][161]=4;
  nstb_map[2][169]=2; chan_map[2][169]=448; row_map[2][169]=26; col_map[2][169]=5;
  nstb_map[2][177]=2; chan_map[2][177]=449; row_map[2][177]=26; col_map[2][177]=6;
  nstb_map[2][185]=2; chan_map[2][185]=450; row_map[2][185]=26; col_map[2][185]=7;
  nstb_map[2][240]=2; chan_map[2][240]=451; row_map[2][240]=26; col_map[2][240]=8;
  nstb_map[2][241]=2; chan_map[2][241]=452; row_map[2][241]=26; col_map[2][241]=9;
  nstb_map[2][242]=2; chan_map[2][242]=453; row_map[2][242]=26; col_map[2][242]=10;
  nstb_map[2][243]=2; chan_map[2][243]=454; row_map[2][243]=26; col_map[2][243]=11;
  nstb_map[2][244]=2; chan_map[2][244]=455; row_map[2][244]=26; col_map[2][244]=12;
  nstb_map[2][245]=2; chan_map[2][245]=456; row_map[2][245]=26; col_map[2][245]=13;
  nstb_map[2][246]=2; chan_map[2][246]=457; row_map[2][246]=26; col_map[2][246]=14;
  nstb_map[2][247]=2; chan_map[2][247]=458; row_map[2][247]=26; col_map[2][247]=15;
  nstb_map[2][338]=2; chan_map[2][338]=459; row_map[2][338]=26; col_map[2][338]=16;
  nstb_map[2][130]=2; chan_map[2][130]=460; row_map[2][130]=27; col_map[2][130]=0;
  nstb_map[2][138]=2; chan_map[2][138]=461; row_map[2][138]=27; col_map[2][138]=1;
  nstb_map[2][146]=2; chan_map[2][146]=462; row_map[2][146]=27; col_map[2][146]=2;
  nstb_map[2][154]=2; chan_map[2][154]=463; row_map[2][154]=27; col_map[2][154]=3;
  nstb_map[2][162]=2; chan_map[2][162]=464; row_map[2][162]=27; col_map[2][162]=4;
  nstb_map[2][170]=2; chan_map[2][170]=465; row_map[2][170]=27; col_map[2][170]=5;
  nstb_map[2][178]=2; chan_map[2][178]=466; row_map[2][178]=27; col_map[2][178]=6;
  nstb_map[2][186]=2; chan_map[2][186]=467; row_map[2][186]=27; col_map[2][186]=7;
  nstb_map[2][232]=2; chan_map[2][232]=468; row_map[2][232]=27; col_map[2][232]=8;
  nstb_map[2][233]=2; chan_map[2][233]=469; row_map[2][233]=27; col_map[2][233]=9;
  nstb_map[2][234]=2; chan_map[2][234]=470; row_map[2][234]=27; col_map[2][234]=10;
  nstb_map[2][235]=2; chan_map[2][235]=471; row_map[2][235]=27; col_map[2][235]=11;
  nstb_map[2][236]=2; chan_map[2][236]=472; row_map[2][236]=27; col_map[2][236]=12;
  nstb_map[2][237]=2; chan_map[2][237]=473; row_map[2][237]=27; col_map[2][237]=13;
  nstb_map[2][238]=2; chan_map[2][238]=474; row_map[2][238]=27; col_map[2][238]=14;
  nstb_map[2][239]=2; chan_map[2][239]=475; row_map[2][239]=27; col_map[2][239]=15;
  nstb_map[2][131]=2; chan_map[2][131]=477; row_map[2][131]=28; col_map[2][131]=0;
  nstb_map[2][139]=2; chan_map[2][139]=478; row_map[2][139]=28; col_map[2][139]=1;
  nstb_map[2][147]=2; chan_map[2][147]=479; row_map[2][147]=28; col_map[2][147]=2;
  nstb_map[2][155]=2; chan_map[2][155]=480; row_map[2][155]=28; col_map[2][155]=3;
  nstb_map[2][163]=2; chan_map[2][163]=481; row_map[2][163]=28; col_map[2][163]=4;
  nstb_map[2][171]=2; chan_map[2][171]=482; row_map[2][171]=28; col_map[2][171]=5;
  nstb_map[2][179]=2; chan_map[2][179]=483; row_map[2][179]=28; col_map[2][179]=6;
  nstb_map[2][187]=2; chan_map[2][187]=484; row_map[2][187]=28; col_map[2][187]=7;
  nstb_map[2][230]=2; chan_map[2][230]=485; row_map[2][230]=28; col_map[2][230]=8;
  nstb_map[2][229]=2; chan_map[2][229]=486; row_map[2][229]=28; col_map[2][229]=9;
  nstb_map[2][228]=2; chan_map[2][228]=487; row_map[2][228]=28; col_map[2][228]=10;
  nstb_map[2][227]=2; chan_map[2][227]=488; row_map[2][227]=28; col_map[2][227]=11;
  nstb_map[2][226]=2; chan_map[2][226]=489; row_map[2][226]=28; col_map[2][226]=12;
  nstb_map[2][225]=2; chan_map[2][225]=490; row_map[2][225]=28; col_map[2][225]=13;
  nstb_map[2][224]=2; chan_map[2][224]=491; row_map[2][224]=28; col_map[2][224]=14;
  nstb_map[2][132]=2; chan_map[2][132]=494; row_map[2][132]=29; col_map[2][132]=0;
  nstb_map[2][140]=2; chan_map[2][140]=495; row_map[2][140]=29; col_map[2][140]=1;
  nstb_map[2][148]=2; chan_map[2][148]=496; row_map[2][148]=29; col_map[2][148]=2;
  nstb_map[2][156]=2; chan_map[2][156]=497; row_map[2][156]=29; col_map[2][156]=3;
  nstb_map[2][164]=2; chan_map[2][164]=498; row_map[2][164]=29; col_map[2][164]=4;
  nstb_map[2][172]=2; chan_map[2][172]=499; row_map[2][172]=29; col_map[2][172]=5;
  nstb_map[2][180]=2; chan_map[2][180]=500; row_map[2][180]=29; col_map[2][180]=6;
  nstb_map[2][188]=2; chan_map[2][188]=501; row_map[2][188]=29; col_map[2][188]=7;
  nstb_map[2][219]=2; chan_map[2][219]=502; row_map[2][219]=29; col_map[2][219]=8;
  nstb_map[2][218]=2; chan_map[2][218]=503; row_map[2][218]=29; col_map[2][218]=9;
  nstb_map[2][217]=2; chan_map[2][217]=504; row_map[2][217]=29; col_map[2][217]=10;
  nstb_map[2][216]=2; chan_map[2][216]=505; row_map[2][216]=29; col_map[2][216]=11;
  nstb_map[2][340]=2; chan_map[2][340]=506; row_map[2][340]=29; col_map[2][340]=12;
  nstb_map[2][339]=2; chan_map[2][339]=507; row_map[2][339]=29; col_map[2][339]=13;
  nstb_map[2][133]=2; chan_map[2][133]=511; row_map[2][133]=30; col_map[2][133]=0;
  nstb_map[2][141]=2; chan_map[2][141]=512; row_map[2][141]=30; col_map[2][141]=1;
  nstb_map[2][149]=2; chan_map[2][149]=513; row_map[2][149]=30; col_map[2][149]=2;
  nstb_map[2][157]=2; chan_map[2][157]=514; row_map[2][157]=30; col_map[2][157]=3;
  nstb_map[2][165]=2; chan_map[2][165]=515; row_map[2][165]=30; col_map[2][165]=4;
  nstb_map[2][173]=2; chan_map[2][173]=516; row_map[2][173]=30; col_map[2][173]=5;
  nstb_map[2][181]=2; chan_map[2][181]=517; row_map[2][181]=30; col_map[2][181]=6;
  nstb_map[2][189]=2; chan_map[2][189]=518; row_map[2][189]=30; col_map[2][189]=7;
  nstb_map[2][211]=2; chan_map[2][211]=519; row_map[2][211]=30; col_map[2][211]=8;
  nstb_map[2][210]=2; chan_map[2][210]=520; row_map[2][210]=30; col_map[2][210]=9;
  nstb_map[2][209]=2; chan_map[2][209]=521; row_map[2][209]=30; col_map[2][209]=10;
  nstb_map[2][208]=2; chan_map[2][208]=522; row_map[2][208]=30; col_map[2][208]=11;
  nstb_map[2][341]=2; chan_map[2][341]=523; row_map[2][341]=30; col_map[2][341]=12;
  nstb_map[2][134]=2; chan_map[2][134]=528; row_map[2][134]=31; col_map[2][134]=0;
  nstb_map[2][142]=2; chan_map[2][142]=529; row_map[2][142]=31; col_map[2][142]=1;
  nstb_map[2][150]=2; chan_map[2][150]=530; row_map[2][150]=31; col_map[2][150]=2;
  nstb_map[2][158]=2; chan_map[2][158]=531; row_map[2][158]=31; col_map[2][158]=3;
  nstb_map[2][166]=2; chan_map[2][166]=532; row_map[2][166]=31; col_map[2][166]=4;
  nstb_map[2][174]=2; chan_map[2][174]=533; row_map[2][174]=31; col_map[2][174]=5;
  nstb_map[2][182]=2; chan_map[2][182]=534; row_map[2][182]=31; col_map[2][182]=6;
  nstb_map[2][190]=2; chan_map[2][190]=535; row_map[2][190]=31; col_map[2][190]=7;
  nstb_map[2][203]=2; chan_map[2][203]=536; row_map[2][203]=31; col_map[2][203]=8;
  nstb_map[2][202]=2; chan_map[2][202]=537; row_map[2][202]=31; col_map[2][202]=9;
  nstb_map[2][201]=2; chan_map[2][201]=538; row_map[2][201]=31; col_map[2][201]=10;
  nstb_map[2][200]=2; chan_map[2][200]=539; row_map[2][200]=31; col_map[2][200]=11;
  nstb_map[2][135]=2; chan_map[2][135]=545; row_map[2][135]=32; col_map[2][135]=0;
  nstb_map[2][143]=2; chan_map[2][143]=546; row_map[2][143]=32; col_map[2][143]=1;
  nstb_map[2][151]=2; chan_map[2][151]=547; row_map[2][151]=32; col_map[2][151]=2;
  nstb_map[2][159]=2; chan_map[2][159]=548; row_map[2][159]=32; col_map[2][159]=3;
  nstb_map[2][167]=2; chan_map[2][167]=549; row_map[2][167]=32; col_map[2][167]=4;
  nstb_map[2][175]=2; chan_map[2][175]=550; row_map[2][175]=32; col_map[2][175]=5;
  nstb_map[2][183]=2; chan_map[2][183]=551; row_map[2][183]=32; col_map[2][183]=6;
  nstb_map[2][191]=2; chan_map[2][191]=552; row_map[2][191]=32; col_map[2][191]=7;
  nstb_map[2][194]=2; chan_map[2][194]=553; row_map[2][194]=32; col_map[2][194]=8;
  nstb_map[2][193]=2; chan_map[2][193]=554; row_map[2][193]=32; col_map[2][193]=9;
  nstb_map[2][192]=2; chan_map[2][192]=555; row_map[2][192]=32; col_map[2][192]=10;
  nstb_map[2][342]=2; chan_map[2][342]=562; row_map[2][342]=33; col_map[2][342]=0;
  nstb_map[2][343]=2; chan_map[2][343]=563; row_map[2][343]=33; col_map[2][343]=1;
  nstb_map[2][328]=2; chan_map[2][328]=564; row_map[2][328]=33; col_map[2][328]=2;
  nstb_map[2][329]=2; chan_map[2][329]=565; row_map[2][329]=33; col_map[2][329]=3;
  nstb_map[2][330]=2; chan_map[2][330]=566; row_map[2][330]=33; col_map[2][330]=4;
  nstb_map[2][331]=2; chan_map[2][331]=567; row_map[2][331]=33; col_map[2][331]=5;
  nstb_map[2][332]=2; chan_map[2][332]=568; row_map[2][332]=33; col_map[2][332]=6;
  nstb_map[2][333]=2; chan_map[2][333]=569; row_map[2][333]=33; col_map[2][333]=7;
  nstb_map[2][334]=2; chan_map[2][334]=570; row_map[2][334]=33; col_map[2][334]=8;
  nstb_map[2][335]=2; chan_map[2][335]=571; row_map[2][335]=33; col_map[2][335]=9;
  nstb_map[3][6]=3; chan_map[3][6]=1; row_map[3][6]=0; col_map[3][6]=0;
  nstb_map[3][14]=3; chan_map[3][14]=2; row_map[3][14]=0; col_map[3][14]=1;
  nstb_map[3][22]=3; chan_map[3][22]=3; row_map[3][22]=0; col_map[3][22]=2;
  nstb_map[3][30]=3; chan_map[3][30]=4; row_map[3][30]=0; col_map[3][30]=3;
  nstb_map[3][32]=3; chan_map[3][32]=5; row_map[3][32]=0; col_map[3][32]=4;
  nstb_map[3][33]=3; chan_map[3][33]=6; row_map[3][33]=0; col_map[3][33]=5;
  nstb_map[3][34]=3; chan_map[3][34]=7; row_map[3][34]=0; col_map[3][34]=6;
  nstb_map[3][35]=3; chan_map[3][35]=8; row_map[3][35]=0; col_map[3][35]=7;
  nstb_map[3][36]=3; chan_map[3][36]=9; row_map[3][36]=0; col_map[3][36]=8;
  nstb_map[3][37]=3; chan_map[3][37]=10; row_map[3][37]=0; col_map[3][37]=9;
  nstb_map[3][38]=3; chan_map[3][38]=11; row_map[3][38]=0; col_map[3][38]=10;
  nstb_map[3][39]=3; chan_map[3][39]=12; row_map[3][39]=0; col_map[3][39]=11;
  nstb_map[3][5]=3; chan_map[3][5]=13; row_map[3][5]=1; col_map[3][5]=0;
  nstb_map[3][13]=3; chan_map[3][13]=14; row_map[3][13]=1; col_map[3][13]=1;
  nstb_map[3][21]=3; chan_map[3][21]=15; row_map[3][21]=1; col_map[3][21]=2;
  nstb_map[3][29]=3; chan_map[3][29]=16; row_map[3][29]=1; col_map[3][29]=3;
  nstb_map[3][40]=3; chan_map[3][40]=17; row_map[3][40]=1; col_map[3][40]=4;
  nstb_map[3][41]=3; chan_map[3][41]=18; row_map[3][41]=1; col_map[3][41]=5;
  nstb_map[3][42]=3; chan_map[3][42]=19; row_map[3][42]=1; col_map[3][42]=6;
  nstb_map[3][43]=3; chan_map[3][43]=20; row_map[3][43]=1; col_map[3][43]=7;
  nstb_map[3][44]=3; chan_map[3][44]=21; row_map[3][44]=1; col_map[3][44]=8;
  nstb_map[3][45]=3; chan_map[3][45]=22; row_map[3][45]=1; col_map[3][45]=9;
  nstb_map[3][46]=3; chan_map[3][46]=23; row_map[3][46]=1; col_map[3][46]=10;
  nstb_map[3][47]=3; chan_map[3][47]=24; row_map[3][47]=1; col_map[3][47]=11;
  nstb_map[3][4]=3; chan_map[3][4]=25; row_map[3][4]=2; col_map[3][4]=0;
  nstb_map[3][12]=3; chan_map[3][12]=26; row_map[3][12]=2; col_map[3][12]=1;
  nstb_map[3][20]=3; chan_map[3][20]=27; row_map[3][20]=2; col_map[3][20]=2;
  nstb_map[3][28]=3; chan_map[3][28]=28; row_map[3][28]=2; col_map[3][28]=3;
  nstb_map[3][48]=3; chan_map[3][48]=29; row_map[3][48]=2; col_map[3][48]=4;
  nstb_map[3][49]=3; chan_map[3][49]=30; row_map[3][49]=2; col_map[3][49]=5;
  nstb_map[3][50]=3; chan_map[3][50]=31; row_map[3][50]=2; col_map[3][50]=6;
  nstb_map[3][51]=3; chan_map[3][51]=32; row_map[3][51]=2; col_map[3][51]=7;
  nstb_map[3][52]=3; chan_map[3][52]=33; row_map[3][52]=2; col_map[3][52]=8;
  nstb_map[3][53]=3; chan_map[3][53]=34; row_map[3][53]=2; col_map[3][53]=9;
  nstb_map[3][54]=3; chan_map[3][54]=35; row_map[3][54]=2; col_map[3][54]=10;
  nstb_map[3][55]=3; chan_map[3][55]=36; row_map[3][55]=2; col_map[3][55]=11;
  nstb_map[3][3]=3; chan_map[3][3]=37; row_map[3][3]=3; col_map[3][3]=0;
  nstb_map[3][11]=3; chan_map[3][11]=38; row_map[3][11]=3; col_map[3][11]=1;
  nstb_map[3][19]=3; chan_map[3][19]=39; row_map[3][19]=3; col_map[3][19]=2;
  nstb_map[3][27]=3; chan_map[3][27]=40; row_map[3][27]=3; col_map[3][27]=3;
  nstb_map[3][56]=3; chan_map[3][56]=41; row_map[3][56]=3; col_map[3][56]=4;
  nstb_map[3][57]=3; chan_map[3][57]=42; row_map[3][57]=3; col_map[3][57]=5;
  nstb_map[3][58]=3; chan_map[3][58]=43; row_map[3][58]=3; col_map[3][58]=6;
  nstb_map[3][59]=3; chan_map[3][59]=44; row_map[3][59]=3; col_map[3][59]=7;
  nstb_map[3][60]=3; chan_map[3][60]=45; row_map[3][60]=3; col_map[3][60]=8;
  nstb_map[3][61]=3; chan_map[3][61]=46; row_map[3][61]=3; col_map[3][61]=9;
  nstb_map[3][62]=3; chan_map[3][62]=47; row_map[3][62]=3; col_map[3][62]=10;
  nstb_map[3][63]=3; chan_map[3][63]=48; row_map[3][63]=3; col_map[3][63]=11;
  nstb_map[3][2]=3; chan_map[3][2]=49; row_map[3][2]=4; col_map[3][2]=0;
  nstb_map[3][10]=3; chan_map[3][10]=50; row_map[3][10]=4; col_map[3][10]=1;
  nstb_map[3][18]=3; chan_map[3][18]=51; row_map[3][18]=4; col_map[3][18]=2;
  nstb_map[3][26]=3; chan_map[3][26]=52; row_map[3][26]=4; col_map[3][26]=3;
  nstb_map[3][64]=3; chan_map[3][64]=53; row_map[3][64]=4; col_map[3][64]=4;
  nstb_map[3][65]=3; chan_map[3][65]=54; row_map[3][65]=4; col_map[3][65]=5;
  nstb_map[3][66]=3; chan_map[3][66]=55; row_map[3][66]=4; col_map[3][66]=6;
  nstb_map[3][67]=3; chan_map[3][67]=56; row_map[3][67]=4; col_map[3][67]=7;
  nstb_map[3][68]=3; chan_map[3][68]=57; row_map[3][68]=4; col_map[3][68]=8;
  nstb_map[3][69]=3; chan_map[3][69]=58; row_map[3][69]=4; col_map[3][69]=9;
  nstb_map[3][70]=3; chan_map[3][70]=59; row_map[3][70]=4; col_map[3][70]=10;
  nstb_map[3][71]=3; chan_map[3][71]=60; row_map[3][71]=4; col_map[3][71]=11;
  nstb_map[3][1]=3; chan_map[3][1]=61; row_map[3][1]=5; col_map[3][1]=0;
  nstb_map[3][9]=3; chan_map[3][9]=62; row_map[3][9]=5; col_map[3][9]=1;
  nstb_map[3][17]=3; chan_map[3][17]=63; row_map[3][17]=5; col_map[3][17]=2;
  nstb_map[3][25]=3; chan_map[3][25]=64; row_map[3][25]=5; col_map[3][25]=3;
  nstb_map[3][72]=3; chan_map[3][72]=65; row_map[3][72]=5; col_map[3][72]=4;
  nstb_map[3][73]=3; chan_map[3][73]=66; row_map[3][73]=5; col_map[3][73]=5;
  nstb_map[3][74]=3; chan_map[3][74]=67; row_map[3][74]=5; col_map[3][74]=6;
  nstb_map[3][75]=3; chan_map[3][75]=68; row_map[3][75]=5; col_map[3][75]=7;
  nstb_map[3][76]=3; chan_map[3][76]=69; row_map[3][76]=5; col_map[3][76]=8;
  nstb_map[3][77]=3; chan_map[3][77]=70; row_map[3][77]=5; col_map[3][77]=9;
  nstb_map[3][78]=3; chan_map[3][78]=71; row_map[3][78]=5; col_map[3][78]=10;
  nstb_map[3][79]=3; chan_map[3][79]=72; row_map[3][79]=5; col_map[3][79]=11;
  nstb_map[3][0]=3; chan_map[3][0]=73; row_map[3][0]=6; col_map[3][0]=0;
  nstb_map[3][8]=3; chan_map[3][8]=74; row_map[3][8]=6; col_map[3][8]=1;
  nstb_map[3][16]=3; chan_map[3][16]=75; row_map[3][16]=6; col_map[3][16]=2;
  nstb_map[3][24]=3; chan_map[3][24]=76; row_map[3][24]=6; col_map[3][24]=3;
  nstb_map[3][80]=3; chan_map[3][80]=77; row_map[3][80]=6; col_map[3][80]=4;
  nstb_map[3][81]=3; chan_map[3][81]=78; row_map[3][81]=6; col_map[3][81]=5;
  nstb_map[3][82]=3; chan_map[3][82]=79; row_map[3][82]=6; col_map[3][82]=6;
  nstb_map[3][83]=3; chan_map[3][83]=80; row_map[3][83]=6; col_map[3][83]=7;
  nstb_map[3][84]=3; chan_map[3][84]=81; row_map[3][84]=6; col_map[3][84]=8;
  nstb_map[3][85]=3; chan_map[3][85]=82; row_map[3][85]=6; col_map[3][85]=9;
  nstb_map[3][86]=3; chan_map[3][86]=83; row_map[3][86]=6; col_map[3][86]=10;
  nstb_map[3][87]=3; chan_map[3][87]=84; row_map[3][87]=6; col_map[3][87]=11;
  nstb_map[3][89]=3; chan_map[3][89]=90; row_map[3][89]=7; col_map[3][89]=5;
  nstb_map[3][90]=3; chan_map[3][90]=91; row_map[3][90]=7; col_map[3][90]=6;
  nstb_map[3][91]=3; chan_map[3][91]=92; row_map[3][91]=7; col_map[3][91]=7;
  nstb_map[3][92]=3; chan_map[3][92]=93; row_map[3][92]=7; col_map[3][92]=8;
  nstb_map[3][93]=3; chan_map[3][93]=94; row_map[3][93]=7; col_map[3][93]=9;
  nstb_map[3][94]=3; chan_map[3][94]=95; row_map[3][94]=7; col_map[3][94]=10;
  nstb_map[3][95]=3; chan_map[3][95]=96; row_map[3][95]=7; col_map[3][95]=11;
  nstb_map[3][96]=3; chan_map[3][96]=102; row_map[3][96]=8; col_map[3][96]=5;
  nstb_map[3][97]=3; chan_map[3][97]=103; row_map[3][97]=8; col_map[3][97]=6;
  nstb_map[3][98]=3; chan_map[3][98]=104; row_map[3][98]=8; col_map[3][98]=7;
  nstb_map[3][99]=3; chan_map[3][99]=105; row_map[3][99]=8; col_map[3][99]=8;
  nstb_map[3][100]=3; chan_map[3][100]=106; row_map[3][100]=8; col_map[3][100]=9;
  nstb_map[3][101]=3; chan_map[3][101]=107; row_map[3][101]=8; col_map[3][101]=10;
  nstb_map[3][102]=3; chan_map[3][102]=108; row_map[3][102]=8; col_map[3][102]=11;
  nstb_map[3][104]=3; chan_map[3][104]=114; row_map[3][104]=9; col_map[3][104]=5;
  nstb_map[3][105]=3; chan_map[3][105]=115; row_map[3][105]=9; col_map[3][105]=6;
  nstb_map[3][106]=3; chan_map[3][106]=116; row_map[3][106]=9; col_map[3][106]=7;
  nstb_map[3][107]=3; chan_map[3][107]=117; row_map[3][107]=9; col_map[3][107]=8;
  nstb_map[3][108]=3; chan_map[3][108]=118; row_map[3][108]=9; col_map[3][108]=9;
  nstb_map[3][109]=3; chan_map[3][109]=119; row_map[3][109]=9; col_map[3][109]=10;
  nstb_map[3][110]=3; chan_map[3][110]=120; row_map[3][110]=9; col_map[3][110]=11;
  nstb_map[3][112]=3; chan_map[3][112]=126; row_map[3][112]=10; col_map[3][112]=5;
  nstb_map[3][113]=3; chan_map[3][113]=127; row_map[3][113]=10; col_map[3][113]=6;
  nstb_map[3][114]=3; chan_map[3][114]=128; row_map[3][114]=10; col_map[3][114]=7;
  nstb_map[3][115]=3; chan_map[3][115]=129; row_map[3][115]=10; col_map[3][115]=8;
  nstb_map[3][116]=3; chan_map[3][116]=130; row_map[3][116]=10; col_map[3][116]=9;
  nstb_map[3][117]=3; chan_map[3][117]=131; row_map[3][117]=10; col_map[3][117]=10;
  nstb_map[3][118]=3; chan_map[3][118]=132; row_map[3][118]=10; col_map[3][118]=11;
  nstb_map[3][120]=3; chan_map[3][120]=138; row_map[3][120]=11; col_map[3][120]=5;
  nstb_map[3][121]=3; chan_map[3][121]=139; row_map[3][121]=11; col_map[3][121]=6;
  nstb_map[3][122]=3; chan_map[3][122]=140; row_map[3][122]=11; col_map[3][122]=7;
  nstb_map[3][123]=3; chan_map[3][123]=141; row_map[3][123]=11; col_map[3][123]=8;
  nstb_map[3][124]=3; chan_map[3][124]=142; row_map[3][124]=11; col_map[3][124]=9;
  nstb_map[3][125]=3; chan_map[3][125]=143; row_map[3][125]=11; col_map[3][125]=10;
  nstb_map[3][126]=3; chan_map[3][126]=144; row_map[3][126]=11; col_map[3][126]=11;
  nstb_map[4][121]=3; chan_map[4][121]=150; row_map[4][121]=12; col_map[4][121]=5;
  nstb_map[4][122]=3; chan_map[4][122]=151; row_map[4][122]=12; col_map[4][122]=6;
  nstb_map[4][123]=3; chan_map[4][123]=152; row_map[4][123]=12; col_map[4][123]=7;
  nstb_map[4][124]=3; chan_map[4][124]=153; row_map[4][124]=12; col_map[4][124]=8;
  nstb_map[4][125]=3; chan_map[4][125]=154; row_map[4][125]=12; col_map[4][125]=9;
  nstb_map[4][126]=3; chan_map[4][126]=155; row_map[4][126]=12; col_map[4][126]=10;
  nstb_map[4][127]=3; chan_map[4][127]=156; row_map[4][127]=12; col_map[4][127]=11;
  nstb_map[4][112]=3; chan_map[4][112]=162; row_map[4][112]=13; col_map[4][112]=5;
  nstb_map[4][113]=3; chan_map[4][113]=163; row_map[4][113]=13; col_map[4][113]=6;
  nstb_map[4][114]=3; chan_map[4][114]=164; row_map[4][114]=13; col_map[4][114]=7;
  nstb_map[4][115]=3; chan_map[4][115]=165; row_map[4][115]=13; col_map[4][115]=8;
  nstb_map[4][116]=3; chan_map[4][116]=166; row_map[4][116]=13; col_map[4][116]=9;
  nstb_map[4][117]=3; chan_map[4][117]=167; row_map[4][117]=13; col_map[4][117]=10;
  nstb_map[4][118]=3; chan_map[4][118]=168; row_map[4][118]=13; col_map[4][118]=11;
  nstb_map[4][104]=3; chan_map[4][104]=174; row_map[4][104]=14; col_map[4][104]=5;
  nstb_map[4][105]=3; chan_map[4][105]=175; row_map[4][105]=14; col_map[4][105]=6;
  nstb_map[4][106]=3; chan_map[4][106]=176; row_map[4][106]=14; col_map[4][106]=7;
  nstb_map[4][107]=3; chan_map[4][107]=177; row_map[4][107]=14; col_map[4][107]=8;
  nstb_map[4][108]=3; chan_map[4][108]=178; row_map[4][108]=14; col_map[4][108]=9;
  nstb_map[4][109]=3; chan_map[4][109]=179; row_map[4][109]=14; col_map[4][109]=10;
  nstb_map[4][110]=3; chan_map[4][110]=180; row_map[4][110]=14; col_map[4][110]=11;
  nstb_map[4][96]=3; chan_map[4][96]=186; row_map[4][96]=15; col_map[4][96]=5;
  nstb_map[4][97]=3; chan_map[4][97]=187; row_map[4][97]=15; col_map[4][97]=6;
  nstb_map[4][98]=3; chan_map[4][98]=188; row_map[4][98]=15; col_map[4][98]=7;
  nstb_map[4][99]=3; chan_map[4][99]=189; row_map[4][99]=15; col_map[4][99]=8;
  nstb_map[4][100]=3; chan_map[4][100]=190; row_map[4][100]=15; col_map[4][100]=9;
  nstb_map[4][101]=3; chan_map[4][101]=191; row_map[4][101]=15; col_map[4][101]=10;
  nstb_map[4][102]=3; chan_map[4][102]=192; row_map[4][102]=15; col_map[4][102]=11;
  nstb_map[4][89]=3; chan_map[4][89]=198; row_map[4][89]=16; col_map[4][89]=5;
  nstb_map[4][90]=3; chan_map[4][90]=199; row_map[4][90]=16; col_map[4][90]=6;
  nstb_map[4][91]=3; chan_map[4][91]=200; row_map[4][91]=16; col_map[4][91]=7;
  nstb_map[4][92]=3; chan_map[4][92]=201; row_map[4][92]=16; col_map[4][92]=8;
  nstb_map[4][93]=3; chan_map[4][93]=202; row_map[4][93]=16; col_map[4][93]=9;
  nstb_map[4][94]=3; chan_map[4][94]=203; row_map[4][94]=16; col_map[4][94]=10;
  nstb_map[4][95]=3; chan_map[4][95]=204; row_map[4][95]=16; col_map[4][95]=11;
  nstb_map[4][0]=3; chan_map[4][0]=205; row_map[4][0]=17; col_map[4][0]=0;
  nstb_map[4][8]=3; chan_map[4][8]=206; row_map[4][8]=17; col_map[4][8]=1;
  nstb_map[4][16]=3; chan_map[4][16]=207; row_map[4][16]=17; col_map[4][16]=2;
  nstb_map[4][24]=3; chan_map[4][24]=208; row_map[4][24]=17; col_map[4][24]=3;
  nstb_map[4][80]=3; chan_map[4][80]=209; row_map[4][80]=17; col_map[4][80]=4;
  nstb_map[4][81]=3; chan_map[4][81]=210; row_map[4][81]=17; col_map[4][81]=5;
  nstb_map[4][82]=3; chan_map[4][82]=211; row_map[4][82]=17; col_map[4][82]=6;
  nstb_map[4][83]=3; chan_map[4][83]=212; row_map[4][83]=17; col_map[4][83]=7;
  nstb_map[4][84]=3; chan_map[4][84]=213; row_map[4][84]=17; col_map[4][84]=8;
  nstb_map[4][85]=3; chan_map[4][85]=214; row_map[4][85]=17; col_map[4][85]=9;
  nstb_map[4][86]=3; chan_map[4][86]=215; row_map[4][86]=17; col_map[4][86]=10;
  nstb_map[4][87]=3; chan_map[4][87]=216; row_map[4][87]=17; col_map[4][87]=11;
  nstb_map[4][1]=3; chan_map[4][1]=217; row_map[4][1]=18; col_map[4][1]=0;
  nstb_map[4][9]=3; chan_map[4][9]=218; row_map[4][9]=18; col_map[4][9]=1;
  nstb_map[4][17]=3; chan_map[4][17]=219; row_map[4][17]=18; col_map[4][17]=2;
  nstb_map[4][25]=3; chan_map[4][25]=220; row_map[4][25]=18; col_map[4][25]=3;
  nstb_map[4][72]=3; chan_map[4][72]=221; row_map[4][72]=18; col_map[4][72]=4;
  nstb_map[4][73]=3; chan_map[4][73]=222; row_map[4][73]=18; col_map[4][73]=5;
  nstb_map[4][74]=3; chan_map[4][74]=223; row_map[4][74]=18; col_map[4][74]=6;
  nstb_map[4][75]=3; chan_map[4][75]=224; row_map[4][75]=18; col_map[4][75]=7;
  nstb_map[4][76]=3; chan_map[4][76]=225; row_map[4][76]=18; col_map[4][76]=8;
  nstb_map[4][77]=3; chan_map[4][77]=226; row_map[4][77]=18; col_map[4][77]=9;
  nstb_map[4][78]=3; chan_map[4][78]=227; row_map[4][78]=18; col_map[4][78]=10;
  nstb_map[4][79]=3; chan_map[4][79]=228; row_map[4][79]=18; col_map[4][79]=11;
  nstb_map[4][2]=3; chan_map[4][2]=229; row_map[4][2]=19; col_map[4][2]=0;
  nstb_map[4][10]=3; chan_map[4][10]=230; row_map[4][10]=19; col_map[4][10]=1;
  nstb_map[4][18]=3; chan_map[4][18]=231; row_map[4][18]=19; col_map[4][18]=2;
  nstb_map[4][26]=3; chan_map[4][26]=232; row_map[4][26]=19; col_map[4][26]=3;
  nstb_map[4][64]=3; chan_map[4][64]=233; row_map[4][64]=19; col_map[4][64]=4;
  nstb_map[4][65]=3; chan_map[4][65]=234; row_map[4][65]=19; col_map[4][65]=5;
  nstb_map[4][66]=3; chan_map[4][66]=235; row_map[4][66]=19; col_map[4][66]=6;
  nstb_map[4][67]=3; chan_map[4][67]=236; row_map[4][67]=19; col_map[4][67]=7;
  nstb_map[4][68]=3; chan_map[4][68]=237; row_map[4][68]=19; col_map[4][68]=8;
  nstb_map[4][69]=3; chan_map[4][69]=238; row_map[4][69]=19; col_map[4][69]=9;
  nstb_map[4][70]=3; chan_map[4][70]=239; row_map[4][70]=19; col_map[4][70]=10;
  nstb_map[4][71]=3; chan_map[4][71]=240; row_map[4][71]=19; col_map[4][71]=11;
  nstb_map[4][3]=3; chan_map[4][3]=241; row_map[4][3]=20; col_map[4][3]=0;
  nstb_map[4][11]=3; chan_map[4][11]=242; row_map[4][11]=20; col_map[4][11]=1;
  nstb_map[4][19]=3; chan_map[4][19]=243; row_map[4][19]=20; col_map[4][19]=2;
  nstb_map[4][27]=3; chan_map[4][27]=244; row_map[4][27]=20; col_map[4][27]=3;
  nstb_map[4][56]=3; chan_map[4][56]=245; row_map[4][56]=20; col_map[4][56]=4;
  nstb_map[4][57]=3; chan_map[4][57]=246; row_map[4][57]=20; col_map[4][57]=5;
  nstb_map[4][58]=3; chan_map[4][58]=247; row_map[4][58]=20; col_map[4][58]=6;
  nstb_map[4][59]=3; chan_map[4][59]=248; row_map[4][59]=20; col_map[4][59]=7;
  nstb_map[4][60]=3; chan_map[4][60]=249; row_map[4][60]=20; col_map[4][60]=8;
  nstb_map[4][61]=3; chan_map[4][61]=250; row_map[4][61]=20; col_map[4][61]=9;
  nstb_map[4][62]=3; chan_map[4][62]=251; row_map[4][62]=20; col_map[4][62]=10;
  nstb_map[4][63]=3; chan_map[4][63]=252; row_map[4][63]=20; col_map[4][63]=11;
  nstb_map[4][4]=3; chan_map[4][4]=253; row_map[4][4]=21; col_map[4][4]=0;
  nstb_map[4][12]=3; chan_map[4][12]=254; row_map[4][12]=21; col_map[4][12]=1;
  nstb_map[4][20]=3; chan_map[4][20]=255; row_map[4][20]=21; col_map[4][20]=2;
  nstb_map[4][28]=3; chan_map[4][28]=256; row_map[4][28]=21; col_map[4][28]=3;
  nstb_map[4][48]=3; chan_map[4][48]=257; row_map[4][48]=21; col_map[4][48]=4;
  nstb_map[4][49]=3; chan_map[4][49]=258; row_map[4][49]=21; col_map[4][49]=5;
  nstb_map[4][50]=3; chan_map[4][50]=259; row_map[4][50]=21; col_map[4][50]=6;
  nstb_map[4][51]=3; chan_map[4][51]=260; row_map[4][51]=21; col_map[4][51]=7;
  nstb_map[4][52]=3; chan_map[4][52]=261; row_map[4][52]=21; col_map[4][52]=8;
  nstb_map[4][53]=3; chan_map[4][53]=262; row_map[4][53]=21; col_map[4][53]=9;
  nstb_map[4][54]=3; chan_map[4][54]=263; row_map[4][54]=21; col_map[4][54]=10;
  nstb_map[4][55]=3; chan_map[4][55]=264; row_map[4][55]=21; col_map[4][55]=11;
  nstb_map[4][5]=3; chan_map[4][5]=265; row_map[4][5]=22; col_map[4][5]=0;
  nstb_map[4][13]=3; chan_map[4][13]=266; row_map[4][13]=22; col_map[4][13]=1;
  nstb_map[4][21]=3; chan_map[4][21]=267; row_map[4][21]=22; col_map[4][21]=2;
  nstb_map[4][29]=3; chan_map[4][29]=268; row_map[4][29]=22; col_map[4][29]=3;
  nstb_map[4][40]=3; chan_map[4][40]=269; row_map[4][40]=22; col_map[4][40]=4;
  nstb_map[4][41]=3; chan_map[4][41]=270; row_map[4][41]=22; col_map[4][41]=5;
  nstb_map[4][42]=3; chan_map[4][42]=271; row_map[4][42]=22; col_map[4][42]=6;
  nstb_map[4][43]=3; chan_map[4][43]=272; row_map[4][43]=22; col_map[4][43]=7;
  nstb_map[4][44]=3; chan_map[4][44]=273; row_map[4][44]=22; col_map[4][44]=8;
  nstb_map[4][45]=3; chan_map[4][45]=274; row_map[4][45]=22; col_map[4][45]=9;
  nstb_map[4][46]=3; chan_map[4][46]=275; row_map[4][46]=22; col_map[4][46]=10;
  nstb_map[4][47]=3; chan_map[4][47]=276; row_map[4][47]=22; col_map[4][47]=11;
  nstb_map[4][6]=3; chan_map[4][6]=277; row_map[4][6]=23; col_map[4][6]=0;
  nstb_map[4][14]=3; chan_map[4][14]=278; row_map[4][14]=23; col_map[4][14]=1;
  nstb_map[4][22]=3; chan_map[4][22]=279; row_map[4][22]=23; col_map[4][22]=2;
  nstb_map[4][30]=3; chan_map[4][30]=280; row_map[4][30]=23; col_map[4][30]=3;
  nstb_map[4][32]=3; chan_map[4][32]=281; row_map[4][32]=23; col_map[4][32]=4;
  nstb_map[4][33]=3; chan_map[4][33]=282; row_map[4][33]=23; col_map[4][33]=5;
  nstb_map[4][34]=3; chan_map[4][34]=283; row_map[4][34]=23; col_map[4][34]=6;
  nstb_map[4][35]=3; chan_map[4][35]=284; row_map[4][35]=23; col_map[4][35]=7;
  nstb_map[4][36]=3; chan_map[4][36]=285; row_map[4][36]=23; col_map[4][36]=8;
  nstb_map[4][37]=3; chan_map[4][37]=286; row_map[4][37]=23; col_map[4][37]=9;
  nstb_map[4][38]=3; chan_map[4][38]=287; row_map[4][38]=23; col_map[4][38]=10;
  nstb_map[4][39]=3; chan_map[4][39]=288; row_map[4][39]=23; col_map[4][39]=11;
  nstb_map[1][6]=4; chan_map[1][6]=1; row_map[1][6]=0; col_map[1][6]=0;
  nstb_map[1][14]=4; chan_map[1][14]=2; row_map[1][14]=0; col_map[1][14]=1;
  nstb_map[1][22]=4; chan_map[1][22]=3; row_map[1][22]=0; col_map[1][22]=2;
  nstb_map[1][30]=4; chan_map[1][30]=4; row_map[1][30]=0; col_map[1][30]=3;
  nstb_map[1][32]=4; chan_map[1][32]=5; row_map[1][32]=0; col_map[1][32]=4;
  nstb_map[1][33]=4; chan_map[1][33]=6; row_map[1][33]=0; col_map[1][33]=5;
  nstb_map[1][34]=4; chan_map[1][34]=7; row_map[1][34]=0; col_map[1][34]=6;
  nstb_map[1][35]=4; chan_map[1][35]=8; row_map[1][35]=0; col_map[1][35]=7;
  nstb_map[1][36]=4; chan_map[1][36]=9; row_map[1][36]=0; col_map[1][36]=8;
  nstb_map[1][37]=4; chan_map[1][37]=10; row_map[1][37]=0; col_map[1][37]=9;
  nstb_map[1][38]=4; chan_map[1][38]=11; row_map[1][38]=0; col_map[1][38]=10;
  nstb_map[1][39]=4; chan_map[1][39]=12; row_map[1][39]=0; col_map[1][39]=11;
  nstb_map[1][5]=4; chan_map[1][5]=13; row_map[1][5]=1; col_map[1][5]=0;
  nstb_map[1][13]=4; chan_map[1][13]=14; row_map[1][13]=1; col_map[1][13]=1;
  nstb_map[1][21]=4; chan_map[1][21]=15; row_map[1][21]=1; col_map[1][21]=2;
  nstb_map[1][29]=4; chan_map[1][29]=16; row_map[1][29]=1; col_map[1][29]=3;
  nstb_map[1][40]=4; chan_map[1][40]=17; row_map[1][40]=1; col_map[1][40]=4;
  nstb_map[1][41]=4; chan_map[1][41]=18; row_map[1][41]=1; col_map[1][41]=5;
  nstb_map[1][42]=4; chan_map[1][42]=19; row_map[1][42]=1; col_map[1][42]=6;
  nstb_map[1][43]=4; chan_map[1][43]=20; row_map[1][43]=1; col_map[1][43]=7;
  nstb_map[1][44]=4; chan_map[1][44]=21; row_map[1][44]=1; col_map[1][44]=8;
  nstb_map[1][45]=4; chan_map[1][45]=22; row_map[1][45]=1; col_map[1][45]=9;
  nstb_map[1][46]=4; chan_map[1][46]=23; row_map[1][46]=1; col_map[1][46]=10;
  nstb_map[1][47]=4; chan_map[1][47]=24; row_map[1][47]=1; col_map[1][47]=11;
  nstb_map[1][4]=4; chan_map[1][4]=25; row_map[1][4]=2; col_map[1][4]=0;
  nstb_map[1][12]=4; chan_map[1][12]=26; row_map[1][12]=2; col_map[1][12]=1;
  nstb_map[1][20]=4; chan_map[1][20]=27; row_map[1][20]=2; col_map[1][20]=2;
  nstb_map[1][28]=4; chan_map[1][28]=28; row_map[1][28]=2; col_map[1][28]=3;
  nstb_map[1][48]=4; chan_map[1][48]=29; row_map[1][48]=2; col_map[1][48]=4;
  nstb_map[1][49]=4; chan_map[1][49]=30; row_map[1][49]=2; col_map[1][49]=5;
  nstb_map[1][50]=4; chan_map[1][50]=31; row_map[1][50]=2; col_map[1][50]=6;
  nstb_map[1][51]=4; chan_map[1][51]=32; row_map[1][51]=2; col_map[1][51]=7;
  nstb_map[1][52]=4; chan_map[1][52]=33; row_map[1][52]=2; col_map[1][52]=8;
  nstb_map[1][53]=4; chan_map[1][53]=34; row_map[1][53]=2; col_map[1][53]=9;
  nstb_map[1][54]=4; chan_map[1][54]=35; row_map[1][54]=2; col_map[1][54]=10;
  nstb_map[1][55]=4; chan_map[1][55]=36; row_map[1][55]=2; col_map[1][55]=11;
  nstb_map[1][3]=4; chan_map[1][3]=37; row_map[1][3]=3; col_map[1][3]=0;
  nstb_map[1][11]=4; chan_map[1][11]=38; row_map[1][11]=3; col_map[1][11]=1;
  nstb_map[1][19]=4; chan_map[1][19]=39; row_map[1][19]=3; col_map[1][19]=2;
  nstb_map[1][27]=4; chan_map[1][27]=40; row_map[1][27]=3; col_map[1][27]=3;
  nstb_map[1][56]=4; chan_map[1][56]=41; row_map[1][56]=3; col_map[1][56]=4;
  nstb_map[1][57]=4; chan_map[1][57]=42; row_map[1][57]=3; col_map[1][57]=5;
  nstb_map[1][58]=4; chan_map[1][58]=43; row_map[1][58]=3; col_map[1][58]=6;
  nstb_map[1][59]=4; chan_map[1][59]=44; row_map[1][59]=3; col_map[1][59]=7;
  nstb_map[1][60]=4; chan_map[1][60]=45; row_map[1][60]=3; col_map[1][60]=8;
  nstb_map[1][61]=4; chan_map[1][61]=46; row_map[1][61]=3; col_map[1][61]=9;
  nstb_map[1][62]=4; chan_map[1][62]=47; row_map[1][62]=3; col_map[1][62]=10;
  nstb_map[1][63]=4; chan_map[1][63]=48; row_map[1][63]=3; col_map[1][63]=11;
  nstb_map[1][2]=4; chan_map[1][2]=49; row_map[1][2]=4; col_map[1][2]=0;
  nstb_map[1][10]=4; chan_map[1][10]=50; row_map[1][10]=4; col_map[1][10]=1;
  nstb_map[1][18]=4; chan_map[1][18]=51; row_map[1][18]=4; col_map[1][18]=2;
  nstb_map[1][26]=4; chan_map[1][26]=52; row_map[1][26]=4; col_map[1][26]=3;
  nstb_map[1][64]=4; chan_map[1][64]=53; row_map[1][64]=4; col_map[1][64]=4;
  nstb_map[1][65]=4; chan_map[1][65]=54; row_map[1][65]=4; col_map[1][65]=5;
  nstb_map[1][66]=4; chan_map[1][66]=55; row_map[1][66]=4; col_map[1][66]=6;
  nstb_map[1][67]=4; chan_map[1][67]=56; row_map[1][67]=4; col_map[1][67]=7;
  nstb_map[1][68]=4; chan_map[1][68]=57; row_map[1][68]=4; col_map[1][68]=8;
  nstb_map[1][69]=4; chan_map[1][69]=58; row_map[1][69]=4; col_map[1][69]=9;
  nstb_map[1][70]=4; chan_map[1][70]=59; row_map[1][70]=4; col_map[1][70]=10;
  nstb_map[1][71]=4; chan_map[1][71]=60; row_map[1][71]=4; col_map[1][71]=11;
  nstb_map[1][1]=4; chan_map[1][1]=61; row_map[1][1]=5; col_map[1][1]=0;
  nstb_map[1][9]=4; chan_map[1][9]=62; row_map[1][9]=5; col_map[1][9]=1;
  nstb_map[1][17]=4; chan_map[1][17]=63; row_map[1][17]=5; col_map[1][17]=2;
  nstb_map[1][25]=4; chan_map[1][25]=64; row_map[1][25]=5; col_map[1][25]=3;
  nstb_map[1][72]=4; chan_map[1][72]=65; row_map[1][72]=5; col_map[1][72]=4;
  nstb_map[1][73]=4; chan_map[1][73]=66; row_map[1][73]=5; col_map[1][73]=5;
  nstb_map[1][74]=4; chan_map[1][74]=67; row_map[1][74]=5; col_map[1][74]=6;
  nstb_map[1][75]=4; chan_map[1][75]=68; row_map[1][75]=5; col_map[1][75]=7;
  nstb_map[1][76]=4; chan_map[1][76]=69; row_map[1][76]=5; col_map[1][76]=8;
  nstb_map[1][77]=4; chan_map[1][77]=70; row_map[1][77]=5; col_map[1][77]=9;
  nstb_map[1][78]=4; chan_map[1][78]=71; row_map[1][78]=5; col_map[1][78]=10;
  nstb_map[1][79]=4; chan_map[1][79]=72; row_map[1][79]=5; col_map[1][79]=11;
  nstb_map[1][0]=4; chan_map[1][0]=73; row_map[1][0]=6; col_map[1][0]=0;
  nstb_map[1][8]=4; chan_map[1][8]=74; row_map[1][8]=6; col_map[1][8]=1;
  nstb_map[1][16]=4; chan_map[1][16]=75; row_map[1][16]=6; col_map[1][16]=2;
  nstb_map[1][24]=4; chan_map[1][24]=76; row_map[1][24]=6; col_map[1][24]=3;
  nstb_map[1][80]=4; chan_map[1][80]=77; row_map[1][80]=6; col_map[1][80]=4;
  nstb_map[1][81]=4; chan_map[1][81]=78; row_map[1][81]=6; col_map[1][81]=5;
  nstb_map[1][82]=4; chan_map[1][82]=79; row_map[1][82]=6; col_map[1][82]=6;
  nstb_map[1][83]=4; chan_map[1][83]=80; row_map[1][83]=6; col_map[1][83]=7;
  nstb_map[1][84]=4; chan_map[1][84]=81; row_map[1][84]=6; col_map[1][84]=8;
  nstb_map[1][85]=4; chan_map[1][85]=82; row_map[1][85]=6; col_map[1][85]=9;
  nstb_map[1][86]=4; chan_map[1][86]=83; row_map[1][86]=6; col_map[1][86]=10;
  nstb_map[1][87]=4; chan_map[1][87]=84; row_map[1][87]=6; col_map[1][87]=11;
  nstb_map[1][89]=4; chan_map[1][89]=90; row_map[1][89]=7; col_map[1][89]=5;
  nstb_map[1][90]=4; chan_map[1][90]=91; row_map[1][90]=7; col_map[1][90]=6;
  nstb_map[1][91]=4; chan_map[1][91]=92; row_map[1][91]=7; col_map[1][91]=7;
  nstb_map[1][92]=4; chan_map[1][92]=93; row_map[1][92]=7; col_map[1][92]=8;
  nstb_map[1][93]=4; chan_map[1][93]=94; row_map[1][93]=7; col_map[1][93]=9;
  nstb_map[1][94]=4; chan_map[1][94]=95; row_map[1][94]=7; col_map[1][94]=10;
  nstb_map[1][95]=4; chan_map[1][95]=96; row_map[1][95]=7; col_map[1][95]=11;
  nstb_map[1][96]=4; chan_map[1][96]=102; row_map[1][96]=8; col_map[1][96]=5;
  nstb_map[1][97]=4; chan_map[1][97]=103; row_map[1][97]=8; col_map[1][97]=6;
  nstb_map[1][98]=4; chan_map[1][98]=104; row_map[1][98]=8; col_map[1][98]=7;
  nstb_map[1][99]=4; chan_map[1][99]=105; row_map[1][99]=8; col_map[1][99]=8;
  nstb_map[1][100]=4; chan_map[1][100]=106; row_map[1][100]=8; col_map[1][100]=9;
  nstb_map[1][101]=4; chan_map[1][101]=107; row_map[1][101]=8; col_map[1][101]=10;
  nstb_map[1][102]=4; chan_map[1][102]=108; row_map[1][102]=8; col_map[1][102]=11;
  nstb_map[1][104]=4; chan_map[1][104]=114; row_map[1][104]=9; col_map[1][104]=5;
  nstb_map[1][105]=4; chan_map[1][105]=115; row_map[1][105]=9; col_map[1][105]=6;
  nstb_map[1][106]=4; chan_map[1][106]=116; row_map[1][106]=9; col_map[1][106]=7;
  nstb_map[1][107]=4; chan_map[1][107]=117; row_map[1][107]=9; col_map[1][107]=8;
  nstb_map[1][108]=4; chan_map[1][108]=118; row_map[1][108]=9; col_map[1][108]=9;
  nstb_map[1][109]=4; chan_map[1][109]=119; row_map[1][109]=9; col_map[1][109]=10;
  nstb_map[1][110]=4; chan_map[1][110]=120; row_map[1][110]=9; col_map[1][110]=11;
  nstb_map[1][112]=4; chan_map[1][112]=126; row_map[1][112]=10; col_map[1][112]=5;
  nstb_map[1][113]=4; chan_map[1][113]=127; row_map[1][113]=10; col_map[1][113]=6;
  nstb_map[1][114]=4; chan_map[1][114]=128; row_map[1][114]=10; col_map[1][114]=7;
  nstb_map[1][115]=4; chan_map[1][115]=129; row_map[1][115]=10; col_map[1][115]=8;
  nstb_map[1][116]=4; chan_map[1][116]=130; row_map[1][116]=10; col_map[1][116]=9;
  nstb_map[1][117]=4; chan_map[1][117]=131; row_map[1][117]=10; col_map[1][117]=10;
  nstb_map[1][118]=4; chan_map[1][118]=132; row_map[1][118]=10; col_map[1][118]=11;
  nstb_map[1][120]=4; chan_map[1][120]=138; row_map[1][120]=11; col_map[1][120]=5;
  nstb_map[1][121]=4; chan_map[1][121]=139; row_map[1][121]=11; col_map[1][121]=6;
  nstb_map[1][122]=4; chan_map[1][122]=140; row_map[1][122]=11; col_map[1][122]=7;
  nstb_map[1][123]=4; chan_map[1][123]=141; row_map[1][123]=11; col_map[1][123]=8;
  nstb_map[1][124]=4; chan_map[1][124]=142; row_map[1][124]=11; col_map[1][124]=9;
  nstb_map[1][125]=4; chan_map[1][125]=143; row_map[1][125]=11; col_map[1][125]=10;
  nstb_map[1][126]=4; chan_map[1][126]=144; row_map[1][126]=11; col_map[1][126]=11;
  nstb_map[2][121]=4; chan_map[2][121]=150; row_map[2][121]=12; col_map[2][121]=5;
  nstb_map[2][122]=4; chan_map[2][122]=151; row_map[2][122]=12; col_map[2][122]=6;
  nstb_map[2][123]=4; chan_map[2][123]=152; row_map[2][123]=12; col_map[2][123]=7;
  nstb_map[2][124]=4; chan_map[2][124]=153; row_map[2][124]=12; col_map[2][124]=8;
  nstb_map[2][125]=4; chan_map[2][125]=154; row_map[2][125]=12; col_map[2][125]=9;
  nstb_map[2][126]=4; chan_map[2][126]=155; row_map[2][126]=12; col_map[2][126]=10;
  nstb_map[2][127]=4; chan_map[2][127]=156; row_map[2][127]=12; col_map[2][127]=11;
  nstb_map[2][112]=4; chan_map[2][112]=162; row_map[2][112]=13; col_map[2][112]=5;
  nstb_map[2][113]=4; chan_map[2][113]=163; row_map[2][113]=13; col_map[2][113]=6;
  nstb_map[2][114]=4; chan_map[2][114]=164; row_map[2][114]=13; col_map[2][114]=7;
  nstb_map[2][115]=4; chan_map[2][115]=165; row_map[2][115]=13; col_map[2][115]=8;
  nstb_map[2][116]=4; chan_map[2][116]=166; row_map[2][116]=13; col_map[2][116]=9;
  nstb_map[2][117]=4; chan_map[2][117]=167; row_map[2][117]=13; col_map[2][117]=10;
  nstb_map[2][118]=4; chan_map[2][118]=168; row_map[2][118]=13; col_map[2][118]=11;
  nstb_map[2][104]=4; chan_map[2][104]=174; row_map[2][104]=14; col_map[2][104]=5;
  nstb_map[2][105]=4; chan_map[2][105]=175; row_map[2][105]=14; col_map[2][105]=6;
  nstb_map[2][106]=4; chan_map[2][106]=176; row_map[2][106]=14; col_map[2][106]=7;
  nstb_map[2][107]=4; chan_map[2][107]=177; row_map[2][107]=14; col_map[2][107]=8;
  nstb_map[2][108]=4; chan_map[2][108]=178; row_map[2][108]=14; col_map[2][108]=9;
  nstb_map[2][109]=4; chan_map[2][109]=179; row_map[2][109]=14; col_map[2][109]=10;
  nstb_map[2][110]=4; chan_map[2][110]=180; row_map[2][110]=14; col_map[2][110]=11;
  nstb_map[2][96]=4; chan_map[2][96]=186; row_map[2][96]=15; col_map[2][96]=5;
  nstb_map[2][97]=4; chan_map[2][97]=187; row_map[2][97]=15; col_map[2][97]=6;
  nstb_map[2][98]=4; chan_map[2][98]=188; row_map[2][98]=15; col_map[2][98]=7;
  nstb_map[2][99]=4; chan_map[2][99]=189; row_map[2][99]=15; col_map[2][99]=8;
  nstb_map[2][100]=4; chan_map[2][100]=190; row_map[2][100]=15; col_map[2][100]=9;
  nstb_map[2][101]=4; chan_map[2][101]=191; row_map[2][101]=15; col_map[2][101]=10;
  nstb_map[2][102]=4; chan_map[2][102]=192; row_map[2][102]=15; col_map[2][102]=11;
  nstb_map[2][89]=4; chan_map[2][89]=198; row_map[2][89]=16; col_map[2][89]=5;
  nstb_map[2][90]=4; chan_map[2][90]=199; row_map[2][90]=16; col_map[2][90]=6;
  nstb_map[2][91]=4; chan_map[2][91]=200; row_map[2][91]=16; col_map[2][91]=7;
  nstb_map[2][92]=4; chan_map[2][92]=201; row_map[2][92]=16; col_map[2][92]=8;
  nstb_map[2][93]=4; chan_map[2][93]=202; row_map[2][93]=16; col_map[2][93]=9;
  nstb_map[2][94]=4; chan_map[2][94]=203; row_map[2][94]=16; col_map[2][94]=10;
  nstb_map[2][95]=4; chan_map[2][95]=204; row_map[2][95]=16; col_map[2][95]=11;
  nstb_map[2][0]=4; chan_map[2][0]=205; row_map[2][0]=17; col_map[2][0]=0;
  nstb_map[2][8]=4; chan_map[2][8]=206; row_map[2][8]=17; col_map[2][8]=1;
  nstb_map[2][16]=4; chan_map[2][16]=207; row_map[2][16]=17; col_map[2][16]=2;
  nstb_map[2][24]=4; chan_map[2][24]=208; row_map[2][24]=17; col_map[2][24]=3;
  nstb_map[2][80]=4; chan_map[2][80]=209; row_map[2][80]=17; col_map[2][80]=4;
  nstb_map[2][81]=4; chan_map[2][81]=210; row_map[2][81]=17; col_map[2][81]=5;
  nstb_map[2][82]=4; chan_map[2][82]=211; row_map[2][82]=17; col_map[2][82]=6;
  nstb_map[2][83]=4; chan_map[2][83]=212; row_map[2][83]=17; col_map[2][83]=7;
  nstb_map[2][84]=4; chan_map[2][84]=213; row_map[2][84]=17; col_map[2][84]=8;
  nstb_map[2][85]=4; chan_map[2][85]=214; row_map[2][85]=17; col_map[2][85]=9;
  nstb_map[2][86]=4; chan_map[2][86]=215; row_map[2][86]=17; col_map[2][86]=10;
  nstb_map[2][87]=4; chan_map[2][87]=216; row_map[2][87]=17; col_map[2][87]=11;
  nstb_map[2][1]=4; chan_map[2][1]=217; row_map[2][1]=18; col_map[2][1]=0;
  nstb_map[2][9]=4; chan_map[2][9]=218; row_map[2][9]=18; col_map[2][9]=1;
  nstb_map[2][17]=4; chan_map[2][17]=219; row_map[2][17]=18; col_map[2][17]=2;
  nstb_map[2][25]=4; chan_map[2][25]=220; row_map[2][25]=18; col_map[2][25]=3;
  nstb_map[2][72]=4; chan_map[2][72]=221; row_map[2][72]=18; col_map[2][72]=4;
  nstb_map[2][73]=4; chan_map[2][73]=222; row_map[2][73]=18; col_map[2][73]=5;
  nstb_map[2][74]=4; chan_map[2][74]=223; row_map[2][74]=18; col_map[2][74]=6;
  nstb_map[2][75]=4; chan_map[2][75]=224; row_map[2][75]=18; col_map[2][75]=7;
  nstb_map[2][76]=4; chan_map[2][76]=225; row_map[2][76]=18; col_map[2][76]=8;
  nstb_map[2][77]=4; chan_map[2][77]=226; row_map[2][77]=18; col_map[2][77]=9;
  nstb_map[2][78]=4; chan_map[2][78]=227; row_map[2][78]=18; col_map[2][78]=10;
  nstb_map[2][79]=4; chan_map[2][79]=228; row_map[2][79]=18; col_map[2][79]=11;
  nstb_map[2][2]=4; chan_map[2][2]=229; row_map[2][2]=19; col_map[2][2]=0;
  nstb_map[2][10]=4; chan_map[2][10]=230; row_map[2][10]=19; col_map[2][10]=1;
  nstb_map[2][18]=4; chan_map[2][18]=231; row_map[2][18]=19; col_map[2][18]=2;
  nstb_map[2][26]=4; chan_map[2][26]=232; row_map[2][26]=19; col_map[2][26]=3;
  nstb_map[2][64]=4; chan_map[2][64]=233; row_map[2][64]=19; col_map[2][64]=4;
  nstb_map[2][65]=4; chan_map[2][65]=234; row_map[2][65]=19; col_map[2][65]=5;
  nstb_map[2][66]=4; chan_map[2][66]=235; row_map[2][66]=19; col_map[2][66]=6;
  nstb_map[2][67]=4; chan_map[2][67]=236; row_map[2][67]=19; col_map[2][67]=7;
  nstb_map[2][68]=4; chan_map[2][68]=237; row_map[2][68]=19; col_map[2][68]=8;
  nstb_map[2][69]=4; chan_map[2][69]=238; row_map[2][69]=19; col_map[2][69]=9;
  nstb_map[2][70]=4; chan_map[2][70]=239; row_map[2][70]=19; col_map[2][70]=10;
  nstb_map[2][71]=4; chan_map[2][71]=240; row_map[2][71]=19; col_map[2][71]=11;
  nstb_map[2][3]=4; chan_map[2][3]=241; row_map[2][3]=20; col_map[2][3]=0;
  nstb_map[2][11]=4; chan_map[2][11]=242; row_map[2][11]=20; col_map[2][11]=1;
  nstb_map[2][19]=4; chan_map[2][19]=243; row_map[2][19]=20; col_map[2][19]=2;
  nstb_map[2][27]=4; chan_map[2][27]=244; row_map[2][27]=20; col_map[2][27]=3;
  nstb_map[2][56]=4; chan_map[2][56]=245; row_map[2][56]=20; col_map[2][56]=4;
  nstb_map[2][57]=4; chan_map[2][57]=246; row_map[2][57]=20; col_map[2][57]=5;
  nstb_map[2][58]=4; chan_map[2][58]=247; row_map[2][58]=20; col_map[2][58]=6;
  nstb_map[2][59]=4; chan_map[2][59]=248; row_map[2][59]=20; col_map[2][59]=7;
  nstb_map[2][60]=4; chan_map[2][60]=249; row_map[2][60]=20; col_map[2][60]=8;
  nstb_map[2][61]=4; chan_map[2][61]=250; row_map[2][61]=20; col_map[2][61]=9;
  nstb_map[2][62]=4; chan_map[2][62]=251; row_map[2][62]=20; col_map[2][62]=10;
  nstb_map[2][63]=4; chan_map[2][63]=252; row_map[2][63]=20; col_map[2][63]=11;
  nstb_map[2][4]=4; chan_map[2][4]=253; row_map[2][4]=21; col_map[2][4]=0;
  nstb_map[2][12]=4; chan_map[2][12]=254; row_map[2][12]=21; col_map[2][12]=1;
  nstb_map[2][20]=4; chan_map[2][20]=255; row_map[2][20]=21; col_map[2][20]=2;
  nstb_map[2][28]=4; chan_map[2][28]=256; row_map[2][28]=21; col_map[2][28]=3;
  nstb_map[2][48]=4; chan_map[2][48]=257; row_map[2][48]=21; col_map[2][48]=4;
  nstb_map[2][49]=4; chan_map[2][49]=258; row_map[2][49]=21; col_map[2][49]=5;
  nstb_map[2][50]=4; chan_map[2][50]=259; row_map[2][50]=21; col_map[2][50]=6;
  nstb_map[2][51]=4; chan_map[2][51]=260; row_map[2][51]=21; col_map[2][51]=7;
  nstb_map[2][52]=4; chan_map[2][52]=261; row_map[2][52]=21; col_map[2][52]=8;
  nstb_map[2][53]=4; chan_map[2][53]=262; row_map[2][53]=21; col_map[2][53]=9;
  nstb_map[2][54]=4; chan_map[2][54]=263; row_map[2][54]=21; col_map[2][54]=10;
  nstb_map[2][55]=4; chan_map[2][55]=264; row_map[2][55]=21; col_map[2][55]=11;
  nstb_map[2][5]=4; chan_map[2][5]=265; row_map[2][5]=22; col_map[2][5]=0;
  nstb_map[2][13]=4; chan_map[2][13]=266; row_map[2][13]=22; col_map[2][13]=1;
  nstb_map[2][21]=4; chan_map[2][21]=267; row_map[2][21]=22; col_map[2][21]=2;
  nstb_map[2][29]=4; chan_map[2][29]=268; row_map[2][29]=22; col_map[2][29]=3;
  nstb_map[2][40]=4; chan_map[2][40]=269; row_map[2][40]=22; col_map[2][40]=4;
  nstb_map[2][41]=4; chan_map[2][41]=270; row_map[2][41]=22; col_map[2][41]=5;
  nstb_map[2][42]=4; chan_map[2][42]=271; row_map[2][42]=22; col_map[2][42]=6;
  nstb_map[2][43]=4; chan_map[2][43]=272; row_map[2][43]=22; col_map[2][43]=7;
  nstb_map[2][44]=4; chan_map[2][44]=273; row_map[2][44]=22; col_map[2][44]=8;
  nstb_map[2][45]=4; chan_map[2][45]=274; row_map[2][45]=22; col_map[2][45]=9;
  nstb_map[2][46]=4; chan_map[2][46]=275; row_map[2][46]=22; col_map[2][46]=10;
  nstb_map[2][47]=4; chan_map[2][47]=276; row_map[2][47]=22; col_map[2][47]=11;
  nstb_map[2][6]=4; chan_map[2][6]=277; row_map[2][6]=23; col_map[2][6]=0;
  nstb_map[2][14]=4; chan_map[2][14]=278; row_map[2][14]=23; col_map[2][14]=1;
  nstb_map[2][22]=4; chan_map[2][22]=279; row_map[2][22]=23; col_map[2][22]=2;
  nstb_map[2][30]=4; chan_map[2][30]=280; row_map[2][30]=23; col_map[2][30]=3;
  nstb_map[2][32]=4; chan_map[2][32]=281; row_map[2][32]=23; col_map[2][32]=4;
  nstb_map[2][33]=4; chan_map[2][33]=282; row_map[2][33]=23; col_map[2][33]=5;
  nstb_map[2][34]=4; chan_map[2][34]=283; row_map[2][34]=23; col_map[2][34]=6;
  nstb_map[2][35]=4; chan_map[2][35]=284; row_map[2][35]=23; col_map[2][35]=7;
  nstb_map[2][36]=4; chan_map[2][36]=285; row_map[2][36]=23; col_map[2][36]=8;
  nstb_map[2][37]=4; chan_map[2][37]=286; row_map[2][37]=23; col_map[2][37]=9;
  nstb_map[2][38]=4; chan_map[2][38]=287; row_map[2][38]=23; col_map[2][38]=10;
  nstb_map[2][39]=4; chan_map[2][39]=288; row_map[2][39]=23; col_map[2][39]=11;
};

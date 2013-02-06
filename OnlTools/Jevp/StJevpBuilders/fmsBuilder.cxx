#include "fmsBuilder.h"

#include <memory>
#include <sstream>
#include <utility>

#include <TH2F.h>

#include "DAQ_READER/daqReader.h"
#include "Jevp/StJevpPlot/JevpPlot.h"

#include <RTS/include/rtsLog.h>

ClassImp(fmsBuilder)

namespace {
  enum StFmsQtCrateNumber {kQt1 = 1, kQt2, kQt3, kQt4, kFpd, kQtError};

  /*
   Basic QT crate geometry.
   Note that physically there are 16 slots per crate, but only 12
   at most are currently actually used.
   */
  const int kNQtSlotsPerCrate = 12;
  const int kNQtChannelsPerSlot = 32;
  const int kNChannels = kNQtSlotsPerCrate * kNQtChannelsPerSlot;
  const int kNAdc = 4096;

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
  std::pair<std::string, std::string> composeQtNameTitle(int qt) {
    std::string name, title;
    if(qt >= kQt1 and qt <= kQt4) {
      std::stringstream stream;
      stream << "fms_qt_channel_adc_crate_" << qt;
      name = stream.str();
      stream.str("");
      stream.clear();
      stream << "Input to FMS QT crate " << qt << " (" << position(qt) << ")";
      title = stream.str();
    } // if
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
: JevpPlotSet(parent) {
  // Set plotsetname inherited from JevpPlotSet.
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
  // Create histograms for each QT crate (1 to 5), including FPD (5).
  for(int qt = kQt1; qt < kQtError; ++qt) {
    // Create the histogram.
    std::pair<std::string, std::string> nameTitle
      = composeQtNameTitle(qt);
    TH2F* h = new TH2F(nameTitle.first.c_str(),
                       nameTitle.second.c_str(),
                       kNChannels, 0., kNChannels,  // Channel axis bins
                       200, 0., kNAdc);           // ADC axis bins
    h->SetBit(TH1::kCanRebin);
    h->SetXTitle("slot * 32 + channel");
    h->SetYTitle("ADC");
    // Store the histogram.
    // Create a JevpPlot owning the histogram and add it to the collection.
    mHists.insert(std::make_pair(qt, h));
    mPlots.push_back(new JevpPlot(h));
    addPlot(mPlots.back()); // Registers the plot with this JevpPlotSet
  } // for
}

void fmsBuilder::startrun(daqReader* reader) {
  LOG(DBG, "fmsBuilder::startrun %d", reader->run);
  // Clear existing histogram contents.
  TH1PtrMap::iterator i;
  for(i = mHists.begin(); i not_eq mHists.end(); ++i) {
    i->second->Reset();
  } // for
}

void fmsBuilder::stoprun(daqReader* reader) {
  LOG(DBG, "fmsBuilder::stoprun %d", reader->run);
}

void fmsBuilder::event(daqReader* reader) {
  LOG(DBG, "fmsBuilder::event %d", reader->event_number);
  // Get trigger data for the current event.
  // getStTriggerData is inherited from JevpPlotSet.
  std::auto_ptr<StTriggerData> trigger(getStTriggerData(reader));
  if(not trigger.get()) {
    return;
  } // if
  // Skip LED events.
  if(isLedEvent(*trigger)) {
    return;
  } // if
  // Loop over histograms for each QT crate.
  TH1PtrMap::iterator i;
  for(i = mHists.begin(); i not_eq mHists.end(); ++i) {
    TH1* histogram = i->second;
    // Fill the histogram for each channel by looping over
    // all slot and channel-in-slot values.
    int crate = i->first;
    for(int slot(0); slot < kNQtSlotsPerCrate; ++slot) {
      for(int channel(0); channel < kNQtChannelsPerSlot; ++channel) {
        int index = slot * kNQtChannelsPerSlot + channel;
        float adc = trigger->fmsADC(crate, slot, channel, 0);
        histogram->Fill(index, adc);
      } // for
    } // for
  } // for
}

void fmsBuilder::main(int argc, char *argv[]) {
  fmsBuilder self;
  self.Main(argc, argv);
}

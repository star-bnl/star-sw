#include "WBosRootFile.h"

#include <climits>
#include <sstream>

#include "St_db_Maker/St_db_Maker.h"
#include "StEmcUtil/database/StBemcTables.h"
#include "StEmcUtil/geometry/StEmcGeom.h"

#include "AllAsymHContainer.h"
#include "AsymHContainer.h"
#include "TrackHContainer.h"
#include "EventHContainer.h"
#include "EventDisplayHContainer.h"
#include "WBosEventHContainer.h"
#include "MCHContainer.h"
#include "WBosEvent.h"
#include "ZBosEvent.h"
#include "VecBosTrack.h"
#include "VecBosVertex.h"

#include "utils/utils.h"

using namespace std;


WBosRootFile::WBosRootFile() : VecBosRootFile()
{
   BookHists();
}


WBosRootFile::WBosRootFile(const char *fname, Option_t *option, Int_t isMc, Bool_t isZ, const char *ftitle, Int_t compress) :
  VecBosRootFile(fname, option, isMc, isZ, ftitle, compress)
{
   gBTowGeom = StEmcGeom::instance("bemc");
   gBTowGeom->printGeom();
   BookHists();
}


WBosRootFile::~WBosRootFile()
{
}


void WBosRootFile::BookHists()
{
   // Delete histograms created in parent class
   if (fHists) { delete fHists; fHists = 0; }
   fHistCuts.clear();

   PlotHelper *ph;

   fHists = new PlotHelper(this);

   fHists->d["tracks"] = ph = new TrackHContainer(new TDirectoryFile("tracks", "tracks", "", this));
   fHistCuts[kCUT_EVENT_NOCUT].insert(ph);

   fHists->d["tracks.cut_cand"] = ph = new TrackHContainer(new TDirectoryFile("tracks.cut_cand", "tracks.cut_cand", "", this));
   fHistCuts[kCUT_EVENT_HAS_CANDIDATE_TRACK].insert(ph);

   fHists->d["tracks.cut_w"] = ph = new TrackHContainer(new TDirectoryFile("tracks.cut_w", "tracks.cut_w", "", this));
   fHistCuts[kCUT_EVENT_W].insert(ph);

   fHists->d["event"] = ph = new EventHContainer(new TDirectoryFile("event", "event", "", this));
   fHistCuts[kCUT_EVENT_NOCUT].insert(ph);

   fHists->d["event.cut_w"] = ph = new EventHContainer(new TDirectoryFile("event.cut_w", "event.cut_w", "", this));
   fHistCuts[kCUT_EVENT_W].insert(ph);

   fHists->d["event_w"] = ph = new WBosEventHContainer(new TDirectoryFile("event_w", "event_w", "", this));
   fHistCuts[kCUT_EVENT_W].insert(ph);

   fHists->d["event_display"] = ph = new EventDisplayHContainer(new TDirectoryFile("event_display", "event_display", "", this));
   fHistCuts[kCUT_EVENT_W].insert(ph);

   fHists->d["event_wp"] = ph = new WBosEventHContainer(new TDirectoryFile("event_wp", "event_wp", "", this));
   fHistCuts[kCUT_EVENT_W_PLUS].insert(ph);

   fHists->d["event_wm"] = ph = new WBosEventHContainer(new TDirectoryFile("event_wm", "event_wm", "", this));
   fHistCuts[kCUT_EVENT_W_MINUS].insert(ph);

   if (fIsMc) {
      fHists->d["event_mc"] = ph = new MCHContainer(new TDirectoryFile("event_mc", "event_mc", "", this), fIsMc);
      fHistCuts[kCUT_EVENT_W].insert(ph);
   } else {

      fHists->d["asym"] = ph = new AllAsymHContainer(new TDirectoryFile("asym", "asym", "", this));
      fHistCuts[kCUT_EVENT_W].insert(ph);
   }

   this->cd();
}


/** */
void WBosRootFile::Fill(ProtoEvent &ev)
{
   WBosEvent& w_event = (WBosEvent&) ev;

   Fill(ev, kCUT_EVENT_NOCUT);

   if ( w_event.HasCandidateEle() )
      Fill(ev, kCUT_EVENT_HAS_CANDIDATE_TRACK);

   if ( w_event.PassedCutWBos(WBosEvent::sMinElectronPtHard) )
   {
      Fill(ev, kCUT_EVENT_W);

      if ( w_event.PassedCutWBosPlus(WBosEvent::sMinElectronPtHard) ) 
         Fill(ev, kCUT_EVENT_W_PLUS);

      if ( w_event.PassedCutWBosMinus(WBosEvent::sMinElectronPtHard) )
         Fill(ev, kCUT_EVENT_W_MINUS);
   }
}


/** */
void WBosRootFile::Fill(ProtoEvent &ev, ECut cut)
{
   VecBosRootFile::Fill(ev, cut);
}

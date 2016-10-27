#include "ZBosRootFile.h"

#include <climits>
#include <sstream>

#include "St_db_Maker/St_db_Maker.h"
#include "StEmcUtil/database/StBemcTables.h"
#include "StEmcUtil/geometry/StEmcGeom.h"

#include "AllAsymHContainer.h"
#include "AsymHContainer.h"
#include "EventHContainer.h"
#include "EventDisplayHContainer.h"
#include "WBosEventHContainer.h"
#include "Z0EventHContainer.h"
#include "MCHContainer.h"
#include "WBosEvent.h"
#include "ZBosEvent.h"
#include "VecBosTrack.h"
#include "VecBosVertex.h"

#include "utils/utils.h"

using namespace std;


ZBosRootFile::ZBosRootFile() : VecBosRootFile()
{
   BookHists();
}


ZBosRootFile::ZBosRootFile(const char *fname, Option_t *option, Int_t isMc, Bool_t isZ, const char *ftitle, Int_t compress) :
  VecBosRootFile(fname, option, isMc, isZ, ftitle, compress)
{
   gBTowGeom = StEmcGeom::instance("bemc");
   gBTowGeom->printGeom();
   BookHists();
}


ZBosRootFile::~ZBosRootFile()
{
}


void ZBosRootFile::BookHists()
{
   // Delete histograms created in parent class
   if (fHists) { delete fHists; fHists = 0; }
   fHistCuts.clear();

   PlotHelper *ph;

   fHists = new PlotHelper(this);

   fHists->d["event"] = ph = new EventHContainer(new TDirectoryFile("event", "event", "", this));
   fHistCuts[kCUT_EVENT_NOCUT].insert(ph);

   fHists->d["event_z"] = ph = new Z0EventHContainer(new TDirectoryFile("event_z", "event_z", "", this));
   fHistCuts[kCUT_EVENT_Z].insert(ph);

   fHists->d["asym"] = ph = new AllAsymHContainer(new TDirectoryFile("asym", "asym", "", this));
   fHistCuts[kCUT_EVENT_Z].insert(ph);

   if (!fIsMc) return;

   /*
   if (!fIsMc==3) {
      fHists->d["event_mc"] = ph = new MCHContainer(new TDirectoryFile("event_mc", "event_mc", "", this), fIsMc);
      fHistCuts[kCUT_EVENT_Z].insert(ph);
   }
   */

   this->cd();
}


/** */
void ZBosRootFile::Fill(ProtoEvent &ev)
{
   ZBosEvent& z_event = (ZBosEvent&) ev;

   Fill(ev, kCUT_EVENT_NOCUT);

   if ( z_event.PassedCutZMass(ZBosEvent::sMinZEleCandPtHard) )
   {
      Fill(ev, kCUT_EVENT_Z);
   }
}


/** */
void ZBosRootFile::Fill(ProtoEvent &ev, ECut cut)
{
   VecBosRootFile::Fill(ev, cut);
}

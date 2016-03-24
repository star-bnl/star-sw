#include <math.h>

#include "AllAsymHContainer.h"

#include "TF1.h"
#include "TF2.h"

#include "utils/utils.h"
#include "utils/MultiGraph.h"
#include "utils/H1I.h"
#include "utils/H1D.h"
#include "utils/H2I.h"
#include "utils/H2D.h"

#include "AsymCalculator.h"
#include "WBosAsymHContainer.h"
#include "WLepAsymHContainer.h"
#include "Z0AsymHContainer.h"
#include "WBosEvent.h"


ClassImp(AllAsymHContainer)

using namespace std;


/** Default constructor. */
AllAsymHContainer::AllAsymHContainer() : PlotHelper()
{
   BookHists();
}


AllAsymHContainer::AllAsymHContainer(TDirectory *dir) : PlotHelper(dir)
{
   BookHists();
}


/** */
void AllAsymHContainer::BookHists()
{
   PlotHelper *ph;

   d["asym_w"]        = ph = new WBosAsymHContainer(new TDirectoryFile("asym_w", "asym_w", "", fDir));
   fHistCuts[kCUT_EVENT_W].insert(ph);

   d["asym_wp"]       = ph = new WBosAsymHContainer(new TDirectoryFile("asym_wp",      "asym_wp", "", fDir));
   fHistCuts[kCUT_EVENT_W_PLUS].insert(ph);
   d["asym_wp_phys"]  = ph = new WBosAsymHContainer(new TDirectoryFile("asym_wp_phys", "asym_wp_phys", "", fDir), kAsymSqrtPhys);
   fHistCuts[kCUT_EVENT_W_PLUS].insert(ph);
   d["asym_wp_geom"]  = ph = new WBosAsymHContainer(new TDirectoryFile("asym_wp_geom", "asym_wp_geom", "", fDir), kAsymSqrtGeom);
   fHistCuts[kCUT_EVENT_W_PLUS].insert(ph);
   d["asym_wp_lumi"]  = ph = new WBosAsymHContainer(new TDirectoryFile("asym_wp_lumi", "asym_wp_lumi", "", fDir), kAsymSqrtLumi);
   fHistCuts[kCUT_EVENT_W_PLUS].insert(ph);

   d["asym_wlp"]      = ph = new WLepAsymHContainer(new TDirectoryFile("asym_wlp",      "asym_wlp", "", fDir));
   fHistCuts[kCUT_EVENT_W_PLUS].insert(ph);
   d["asym_wlp_phys"] = ph = new WLepAsymHContainer(new TDirectoryFile("asym_wlp_phys", "asym_wlp_phys", "", fDir), kAsymSqrtPhys);
   fHistCuts[kCUT_EVENT_W_PLUS].insert(ph);
   d["asym_wlp_geom"] = ph = new WLepAsymHContainer(new TDirectoryFile("asym_wlp_geom", "asym_wlp_geom", "", fDir), kAsymSqrtGeom);
   fHistCuts[kCUT_EVENT_W_PLUS].insert(ph);
   d["asym_wlp_lumi"] = ph = new WLepAsymHContainer(new TDirectoryFile("asym_wlp_lumi", "asym_wlp_lumi", "", fDir), kAsymSqrtLumi);
   fHistCuts[kCUT_EVENT_W_PLUS].insert(ph);

   d["asym_wm"]       = ph = new WBosAsymHContainer(new TDirectoryFile("asym_wm",      "asym_wm", "", fDir));
   fHistCuts[kCUT_EVENT_W_MINUS].insert(ph);
   d["asym_wm_phys"]  = ph = new WBosAsymHContainer(new TDirectoryFile("asym_wm_phys", "asym_wm_phys", "", fDir), kAsymSqrtPhys);
   fHistCuts[kCUT_EVENT_W_MINUS].insert(ph);
   d["asym_wm_geom"]  = ph = new WBosAsymHContainer(new TDirectoryFile("asym_wm_geom", "asym_wm_geom", "", fDir), kAsymSqrtGeom);
   fHistCuts[kCUT_EVENT_W_MINUS].insert(ph);
   d["asym_wm_lumi"]  = ph = new WBosAsymHContainer(new TDirectoryFile("asym_wm_lumi", "asym_wm_lumi", "", fDir), kAsymSqrtLumi);
   fHistCuts[kCUT_EVENT_W_MINUS].insert(ph);

   d["asym_wlm"]      = ph = new WLepAsymHContainer(new TDirectoryFile("asym_wlm",      "asym_wlm", "", fDir));
   fHistCuts[kCUT_EVENT_W_MINUS].insert(ph);
   d["asym_wlm_phys"] = ph = new WLepAsymHContainer(new TDirectoryFile("asym_wlm_phys", "asym_wlm_phys", "", fDir), kAsymSqrtPhys);
   fHistCuts[kCUT_EVENT_W_MINUS].insert(ph);
   d["asym_wlm_geom"] = ph = new WLepAsymHContainer(new TDirectoryFile("asym_wlm_geom", "asym_wlm_geom", "", fDir), kAsymSqrtGeom);
   fHistCuts[kCUT_EVENT_W_MINUS].insert(ph);
   d["asym_wlm_lumi"] = ph = new WLepAsymHContainer(new TDirectoryFile("asym_wlm_lumi", "asym_wlm_lumi", "", fDir), kAsymSqrtLumi);
   fHistCuts[kCUT_EVENT_W_MINUS].insert(ph);

   d["asym_z"]       = ph = new Z0AsymHContainer(new TDirectoryFile("asym_z",      "asym_z", "", fDir));
   fHistCuts[kCUT_EVENT_Z].insert(ph);
   d["asym_z_phys"]  = ph = new Z0AsymHContainer(new TDirectoryFile("asym_z_phys", "asym_z_phys", "", fDir), kAsymSqrtPhys);
   fHistCuts[kCUT_EVENT_Z].insert(ph);
   d["asym_z_geom"]  = ph = new Z0AsymHContainer(new TDirectoryFile("asym_z_geom", "asym_z_geom", "", fDir), kAsymSqrtGeom);
   fHistCuts[kCUT_EVENT_Z].insert(ph);
   d["asym_z_lumi"]  = ph = new Z0AsymHContainer(new TDirectoryFile("asym_z_lumi", "asym_z_lumi", "", fDir), kAsymSqrtLumi);
   fHistCuts[kCUT_EVENT_Z].insert(ph);
}


/** */
void AllAsymHContainer::Fill(ProtoEvent &ev)
{
   if (ev.InheritsFrom("WBosEvent"))
   {
      WBosEvent& w_event = (WBosEvent&) ev;

      Fill(ev, kCUT_EVENT_W);

      if ( w_event.PassedCutWBosPlus(WBosEvent::sMinElectronPtHard) )
         Fill(ev, kCUT_EVENT_W_PLUS);

      if ( w_event.PassedCutWBosMinus(WBosEvent::sMinElectronPtHard) )
         Fill(ev, kCUT_EVENT_W_MINUS);

   } else if (ev.InheritsFrom("ZBosEvent"))
   {
      Fill(ev, kCUT_EVENT_Z);
   }
}


/** */
void AllAsymHContainer::Fill(ProtoEvent &ev, ECut cut)
{
   PlotHelperSet     hists = fHistCuts[cut];
   PlotHelperSetIter hi    = hists.begin();

   for ( ; hi != hists.end(); ++hi) {
      (*hi)->Fill(ev);
   }
}

#include "JetHContainer.h"

#include "TF1.h"

#include "VecBosEvent.h"
#include "VecBosJet.h"


ClassImp(JetHContainer)

using namespace std;


/** Default constructor. */
JetHContainer::JetHContainer() : PlotHelper()
{
   BookHists();
}


JetHContainer::JetHContainer(TDirectory *dir) : PlotHelper(dir)
{
   BookHists();
}


/** */
void JetHContainer::BookHists()
{
   string shName;
   TH1*   hist;

   fDir->cd();

   o["hJetEta"] = hist = new TH1I("hJetEta", "; Jet #eta; Num. of Jets", 60, -3, 3);
   hist->SetOption("hist GRIDX GRIDY");

   o["hJetPhi"] = hist = new TH1I("hJetPhi", "; Jet #phi; Num. of Jets", 60, -M_PI, M_PI);
   hist->SetOption("hist GRIDX GRIDY");

   o["hJetPt"] = hist = new TH1I("hJetPt", "; Jet P_T; Num. of Jets", 60, 0, 60);
   hist->SetOption("hist GRIDX GRIDY XY");

   o["hJetNumTracks"] = hist = new TH1I("hJetNumTracks", "; Num. of Tracks in Jet; Num. of Jets", 20, 0, 20);
   hist->SetOption("hist GRIDX GRIDY");
}


/** */
void JetHContainer::Fill(ProtoEvent &ev)
{
   VecBosEvent& event = (VecBosEvent&) ev;

   Fill(event.mJets);
}


void JetHContainer::Fill(VecBosJetPtrSet &vbJets)
{
   VecBosJetPtrSetIter iJet = vbJets.begin();
   for ( ; iJet!=vbJets.end(); ++iJet)
   {
      Fill(**iJet);
   }
}


/** */
void JetHContainer::Fill(VecBosJet &vbJet)
{
   ((TH1*) o["hJetEta"])->Fill(vbJet.Eta());
   ((TH1*) o["hJetPhi"])->Fill(vbJet.Phi());
   ((TH1*) o["hJetPt"])->Fill(vbJet.Pt());
   ((TH1*) o["hJetNumTracks"])->Fill(vbJet.GetTracks().size());
}

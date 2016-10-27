#include "VecBosRootFile.h"

#include <climits>
#include <sstream>

#include "TROOT.h"
#include "TStyle.h"

#include "Z0AsymHContainer.h"
#include "EventHContainer.h"
#include "Z0EventHContainer.h"
#include "JetHContainer.h"
#include "TrackHContainer.h"
#include "VertexHContainer.h"
#include "KinemaHContainer.h"
#include "MCHContainer.h"
#include "WBosEvent.h"
#include "ZBosEvent.h"
#include "VecBosTrack.h"
#include "VecBosVertex.h"

#include "utils/utils.h"

using namespace std;


VecBosRootFile::VecBosRootFile() : TFile(),
   fHists(0), fHistCuts(),
   fMinFill(UINT_MAX), fMaxFill(0),
   fMinTime(UINT_MAX), fMaxTime(0),
   fIsMc(0), fIsZ(kFALSE)
{
   BookHists();
}


VecBosRootFile::VecBosRootFile(const char *fname, Option_t *option, Int_t isMc, Bool_t isZ, const char *ftitle, Int_t compress) :
   TFile(fname, option, ftitle, compress),
   fHists(0), fHistCuts(),
   fMinFill(UINT_MAX), fMaxFill(0),
   fMinTime(UINT_MAX), fMaxTime(0),
   fIsMc(isMc), fIsZ(isZ)
{
   printf("Created ROOT file: %s\n", GetName());

   BookHists();
}


void VecBosRootFile::BookHists()
{
   // Delete histograms created in parent class
   if (fHists) { delete fHists; fHists = 0; }
   fHistCuts.clear();

   PlotHelper *ph;

   fHists = new PlotHelper(this);

  if (!fIsZ) {

   fHists->d["event"] = ph = new EventHContainer(new TDirectoryFile("event", "event", "", this));
   fHistCuts[kCUT_EVENT_NOCUT].insert(ph);

   fHists->d["event_has_jetrecoil"] = ph = new EventHContainer(new TDirectoryFile("event_has_jetrecoil", "event_has_jetrecoil", "", this));
   fHistCuts[kCUT_EVENT_HAS_JETRECOIL].insert(ph);

   fHists->d["event_has_trackrecoil"] = ph = new EventHContainer(new TDirectoryFile("event_has_trackrecoil", "event_has_trackrecoil", "", this));
   fHistCuts[kCUT_EVENT_HAS_TRACKRECOIL].insert(ph);

   fHists->d["event_has_candidate"] = ph = new EventHContainer(new TDirectoryFile("event_has_candidate", "event_has_candidate", "", this));
   fHistCuts[kCUT_EVENT_HAS_CANDIDATE_TRACK].insert(ph);

   fHists->d["event_has_candidate_pt>15"] = ph = new EventHContainer(new TDirectoryFile("event_has_candidate_pt>15", "event_has_candidate_pt>15", "", this));
   fHistCuts[kCUT_EVENT_HAS_CANDIDATE_TRACK_PT15].insert(ph);

   fHists->d["event_pass_wbos_pt>15"] = ph = new EventHContainer(new TDirectoryFile("event_pass_wbos_pt>15", "event_pass_wbos_pt>15", "", this));
   fHistCuts[kCUT_EVENT_PASS_WBOS_PT15].insert(ph);

   fHists->d["W+_event_pass_wbos_pt>15"] = ph = new EventHContainer(new TDirectoryFile("W+_event_pass_wbos_pt>15", "W+_event_pass_wbos_pt>15", "", this));
   fHistCuts[kCUT_POSITIVE_EVENT_PASS_WBOS_PT15].insert(ph);

   fHists->d["W-_event_pass_wbos_pt>15"] = ph = new EventHContainer(new TDirectoryFile("W-_event_pass_wbos_pt>15", "W-_event_pass_wbos_pt>15", "", this));
   fHistCuts[kCUT_NEGATIVE_EVENT_PASS_WBOS_PT15].insert(ph);

   fHists->d["W+_event_pass_wbos"] = ph = new EventHContainer(new TDirectoryFile("W+_event_pass_wbos", "W+_event_pass_wbos_pt>15", "", this));
   fHistCuts[kCUT_POSITIVE_EVENT_PASS_WBOS].insert(ph);

   fHists->d["W-_event_pass_wbos"] = ph = new EventHContainer(new TDirectoryFile("W-_event_pass_wbos", "W-_event_pass_wbos_pt>15", "", this));
   fHistCuts[kCUT_NEGATIVE_EVENT_PASS_WBOS].insert(ph);

   fHists->d["event_pass_wbos"] = ph = new EventHContainer(new TDirectoryFile("event_pass_wbos", "event_pass_wbos", "", this));
   fHistCuts[kCUT_EVENT_PASS_WBOS].insert(ph);

   fHists->d["event_pass_qcd"]     = ph = new EventHContainer(new TDirectoryFile("event_pass_qcd", "event_pass_qcd", "", this));
   fHistCuts[kCUT_EVENT_PASS_QCD].insert(ph);

   fHists->d["event_pass_qcd_pt>15"]     = ph = new EventHContainer(new TDirectoryFile("event_pass_qcd_pt>15", "event_pass_qcd_pt>15", "", this));
   fHistCuts[kCUT_EVENT_PASS_QCD_PT15].insert(ph);

   fHists->d["W+_event_pass_qcd"]     = ph = new EventHContainer(new TDirectoryFile("W+_event_pass_qcd", "W+_event_pass_qcd", "", this));
   fHistCuts[kCUT_POSITIVE_EVENT_PASS_QCD].insert(ph);

   fHists->d["W+_event_pass_qcd_pt>15"]     = ph = new EventHContainer(new TDirectoryFile("W+_event_pass_qcd_pt>15", "W+_event_pass_qcd_pt>15", "", this));
   fHistCuts[kCUT_POSITIVE_EVENT_PASS_QCD_PT15].insert(ph);

   fHists->d["W-_event_pass_qcd"]     = ph = new EventHContainer(new TDirectoryFile("W-_event_pass_qcd", "W-_event_pass_qcd", "", this));
   fHistCuts[kCUT_NEGATIVE_EVENT_PASS_QCD].insert(ph);

   fHists->d["W-_event_pass_qcd_pt>15"]     = ph = new EventHContainer(new TDirectoryFile("W-_event_pass_qcd_pt>15", "W-_event_pass_qcd_pt>15", "", this));
   fHistCuts[kCUT_NEGATIVE_EVENT_PASS_QCD_PT15].insert(ph);

   fHists->d["event_has_jetrecoil_pass_wbos"] = ph = new EventHContainer(new TDirectoryFile("event_has_jetrecoil_pass_wbos", "event_has_jetrecoil_pass_wbos", "", this));
   fHistCuts[kCUT_EVENT_PASS_WBOS].insert(ph);

   fHists->d["event_jets"] = ph = new JetHContainer(new TDirectoryFile("event_jets", "event_jets", "", this));
   fHistCuts[kCUT_EVENT_NOCUT].insert(ph);

   fHists->d["event_jets_has_jetrecoil"] = ph = new JetHContainer(new TDirectoryFile("event_jets_has_jetrecoil", "event_jets_has_jetrecoil", "", this));
   fHistCuts[kCUT_EVENT_HAS_JETRECOIL].insert(ph);

   fHists->d["event_vertices"] = ph = new VertexHContainer(new TDirectoryFile("event_vertices", "event_vertices", "", this));
   fHistCuts[kCUT_EVENT_NOCUT].insert(ph);

   fHists->d["event_tracks"] = ph = new TrackHContainer(new TDirectoryFile("event_tracks", "event_tracks", "", this));
   fHistCuts[kCUT_EVENT_NOCUT].insert(ph);

   fHists->d["event_tracks_has_candidate"] = ph = new TrackHContainer(new TDirectoryFile("event_tracks_has_candidate", "event_tracks_has_candidate", "", this));
   fHistCuts[kCUT_EVENT_HAS_CANDIDATE_TRACK].insert(ph);

   fHists->d["event_tracks_pass_wbos"] = ph = new TrackHContainer(new TDirectoryFile("event_tracks_pass_wbos", "event_tracks_pass_wbos", "", this));
   fHistCuts[kCUT_EVENT_PASS_WBOS].insert(ph);

   fHists->d["vertex"]           = ph = new VertexHContainer(new TDirectoryFile("vertex", "vertex", "", this));

   fHists->d["vertex_good"]      = ph = new VertexHContainer(new TDirectoryFile("vertex_good", "vertex_good", "", this));

   fHists->d["track"]            = ph = new TrackHContainer(new TDirectoryFile("track", "track", "", this));

   fHists->d["track_candidates"] = ph = new TrackHContainer(new TDirectoryFile("track_candidates", "track_candidates", "", this));

   fHists->d["track_cand_pass_wbos"] = ph = new TrackHContainer(new TDirectoryFile("track_cand_pass_wbos", "track_cand_pass_wbos", "", this));

   fHists->d["track_cand_pass_wbos_Pt>15"] = ph = new TrackHContainer(new TDirectoryFile("track_cand_pass_wbos_Pt>15", "track_cand_pass_wbos_Pt>15", "", this));

   fHists->d["W+_track_cand_pass_wbos"] = ph = new TrackHContainer(new TDirectoryFile("W+_track_cand_pass_wbos", "W+_track_cand_pass_wbos", "", this));

   fHists->d["W-_track_cand_pass_wbos"] = ph = new TrackHContainer(new TDirectoryFile("W-_track_cand_pass_wbos", "W-_track_cand_pass_wbos", "", this));

   fHists->d["track_cand_pass_qcd"] = ph = new TrackHContainer(new TDirectoryFile("track_cand_pass_qcd", "track_cand_pass_qcd", "", this));

   fHists->d["track_cand_pass_qcd_Pt>15"] = ph = new TrackHContainer(new TDirectoryFile("track_cand_pass_qcd_Pt>15", "track_cand_pass_qcd_Pt>15", "", this));

   fHists->d["W+_track_cand_pass_qcd"] = ph = new TrackHContainer(new TDirectoryFile("W+_track_cand_pass_qcd", "W+_track_cand_pass_qcd", "", this));

   fHists->d["W+_track_cand_pass_qcd_Pt>15"] = ph = new TrackHContainer(new TDirectoryFile("W+_track_cand_pass_qcd_Pt>15", "W+_track_cand_pass_qcd_Pt>15", "", this));

   fHists->d["W-_track_cand_pass_qcd"] = ph = new TrackHContainer(new TDirectoryFile("W-_track_cand_pass_qcd", "W-_track_cand_pass_qcd", "", this));

   fHists->d["W-_track_cand_pass_qcd_Pt>15"] = ph = new TrackHContainer(new TDirectoryFile("W-_track_cand_pass_qcd_Pt>15", "W-_track_cand_pass_qcd_Pt>15", "", this));


   if (!fIsMc) return;

   fHists->d["event_mc"] = ph = new MCHContainer(new TDirectoryFile("event_mc", "event_mc", "", this), fIsMc);
   fHistCuts[kCUT_EVENT_NOCUT].insert(ph);

   fHists->d["event_mc_has_jetrecoil"] = ph = new MCHContainer(new TDirectoryFile("event_mc_has_jetrecoil", "event_mc_has_jetrecoil", "", this), fIsMc);
   fHistCuts[kCUT_EVENT_HAS_JETRECOIL].insert(ph);

   fHists->d["event_mc_has_trackrecoil"] = ph = new MCHContainer(new TDirectoryFile("event_mc_has_trackrecoil", "event_mc_has_trackrecoil", "", this), fIsMc);
   fHistCuts[kCUT_EVENT_HAS_TRACKRECOIL].insert(ph);

   fHists->d["event_mc_pass_wbos"] = ph = new MCHContainer(new TDirectoryFile("event_mc_pass_wbos", "event_mc_pass_wbos", "", this), fIsMc);
   fHistCuts[kCUT_EVENT_PASS_WBOS].insert(ph);

  } else {

   fHists->d["event"] = ph = new Z0EventHContainer(new TDirectoryFile("event", "event", "", this));
   fHistCuts[kCUT_EVENT_NOCUT].insert(ph);

   fHists->d["Z0_event_pass_elePt"]     = ph = new Z0EventHContainer(new TDirectoryFile("Z0_event_pass_elePt", "Z0_event_pass_elePt", "", this));
   fHistCuts[kCUT_EVENT_PASS_Z0_ELECTRONPT].insert(ph);

   fHists->d["Z0_event_pass_final"]     = ph = new Z0EventHContainer(new TDirectoryFile("Z0_event_pass_final", "Z0_event_pass_final", "", this));
   fHistCuts[kCUT_EVENT_PASS_Z0_FINAL].insert(ph);

   fHists->d["asym_z"] = ph = new Z0AsymHContainer(new TDirectoryFile("asym_z", "asym_z", "", this));
   fHistCuts[kCUT_EVENT_PASS_Z0_FINAL].insert(ph);

   fHists->d["asym_z_phys"] = ph = new Z0AsymHContainer(new TDirectoryFile("asym_z_phys", "asym_z_phys", "", this), kAsymSqrtPhys);
   fHistCuts[kCUT_EVENT_PASS_Z0_FINAL].insert(ph);

   fHists->d["asym_z_geom"] = ph = new Z0AsymHContainer(new TDirectoryFile("asym_z_geom", "asym_z_geom", "", this), kAsymSqrtGeom);
   fHistCuts[kCUT_EVENT_PASS_Z0_FINAL].insert(ph);

   fHists->d["asym_z_lumi"] = ph = new Z0AsymHContainer(new TDirectoryFile("asym_z_lumi", "asym_z_lumi", "", this), kAsymSqrtLumi);
   fHistCuts[kCUT_EVENT_PASS_Z0_FINAL].insert(ph);

  }

   this->cd();
}


PlotHelper *VecBosRootFile::GetHists() { return fHists; }
void VecBosRootFile::SetHists(PlotHelper &hists) { fHists = &hists; }


/** */
void VecBosRootFile::Fill(ProtoEvent &ev)
{
  if (!fIsZ) {

    WBosEvent& event = (WBosEvent&) ev;

   Fill(ev, kCUT_EVENT_NOCUT);

     if ( event.HasJetRecoil() ) {
        Fill(ev, kCUT_EVENT_HAS_JETRECOIL);
        //((EventHContainer*) fHists->d["event_has_jetrecoil"])->Fill(ev);

        if ( event.PassedCutWBos() )
           ((EventHContainer *) fHists->d["event_has_jetrecoil_pass_wbos"])->Fill(ev);
           //((JetHContainer*) fHists->d["event_jets_has_jetrecoil_pass_final"])->Fill(ev);
     }

     if ( event.mP3TrackRecoilTpcNeutrals.Mag() > 0)
        Fill(ev, kCUT_EVENT_HAS_TRACKRECOIL);

     if ( event.HasCandidateEle() )
        Fill(ev, kCUT_EVENT_HAS_CANDIDATE_TRACK);

     if ( event.HasCandidateEle() && event.GetElectronP3().Pt() > 15)
        Fill(ev, kCUT_EVENT_HAS_CANDIDATE_TRACK_PT15);

     if ( event.PassedCutWBos(WBosEvent::sMinElectronPtLight) )
        Fill(ev, kCUT_EVENT_PASS_WBOS_PT15);

     if ( event.PassedCutWBos(WBosEvent::sMinElectronPtHard) )
        Fill(ev, kCUT_EVENT_PASS_WBOS);

     if ( event.PassedCutWBosPlus(WBosEvent::sMinElectronPtLight) ) 
        Fill(ev, kCUT_POSITIVE_EVENT_PASS_WBOS_PT15);

     if ( event.PassedCutWBosMinus(WBosEvent::sMinElectronPtLight) ) 
        Fill(ev, kCUT_NEGATIVE_EVENT_PASS_WBOS_PT15);

     if ( event.PassedCutWBosPlus(WBosEvent::sMinElectronPtHard) ) 
        Fill(ev, kCUT_POSITIVE_EVENT_PASS_WBOS);

     if ( event.PassedCutWBosMinus(WBosEvent::sMinElectronPtHard) ) 
        Fill(ev, kCUT_NEGATIVE_EVENT_PASS_WBOS);


   // Fill vertex histos
   VecBosVertexPtrSetIter iVertex = event.mVertices.begin();
   for ( ; iVertex != event.mVertices.end(); ++iVertex)
   {
      VecBosVertex &vertex = **iVertex;

      ((VertexHContainer *) fHists->d["vertex"])->Fill(vertex);

      if ( !vertex.IsGood() ) continue;

      ((VertexHContainer *) fHists->d["vertex_good"])->Fill(vertex);
   }


   // Fill track histos
   VecBosTrackPtrSetConstIter iTrack = event.mTracks.begin();
   for ( ; iTrack != event.mTracks.end(); ++iTrack)
   {
      VecBosTrack &track = **iTrack;

      ((TrackHContainer *) fHists->d["track"])->Fill(track);

      if ( !track.IsCandidate() ) continue;

      ((TrackHContainer *) fHists->d["track_candidates"])->Fill(track);

      // Select QCD with Pt>15
      if ( event.PassedCutQcdBkg(WBosEvent::sMinElectronPtLight) )
      {
            Fill(ev, kCUT_EVENT_PASS_QCD_PT15);

            ((TrackHContainer *) fHists->d["track_cand_pass_qcd_Pt>15"])->Fill(track);

            if ( track.mStMuTrack->charge() > 0.){
               Fill(ev, kCUT_POSITIVE_EVENT_PASS_QCD_PT15);
               ((TrackHContainer *) fHists->d["W+_track_cand_pass_qcd_Pt>15"])->Fill(track);
            }

            if ( track.mStMuTrack->charge() < 0.){
               Fill(ev, kCUT_NEGATIVE_EVENT_PASS_QCD_PT15);
               ((TrackHContainer *) fHists->d["W-_track_cand_pass_qcd_Pt>15"])->Fill(track);
            }
      }

      // Select QCD events
      if ( event.PassedCutQcdBkg(WBosEvent::sMinElectronPtHard) )
      {
            Fill(ev, kCUT_EVENT_PASS_QCD);

            ((TrackHContainer *) fHists->d["track_cand_pass_qcd"])->Fill(track);

            if ( track.mStMuTrack->charge() > 0.){
               Fill(ev, kCUT_POSITIVE_EVENT_PASS_QCD);
               ((TrackHContainer *) fHists->d["W+_track_cand_pass_qcd"])->Fill(track);
            }

            if ( track.mStMuTrack->charge() < 0.){
               Fill(ev, kCUT_NEGATIVE_EVENT_PASS_QCD);
               ((TrackHContainer *) fHists->d["W-_track_cand_pass_qcd"])->Fill(track);
            }
      } // end QCD events


      if ( event.PassedCutWBos(WBosEvent::sMinElectronPtHard) ) {

	//Fill(ev, kCUT_EVENT_PASS_WBOS);

            ((TrackHContainer *) fHists->d["track_cand_pass_wbos"])->Fill(track);

            if ( event.PassedCutWBosPlus(WBosEvent::sMinElectronPtHard) ) {
	      //Fill(ev, kCUT_POSITIVE_EVENT_PASS_WBOS);
               ((TrackHContainer *) fHists->d["W+_track_cand_pass_wbos"])->Fill(track);
            }

            if ( event.PassedCutWBosMinus(WBosEvent::sMinElectronPtHard) ) {
	      //Fill(ev, kCUT_NEGATIVE_EVENT_PASS_WBOS);
               ((TrackHContainer *) fHists->d["W-_track_cand_pass_wbos"])->Fill(track);
            }
      }
   }

  } else {

   ZBosEvent& event = (ZBosEvent&) ev;

   Fill(ev, kCUT_EVENT_NOCUT);
     
     if ( event.PassedCutZBos(ZBosEvent::sMinZEleCandPtHard) ) {
        Fill(ev, kCUT_EVENT_PASS_Z0_ELECTRONPT);
     }
     
     if ( event.PassedCutZMass(ZBosEvent::sMinZEleCandPtHard) ) {
        Fill(ev, kCUT_EVENT_PASS_Z0_FINAL);
     }

  }

}


/** */
void VecBosRootFile::Fill(ProtoEvent &ev, ECut cut)
{
   PlotHelperSet     hists = fHistCuts[cut];
   PlotHelperSetIter hi    = hists.begin();

   for ( ; hi != hists.end(); ++hi) {
      (*hi)->Fill(ev);
   }
}


/** */
void VecBosRootFile::FillDerived()
{
   Info("FillDerived", "Called");

   fHists->FillDerived();
}


/** */
void VecBosRootFile::PostFill()
{
   Info("PostFill", "Called");

   fHists->PostFill();
}


/** */
void VecBosRootFile::SaveAs(string pattern, string dir)
{
   TCanvas canvas("canvas", "canvas", 1000, 600);
   canvas.UseCurrentStyle();

   stringstream ssSignature("");

   char strAnaTime[25];
   time_t currentTime = time(0);
   tm *ltime = localtime( &currentTime );
   strftime(strAnaTime, 25, "%c", ltime);

   //ssSignature << "Fills " << fMinFill << "--" << fMaxFill << ", Analyzed " << strAnaTime;
   //ssSignature << ", Version " << fAnaInfo->fAsymVersion << ", " << fAnaInfo->fUserGroup.fUser;

   fHists->SetSignature(ssSignature.str());
   fHists->SaveAllAs(canvas, pattern, dir);
}


/** */
void VecBosRootFile::UpdMinMaxFill(UInt_t fillId)
{
   if (fillId < fMinFill ) fMinFill = fillId;
   if (fillId > fMaxFill ) fMaxFill = fillId;
}


/** */
void VecBosRootFile::UpdMinMaxTime(time_t time)
{
   if (time < fMinTime ) fMinTime = time;
   if (time > fMaxTime ) fMaxTime = time;
}


/** */
void VecBosRootFile::Print(const Option_t *opt) const
{
   Info("Print", "Called");
   //PrintAsPhp(fFilePhp);
}


/** */
void VecBosRootFile::PrintAsPhp(FILE *f) const
{
   fprintf(f, "<?php\n");

   fprintf(f, "\n// AnaGlobResult data\n");
   //if (!fAnaGlobResult) {
   //   Error("PrintAsPhp", "fAnaGlobResult not defined");
   //} else {
   //   fAnaGlobResult->PrintAsPhp(f);
   //}

   fprintf(f, "?>\n");
}


/** */
void VecBosRootFile::Close(Option_t *option)
{
   fHists->Write();
   Info("Close", "%s", GetName());
   TFile::Close(option);
}

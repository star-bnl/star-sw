#ifndef QUALITY_PLOTTER_H
#define QUALITY_PLOTTER_H

#include <map>
#include <string>

#include "TH1.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TH3F.h"
#include "TVector3.h"

#include "GenFit/FitStatus.h"

#include "StFwdTrackMaker/FwdTrackerConfig.h"
#include "StFwdTrackMaker/Common.h"
#include "StFwdTrackMaker/include/Tracker/FwdHit.h"


class QualityPlotter {
  public:
    QualityPlotter(FwdTrackerConfig &_cfg) : cfg(_cfg) {
    }

    void makeHistograms(size_t maxI) {
        using namespace std;
        maxIterations = maxI;
        hist["FinalEff"] = new TH1F("FinalEff", ";Eff", 100, 0, 2.0);
        hist["FinalN7Eff"] = new TH1F("FinalN7Eff", ";Eff", 100, 0, 2.0);
        hist["FinalN6Eff"] = new TH1F("FinalN6Eff", ";Eff", 100, 0, 2.0);
        hist["FinalN5Eff"] = new TH1F("FinalN5Eff", ";Eff", 100, 0, 2.0);
        hist["FinalN4Eff"] = new TH1F("FinalN4Eff", ";Eff", 100, 0, 2.0);
        hist["FinalN7Quality"] = new TH1F("FinalN7Quality", ";Quality", 150, 0, 1.5);
        hist["AllQuality"] = new TH1F("AllQuality", ";Quality", 150, 0, 1.5);
        hist["DurationPerEvent"] = new TH1F("DurationPerEvent", ";Duration(ms) per Event", 1e5, 0, 1e5);

        hist["nHitsOnTrack"] = new TH1F("nHitsOnTrack", ";nHit", 10, 0, 10);
        hist["nHitsOnTrackMc"] = new TH1F("nHitsOnTrackMc", ";nHit", 10, 0, 10);

        hist["InvPtRes"] = new TH1F("InvPtRes", ";(p_{T}^{MC} - p_{T}^{RC}) / p_{T}^{MC}", 500, -5, 5);
        hist["InvPtResVsNHits"] = new TH2F("InvPtResVsNHits", ";(p_{T}^{MC} - p_{T}^{RC}) / p_{T}^{MC}", 10, 0, 10, 500, -5, 5);
        hist["PtRes"] = new TH1F("PtRes", ";(p_{T}^{MC} - p_{T}^{RC}) / p_{T}^{MC}", 500, -5, 5);
        hist["PtResVsTrue"] = new TH2F("PtResVsTrue", ";q^{MC} #times p_{T}^{MC};(p_{T}^{MC} - p_{T}^{RC}) / p_{T}^{MC}", 100, -5, 5, 200, -2, 2);
        hist["InvPtResVsTrue"] = new TH2F("InvPtResVsTrue", ";q^{MC} #times p_{T}^{MC}; #sigma_{p_{T}^{-1}}", 100, -5, 5, 200, -2, 2);
        hist["DeltaPt"] = new TH1F("DeltaPt", ";p_{T}^{RC} - p_{T}^{MC} (GeV/c)", 500, -5, 5);
        hist["InvPtResVsEta"] = new TH2F("InvPtResVsEta", ";#eta^{MC}; #sigma_{p_{T}^{-1}}", 300, 2, 5, 200, -2, 2);

        hist["RecoPtVsMcPt"] = new TH2F("RecoPtVsMcPt", "; p_{T}^{MC}; p_{T}^{RC}", 100, 0, 5, 100, 0, 5);
        hist["QMatrix"] = new TH2F("QMatrix", ";GEN;RECO", 6, -3, 3, 6, -3, 3);
        hist["FitPValue"] = new TH1F("FitPValue", ";", 1000, 0, 10);
        hist["FitChi2"] = new TH1F("FitChi2", ";", 2000, 0, 200);
        hist["FitChi2Ndf"] = new TH1F("FitChi2Ndf", ";", 1500, 0, 150);
        hist["FitNFailedHits"] = new TH1F("FitNFailedHits", ";", 10, 0, 10);

        hist["RightQVsMcPt"] = new TH1F("RightQVsMcPt", ";p_{T}^{GEN}; N", 100, -5, 5);
        hist["WrongQVsMcPt"] = new TH1F("WrongQVsMcPt", ";p_{T}^{GEN}; N", 100, -5, 5);
        hist["AllQVsMcPt"] = new TH1F("AllQVsMcPt", ";p_{T}^{GEN}; N", 100, -5, 5);

        hist["McPt"] = new TH1F("McPt", ";p_{T}^{MC} (GeV/c)", 100, 0, 10);
        hist["McPt_4hits"] = new TH1F("McPt_4hits", ";p_{T}^{MC} (GeV/c)", 100, 0, 10);
        hist["McPt_5hits"] = new TH1F("McPt_5hits", ";p_{T}^{MC} (GeV/c)", 100, 0, 10);
        hist["McPt_6hits"] = new TH1F("McPt_6hits", ";p_{T}^{MC} (GeV/c)", 100, 0, 10);
        hist["McPt_7hits"] = new TH1F("McPt_7hits", ";p_{T}^{MC} (GeV/c)", 100, 0, 10);
        hist["McPtFound"] = new TH1F("McPtFound", ";p_{T}^{MC} (GeV/c)", 100, 0, 10);
        hist["McPtFoundAllQ"] = new TH1F("McPtFoundAllQ", ";p_{T}^{MC} (GeV/c)", 100, 0, 10);

        hist["McEta"] = new TH1F("McEta", ";#eta^{MC} (GeV/c)", 100, 2, 5);
        hist["McEta_4hits"] = new TH1F("McEta_4hits", ";#eta^{MC} (GeV/c)", 100, 2, 5);
        hist["McEta_5hits"] = new TH1F("McEta_5hits", ";#eta^{MC} (GeV/c)", 100, 2, 5);
        hist["McEta_6hits"] = new TH1F("McEta_6hits", ";#eta^{MC} (GeV/c)", 100, 2, 5);
        hist["McEta_7hits"] = new TH1F("McEta_7hits", ";#eta^{MC} (GeV/c)", 100, 2, 5);
        hist["McEtaFound"] = new TH1F("McEtaFound", ";#eta^{MC} (GeV/c)", 100, 2, 5);
        hist["McEtaFoundAllQ"] = new TH1F("McEtaFoundAllQ", ";#eta^{MC} (GeV/c)", 100, 2, 5);

        hist["McPhi"] = new TH1F("McPhi", ";#phi^{MC} (GeV/c)", 128, -3.2, 3.2);
        hist["McPhi_4hits"] = new TH1F("McPhi_4hits", ";#phi^{MC} (GeV/c)", 128, -3.2, 3.2);
        hist["McPhi_5hits"] = new TH1F("McPhi_5hits", ";#phi^{MC} (GeV/c)", 128, -3.2, 3.2);
        hist["McPhi_6hits"] = new TH1F("McPhi_6hits", ";#phi^{MC} (GeV/c)", 128, -3.2, 3.2);
        hist["McPhi_7hits"] = new TH1F("McPhi_7hits", ";#phi^{MC} (GeV/c)", 128, -3.2, 3.2);
        hist["McPhiFound"] = new TH1F("McPhiFound", ";#phi^{MC} (GeV/c)", 128, -3.2, 3.2);
        hist["McPhiFoundAllQ"] = new TH1F("McPhiFoundAllQ", ";#phi^{MC} (GeV/c)", 128, -3.2, 3.2);
        hist["nFailedFits"] = new TH1F("nFailedFits", ";;nFailedFits", 5, 0, 5);

        // 2D efficiency
        hist["McPtPhiFound"] = new TH2F("McPtPhiFound", "; p_{T}^{MC} (GeV/c);#phi^{MC} (GeV/c)", 20, 0, 10, 64, -3.2, 3.2);
        hist["McPtPhiFoundAllQ"] = new TH2F("McPtPhiFoundAllQ", "; p_{T}^{MC} (GeV/c);#phi^{MC} (GeV/c)", 20, 0, 10, 64, -3.2, 3.2);
        hist["McPtPhi_4hits"] = new TH2F("McPtPhi_4hits", "; p_{T}^{MC} (GeV/c);#phi^{MC} (GeV/c)", 20, 0, 10, 64, -3.2, 3.2);

        // 3D Efficiency
        hist["McPtEtaPhiFound"] = new TH3F("McPtEtaPhiFound", "; p_{T}^{MC} (GeV/c); #eta; #phi^{MC} (GeV/c)", 50, 0, 5, 30, 2, 5, 64, -3.2, 3.2);
        hist["McPtEtaPhiFoundAllQ"] = new TH3F("McPtEtaPhiFoundAllQ", "; p_{T}^{MC} (GeV/c); #eta; #phi^{MC} (GeV/c)", 50, 0, 5, 30, 2, 5, 64, -3.2, 3.2);
        hist["McPtEtaPhi_4hits"] = new TH3F("McPtEtaPhi_4hits", "; p_{T}^{MC} (GeV/c); #eta; #phi^{MC} (GeV/c)", 50, 0, 5, 30, 2, 5, 64, -3.2, 3.2);

        // for ( size_t track_len : { 4, 5, 6, 7 } )
        {
            size_t track_len = 4;
            // only Q > 0.9
            string n = TString::Format("McPtFound%u", track_len).Data();
            hist[n] = new TH1F(n.c_str(), ";p_{T}^{MC} (GeV/c)", 100, 0, 10);
            n = TString::Format("McEtaFound%u", track_len).Data();
            hist[n] = new TH1F(n.c_str(), ";#eta^{MC} (GeV/c)", 100, 2, 5);
            n = TString::Format("McPhiFound%u", track_len).Data();
            hist[n] = new TH1F(n.c_str(), ";#phi^{MC} (GeV/c)", 128, -3.2, 3.2);

            n = TString::Format("McPtPhiFound%u", track_len).Data();
            hist[n] = new TH2F(n.c_str(), "; p_{T}^{MC} (GeV/c);#phi^{MC} (GeV/c)", 20, 0, 10, 64, -3.2, 3.2);

            n = TString::Format("McPtEtaPhiFound%u", track_len).Data();
            hist[n] = new TH3F(n.c_str(), "; p_{T}^{MC} (GeV/c); #eta; #phi^{MC} (GeV/c)", 50, 0, 5, 30, 2, 5, 64, -3.2, 3.2);

            // all Q versions
            n = TString::Format("McPtFound%uAllQ", track_len).Data();
            hist[n] = new TH1F(n.c_str(), ";p_{T}^{MC} (GeV/c)", 100, 0, 10);
            n = TString::Format("McEtaFound%uAllQ", track_len).Data();
            hist[n] = new TH1F(n.c_str(), ";#eta^{MC} (GeV/c)", 100, 2, 5);
            n = TString::Format("McPhiFound%uAllQ", track_len).Data();
            hist[n] = new TH1F(n.c_str(), ";#phi^{MC} (GeV/c)", 128, -3.2, 3.2);

            n = TString::Format("McPtPhiFound%uAllQ", track_len).Data();
            hist[n] = new TH2F(n.c_str(), "; p_{T}^{MC} (GeV/c);#phi^{MC} (GeV/c)", 20, 0, 10, 64, -3.2, 3.2);

            n = TString::Format("McPtEtaPhiFound%uAllQ", track_len).Data();
            hist[n] = new TH3F(n.c_str(), "; p_{T}^{MC} (GeV/c); #eta; #phi^{MC} (GeV/c)", 50, 0, 5, 30, 2, 5, 64, -3.2, 3.2);
        }

        hist["McHitMap"] = new TH1F("McHitMap", ";VID", 15, 0, 15);

        for (size_t i = 0; i < 15; i++) { // hack to prevent crash...
            string n = "McHitMapLayer" + to_string(i);
            hist[n] = new TH2F(n.c_str(), ("Layer " + to_string(i) + ";x;y").c_str(), 200, 100, 100, 200, 100, 100);
        }

        for (size_t i = 0; i < maxIterations; i++) {
            string n = "NTracksAfter" + to_string(i);
            hist[n] = new TH1F(n.c_str(), ("N Tracks After Iteration" + to_string(i)).c_str(), 500, 0, 500);

            n = "RunningFractionFoundVsIt" + to_string(i);
            hist[n] = new TH1F(n.c_str(), (";Found ( All It <= " + to_string(i) + " ) / Total Found").c_str(), 110, 0, 1.1);

            n = "FractionFoundVsIt" + to_string(i);
            hist[n] = new TH1F(n.c_str(), (";Found ( It = " + to_string(i) + " ) / Total Found").c_str(), 110, 0, 1.1);

            n = "DurationIt" + to_string(i);
            hist[n] = new TH1F(n.c_str(), (";Duration(ms) for Iteration " + to_string(i)).c_str(), 1e5, 0, 1e5);
        }
    }

    void writeHistograms() {
        for (auto nh : hist) {
            nh.second->SetDirectory(gDirectory);
            nh.second->Write();
        }
    }

    TH1 *get(std::string hn) {
        if (hist.count(hn) > 0)
            return hist[hn];
        return nullptr; //careful
    }

    void startIteration() {
        // start the timer
        itStart = FwdTrackerUtils::nowNanoSecond();
    }
    void afterIteration(size_t iteration, std::vector<Seed_t> acceptedTracks) {

        size_t nTracks = acceptedTracks.size();
        nTracksAfterIteration.push_back(nTracks); // assume that we call the iterations in order

        long long itEnd = FwdTrackerUtils::nowNanoSecond();
        long long duration = (itEnd - itStart) * 1e-6; // milliseconds
        this->get("DurationIt" + to_string(iteration))->Fill(duration);
    }

    void startEvent() {
        eventStart = FwdTrackerUtils::nowNanoSecond();
    }
    void summarizeEvent(std::vector<Seed_t> foundTracks, std::map<int, shared_ptr<McTrack>> &mcTrackMap, std::vector<TVector3> fitMoms, std::vector<genfit::FitStatus> fitStatus) {

        using namespace std;

        long long duration = (FwdTrackerUtils::nowNanoSecond() - eventStart) * 1e-6; // milliseconds
        this->get("DurationPerEvent")->Fill(duration);

        // make a map of the number of tracks found for each # of hits
        map<size_t, size_t> tracks_found_by_nHits;

        for (auto t : foundTracks) {
            tracks_found_by_nHits[t.size()]++;
        }

        for (size_t i = 0; i < 9; i++) {
            if (tracks_found_by_nHits.count(i) > 0)
                this->get("nHitsOnTrack")->Fill(i, tracks_found_by_nHits[i]);
        }

        // the total number of tracks found (all nHits)
        size_t nTotal = foundTracks.size();

        // Now compute fraction of tracks found vs. iterations
        float runningFrac = 0;

        for (size_t i = 0; i < maxIterations; i++) {
            if (nTracksAfterIteration.size() < i + 1) {
                break;
            }

            float frac = (float)nTracksAfterIteration[i] / (float)nTotal;
            this->get("FractionFoundVsIt" + to_string(i))->Fill(frac);
            runningFrac += frac;
            this->get("RunningFractionFoundVsIt" + to_string(i))->Fill(runningFrac);
        }

        // fill McInfo
        for (auto kv : mcTrackMap) {

            if (kv.second == nullptr)
                continue;

            this->get("nHitsOnTrackMc")->Fill(kv.second->mHits.size());
            this->get("McPt")->Fill(kv.second->mPt);
            this->get("McEta")->Fill(kv.second->mEta);
            this->get("McPhi")->Fill(kv.second->mPhi);

            if (kv.second->mHits.size() >= 4) {
                this->get("McPt_4hits")->Fill(kv.second->mPt);
                this->get("McEta_4hits")->Fill(kv.second->mEta);
                this->get("McPhi_4hits")->Fill(kv.second->mPhi);

                this->get("McPtPhi_4hits")->Fill(kv.second->mPt, kv.second->mPhi);
                ((TH3 *)this->get("McPtEtaPhi_4hits"))->Fill(kv.second->mPt, kv.second->mEta, kv.second->mPhi);
            }

            if (kv.second->mHits.size() >= 5) {
                this->get("McPt_5hits")->Fill(kv.second->mPt);
                this->get("McEta_5hits")->Fill(kv.second->mEta);
                this->get("McPhi_5hits")->Fill(kv.second->mPhi);
            }

            if (kv.second->mHits.size() >= 6) {
                this->get("McPt_6hits")->Fill(kv.second->mPt);
                this->get("McEta_6hits")->Fill(kv.second->mEta);
                this->get("McPhi_6hits")->Fill(kv.second->mPhi);
            }

            if (kv.second->mHits.size() >= 7) {
                this->get("McPt_7hits")->Fill(kv.second->mPt);
                this->get("McEta_7hits")->Fill(kv.second->mEta);
                this->get("McPhi_7hits")->Fill(kv.second->mPhi);
            }

            for (auto h : kv.second->mHits) {
                auto fh = static_cast<FwdHit *>(h);
                this->get("McHitMap")->Fill(abs(fh->_vid));
                std::string n = "McHitMapLayer" + std::to_string(fh->getLayer());
                this->get(n)->Fill(fh->getX(), fh->getY());
            }
        }

        float avgQuality = 0;
        // calculate quality of tracks
        size_t track_index = 0;

        for (auto t : foundTracks) {

            map<int, float> qual_map;

            for (auto hit : t) {
                qual_map[static_cast<FwdHit *>(hit)->_tid]++;
            }

            for (auto &kv : qual_map) {
                kv.second = kv.second / t.size();
            }

            // now get the quality corresponding to most hits on single track
            // we need to remeber the key also, so we can lookup a hit which corresponds to maxq - to retrieve track info
            float quality = 0;
            int mctid = -1;

            for (auto kv : qual_map) {
                if (kv.second >= quality) {
                    quality = kv.second;
                    mctid = kv.first;
                }
            }

            avgQuality += quality;
            this->get("AllQuality")->Fill(quality);

            if (mctid > 0 && quality >= 3.0 / 4.0 - 0.001) {
                this->get("McPtFoundAllQ")->Fill(mcTrackMap[mctid]->mPt);
                this->get("McEtaFoundAllQ")->Fill(mcTrackMap[mctid]->mEta);
                this->get("McPhiFoundAllQ")->Fill(mcTrackMap[mctid]->mPhi);

                // for ( size_t min_track_len : { 4, 5, 6, 7 } )
                {
                    size_t min_track_len = 4;
                    if (t.size() >= min_track_len) {
                        this->get(TString::Format("McPtPhiFound%uAllQ", min_track_len).Data())->Fill(mcTrackMap[mctid]->mPt, mcTrackMap[mctid]->mPhi);
                        ((TH3 *)this->get(TString::Format("McPtEtaPhiFound%uAllQ", min_track_len).Data()))->Fill(mcTrackMap[mctid]->mPt, mcTrackMap[mctid]->mEta, mcTrackMap[mctid]->mPhi);
                        this->get(TString::Format("McPtFound%uAllQ", min_track_len).Data())->Fill(mcTrackMap[mctid]->mPt);
                        this->get(TString::Format("McEtaFound%uAllQ", min_track_len).Data())->Fill(mcTrackMap[mctid]->mEta);
                        this->get(TString::Format("McPhiFound%uAllQ", min_track_len).Data())->Fill(mcTrackMap[mctid]->mPhi);
                    }
                }

            } // quality >= 3/4

            // if ( mctid > 0 && quality >= 3.0/4.0 ) {
            if (mctid > 0 && quality >= 4.0 / 4.0) {
                // for ( size_t min_track_len : { 4, 5, 6, 7 } )
                {
                    size_t min_track_len = 4;
                    if (t.size() >= min_track_len) {
                        this->get(TString::Format("McPtPhiFound%u", min_track_len).Data())->Fill(mcTrackMap[mctid]->mPt, mcTrackMap[mctid]->mPhi);
                        ((TH3 *)this->get(TString::Format("McPtEtaPhiFound%u", min_track_len).Data()))->Fill(mcTrackMap[mctid]->mPt, mcTrackMap[mctid]->mEta, mcTrackMap[mctid]->mPhi);
                        this->get(TString::Format("McPtFound%u", min_track_len).Data())->Fill(mcTrackMap[mctid]->mPt);
                        this->get(TString::Format("McEtaFound%u", min_track_len).Data())->Fill(mcTrackMap[mctid]->mEta);
                        this->get(TString::Format("McPhiFound%u", min_track_len).Data())->Fill(mcTrackMap[mctid]->mPhi);
                    }
                }

                this->get("McPtFound")->Fill(mcTrackMap[mctid]->mPt);
                this->get("McPtPhiFound")->Fill(mcTrackMap[mctid]->mPt, mcTrackMap[mctid]->mPhi);
                ((TH3 *)this->get("McPtEtaPhiFound"))->Fill(mcTrackMap[mctid]->mPt, mcTrackMap[mctid]->mEta, mcTrackMap[mctid]->mPhi);
                this->get("McEtaFound")->Fill(mcTrackMap[mctid]->mEta);
                this->get("McPhiFound")->Fill(mcTrackMap[mctid]->mPhi);

                float mcpt = mcTrackMap[mctid]->mPt;
                float mceta = mcTrackMap[mctid]->mEta;
                int mcq = (int)mcTrackMap[mctid]->mQ;
                float rcpt = 0;

                int rcq = 0;
                float pval = 999;
                float chi2 = 999;
                float rchi2 = 999;
                int nFailedHits = 0;

                if (track_index < fitStatus.size()) {
                    rcq = (int)fitStatus[track_index].getCharge();
                    pval = fitStatus[track_index].getPVal();
                    chi2 = fitStatus[track_index].getChi2();
                    rchi2 = fitStatus[track_index].getChi2() / fitStatus[track_index].getNdf();
                    nFailedHits = fitStatus[track_index].getNFailedPoints();
                    rcpt = fitMoms[track_index].Pt();

                    if (fitStatus[track_index].isFitConvergedFully() == false) {
                        rcq = -10;
                        pval = 999;
                        chi2 = -10;
                        rchi2 = -10;
                        nFailedHits = 9;
                    }
                }

                float dPt = mcpt - rcpt;
                float dInvPt = (1.0 / mcpt) - (1.0 / rcpt);

                if (t.size() >= 4 && rcpt > 0.01) {
                    this->get("DeltaPt")->Fill(dPt);
                    this->get("PtRes")->Fill(dPt / mcpt);
                    this->get("InvPtRes")->Fill(dInvPt / (1.0 / mcpt));
                    this->get("InvPtResVsNHits")->Fill(t.size(), dInvPt / (1.0 / mcpt));
                    this->get("PtResVsTrue")->Fill(mcpt * mcq, dPt / mcpt);
                    this->get("InvPtResVsTrue")->Fill(mcpt * mcq, dInvPt / (1.0 / mcpt));
                    this->get("InvPtResVsEta")->Fill(mceta, dInvPt / (1.0 / mcpt));
                    this->get("RecoPtVsMcPt")->Fill(mcpt, rcpt);
                    this->get("FitPValue")->Fill(pval);
                    this->get("FitChi2")->Fill(chi2);
                    this->get("FitChi2Ndf")->Fill(rchi2);
                    this->get("FitNFailedHits")->Fill(nFailedHits);

                    if (abs(rcq) == 1) {
                        this->get("QMatrix")->Fill(mcq, rcq);
                    }

                    if (mcq == rcq)
                        this->get("RightQVsMcPt")->Fill(mcpt * mcq);
                    else if (rcq != -10)
                        this->get("WrongQVsMcPt")->Fill(mcpt * mcq);

                    if (rcq != -10)
                        this->get("AllQVsMcPt")->Fill(mcpt * mcq);
                }

                if (rcpt < 0.01) {
                    this->get("nFailedFits")->Fill(1);
                }

            } else if (mctid == 0) {
                // this->get( "McPtFound" )->Fill( 0 );
                // this->get( "McEtaFound" )->Fill( 0 );
                // this->get( "McPhiFound" )->Fill( -10 );
            }

            // fill the pT versus efficiency for found tracks
            // this->
            track_index++;
        } // found track

        avgQuality /= (float)nTotal;
        this->get("FinalN7Quality")->Fill(avgQuality);

        nTracksAfterIteration.clear();
    } // summarize event

    void finish() {
        hist["NQMatrix"] = (TH2 *)this->get("QMatrix")->Clone("NQMatrix");
        this->get("NQMatrix")->Scale(1.0 / this->get("NQMatrix")->GetEntries());

        this->hist["EffVsMcPt"] = (TH1 *)this->get("McPtFound")->Clone("EffVsMcPt");
        this->get("EffVsMcPt")->Divide(this->get("McPt"));

        this->hist["EffVsMcEta"] = (TH1 *)this->get("McEtaFound")->Clone("EffVsMcEta");
        this->get("EffVsMcEta")->Divide(this->get("McEta"));

        this->hist["EffVsMcPhi"] = (TH1 *)this->get("McPhiFound")->Clone("EffVsMcPhi");
        this->get("EffVsMcPhi")->Divide(this->get("McPhi"));

        // for ( size_t i : { 4 } ) {
        {
            size_t i = 4;
            string n = TString::Format("McPt_%uhits", i).Data();
            this->hist["EffVs" + n] = (TH1 *)this->get(TString::Format("McPtFound%u", i).Data())->Clone(("EffVs" + n).c_str());
            this->get("EffVs" + n)->Divide(this->get(n));

            this->hist["EffVs" + n + "_AllQ"] = (TH1 *)this->get(TString::Format("McPtFound%uAllQ", i).Data())->Clone(("EffVs" + n + "_AllQ").c_str());
            this->get("EffVs" + n + "_AllQ")->Divide(this->get(n));

            n = TString::Format("McEta_%uhits", i).Data();
            this->hist["EffVs" + n] = (TH1 *)this->get(TString::Format("McEtaFound%u", i).Data())->Clone(("EffVs" + n).c_str());
            this->get("EffVs" + n)->Divide(this->get(n));

            this->hist["EffVs" + n + "_AllQ"] = (TH1 *)this->get(TString::Format("McEtaFound%uAllQ", i).Data())->Clone(("EffVs" + n + "_AllQ").c_str());
            this->get("EffVs" + n + "_AllQ")->Divide(this->get(n));

            n = TString::Format("McPhi_%uhits", i).Data();
            this->hist["EffVs" + n] = (TH1 *)this->get(TString::Format("McPhiFound%u", i).Data())->Clone(("EffVs" + n).c_str());
            this->get("EffVs" + n)->Divide(this->get(n));

            this->hist["EffVs" + n + "_AllQ"] = (TH1 *)this->get(TString::Format("McPhiFound%uAllQ", i).Data())->Clone(("EffVs" + n + "_AllQ").c_str());
            this->get("EffVs" + n + "_AllQ")->Divide(this->get(n));

            n = TString::Format("McPtPhi_%uhits", i).Data();
            this->hist["EffVs" + n] = (TH1 *)this->get(TString::Format("McPtPhiFound%u", i).Data())->Clone(("EffVs" + n).c_str());
            this->get("EffVs" + n)->Divide(this->get(n));

            this->hist["EffVs" + n + "_AllQ"] = (TH1 *)this->get(TString::Format("McPtPhiFound%uAllQ", i).Data())->Clone(("EffVs" + n + "_AllQ").c_str());
            this->get("EffVs" + n + "_AllQ")->Divide(this->get(n));

            n = TString::Format("McPtEtaPhi_%uhits", i).Data();
            this->hist["EffVs" + n] = (TH1 *)this->get(TString::Format("McPtEtaPhiFound%u", i).Data())->Clone(("EffVs" + n).c_str());
            this->get("EffVs" + n)->Divide(this->get(n));

            this->hist["EffVs" + n + "_AllQ"] = (TH1 *)this->get(TString::Format("McPtEtaPhiFound%uAllQ", i).Data())->Clone(("EffVs" + n + "_AllQ").c_str());
            this->get("EffVs" + n + "_AllQ")->Divide(this->get(n));
        }

        this->hist["EffVsMcPtAllQ"] = (TH1 *)this->get("McPtFoundAllQ")->Clone("EffVsMcPtAllQ");
        this->get("EffVsMcPtAllQ")->Divide(this->get("McPt"));

        this->hist["EffVsMcEtaAllQ"] = (TH1 *)this->get("McEtaFoundAllQ")->Clone("EffVsMcEtaAllQ");
        this->get("EffVsMcEtaAllQ")->Divide(this->get("McEta"));

        this->hist["EffVsMcPhiAllQ"] = (TH1 *)this->get("McPhiFoundAllQ")->Clone("EffVsMcPhiAllQ");
        this->get("EffVsMcPhiAllQ")->Divide(this->get("McPhi"));

        // make the charge misid hists
        auto hRQ = this->get("RightQVsMcPt");
        auto hWQ = this->get("WrongQVsMcPt");
        TH1 *hAllQSum = (TH1 *)hRQ->Clone("hAllQSum");
        hAllQSum->Add(hWQ);

        this->hist["ChargeMisIdVsMcPt"] = (TH1 *)hWQ->Clone("ChargeMisIdVsMcPt");
        this->hist["ChargeMisIdVsMcPt"]->Divide(hAllQSum);
    }

  private:
    FwdTrackerConfig &cfg;
    std::map<std::string, TH1 *> hist;

    vector<size_t> nTracksAfterIteration;
    size_t maxIterations;
    long long itStart;
    long long eventStart;
};

#endif

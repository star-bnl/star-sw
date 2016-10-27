#include "StZBosMaker.h"

#include "Globals.h"


ClassImp(StZBosMaker)


StZBosMaker::StZBosMaker(const char *name): StMaker(name)
{
   wMK   = 0;
   muMK  = 0;
   HList = 0;
}


Int_t StZBosMaker::Init()
{
   assert(wMK);
   assert(HList);
   initHistos();
   return StMaker::Init();
}


Int_t StZBosMaker::InitRun(int runumber)
{
   LOG_INFO << Form("::InitRun(%d) done, Z-algo params: nearTotEtFrac=%.2f,  clusterEt=%.1f GeV, delPhi12>%.2f rad, Zmass in[%.1f,%.1f]\n",
                    runumber, par_nearTotEtFracZ, par_clusterEtZ, par_delPhi12, par_minMassZ, par_maxMassZ) << endm;
   return 0;
}


Int_t StZBosMaker::FinishRun(int runnumber)
{
   return 0;
}


Int_t StZBosMaker::Make()
{
   //fill various histograms and arrays with event data
   FindZBosBarrel();
   FindZBosEndcap();

   return kStOK;
}


void StZBosMaker::printJan(VecBosTrack *T)
{
   CalTower poiTw = T->mMatchedTower;
   VecBosCluster cluster  = T->mCluster2x2;

   int ibp    = kBTow;
   int id     = poiTw.id;
   float adc  = wMK->mVecBosEvent->bemc.adcTile[ibp][id - 1];
   float frac = adc / 4096 * 60 / cluster.ET;

   printf("Ztower Q=%d pointTw: id=%d ADC=%.0f  2x2ET=%.1f frac=%.2f\n", T->mStMuTrack->charge(), id, adc, cluster.ET, frac);
}


void StZBosMaker::FindZBosBarrel()
{
   VecBosEvent *vecBosEvent = wMK->mVecBosEvent;

   hA[31]->Fill(vecBosEvent->mVertices.size());
   hA[0]->Fill("inp", 1.);

   // search for  Zs
   VecBosVertexPtrSetIter iVertex = wMK->GetVecBosEvent()->mVertices.begin();

   for (short iv=0 ; iVertex != wMK->GetVecBosEvent()->mVertices.end(); ++iVertex, iv++)
   {
      VecBosVertex &V = **iVertex;

      hA[0]->Fill("vert", 1.);

      hA[32]->Fill(V.eleTrack.size());
      if (V.eleTrack.size() < 2) continue;
      hA[0]->Fill("TT", 1.); // at least 2 isolated tracks exist

      // only one Z can come from a vertex, and it should be the highest-energy object
      // hence, the two highest-et clusters should correspond to the z.  Pick those
      // eventually, but for now, just try all of them.
      // select first track:
      for (uint it = 0; it < V.eleTrack.size() - 1; it++)
      {
         VecBosTrack &T1 = V.eleTrack[it];
         if (T1.mMatchedTower.id <= 0) continue; //skip endcap towers
         if (T1.HasCluster() == false) continue;

         assert(T1.mCluster2x2.nTower > 0); // internal logical error
         assert(T1.mP3InNearCone.Pt() > 0); // internal logical error

         float isoET1 = T1.mCluster2x2.ET / T1.mCluster4x4.ET;
         hA[29]->Fill(isoET1);

         hA[23]->Fill(T1.mCluster2x2.ET);
         hA[0]->Fill("tr1", 1.);
         if (T1.mCluster2x2.ET < par_clusterEtZ) continue;
         hA[0]->Fill("et1", 1.);

         float fracET1 = T1.mCluster2x2.ET / T1.mP3InNearCone.Pt();
         hA[24]->Fill(fracET1);
         if (fracET1 < par_nearTotEtFracZ) continue;
         hA[0]->Fill("con1", 1.);

         // select second track:
         for (uint it2 = it + 1; it2 < V.eleTrack.size(); it2++)
         {
            VecBosTrack &T2 = V.eleTrack[it2];
            if (T2.mMatchedTower.id <= 0) continue; //skip endcap towers
            if (T2.HasCluster() == false) continue;

            assert(T2.mCluster2x2.nTower > 0); // internal logical error
            assert(T2.mP3InNearCone.Pt() > 0); // internal logical error

            float isoET2 = T2.mCluster2x2.ET / T2.mCluster4x4.ET;
            hA[30]->Fill(isoET2);

            hA[25]->Fill(T2.mCluster2x2.ET);
            hA[0]->Fill("tr2", 1.);
            if (T2.mCluster2x2.ET < par_clusterEtZ) continue;
            hA[0]->Fill("et2", 1.);

            float fracET2 = T2.mCluster2x2.ET / T2.mP3InNearCone.Pt();
            hA[26]->Fill(fracET2);
            if (fracET2 < par_nearTotEtFracZ) continue;
            hA[0]->Fill("con2", 1.);

            float e1 = T1.mCluster2x2.mEnergy;
            float e2 = T2.mCluster2x2.mEnergy;
            TVector3 p1 = T1.mP3AtDca; p1.SetMag(e1); //cluster.position;
            TVector3 p2 = T2.mP3AtDca; p2.SetMag(e2); //cluster.position;

            float del_phi = p1.DeltaPhi(p2);
            float xx = del_phi;
            if (xx < -TMath::Pi() + 1) xx += 2 * TMath::Pi();
            hA[27]->Fill(xx);
            if (fabs(del_phi) < par_delPhi12) continue;
            hA[0]->Fill("phi12", 1.);

            TVector3 psum = p1 + p2;
            float mass2 = (e1 + e2) * (e1 + e2) - (psum.Dot(psum));
            if (mass2 < 1.) continue; // 9GeV^2) should be param, I'm tired today
            hA[0]->Fill("m2", 1.);

            float mass = sqrt(mass2);
            int Q1Q2 = T1.mStMuTrack->charge() * T2.mStMuTrack->charge();
            if (Q1Q2 == 1) { //..  same sign , can't be Z-> e+ e-
               hA[14]->Fill(mass);
               continue;
            }

            // now only opposite sign
            hA[0]->Fill("QQ", 1.);
            hA[15]->Fill(mass);
            hA[33]->Fill(T1.mCluster2x2.ET, T1.mStMuTrack->charge() / T1.mStMuTrack->pt());
            hA[33]->Fill(T2.mCluster2x2.ET, T2.mStMuTrack->charge() / T2.mStMuTrack->pt());
            hA[34]->Fill(T1.mMatchedTower.iEta , T1.mCluster2x2.mEnergy);
            hA[34]->Fill(T2.mMatchedTower.iEta , T2.mCluster2x2.mEnergy);
            hA[35]->Fill(p1.Eta(), p2.Eta());
            hA[36]->Fill(psum.Eta());
            hA[37]->Fill(psum.Pt());

#if 0
            printf("RCC:  Found Z w/ invmass=%f\n", mass);

            if (!wMK->isMC || (wMK->isMC && vecBosEvent->id < 500) ) {
               printf("\n ZZZZZZZZZZZZZZZZZZZ\n");
               if (mass < par_minMassZ)
                  wMK->wDisaply->exportEvent("Zlow", V, T1);
               else
                  wMK->wDisaply->exportEvent("Zgood", V, T1);
               printf("RCC:  Found Z w/ invmass=%f\n", mass);
               vecBosEvent->print();
            }
#endif

            if (mass < par_minMassZ) continue; //enforce a lower bound
            hA[0]->Fill("Zlow", 1.);

            if (mass > par_maxMassZ) continue; //enforce an upper bound
            hA[0]->Fill("Zhigh", 1.);

            float fmax1 = T1.mCluster2x2.ET / T1.mCluster4x4.ET;
            float fmax2 = T2.mCluster2x2.ET / T2.mCluster4x4.ET;

            hA[21]->Fill(fmax1, fmax2);
            hA[22]->Fill(T1.mCluster2x2.ET, T2.mCluster2x2.ET);

            hA[1]->Fill(mass);
            hA[2]->Fill(T1.mStMuTrack->charge(), T2.mStMuTrack->charge());
            hA[3]->Fill(T1.mStMuTrack->charge()*T2.mStMuTrack->charge());
            hA[4]->Fill(p1.Phi(), p2.Phi());
            hA[5]->Fill(del_phi);
            hA[6]->Fill(mass, T1.mStMuTrack->charge() / T1.mP3AtDca.Perp()*T2.mStMuTrack->charge() / T1.mP3AtDca.Perp());
            hA[7]->Fill(mass, T1.mStMuTrack->charge()*T2.mStMuTrack->charge());
            hA[8]->Fill(T1.mCluster2x2.ET);

            if (T1.mStMuTrack->charge() > 0) {
               hA[9]->Fill(p1.Eta(), p1.Phi());
               hA[10]->Fill(p2.Eta(), p2.Phi());
            }
            else {
               hA[10]->Fill(p1.Eta(), p1.Phi());
               hA[9]->Fill(p2.Eta(), p2.Phi());
            }

            hA[11]->Fill(fmax1, fmax2);
            hA[12]->Fill(T1.mCluster2x2.ET, T2.mCluster2x2.ET);
            hA[13]->Fill(mass, del_phi);
         }
      } // loop over first track
   } // loop over vertices
}


void StZBosMaker::FindZBosEndcap()
{
   VecBosEvent *vecBosEvent = wMK->mVecBosEvent;

   hA[50]->Fill("inp", 1.); hA[60]->Fill("inp", 1.);

   // search for Zs
   VecBosVertexPtrSetIter iVertex = wMK->GetVecBosEvent()->mVertices.begin();

   for (short iv=0 ; iVertex != wMK->GetVecBosEvent()->mVertices.end(); ++iVertex, iv++)
   {
      VecBosVertex &V = **iVertex;

      hA[50]->Fill("vert", 1.); hA[60]->Fill("vert", 1.);

      // first, loop over good barrel tracks
      for (uint it = 0; it < V.eleTrack.size(); it++)
      {
         VecBosTrack &TB = V.eleTrack[it];
         if (TB.mMatchedTower.id <= 0) continue; //skip endcap towers
         if (TB.HasCluster() == false) continue;
         assert(TB.mCluster2x2.nTower > 0); // internal logical error
         assert(TB.mP3InNearCone.Pt() > 0); // internal logical error

         //place cuts on reco track first (both barrel and endcap tracks allowed)
         float isoET1 = TB.mCluster2x2.ET / TB.mCluster4x4.ET;
         hA[51]->Fill(isoET1);
         hA[52]->Fill(TB.mCluster2x2.ET);
         hA[50]->Fill("trB", 1.); hA[60]->Fill("trB", 1.);
         if (TB.mCluster2x2.ET < par_clusterEtZ) continue;
         hA[50]->Fill("etB", 1.); hA[60]->Fill("etB", 1.);

         float fracET1 = TB.mCluster2x2.ET / TB.mP3InNearCone.Pt();
         hA[53]->Fill(fracET1);
         if (fracET1 < par_nearTotEtFracZ) continue;
         hA[50]->Fill("conB", 1.); hA[60]->Fill("conB", 1.);

         // second, try to find candidate track in the endcap
         for (uint it = 0; it < V.eleTrack.size(); it++)
         {
            VecBosTrack &TE = V.eleTrack[it];
            if (TE.mMatchedTower.id >= 0) continue; //skip barrel towers
            if (TE.HasCluster() == false) continue;
            assert(TE.mCluster2x2.nTower > 0); // internal logical error
            assert(TE.mP3InNearCone.Pt() > 0); // internal logical error

            float isoET2 = TE.mCluster2x2.ET / TE.mCluster4x4.ET;
            hA[71]->Fill(isoET2);
            hA[72]->Fill(TE.mCluster2x2.ET);
            hA[70]->Fill("trE", 1.);
            if (TE.mCluster2x2.ET < par_clusterEtZ) continue;
            hA[70]->Fill("etE", 1.);

            float fracET2 = TE.mCluster2x2.ET / TE.mP3InNearCone.Pt();
            hA[73]->Fill(fracET2);
            if (fracET2 < par_nearTotEtFracZ) continue;
            hA[70]->Fill("conE", 1.);

            float e1 = TB.mCluster2x2.mEnergy;
            float e2 = TE.mCluster2x2.mEnergy;
            TVector3 p1 = TB.mP3AtDca; p1.SetMag(e1); //cluster.position;
            TVector3 p2 = TE.mP3AtDca; p2.SetMag(e2); //cluster.position;

            float del_phi = p1.DeltaPhi(p2);
            float xx = del_phi;

            if (xx < -TMath::Pi() + 1) xx += 2 * TMath::Pi();
            hA[74]->Fill(xx);

            if (fabs(del_phi) < par_delPhi12) continue;
            hA[70]->Fill("phi12", 1.);

            TVector3 psum = p1 + p2;
            float mass2 = (e1 + e2) * (e1 + e2) - (psum.Dot(psum));
            if (mass2 < 1.) continue; // 9GeV^2) should be param, I'm tired today
            hA[70]->Fill("m2", 1.);
            hA[77]->Fill(p1.Eta(), p2.Eta());
            hA[78]->Fill(psum.Eta());
            hA[79]->Fill(psum.Pt());

            float mass = sqrt(mass2);
            int Q1Q2 = TB.mStMuTrack->charge() * TE.mStMuTrack->charge();
            if (Q1Q2 == 1) { //..  same sign , can't be Z-> e+ e-
               hA[76]->Fill(mass);
               hA[80]->Fill(TE.mCluster2x2.ET, TE.mStMuTrack->charge() / TE.mStMuTrack->pt());
               continue;
            }

            // now only opposite sign
            hA[70]->Fill("QQ", 1.);
            hA[75]->Fill(mass);
            hA[81]->Fill(TE.mCluster2x2.ET, TE.mStMuTrack->charge() / TE.mStMuTrack->pt());
         }

         // 2) use highest ET endcap mCluster2x2 with no track requirement
         float maxET = 0;
         VecBosCluster maxCluster;

         for (int iEta = 0; iEta < 12; iEta++) { //loop over eta bins
            for (int iPhi = 0; iPhi < 60; iPhi++) { //loop over phi bins

               VecBosCluster eclust = vecBosEvent->FindMaxETow2x2(iEta, iPhi, V.z);
               if (eclust.ET < par_clusterEtZ) continue;
               if (maxET > eclust.ET) continue;
               else {
                  maxET = eclust.ET;
                  maxCluster = eclust;
               }
            }
         }

         if (maxCluster.ET <= 1.0) continue; //remove low E clusters

         // Apply cuts to max ETOW cluster and isolation sums
         VecBosCluster mCluster4x4 = vecBosEvent->SumETowPatch(maxCluster.iEta-1, maxCluster.iPhi-1, 4, 4, V.z);
         hA[54]->Fill(maxCluster.ET / mCluster4x4.ET);
         if (maxCluster.ET / mCluster4x4.ET < wMK->mMinEClusterEnergyIsoRatio) continue;
         hA[55]->Fill(maxCluster.ET);
         hA[50]->Fill("trE", 1.);
         if (maxCluster.ET < par_clusterEtZ) continue;
         hA[50]->Fill("etE", 1.);

         // Assume poor tracking effic so only towers in nearCone
         float nearBtow = wMK->mVecBosEvent->CalcP3InConeBTow(&TB, 2).Pt();
         float nearEtow = wMK->mVecBosEvent->CalcP3InConeETow(&TB, 2).Pt();
         float nearSum  = nearBtow;
         nearSum += nearEtow;
         hA[56]->Fill(maxCluster.ET / nearSum);
         if (maxCluster.ET / nearSum < wMK->parE_nearTotEtFrac) continue;
         hA[50]->Fill("conE", 1.);

         //add plots of good candidates
         float e1 = TB.mCluster2x2.mEnergy;
         float e2 = maxCluster.mEnergy;
         TVector3 p1 = TB.mP3AtDca; p1.SetMag(e1);
         TVector3 p2 = maxCluster.position; p2.SetMag(e2);
         float del_phi = p1.DeltaPhi(p2);
         float xx = del_phi;
         if (xx < -TMath::Pi() + 1) xx += 2 * TMath::Pi();
         hA[57]->Fill(xx);
         if (fabs(del_phi) < par_delPhi12) continue;
         hA[50]->Fill("phi12", 1.);
         TVector3 psum = p1 + p2;
         float mass2 = (e1 + e2) * (e1 + e2) - (psum.Dot(psum));
         if (mass2 < 1.) continue;
         hA[50]->Fill("m2", 1.);
         float mass = sqrt(mass2);
         hA[58]->Fill(mass);
         hA[59]->Fill(p1.Eta(), p2.Eta());
         hA[60]->Fill(psum.Eta());
         hA[61]->Fill(psum.Pt());
      }
   }
}

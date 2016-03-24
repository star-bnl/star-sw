#include "St2011pubSpinMaker.h"

#include "StVecBosMaker.h"


ClassImp(St2011pubSpinMaker)

//
St2011pubSpinMaker::St2011pubSpinMaker(const char *name, const char *etaName): StMaker(name)
{
   wMK = 0; HList = 0;
   core = name;
   coreTitle = etaName;
   par_QPTlow = 0.010;

   par_QPThighET0 = 25;
   par_QPThighET1 = 50;
   par_QPThighA = 0.08;
   par_QPThighB = 0.0013;
   par_leptonEta1 = -1.; par_leptonEta2 = 1.;
   par_useNoEEMC = 0;
}


//
Int_t St2011pubSpinMaker::Init()
{
   assert(wMK);
   assert(HList);
   initHistos();
   return StMaker::Init();
}


//
Int_t St2011pubSpinMaker::FinishRun(int runNo)
{
   return kStOK;
}


//
Int_t St2011pubSpinMaker::InitRun(int runNo)
{
   //j1  sprintf(txt,"bXing= bx48+off=%d",spinDb->BX48offset());
   //j1 hA[3]->GetXaxis()->SetTitle(txt);

   //j1 sprintf(txt,"bXing= bx7+off=%d",spinDb->BX7offset());
   //j1 hA[4]->GetXaxis()->SetTitle(txt);

   LOG_INFO << Form("::InitRun(%d) done, W-spin sorting  params: exclude |Q/PT| < %.2f OR |Q/PT| above line %.3f*(ET-%.1f)-%6e if ET<%.1f, for AL use leptonEta in[%.1f,%.1f] useNoEEMC=%d", runNo,
                    par_QPTlow, par_QPThighET0, par_QPThighA , par_QPThighB, par_QPThighET1, par_leptonEta1, par_leptonEta2, par_useNoEEMC
                   ) << endm;
   return kStOK;
}


//
Int_t St2011pubSpinMaker::Make()
{
   bXingSort();
   return kStOK;
}


//
void St2011pubSpinMaker::bXingSort()
{
   //has access to whole W-algo-maker data via pointer 'wMK'

   hA[0]->Fill("inp", 1.);

   if (wMK->GetVecBosEvent()->mVertices.size() <= 0) return;
   // require: L2W-trig (ET or rnd) & vertex is reasonable

   int bx48     = wMK->GetVecBosEvent()->bx48;
   int bx7      = wMK->GetVecBosEvent()->bx7;
   int bxStar48 = wMK->GetVecBosEvent()->bxStar48;
   int bxStar7  = wMK->GetVecBosEvent()->bxStar7;

   if (bxStar48 != bxStar7) {
      printf("BAD bx7=%d bx48=%d del=%d\n", bx7, bx48, bxStar48 - bxStar7);
      hA[0]->Fill("badBx48", 1.);
      return; // both counters must be in sync
   }

   //remove events tagged as Zs
   if (wMK->GetVecBosEvent()->zTag) return;
   hA[0]->Fill("noZ", 1.);

   hA[1]->Fill(bx48);
   hA[2]->Fill(bx7);

   hA[3]->Fill(bxStar48);
   hA[4]->Fill(bxStar7);

   int spin4 = wMK->GetVecBosEvent()->mSpinPattern4Bits;
   hA[5]->Fill(bxStar7, spin4);

   float par_maxDsmThr = 58;
   float par_myET = 25; // monitoring cut

   if ( wMK->GetVecBosEvent()->l2bitRnd) // lumi monitor BHT3-random
   {
      // avoid too much energy - can be W-events (1/milion :)
      if (wMK-> GetVecBosEvent()->bemc.maxHtDsm < par_maxDsmThr)
      {
         hA[6]->Fill(spin4);
         hA[0]->Fill("BG1", 1.);
      }
      return; // LOGICAL ERROR - FIX IT LATER
   }

   if ( wMK->GetVecBosEvent()->l2bitET == 0) return;
   //..... it is guaranteed ..... L2W-ET>13 did fired  ......

   // search for  Ws ............
   VecBosVertexPtrSetIter iVertex = wMK->GetVecBosEvent()->mVertices.begin();

   int iv = 0;

   for ( ; iVertex != wMK->GetVecBosEvent()->mVertices.end(); ++iVertex, iv++)
   {
      VecBosVertex &V = **iVertex;

      for (uint it = 0; it < V.eleTrack.size(); it++) {
         VecBosTrack &T = V.eleTrack[it];
         if (T.mMatchedTower.id == 0) continue;

         // Collect QCD background for lumi monitors
         float frac24 = T.mCluster2x2.ET / (T.mCluster4x4.ET);

         if (iv == 0 && it == 0 && frac24 < wMK->GetMinBClusterEnergyIsoRatio() ) {
            hA[31]->Fill(T.mCluster2x2.ET);
            if ( T.mCluster2x2.ET < 20. ) { hA[7]->Fill(spin4);  hA[0]->Fill("BG2", 1.);}
         }

         if (T.HasCluster() == false) continue;
         assert(T.mCluster2x2.nTower > 0); // internal logical error
         assert(T.mP3InNearCone.Pt() > 0); // internal logical error

         int iQ = 0; // plus
         float p_Q = T.mStMuTrack->charge();
         if ( p_Q < 0 ) iQ = 1; // minus
         float ET = T.mCluster2x2.ET;

         //put final W cut here
         bool isW = T.mCluster2x2.ET / T.mP3InNearCone.Pt() > wMK->GetNearTotEtFrac(); // near cone

         if (par_useNoEEMC)
            isW = isW && T.sPtBalance_noEEMC > wMK->GetPtBalance(); // awayET
         else
            isW = isW && T.sPtBalance > wMK->GetPtBalance(); // awayET

         if (!isW) { // AL(QCD)
            if (ET > 15 && ET < 20 ) hA[16 + iQ]->Fill(spin4);
            continue;
         }

         hA[0]->Fill("Wcut", 1.);

         hA[30]->Fill(T.mStMuTrack->eta());
         // allows spin specific cuts on eta
         if (T.mStMuTrack->eta() < par_leptonEta1) continue;
         if (T.mStMuTrack->eta() > par_leptonEta2) continue;
         hA[0]->Fill("eta", 1.);

         //::::::::::::::::::::::::::::::::::::::::::::::::
         //:::::accepted W events for x-section :::::::::::
         //::::::::::::::::::::::::::::::::::::::::::::::::

         if (ET > par_myET) hA[0]->Fill("W25", 1.);
         float q2pt = T.mStMuTrack->charge() / T.mStMuTrack->pt();
         if (ET > par_myET) hA[8]->Fill(q2pt);
         hA[9]->Fill(ET, q2pt);

         // apply cut on reco charge
         if ( fabs(q2pt) < par_QPTlow) continue;
         if (ET > par_myET) hA[0]->Fill("Qlow", 1.);

         if (par_QPTlow > 0) { // abaility to skip all Q/PT cuts
            if ( fabs(q2pt) < par_QPTlow) continue;
            float highCut = par_QPThighA - (ET - par_QPThighET0) * par_QPThighB;
            // printf("fff ET=%f q2pr=%f highCut=%f passed=%d\n",ET, q2pt,highCut,fabs(q2pt)<highCut);
            if ( ET > par_myET && ET < par_QPThighET1 && fabs(q2pt) > highCut) continue;
         }

         if (ET > par_myET) {
            hA[0]->Fill("Qhigh", 1.);
            if (p_Q > 0) hA[0]->Fill("Q +", 1.);
            else  hA[0]->Fill("Q -", 1.);
         }

         hA[10 + iQ]->Fill(ET);
         if (ET > 25 && ET < 50 ) hA[12 + iQ]->Fill(spin4);
         if (ET > 32 && ET < 44 ) hA[14 + iQ]->Fill(spin4);

         hA[18 + iQ]->Fill(spin4, ET);
      } // end of loop over tracks
   } // end of loop ove vertices
}

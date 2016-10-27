#include <StEmcUtil/database/StEmcDecoder.h>

#include "StVecBosMaker.h"
#include "St2011pubWanaMaker.h"


ClassImp(St2011pubWanaMaker)


//
St2011pubWanaMaker::St2011pubWanaMaker(const char *name): StMaker(name)
{
   wMK        = 0;
   HList      = 0;
   par_highET = 20; // GeV
   mMappB     = 0;
}


//
Int_t St2011pubWanaMaker::Init()
{
   assert(wMK);
   assert(HList);
   initHistos();
   return StMaker::Init();
}


//
Int_t St2011pubWanaMaker::InitRun(int runNo)
{
   int yyyymmdd = this->GetDBTime().GetDate();
   int hhmmss   = this->GetDBTime().GetTime();
   // this is how BTOW mapping is accesible
   //assert( mMappB==0) ; // do not know how to destroy previous instance, JB
   mMappB = new StEmcDecoder(yyyymmdd, hhmmss);
   return kStOK;
}


//
Int_t St2011pubWanaMaker::Make()
{
   //printf("in %s\n", GetName());
   hA[0]->Fill("inp", 1.);

   evalWeleTrackSign();
   scanCrateRate();
   varyCuts4backgStudy();

   return kStOK;
}


//
void St2011pubWanaMaker::evalWeleTrackSign()
{
   // has access to whole W-algo-maker data via pointer 'wMK'

   // search for Ws
   VecBosVertexPtrSetIter iVertex = wMK->GetVecBosEvent()->mVertices.begin();
   for ( ; iVertex != wMK->GetVecBosEvent()->mVertices.end(); ++iVertex)
   {
      VecBosVertex &V = **iVertex;

      for (uint it = 0; it < V.eleTrack.size(); it++)
      {
         VecBosTrack &track = V.eleTrack[it];

         if ( !track.HasCluster() ) continue;

         assert(track.mCluster2x2.nTower > 0); // internal logical error
         assert(track.mP3InNearCone.Pt() > 0); // internal logical error

         // work with W-track
         float ET = track.mCluster2x2.ET;
         const StMuTrack *glTr = track.glMuTrack; assert(glTr);
         const StMuTrack *prTr = track.mStMuTrack; assert(prTr);
         float PT     = glTr->pt();
         float g_chrg = glTr->charge();
         float p_chrg = prTr->charge();

         //printf("aaa %f %f %f\n",ET,PT,chrg);
         int g_ipn = 0;
         int p_ipn = 0; // plus
         if ( g_chrg < 0 ) g_ipn = 1; // minus
         if ( p_chrg < 0 ) p_ipn = 1; // minus

         // Make cut on lepton |eta| for cross section
         if (fabs(track.mP3AtDca.Eta()) > 1) continue;

         float absEta = fabs(track.mP3AtDca.Eta());

         if (track.mCluster2x2.ET / track.mP3InNearConeNoETow.Pt() > wMK->par_nearTotEtFrac)
         {
            if (track.sPtBalance_noEEMC > wMK->par_ptBalance) { //signal w/o endcap in veto
               // Charge sorted
               if (p_ipn == 0)
                  hA[43]->Fill(track.mCluster2x2.ET);
               else
                  hA[46]->Fill(track.mCluster2x2.ET);

               // eta sorted
               if (absEta > 0.6 && absEta < 1)
                  hA[38]->Fill(track.mCluster2x2.ET);
               if (absEta > 0.3 && absEta < 0.6)
                  hA[39]->Fill(track.mCluster2x2.ET);
               if (absEta > 0.0 && absEta < 0.3)
                  hA[40]->Fill(track.mCluster2x2.ET);
            }
         }

         if (track.mCluster2x2.ET / track.mP3InNearCone.Pt() < wMK->par_nearTotEtFrac) continue; // too large nearET

         // xSec binned
         if (track.sPtBalance > wMK->par_ptBalance ) { //signal
            //charge sorted
            if (p_ipn == 0)
               hA[41]->Fill(track.mCluster2x2.ET);
            else
               hA[44]->Fill(track.mCluster2x2.ET);
            //eta sorted
            if (absEta > 0.6 && absEta < 1)
               hA[32]->Fill(track.mCluster2x2.ET);
            if (absEta > 0.3 && absEta < 0.6)
               hA[33]->Fill(track.mCluster2x2.ET);
            if (absEta > 0.0 && absEta < 0.3)
               hA[34]->Fill(track.mCluster2x2.ET);
         }
         else {//background
            //charge sorted
            if (p_ipn == 0)
               hA[42]->Fill(track.mCluster2x2.ET);
            else
               hA[45]->Fill(track.mCluster2x2.ET);
            //eta sorted
            if (absEta > 0.6 && absEta < 1)
               hA[35]->Fill(track.mCluster2x2.ET);
            if (absEta > 0.3 && absEta < 0.6)
               hA[36]->Fill(track.mCluster2x2.ET);
            if (absEta > 0.0 && absEta < 0.3)
               hA[37]->Fill(track.mCluster2x2.ET);
         }

         if (track.sPtBalance < wMK->par_ptBalance )  continue;

         hA[0]->Fill("acc", 1.);

         hA[5]->Fill(ET);
         hA[10 + g_ipn]->Fill(ET);
         hA[12 + p_ipn]->Fill(ET);
         if (g_chrg * p_chrg < -0.5) hA[14 + p_ipn]->Fill(ET); // charge flip
         hA[6]->Fill(ET, g_chrg / PT);

         // Change in pT from global to primary
         float mP3AtDcaT = prTr->pt();
         float globPT = glTr->pt();
         hA[28]->Fill(mP3AtDcaT, globPT);
         hA[29]->Fill(globPT - mP3AtDcaT);

         if (fabs(globPT - mP3AtDcaT) > 1) hA[30]->Fill(ET);
         if (g_chrg * p_chrg < -0.5) hA[31]->Fill(globPT - mP3AtDcaT);

         // work with prim component
         hA[7]->Fill(ET, p_chrg / prTr->pt());
         if (ET < par_highET) continue;
         hA[0]->Fill("W", 1.);
         hA[16 + p_ipn]->Fill(prTr->eta());
      }
   }
}


//
void St2011pubWanaMaker::scanCrateRate()
{
   //has access to whole W-algo-maker data via pointer 'wMK'
   // printf("crateScan: eveID=%d\n",wMK->mVecBosEvent->id);

   // search for  Ws ............
   VecBosVertexPtrSetIter iVertex = wMK->GetVecBosEvent()->mVertices.begin();
   for ( ; iVertex != wMK->GetVecBosEvent()->mVertices.end(); ++iVertex)
   {
      VecBosVertex &V = **iVertex;

      for (uint it = 0; it < V.eleTrack.size(); it++)
      {
         VecBosTrack &track = V.eleTrack[it];
         // track.mMatchedTower.print();
         int softID = track.mMatchedTower.id;
         if (softID <= 0) continue;
         if (wMK->mVecBosEvent->bemc.statTile[kBTow][softID - 1]) continue; // skip masked towers

         int RDO, CR, CHAN;
         assert(mMappB->GetDaqIdFromTowerId(softID, RDO) == 1); // is good range
         assert(mMappB->GetTowerCrateFromDaqId(RDO, CR, CHAN) == 1);

         float adc = wMK->mVecBosEvent->bemc.adcTile[kBTow][softID - 1];
         hA[1]->Fill(adc, CR);
         // printf("soft=%d DRO=%d CR=%d CHAN=%d adc=%.0f\n",softID,RDO,CR,CHAN,adc);
      }
   }
}


//
void St2011pubWanaMaker::varyCuts4backgStudy()
{
   VecBosVertexPtrSetIter iVertex = wMK->GetVecBosEvent()->mVertices.begin();

   for ( ; iVertex != wMK->GetVecBosEvent()->mVertices.end(); ++iVertex)
   {
      VecBosVertex &V = **iVertex;

      for (uint it = 0; it < V.eleTrack.size(); it++)
      {
         VecBosTrack &track = V.eleTrack[it];
         if ( !track.HasCluster() ) continue;

         assert(track.mCluster2x2.nTower > 0); // internal logical error
         assert(track.mP3InNearCone.Pt() > 0); // internal logical error

         float nearR  = track.mCluster2x2.ET / track.mP3InNearCone.Pt();
         float awayET = track.awayTotET;
         float ET     = track.mCluster2x2.ET;

         // logic of histos
         if (nearR > 0.9) {
            hA[27] -> Fill(ET, awayET);
            if (awayET < 8.) {
               hA[20]->Fill(ET);
            }
            else {
               hA[21]->Fill(ET);
            }
         }

         if (nearR > 0.8 && nearR < 0.9) {
            if (awayET < 8.) {
               hA[22]->Fill(ET);
            }
            else {
               hA[23]->Fill(ET);
            }
         }

         if (nearR > 0. && nearR < 0.8) {
            if (awayET < 8.) {
               hA[24]->Fill(ET);
            }
            else {
               hA[25]->Fill(ET);
            }
         }

         if (awayET < 8.) {
            hA[26]->Fill(nearR);
         }
      }
   }
}

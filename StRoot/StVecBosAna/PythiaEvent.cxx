
#include "PythiaEvent.h"
#include "WBosMcEvent.h"

ClassImp(PythiaEvent)


using namespace std;


Bool_t PythiaEvent::AcceptWEvent(WBosMcEvent& wEvent)
{
   Bool_t accept1 = kFALSE;
   Bool_t accept2 = kFALSE;

   vector<PythiaParticle>::const_iterator iParticle;
   vector<PythiaParticle>::const_iterator iParticle2;
   vector<PythiaParticle>::const_iterator firstParticle = tracks.begin();
   vector<PythiaParticle>::const_iterator lastParticle = tracks.end();

   for (iParticle=firstParticle; iParticle!=lastParticle; ++iParticle)
   {
      if (fabs(iParticle->id) != 24 || iParticle->daughter1 == 0 || iParticle->daughter2 == 0) continue;
      //if (fabs(iParticle->id) != 24 || iParticle->KS != 21 | iParticle->daughter1 != 0 || iParticle->daughter2 != 0) continue;

      wEvent.fLeptonGen = 0; // reset lepton generation

      //UShort_t WIndex = iParticle->index;

      // Now look for daughters
      for (iParticle2=iParticle+1; iParticle2!=lastParticle; ++iParticle2)
      {
         // Consider only daughters
         //if (iParticle2->mother != WIndex || iParticle2->KS != 21) continue;

         if (iParticle2->index != iParticle->daughter1 &&
             iParticle2->index != iParticle->daughter2 ) continue;

         // Consider only first two lepton generations
         if ( fabs(iParticle2->id) < 11 || fabs(iParticle2->id) > 14 ) continue;
         //if ( fabs(iParticle2->id) != 11 || fabs(iParticle2->id) != 12 ) continue;

         UShort_t lgen  = ceil( (abs(iParticle2->id) - 10)/2. );
         UShort_t ltype = (abs(iParticle2->id) - 10)%2 == 0 ? 2 : 1; // neutrino : lepton

         if (wEvent.fLeptonGen && wEvent.fLeptonGen != lgen) {
            // something is wrong: daughters generation does not match
            Error("AcceptWEvent", "Lepton generation does not match. Event id: %d, gen1: %d, gen2: %d", ievent, wEvent.fLeptonGen, lgen);
            return false;
         }

         wEvent.fLeptonGen  = lgen;
         wEvent.fLeptonSign = (iParticle2->id > 0) - (iParticle2->id < 0);

         if (ltype == 1) {
            wEvent.mP4Lepton.SetPxPyPzE(iParticle2->px, iParticle2->py, iParticle2->pz, iParticle2->E);
            wEvent.fLeptonIndex = iParticle2->index;
            accept1 = kTRUE;
         }

         if (ltype == 2) {
            wEvent.mP4Neutrino.SetPxPyPzE(iParticle2->px, iParticle2->py, iParticle2->pz, iParticle2->E);
            wEvent.fNeutrinoIndex = iParticle2->index;
            accept2 = kTRUE;
         }

         if (accept1 && accept2) {
            wEvent.mP4WBoson.SetPxPyPzE(iParticle->px, iParticle->py, iParticle->pz, iParticle->E);
            break;
         }
      }
   }

   return accept1 && accept2 ? kTRUE : kFALSE;
}

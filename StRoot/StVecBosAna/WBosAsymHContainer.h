#ifndef WBosAsymHContainer_h
#define WBosAsymHContainer_h

#include "utils/ProtoEvent.h"

#include "AsymHContainer.h"
#include "Globals.h"


/**
 * A container to hold histograms with calculated asymmetries of the W boson.
 */
class WBosAsymHContainer : public AsymHContainer
{
public:

   WBosAsymHContainer();
   WBosAsymHContainer(TDirectory *dir, EAsymType asymType=kAsymPlain);

   void Fill(ProtoEvent &ev);

private:

   void BookHists();

   static const int RapBins = 3;
   Double_t xBinsRap[RapBins+1];

   ClassDef(WBosAsymHContainer, 1)
};

#endif

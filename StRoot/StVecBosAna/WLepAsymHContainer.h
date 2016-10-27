#ifndef WLepAsymHContainer_h
#define WLepAsymHContainer_h

#include "utils/ProtoEvent.h"

#include "AsymHContainer.h"
#include "Globals.h"


/**
 * A container to hold histograms with calculated asymmetries of the lepton from
 * the W boson decay.
 */
class WLepAsymHContainer : public AsymHContainer
{
public:

   WLepAsymHContainer();
   WLepAsymHContainer(TDirectory *dir, EAsymType asymType=kAsymPlain);

   void Fill(ProtoEvent &ev);

private:

   void BookHists();


   ClassDef(WLepAsymHContainer, 1)
};

#endif

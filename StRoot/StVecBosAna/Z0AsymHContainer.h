#ifndef Z0AsymHContainer_h
#define Z0AsymHContainer_h

#include "utils/ProtoEvent.h"

#include "AsymHContainer.h"
#include "Globals.h"


/**
 * A container to hold histograms with calculated asymmetries of the Z0 boson.
 */
class Z0AsymHContainer : public AsymHContainer
{
public:

   Z0AsymHContainer();
   Z0AsymHContainer(TDirectory *dir, EAsymType asymType=kAsymPlain);

   void Fill(ProtoEvent &ev);

private:

   void BookHists();


   ClassDef(Z0AsymHContainer, 1)
};

#endif

#ifndef AllAsymHContainer_h
#define AllAsymHContainer_h

#include "utils/PlotHelper.h"
#include "utils/ProtoEvent.h"

#include "Globals.h"


/**
 * A container to hold histograms with calculated asymmetries for various
 * physics objects, i.e. W boson, lepton from W decay, Z boson, etc.
 */
class AllAsymHContainer : public PlotHelper
{
public:

   enum ECut {kCUT_UNKNOWN, kCUT_EVENT_NOCUT, kCUT_EVENT_W, kCUT_EVENT_W_PLUS,
      kCUT_EVENT_W_MINUS, kCUT_EVENT_Z};

   typedef std::map<ECut, std::set<PlotHelper*> >   Cut2PlotHelperMap;
   typedef Cut2PlotHelperMap::iterator              Cut2PlotHelperMapIter;

   AllAsymHContainer();
   AllAsymHContainer(TDirectory *dir);

   void Fill(ProtoEvent &ev);
   void Fill(ProtoEvent &ev, ECut cut);

protected:

   Cut2PlotHelperMap fHistCuts;

private:

   void BookHists();


   ClassDef(AllAsymHContainer, 1)
};

#endif

#ifndef EventDisplayHContainer_h
#define EventDisplayHContainer_h

#include <stdint.h>

#include "TDirectoryFile.h"

#include "utils/PlotHelper.h"
#include "utils/ProtoEvent.h"


/**
 * A container with histograms to display basic event kinematics of W boson and its decay products.
 */
class EventDisplayHContainer : public PlotHelper
{
public:

   EventDisplayHContainer();
   EventDisplayHContainer(TDirectory *dir);
   ~EventDisplayHContainer();

   void Fill(ProtoEvent &ev);

private:

	uint32_t fEventCounter; ///< Counter for requested event displays

   void BookHists();
   PlotHelper *GetEventDisplayHists(const ProtoEvent &ev);


   ClassDef(EventDisplayHContainer, 1)
};

#endif

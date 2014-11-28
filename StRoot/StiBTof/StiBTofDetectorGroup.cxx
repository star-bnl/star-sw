#include <stdexcept>
#include "StEvent.h"
#include "StiBTofDetectorGroup.h"
#include "StiBTofDetectorBuilder.h"
#include "StiBTofHitLoader.h"

StiBTofDetectorGroup::StiBTofDetectorGroup(Bool_t active)
    : StiDetectorGroup<StEvent>("BTof",
					  active?new StiBTofHitLoader():0,
					  new StiBTofDetectorBuilder(active))
{}

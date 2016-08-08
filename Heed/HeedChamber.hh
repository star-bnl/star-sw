
#ifndef G_HEED_CHAMBER_H
#define G_HEED_CHAMBER_H

#include "wcpplib/geometry/box.h"
#include "heed++/code/EnTransfCS.h"
#include "heed++/code/HeedCondElectron.h"
#include "heed++/code/HeedDeltaElectronCS.h"

namespace Garfield {

class HeedChamber : 
    public Heed::sh_manip_absvol, public Heed::box,
    public Heed::EnTransfCSType, public Heed::HeedDeltaElectronCSType,
    public Heed::SensitiveVolume {
                    
  public:
    HeedChamber(const abssyscoor& fcsys,
                const double dx, const double dy, const double dz, 
                const Heed::EnTransfCSType etcst,
                const Heed::HeedDeltaElectronCSType hdecst) :
        Heed::sh_manip_absvol(fcsys),
        Heed::box(dx * Heed::cm, dy * Heed::cm, dz * Heed::cm, "chamber"),
        Heed::EnTransfCSType(etcst), Heed::HeedDeltaElectronCSType(hdecst) {

    }

    macro_copy_total(Garfield::HeedChamber);
    virtual absvol* Gavol() const {return (Heed::box*) this;}
    
  protected:
    virtual void get_components(ActivePtr<absref_transmit>& aref_tran) {
      sh_manip_absvol::get_components(aref_tran);
    }
    
};

}

#endif

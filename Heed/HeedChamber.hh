
#ifndef G_HEED_CHAMBER_H
#define G_HEED_CHAMBER_H

#include "wcpplib/geometry/box.h"
#include "heed++/code/EnTransfCS.h"
#include "heed++/code/HeedCondElectron.h"
#include "heed++/code/HeedDeltaElectronCS.h"

namespace Garfield {

class HeedChamber : 
    public sh_manip_absvol, public box,
    public EnTransfCSType, public HeedDeltaElectronCSType,
    public SensitiveVolume {
                    
  public:
    HeedChamber() {};
    HeedChamber(const abssyscoor& fcsys, const EnTransfCSType etcst,
                const HeedDeltaElectronCSType hdecst) :
        sh_manip_absvol(fcsys),
        box(1000. * mm, 1000. * mm,  0.01 * mm, "chamber"),
        EnTransfCSType(etcst), HeedDeltaElectronCSType(hdecst) {

    }

    macro_copy_total(Garfield::HeedChamber);
    virtual absvol* Gavol() const {return (box*) this;}
      
  protected:
    virtual void get_components(ActivePtr<absref_transmit>& aref_tran) {
      sh_manip_absvol::get_components(aref_tran);
    }
    
};

}

#endif

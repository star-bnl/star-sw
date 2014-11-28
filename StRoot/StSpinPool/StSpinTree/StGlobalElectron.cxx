#include "StGlobalElectron.h"

ClassImp(StGlobalElectron)

StGlobalElectron::StGlobalElectron(const StGlobalElectron &t) : TObject(t) {
    this->Charge            = t.Charge;
    this->dEdx              = t.dEdx;
    this->dEdxSigmaElectron = t.dEdxSigmaElectron;
    this->PGlobal           = t.PGlobal;
    this->OGlobal           = t.OGlobal;
/*
    this->PxGlobal          = t.PxGlobal;
    this->PyGlobal          = t.PyGlobal;
    this->PzGlobal          = t.PzGlobal;
    this->OxGlobal          = t.OxGlobal;
    this->OyGlobal          = t.OyGlobal;
    this->OzGlobal          = t.OzGlobal;
*/
    this->DCAGlobal         = t.DCAGlobal;
    this->Chi2              = t.Chi2;
    this->NHitsFit          = t.NHitsFit;
    this->NdEdxPts          = t.NdEdxPts;
    this->NMaxPts           = t.NMaxPts;
}

/*StGlobalElectron::StGlobalElectron(const Tracks &t) {
    this->Charge            = (Char_t)t.Charge;
    this->dEdx              = t.dEdx;
    this->dEdxSigmaElectron = t.dEdxSigmaElectron;

    this->PGlobal.setX(t.PxGlobal);
    this->PGlobal.setY(t.PyGlobal);
    this->PGlobal.setZ(t.PzGlobal);
    this->OGlobal.setX(t.OxGlobal);
    this->OGlobal.setY(t.OyGlobal);
    this->OGlobal.setZ(t.OzGlobal);

//    this->PxGlobal          = t.PxGlobal;
//    this->PyGlobal          = t.PyGlobal;
//    this->PzGlobal          = t.PzGlobal;
//    this->OxGlobal          = t.OxGlobal;
//    this->OyGlobal          = t.OyGlobal;
//    this->OzGlobal          = t.OzGlobal;

    this->DCAGlobal         = t.DCAGlobal;
    this->Chi2              = t.Chi2;
    this->NHitsFit          = (UChar_t)t.NHitsFit;
    this->NdEdxPts          = (UChar_t)t.NdEdxPts;
    this->NMaxPts           = (UChar_t)t.NMaxPts;
}*/

void StGlobalElectron::setAll(Float_t data[14]){
    this->Charge            = (Char_t)data[0];
    this->dEdx              = data[1];
    this->dEdxSigmaElectron = data[2];
    this->setPGlobal(data[3],data[4],data[5]);
    this->setOGlobal(data[6],data[7],data[8]);
    this->DCAGlobal         = data[9];
    this->Chi2              = data[10];
    this->NHitsFit          = (UChar_t)data[11];
    this->NdEdxPts          = (UChar_t)data[12];
    this->NMaxPts           = (UChar_t)data[13];

    return;
}

void StGlobalElectron::setPGlobal(Float_t px, Float_t py, Float_t pz){

    this->PGlobal.setX(px);
    this->PGlobal.setY(py);
    this->PGlobal.setZ(pz);
/*
    this->PxGlobal = px;
    this->PyGlobal = py;
    this->PzGlobal = pz;
*/
    return;
}

void StGlobalElectron::setOGlobal(Float_t ox, Float_t oy, Float_t oz){

    this->OGlobal.setX(ox);
    this->OGlobal.setY(oy);
    this->OGlobal.setZ(oz);
/*
    this->OxGlobal = ox;
    this->OyGlobal = oy;
    this->OzGlobal = oz;
*/
    return;
}

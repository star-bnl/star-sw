#include <assert.h>
#include "StPrimaryElectron.h"
#include "StGlobalElectron.h"
#include <cassert>

ClassImp(StPrimaryElectron)

StPrimaryElectron::StPrimaryElectron(const StPrimaryElectron &t) : TObject(t) {
    assert(&t!=NULL);
    this->Charge            = t.Charge;
    this->dEdx              = t.dEdx;
    this->dEdxSigmaElectron = t.dEdxSigmaElectron;
    this->P                 = t.P;
    this->DCA               = t.DCA;
    this->Eta               = t.Eta;
    this->Phi               = t.Phi;
    this->Pt                = t.Pt;
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
    this->ProjEta           = t.ProjEta;
    this->ProjPhi           = t.ProjPhi;
    this->ProjZ             = t.ProjZ;
    this->PointEta          = t.PointEta;
    this->PointPhi          = t.PointPhi;
    this->PointZ            = t.PointZ;
    this->E                 = t.E;
    this->POverE            = t.POverE;
    this->NEtaStrips        = t.NEtaStrips;
    this->NPhiStrips        = t.NPhiStrips;
    this->NTowers           = t.NTowers;
    this->ZDist             = t.ZDist;
    this->EtaDist           = t.EtaDist;
    this->PhiDist           = t.PhiDist;
    this->R                 = t.R;
    this->NPoints           = t.NPoints;
    this->PointNTracks      = t.PointNTracks;

    this->mGlobalRef        = t.mGlobalRef;
}

/*StPrimaryElectron::StPrimaryElectron(const Tracks &t) {
    assert(&t!=NULL);
    this->Charge            = (Char_t)t.Charge;
    this->dEdx              = t.dEdx;
    this->dEdxSigmaElectron = t.dEdxSigmaElectron;
    this->P                 = t.P;
    this->DCA               = t.DCA;
    this->Eta               = t.Eta;
    this->Phi               = t.Phi;
    this->Pt                = t.Pt;

    //this->PxGlobal          = t.PxGlobal;
    //this->PyGlobal          = t.PyGlobal;
    //this->PzGlobal          = t.PzGlobal;
    //this->OxGlobal          = t.OxGlobal;
    //this->OyGlobal          = t.OyGlobal;
    //this->OzGlobal          = t.OzGlobal;

    this->DCAGlobal         = t.DCAGlobal;
    this->Chi2              = t.Chi2;
    this->NHitsFit          = (UChar_t)t.NHitsFit;
    this->NdEdxPts          = (UChar_t)t.NdEdxPts;
    this->NMaxPts           = (UChar_t)t.NMaxPts;
    this->ProjEta           = t.ProjEta;
    this->ProjPhi           = t.ProjPhi;
    this->ProjZ             = t.ProjZ;
    this->PointEta          = t.PointEta;
    this->PointPhi          = t.PointPhi;
    this->PointZ            = t.PointZ;
    this->E                 = t.E;
    this->POverE            = t.POverE;
    this->NEtaStrips        = (UChar_t)t.NEtaStrips;
    this->NPhiStrips        = (UChar_t)t.NPhiStrips;
    this->NTowers           = (UChar_t)t.NTowers;
    this->ZDist             = t.ZDist;
    this->EtaDist           = t.EtaDist;
    this->PhiDist           = t.PhiDist;
    this->R                 = t.R;
    this->NPoints           = (UChar_t)t.NPoints;
    this->PointNTracks      = (UChar_t)t.PointNTracks;
}*/

void StPrimaryElectron::setAll(Float_t data[29]){
    
    this->Charge            = (Char_t)data[0];
    this->dEdx              = data[1];
    this->dEdxSigmaElectron = data[2];
    this->P                 = data[3];
    this->DCA               = data[4];
    this->Eta               = data[5];
    this->Phi               = data[6];
    this->Pt                = data[7];
    this->DCAGlobal         = data[8];
    this->Chi2              = data[9];
    this->NHitsFit          = (UChar_t)data[10];
    this->NdEdxPts          = (UChar_t)data[11];
    this->NMaxPts           = (UChar_t)data[12];
    this->ProjEta           = data[13];
    this->ProjPhi           = data[14];
    this->ProjZ             = data[15];
    this->PointEta          = data[16];
    this->PointPhi          = data[17];
    this->PointZ            = data[18];
    this->E                 = data[19];
    this->POverE            = data[20];
    this->NEtaStrips        = (UChar_t)data[21];
    this->NPhiStrips        = (UChar_t)data[22];
    this->NTowers           = (UChar_t)data[23];
    this->ZDist             = data[24];
    this->EtaDist           = data[25];
    this->PhiDist           = data[26];
    this->NPoints           = (UChar_t)data[27];
    this->PointNTracks      = (UChar_t)data[28];
   
    return;
}

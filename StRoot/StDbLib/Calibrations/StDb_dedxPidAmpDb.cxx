#include "StDb_dedxPidAmpDb.h"

ClassImp(StDb_dedxPidAmpDb)
//_______________________________________________________
void StDb_dedxPidAmpDb::Streamer(typeAcceptor* accept){

if(!mstruct) mstruct = new dedxPidAmpDb;

accept->pass("gasCalib",mstruct->gasCalib,sizeof(mstruct->gasCalib));
accept->pass("saturationCalib",mstruct->saturationCalib,sizeof(mstruct->saturationCalib));
accept->pass("eMeanPar",mstruct->eMeanPar,sizeof(mstruct->eMeanPar));
accept->pass("eAmpPar",mstruct->eAmpPar,sizeof(mstruct->eAmpPar));
accept->pass("eSigPar",mstruct->eSigPar,sizeof(mstruct->eSigPar));
accept->pass("pionMeanPar",mstruct->pionMeanPar,sizeof(mstruct->pionMeanPar));
accept->pass("pionAmpPar",mstruct->pionAmpPar,sizeof(mstruct->pionAmpPar));
accept->pass("pionSigPar",mstruct->pionSigPar,sizeof(mstruct->pionSigPar));
accept->pass("kaonMeanPar",mstruct->kaonMeanPar,sizeof(mstruct->kaonMeanPar));
accept->pass("kaonAmpPar",mstruct->kaonAmpPar,sizeof(mstruct->kaonAmpPar));
accept->pass("kaonSigPar",mstruct->kaonSigPar,sizeof(mstruct->kaonSigPar));
accept->pass("protonMeanPar",mstruct->protonMeanPar,sizeof(mstruct->protonMeanPar));
accept->pass("protonAmpPar",mstruct->protonAmpPar,sizeof(mstruct->protonAmpPar));
accept->pass("protonSigPar",mstruct->protonSigPar,sizeof(mstruct->protonSigPar));
accept->pass("chargedMeanPar",mstruct->chargedMeanPar,sizeof(mstruct->chargedMeanPar));
accept->pass("chargedSigPar",mstruct->chargedSigPar,sizeof(mstruct->chargedSigPar));

}


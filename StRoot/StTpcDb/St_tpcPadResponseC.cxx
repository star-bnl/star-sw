#include "St_tpcPadResponseC.h"
ClassImp(St_tpcPadResponseC);
Float_t  St_tpcPadResponseC::innerGasGainFluctuation() {tpcPadResponse_st *t = GetTable(0); return t ? t->innerGasGainFluctuation:0;}
Float_t  St_tpcPadResponseC::outerGasGainFluctuation() {tpcPadResponse_st *t = GetTable(0); return t ? t->outerGasGainFluctuation:0;}
Float_t  St_tpcPadResponseC::innerPadResponseSigma()   {tpcPadResponse_st *t = GetTable(0); return t ? t->innerPadResponseSigma:0;}
Float_t  St_tpcPadResponseC::outerPadResponseSigma()   {tpcPadResponse_st *t = GetTable(0); return t ? t->outerPadResponseSigma:0;}
Float_t  St_tpcPadResponseC::innerWirePadCoupling()    {tpcPadResponse_st *t = GetTable(0); return t ? t->innerWirePadCoupling:0;}
Float_t  St_tpcPadResponseC::outerWirePadCoupling()    {tpcPadResponse_st *t = GetTable(0); return t ? t->outerWirePadCoupling:0;}
Float_t  St_tpcPadResponseC::innerRowNormalization()   {tpcPadResponse_st *t = GetTable(0); return t ? t->innerRowNormalization:0;}
Float_t  St_tpcPadResponseC::outerRowNormalization()   {tpcPadResponse_st *t = GetTable(0); return t ? t->outerRowNormalization:0;}
Float_t  St_tpcPadResponseC::BoundaryOfStepFunctions(Int_t i) {tpcPadResponse_st *t = GetTable(0); return t ? t->BoundaryOfStepFunctions[i]:0;}
Float_t  St_tpcPadResponseC::innerChargeFractionConstants(Int_t i) {tpcPadResponse_st *t = GetTable(0); return t ? t->innerChargeFractionConstants[i]:0;}
Float_t  St_tpcPadResponseC::outerChargeFractionConstants(Int_t i) {tpcPadResponse_st *t = GetTable(0); return t ? t->outerChargeFractionConstants[i]:0;}
Float_t  St_tpcPadResponseC::errorFunctionRange() {tpcPadResponse_st *t = GetTable(0); return t ? t->errorFunctionRange:0;}
Int_t    St_tpcPadResponseC::errorFunctionEntry() {tpcPadResponse_st *t = GetTable(0); return t ? t->errorFunctionEntry:0;}
Float_t  St_tpcPadResponseC::longitudinalDiffusionConstant() {tpcPadResponse_st *t = GetTable(0); return t ? t->longitudinalDiffusionConstant:0;}
Float_t  St_tpcPadResponseC::transverseDiffusionConstant() {tpcPadResponse_st *t = GetTable(0); return t ? t->transverseDiffusionConstant:0;}
Float_t  St_tpcPadResponseC::InnerOuterFactor() {tpcPadResponse_st *t = GetTable(0); return t ? t->InnerOuterFactor:0;}

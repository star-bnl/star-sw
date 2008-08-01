#include "StarChairDefs.h"
#include "St_db_Maker/St_db_Maker.h"
//___________________Calibrations/ftpc_____________________________________________________________
#include "StDetectorDbFTPCGas.h"
StDetectorDbFTPCGas* StDetectorDbFTPCGas::fgInstance = 0; 
#include "St_ftpcGasSystemC.h"
MakeChairInstance(ftpcGasSystem,Calibrations/ftpc/ftpcGasSystem);
#include "St_ftpcGasOutC.h"
MakeChairInstance(ftpcGasOut,Calibrations/ftpc/ftpcGasOut);
#include "St_ftpcVoltageC.h"
MakeChairInstance(ftpcVoltage,Calibrations/ftpc/ftpcVoltage);
#include "St_ftpcVoltageStatusC.h"
MakeChairInstance(ftpcVoltageStatus,Calibrations/ftpc/ftpcVoltageStatus);
//___________________tpc_____________________________________________________________
#include "St_tss_tssparC.h"
MakeChairInstance(tss_tsspar,tpc/tsspars/tsspar);
//__________________Calibrations/tpc______________________________________________________________
#include "St_tpcGasC.h"
MakeChairInstance(tpcGas,Calibrations/tpc/tpcGas);
#include "St_tpcGridLeakC.h"
MakeChairInstance(tpcGridLeak,Calibrations/tpc/tpcGridLeak);
#include "St_tpcOmegaTauC.h"
MakeChairInstance(tpcOmegaTau,Calibrations/tpc/tpcOmegaTau);
#include "St_tpcDriftVelocityC.h"
MakeChairInstance(tpcDriftVelocity,Calibrations/tpc/tpcDriftVelocity);
#include "St_TpcSecRowCorC.h"
MakeChairInstance(TpcSecRowCor,Calibrations/tpc/TpcSecRowCor);
#include "St_tpcEffectiveGeomC.h"
MakeChairInstance(tpcEffectiveGeom,Calibrations/tpc/tpcEffectiveGeom);
#include "St_tpcElectronicsC.h"
MakeChairInstance(tpcElectronics,Calibrations/tpc/tpcElectronics);
#include "St_tpcPedestalC.h"
MakeChairInstance(tpcPedestal,Calibrations/tpc/tpcPedestal);
#include "St_tpcPadResponseC.h"
MakeChairInstance(tpcPadResponse,Calibrations/tpc/tpcPadResponse);
#include "St_tpcSlowControlSimC.h"
MakeChairInstance(tpcSlowControlSim,Calibrations/tpc/tpcSlowControlSim);
#include "St_tpcHitErrorsC.h"
MakeChairInstance(tpcHitErrors,Calibrations/tpc/tpcHitErrors);
#include "St_tpcGainMonitorC.h"
MakeChairInstance(tpcGainMonitor,Calibrations/tpc/tpcGainMonitor);
#include "St_tpcHighVoltagesC.h"
MakeChairInstance(tpcHighVoltages,Calibrations/tpc/tpcHighVoltages);
#include "St_tpcGainC.h"
MakeChairInstance(tpcGain,Calibrations/tpc/tpcGain);
#include "St_tpcT0C.h"
MakeChairInstance(tpcT0,Calibrations/tpc/tpcT0);
#include "St_tpcSectorT0offsetC.h"
MakeChairInstance(tpcSectorT0offset,Calibrations/tpc/tpcSectorT0offset);
#include "St_TpcAltroParametersC.h"
MakeChairInstance(TpcAltroParameters,Calibrations/tpc/TpcAltroParameters);
#include "St_asic_thresholdsC.h"
MakeChairInstance(asic_thresholds,Calibrations/tpc/asic_thresholds);
//__________________Calibrations/trg______________________________________________________________
#include "St_defaultTrgLvlC.h"
MakeChairInstance(defaultTrgLvl,Calibrations/trg/defaultTrgLvl);
#include "St_trigDetSumsC.h"
St_trigDetSumsC *St_trigDetSumsC::fgInstance = 0;
ClassImp(St_trigDetSumsC);
//__________________Calibrations/rich______________________________________________________________
#include "StDetectorDbRichScalers.h"
StDetectorDbRichScalers *StDetectorDbRichScalers::fgInstance = 0;
ClassImp(StDetectorDbRichScalers);
#include "St_richvoltagesC.h"
MakeChairInstance(richvoltages,Calibrations/rich/richvoltages);
#include "St_y1MultC.h"
MakeChairInstance(y1Mult,Calibrations/rich/y1Mult);
#include "St_spaceChargeCorC.h"
ClassImp(St_spaceChargeCorC);
MakeChairInstance2(spaceChargeCor,St_spaceChargeCorR1C,Calibrations/rich/spaceChargeCor);
MakeChairInstance2(spaceChargeCor,St_spaceChargeCorR2C,Calibrations/rich/spaceChargeCorR2);
//_________________RunLog_____________________________________________________________
#include "St_MagFactorC.h"
MakeChairInstance(MagFactor,RunLog/MagFactor);
//_________________RunLog/onl_______________________________________________________________
#include "St_starClockOnlC.h"
MakeChairInstance(starClockOnl,RunLog/onl/starClockOnl);
//________________________________________________________________________________
starClockOnl_st *St_starClockOnlC::Struct(Int_t i) {
  starClockOnl_st *s = ((St_starClockOnl* ) instance()->Table())->GetTable(); 
  Int_t N =  getNumRows(); // with i < 0 look for positive frequency
  if (i >= 0 && i < N) return s + i;
  for (Int_t j = 0; j < N; j++, s++) if (s->frequency > 0) break;
  return s;
}
#include "St_starMagOnlC.h"
MakeChairInstance(starMagOnl,RunLog/onl/starMagOnl);
#include "St_beamInfoC.h"
MakeChairInstance(beamInfo,RunLog/onl/beamInfo);
#include "St_tpcRDOMasksC.h"
MakeChairInstance(tpcRDOMasks,RunLog/onl/tpcRDOMasks);
//________________________________________________________________________________
UInt_t       St_tpcRDOMasksC::getSectorMask(UInt_t sector) {
  UInt_t MASK = 0x0000; // default is to mask it out
  //UInt_t MASK = 0xFFFF; // change to  ON by default ** THIS WAS A HACK
  if(sector < 1 || sector > 24 || getNumRows() == 0){
    LOG_WARN << "St_tpcRDOMasksC:: getSectorMask : return default mask for " 
	     << "sector= " << sector << " getNumRows()=" << getNumRows() << endm;
    return MASK;
  }
  MASK = mask(((sector + 1) / 2) - 1); // does the mapping from sector 1-24 to packed sectors
  if( sector % 2 == 0){ // if its even relevent bits are 6-11
    MASK = MASK >> 6;
  }
  // Otherwise want lower 6 bits    
  MASK &= 0x000003F; // Mask out higher order bits
  if (sector == 16 && MASK == 0 && runNumber() > 8181000 && runNumber() < 9181000) MASK = 4095;
  return MASK;
}
//________________________________________________________________________________
#include "St_triggerInfoC.h"
MakeChairInstance(triggerInfo,RunLog/onl/triggerInfo);
#include "St_triggerIDC.h"
MakeChairInstance(triggerID,RunLog/onl/triggerID);
//________________________________________________________________________________
#include "St_trigPrescalesC.h"
MakeChairOptionalInstance(trigPrescales,RunLog/onl/trigPrescales);
//________________________________________________________________________________
#include "St_L0TriggerInfoC.h"
MakeChairInstance(L0TriggerInfo,RunLog/onl/L0TriggerInfo);
#include "St_trigL3ExpandedC.h"
MakeChairOptionalInstance(trigL3Expanded,RunLog/onl/trigL3Expanded);
#include "St_dsmPrescalesC.h"
MakeChairOptionalInstance(dsmPrescales,RunLog/onl/dsmPrescales);
#include "St_additionalTriggerIDC.h"
MakeChairOptionalInstance(additionalTriggerID,RunLog/onl/additionalTriggerID);
#include "StDetectorDbTriggerID.h"
StDetectorDbTriggerID *StDetectorDbTriggerID::fgInstance = 0;
//________________________________________________________________________________
map<Int_t,Float_t> StDetectorDbTriggerID::getTotalPrescales() {
  map<Int_t,Float_t> value;
  // First walk forward through the multiple levels of prescales
  for (UInt_t irow=0;irow<getDsmPrescalesNumRows(); ++irow) {
    Int_t trgId = getDsmPrescalesTrgId(irow);
    value[trgId] = Float_t(getDsmPrescalesDsmPrescale(irow));
  }
  
  for (UInt_t irow=0; irow<getL0NumRows(); ++irow) {
    Int_t trgId = getL0OfflineTrgId(irow);
    map<Int_t,Float_t>::iterator p=value.find(trgId);
    if (p != value.end()) {
      (*p).second *= Float_t(getPsL0(irow));
    }
    else {
      value[trgId] = Float_t(getPsL0(irow));
    }
  }
  // For completeness: this one is always unity as far as I can tell
  for (UInt_t irow=0; irow<getSNumRows(); ++irow) {
    UInt_t idxTrigger = getIdxTrigger(irow);
    Int_t trgId = 0;
    for (UInt_t jrow=0; jrow<getIDNumRows(); ++jrow) {
      if (idxTrigger == getIdxTrg(jrow)) {
	trgId = getOfflineTrgId(jrow);
	break;
      }
    }
    map<Int_t,Float_t>::iterator p=value.find(trgId);
    
    if (p != value.end()) {
      (*p).second *= Float_t(getPs(irow));
    }
    else {
      value[trgId] = Float_t(getPs(irow));
    }
  }
  
  // Now deal with L3Expanded
  for (UInt_t irow=0; irow<getTrigL3ExpandedNumRows(); ++irow) {
    Int_t oldtid = getTrigL3ExpandedL3TrgId(irow);
    Int_t newtid = getTrigL3ExpandedL3ExpandedTrgId(irow);
    Float_t l2ps = getTrigL3ExpandedL2Ps(irow);
    
    map<Int_t,Float_t>::iterator p = value.find(oldtid);
    if (p!= value.end()) {
      value[newtid] = ((*p).second)*l2ps;
    }
    else {
      value[newtid] = l2ps;
    }
    
  }
  return value;
}
//________________________________________________________________________________
Float_t StDetectorDbTriggerID::getTotalPrescaleByTrgId(Int_t trgId) {
  map<Int_t,Float_t> theMap = getTotalPrescales();
  map<Int_t,Float_t>::const_iterator p = theMap.find(trgId);
  if (p != theMap.end()) {
    return (*p).second;
  }
  else {
    return 0;
  }
}
//________________________________________________________________________________
#include "StDetectorDbIntegratedTriggerID.h"
StDetectorDbIntegratedTriggerID *StDetectorDbIntegratedTriggerID::fgInstance = 0;
//___________________Conditions/trg_____________________________________________________________
#include "St_trgTimeOffsetC.h"
MakeChairInstance(trgTimeOffset,Conditions/trg/trgTimeOffset);
//___________________Geometry/tpc_____________________________________________________________
#include "St_tpcDimensionsC.h"
MakeChairInstance(tpcDimensions,Geometry/tpc/tpcDimensions);
#include "St_tpcWirePlanesC.h"
MakeChairInstance(tpcWirePlanes,Geometry/tpc/tpcWirePlanes);
#include "St_tpcSectorPositionC.h"
MakeChairInstance(tpcSectorPosition,Geometry/tpc/tpcSectorPosition);
#include "St_tpcFieldCageC.h"
MakeChairInstance(tpcFieldCage,Geometry/tpc/tpcFieldCage);
#include "St_tpcPadPlanesC.h"
MakeChairInstance(tpcPadPlanes,Geometry/tpc/tpcPadPlanes);
#include "St_tpcGlobalPositionC.h"
MakeChairInstance(tpcGlobalPosition,Geometry/tpc/tpcGlobalPosition);
#include "St_tpcFieldCageShortC.h"
MakeChairInstance(tpcFieldCageShort,Geometry/tpc/tpcFieldCageShort);

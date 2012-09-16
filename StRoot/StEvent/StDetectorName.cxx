#include "StDetectorName.h"
#include <map>
const Char_t *DetectorName(Int_t id) {
  static Bool_t Init = kTRUE;
  static std::map<Int_t,const Char_t *> DetectorNames;
  if (Init) {
    DetectorNames[kUnknownId  ] = "Unknown";
    DetectorNames[kTpcId      ] = "Tpc";
    DetectorNames[kSvtId      ] = "Svt";
    DetectorNames[kRichId     ] = "Rich";
    DetectorNames[kFtpcWestId ] = "FtpcWest";
    DetectorNames[kFtpcEastId ] = "FtpcEast";
    DetectorNames[kTofId      ] = "Tof";
    DetectorNames[kBTofId     ] = "Tof";
    DetectorNames[kCtbId      ] = "Ctb";
    DetectorNames[kSsdId      ] = "Ssd";
    DetectorNames[kBarrelEmcTowerId    ] = "BarrelEmcTower";
    DetectorNames[kBarrelEmcPreShowerId] = "BarrelEmcPreShower";
    DetectorNames[kBarrelSmdEtaStripId ] = "BarrelSmdEtaStrip";
    DetectorNames[kBarrelSmdPhiStripId ] = "BarrelSmdPhiStrip";
    DetectorNames[kEndcapEmcTowerId    ] = "EndcapEmcTower";
    DetectorNames[kEndcapEmcPreShowerId] = "EndcapEmcPreShower";
    DetectorNames[kEndcapSmdUStripId   ] = "EndcapSmdUStrip";
    DetectorNames[kEndcapSmdVStripId   ] = "EndcapSmdVStrip";
    DetectorNames[kZdcWestId  ] = "ZdcWest";
    DetectorNames[kZdcEastId  ] = "ZdcEast";
    DetectorNames[kMwpcWestId ] = "MwpcWest";
    DetectorNames[kMwpcEastId ] = "MwpcEast";
    DetectorNames[kTpcSsdId   ] = "TpcSsd";
    DetectorNames[kTpcSvtId   ] = "TpcSvt";
    DetectorNames[kTpcSsdSvtId] = "TpcSsdSvt";
    DetectorNames[kSsdSvtId   ] = "SsdSvt";
    DetectorNames[kPhmdCpvId  ] = "PhmdCpv";
    DetectorNames[kPhmdId     ] = "Phmd";
    DetectorNames[kPxlId      ] = "Pxl";
    DetectorNames[kIstId      ] = "Ist";
    DetectorNames[kFgtId      ] = "Fgt";
    DetectorNames[kEtrId      ] = "Etr";
    DetectorNames[kFpdWestId  ] = "FpdWest";
    DetectorNames[kFpdEastId  ] = "FpdEast"; 
    DetectorNames[kFmsId      ] = "Fms";  
    DetectorNames[kRpsId      ] = "Rps";
    DetectorNames[kMtdId      ] = "Mtd";
  }
  if (id < kMaxDetectorId) return DetectorNames[id];
  else                     return DetectorNames[0];
}

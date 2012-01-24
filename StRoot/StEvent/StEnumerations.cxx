#include "StEnumerations.h"
#include "TString.h"

static const char *detName[] = {
                  "Unknown",
                  "Tpc",
                  "Svt",
                  "Rich",
                  "FtpcWest",
                  "FtpcEast",
                  "Tof",
                  "Ctb",
                  "Ssd",
                  "BarrelEmcTower",
                  "BarrelEmcPreShower",
                  "BarrelSmdEtaStrip",
                  "BarrelSmdPhiStrip",
                  "EndcapEmcTower",
                  "EndcapEmcPreShower",
                  "EndcapSmdUStrip",
                  "EndcapSmdVStrip",
                  "ZdcWest",
                  "ZdcEast",
                  "MwpcWest",
                  "MwpcEast",
                  "TpcSsd",
                  "TpcSvt",
                  "TpcSsdSvt",
                  "SsdSvt",
                  "PhmdCpv",
                  "Phmd",
                  "Pxl",
                  "Ist",
                  "Fgt",
                  "Etr",
                  "FpdWest",
                  "FpdEast",
                  "Fms",
                  "Rps",
                  "Mtd",
                  "MaxDetector"};
//_____________________________________________________________________________
const char *detectorNameById(StDetectorId id)
{
   if (id<0 || id >= kMaxDetectorId) id = kUnknownId;
   return detName[id];
}
//_____________________________________________________________________________
StDetectorId detectorIdByName(const char *name)
{
  int lmin=99,imin=0;
  
  for (int id =0;id<kMaxDetectorId; id++) {
    if (!TString(detName[id]).BeginsWith(name)) continue;
    int l = strlen(detName[id]);
    if (l>=lmin) continue;
    lmin = l; imin=id;
  }
  return (StDetectorId)imin;
}

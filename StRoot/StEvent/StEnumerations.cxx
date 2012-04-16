#include "StEnumerations.h"
#include "TString.h"
struct myMap {int id; const char *name;};
static myMap gMyMap[] = {
{kUnknownId             ,"Unknown"  },
{kTpcId       		,"Tpc"      },
{kSvtId       		,"Svt"      },
{kRichId      		,"Rich"     },
{kFtpcWestId  		,"FtpcWest" },
{kFtpcEastId  		,"FtpcEast" },
{kTofId       		,"Tof"      },
{kCtbId       		,"Ctb"      },
{kSsdId       		,"Ssd"      },
{kBarrelEmcTowerId     	,"BarrelEmcTower"    },
{kBarrelEmcPreShowerId 	,"BarrelEmcPreShower"},
{kBarrelSmdEtaStripId  	,"BarrelSmdEtaStrip" },
{kBarrelSmdPhiStripId  	,"BarrelSmdPhiStrip" },
{kEndcapEmcTowerId     	,"EndcapEmcTower"    },
{kEndcapEmcPreShowerId 	,"EndcapEmcPreShower"},
{kEndcapSmdUStripId    	,"EndcapSmdUStrip"   },
{kEndcapSmdVStripId    	,"EndcapSmdVStrip"   },
{kZdcWestId   		,"ZdcWest"  },
{kZdcEastId   		,"ZdcEast"  },
{kMwpcWestId  		,"MwpcWest" },
{kMwpcEastId  		,"MwpcEast" },
{kPhmdCpvId   		,"PhmdCpv"  },
{kPhmdId      		,"Phmd"     },
{kPxlId       		,"Pxl"      },
{kIstId       		,"Ist"      },
{kFgtId       		,"Fgt"      },
{kEtrId       		,"Etr"      },
{kFpdWestId   		,"FpdWest"  },
{kFpdEastId   		,"FpdEast"  },
{kFmsId       		,"Fms"      },
{kRpsId       		,"Rps"      },
{kMtdId       		,"Mtd"      },
{0,0}};
//_____________________________________________________________________________
const char *detectorNameById(StDetectorId id)
{
    for (int i=0;gMyMap[i].name;i++) { if (gMyMap[i].id==id) return gMyMap[i].name;}
    return gMyMap[0].name;
}
//_____________________________________________________________________________
StDetectorId detectorIdByName(const char *name)
{
    int lmin=99,imin=0;
    
    for (int i =0;gMyMap[i].name; i++) {
        if (!TString(gMyMap[i].name).BeginsWith(name,TString::kIgnoreCase))
            continue;
        int l = strlen(gMyMap[i].name);
        if (l>=lmin) 	continue;
        lmin = l; imin=i;
    }
    return (StDetectorId)gMyMap[imin].id;
}

#ifndef StRHICfSimPar_HH
#define StRHICfSimPar_HH

#include "TString.h"

enum GeneratorID{
    rPythia8 = 0,
    rQGSJETIII = 1,
    rSIBYLL = 2,
    rEPOSLHCR_S = 3,
    rEPOSLHCR_F = 4,
    QGSJETII04 = 5,
    rGeneratorNum = 6
};

enum SimProcessPar{
    rSingleDiff = 1,
    rDoubleDiff = 2,
    rNonDiff = 3
};

enum SimRHICfRunTypePar{
    rTLtype = 0,
    rTStype = 1,
    rTOPtype = 2
};

enum SimDirection{
    rEast = 0,
    rWest = 1,
    rSideNum = 2
};

enum SimBTofPar{
    rBTofTrayNum = 120,
    rBTofModuleNum = 32,
    rBTofCellnum = 6
};

enum SimVPDPar{
    rVPDEastTray = 121,
    rVPDWestTray = 122,
    rVPDCellNum = 19
};

enum SimBBCPar{
    rBBCLargePmtNum = 8,
    rBBCSmallPmtNum = 16
};

enum SimRHICfPar{
    rTS = 0,
    rTL = 1,
    rTowerNum = 2,
    rPlateNum = 16,
    rLayerNum = 4,
    rXYNum = 2,
    rSmallBarNum = 20,
    rLargeBarNum = 40,
    rShowerTrigger = 1,
    rType1Pi0Trigger = 2,
    rHighEMTrigger = 3
};

enum SimZDCPar{
    rZDCPMTNum = 3,
    rSMDXNum = 8,
    rSMDYNum = 7
};

enum SimIOMode{
    rWrite = 1,
    rRead = 2
};

class StRHICfSimPar 
{
    public:
        inline static TString GetGeneratorName(int generatorId){
            if(generatorId == rPythia8){return "Pythia8";}
            if(generatorId == rQGSJETIII){return "QGSJETIII01";}
            if(generatorId == rSIBYLL){return "SIBYLL";}
            if(generatorId == rEPOSLHCR_S){return "EPOSLHCR";}
            if(generatorId == rEPOSLHCR_F){return "EPOSLHCR_FAST";}
            if(generatorId == QGSJETII04){return "QGSJETII04";}
            return "Non";
        }
};


#endif
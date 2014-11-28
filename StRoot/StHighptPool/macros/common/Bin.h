/*
  standard histogram bins and ranges
*/

#ifndef Bin_H
#define Bin_H

#include "TObject.h"
#include "TArrayD.h"
#include <cmath>
#include <cstdlib>

namespace Bin
{
  //
  // event stuff
  //

  // 0-9 9 is overflow(most central)
  const Double_t flowCentMin = -0.5;
  const Double_t flowCentMax = 9.5;
  const Int_t    nFlowCentBin = 10;
  
  const Double_t vertexZEvtMin = -200;
  const Double_t vertexZEvtMax = 200;
  const Int_t    nVertexZEvtBin = 
    (Int_t) ceil((vertexZEvtMax-vertexZEvtMin)/5);
  const Int_t    nVertexZEvtThinBin = (Int_t)ceil((vertexZEvtMax-vertexZEvtMin)/1);

  // 0-9 0 is most central (kFive)
   const Double_t zdcCentMin = -0.5; 
   const Double_t zdcCentMax = 9.5;
   const Int_t    nZdcCentBin = 10;

   const Double_t vertexXMin = -1;
   const Double_t vertexXMax = 1;
   const Int_t nVertexXBin = (Int_t)ceil((vertexXMax-vertexXMin)/.05);

   const Double_t vertexREvtMin = 0;
   const Double_t vertexREvtMax = 5;
   const Int_t nVertexREvtBin = (Int_t)ceil((vertexREvtMax-vertexREvtMin)/.1);

   const Double_t zdcMin=-0.5;
   const Double_t zdcMax=200.5;
   const Int_t nZdcBin=(Int_t)ceil((zdcMax-zdcMin));
   
   const Double_t hMinusMin=-0.5;
   const Double_t hMinusMax=400.5;
   const Int_t nHMinusBin=(Int_t)ceil((hMinusMax-hMinusMin));

   const Double_t nChMin=-0.5;
   const Double_t nChMax=800.5;
   const Int_t nNchBin=(Int_t)ceil((nChMax-nChMin));


   const Double_t ctbMin = -0.5;
   const Double_t ctbMax = 30000.5;
   const Int_t nCtbBin=(Int_t)ceil((ctbMax-ctbMin)/100);

  //
  //****** track stuff
  //
  void initPtAry(TArrayD* a, int type);

   const Double_t ptMin = 0.5;
   const Double_t ptMax = 8.0;
   const Int_t    nPtBin = (Int_t)ceil((ptMax-ptMin)/.5);
  
   const Double_t ptThinMin = 1.2;
   const Double_t ptThinMax = 8.0;
   const Int_t    nPtThinBin = (Int_t)ceil((ptThinMax-ptThinMin)/.2);

   const Double_t ptTinyMin = 0;
   const Double_t ptTinyMax = 8;
   const Int_t nPtTinyBin = (Int_t)ceil((ptTinyMax-ptTinyMin)/.2);

   // will rebin with these
   const double ptRebinMin = 1;
   const double ptRebinMax = 6;
   const double nPtRebinBin = (int)ceil((ptRebinMax-ptRebinMin)/.05);
   

   const Double_t resPtMin = -1.5;
   const Double_t resPtMax = 1.5;
   const Int_t nResPtBin = (Int_t)ceil((resPtMax-resPtMin)/.005);

   const Double_t resEtaMin = -.5;
   const Double_t resEtaMax = .5;
   const Int_t nResEtaBin = (Int_t)ceil((resEtaMax-resEtaMin)/.02);

   const Double_t resCurvMin = -2;
   const Double_t resCurvMax = 1;
   const Int_t nResCurvBin = (Int_t)ceil((resCurvMax-resCurvMin)/.02);
			  
   const Double_t etaMin = -1.0;
   const Double_t etaMax = 1.0;
   const Int_t nEtaBin = (Int_t)ceil((etaMax-etaMin)/.1);

   const Double_t etaBigMin = -1.0;
   const Double_t etaBigMax = 1.0;
   const Int_t nEtaBigBin = (Int_t)ceil((etaBigMax-etaBigMin)/.25);

   const Double_t etaThinMin = -1.;
   const Double_t etaThinMax = 1;
   const Int_t    nEtaThinBin = (Int_t)ceil((etaThinMax-etaThinMin)/.025);

   const Double_t etaSmallMin = -1.0;
   const Double_t etaSmallMax = 1.0;
   const Int_t nEtaSmallBin = (Int_t)ceil((etaSmallMax-etaSmallMin)/.05); 

   const Double_t sDcaMin = -3;
   const Double_t sDcaMax = 3;
   const Int_t nSDcaBin = (Int_t)ceil((sDcaMax-sDcaMin)/.1);

   const Double_t dcaXYGlMin = -3;
   const Double_t dcaXYGlMax = 3;
   const Double_t nDcaXYGlBin =(Int_t)ceil((dcaXYGlMax-dcaXYGlMin)/.05);

   const Double_t dcaXYGlWideMin = -3;
   const Double_t dcaXYGlWideMax = 3;
   const Double_t nDcaXYGlWideBin =(Int_t)ceil((dcaXYGlMax-dcaXYGlMin)/.1);


  
   const Double_t dcaMin = 0;
   const Double_t dcaMax = 3;
   const Int_t nDcaBin = (Int_t)ceil((dcaMax-dcaMin)/.05);

   const Double_t dcaThinMin = 0;
   const Double_t dcaThinMax = 3;
   const Int_t nDcaThinBin = (Int_t)ceil((dcaThinMax-dcaThinMin)/.05);

   const Double_t commonFracMin = 0;
   const Double_t commonFracMax = 1;
   const Int_t nCommonFracBin = (Int_t)ceil((commonFracMax-commonFracMin)/.02);

  const Double_t fitPtsMin = 0.5;
  const Double_t fitPtsMax = 45.5;
  const Int_t nFitPtsBin = 45;

  const Double_t fitPtsWideMin=25.5; // 26-29,30-33,34-37,38-41,42-45
  const Double_t fitPtsWideMax=45.5;
  const Int_t nFitPtsWideBin=5;

  const Double_t fitPtsCentMin = 0.5;
  const Double_t fitPtsCentMax = 45.5;
  const Int_t nFitPtsCentBin = 45;

   const Double_t fitHitMin = 0.5;
   const Double_t fitHitMax = 45.5;
   const Int_t nFitHitBin = 1;

   const Double_t fracHitMin = 0;
   const Double_t fracHitMax = 1;
   const Int_t nFracHitBin = (Int_t)ceil((fracHitMax-fracHitMin)/.02);

   const Double_t fitHitThinMin = 0.5;
   const Double_t fitHitThinMax = 45.5;
   const Int_t nFitHitThinBin = 45;
  
   const Double_t vertexZMin = -200;
   const Double_t vertexZMax = 200;
   const Int_t nVertexZBin = (Int_t)ceil((vertexZMax-vertexZMin)/10);
   const Int_t nEnterZBin  = (Int_t)ceil((vertexZMax-vertexZMin)/10);

  const Double_t vertexZWideMin = -200;
  const Double_t vertexZWideMax = 200;
  const Int_t nVertexZWideBin = (Int_t)ceil((vertexZWideMax-vertexZWideMin)/25);

  const Double_t vertexZBigMin = -200;
  const Double_t vertexZBigMax = 200;
  const Int_t nVertexZBigBin = (Int_t) ceil((vertexZBigMax-vertexZBigMin)/50);
    
   const Double_t midZBigMin = -200;
   const Double_t midZBigMax = 200;
   const Int_t nMidZBigBin = (Int_t) ceil((midZBigMax-midZBigMin)/50);

   const Double_t absZBigMin = 0;
   const Double_t absZBigMax = 200;
   const Double_t nAbsZBigBin = 4;
  
   const Double_t sectorWestMin = 0.5;
   const Double_t sectorWestMax = 12.5;

   const Double_t sectorEastMin = 12.5;
   const Double_t sectorEastMax = 24.5;
  
   const Int_t nSectorBin = 12;


   const Double_t dPhiMin = -.02;
   const Double_t dPhiMax = .02;
   const Int_t nDPhiBin = (Int_t)ceil((dPhiMax-dPhiMin)/.001);

   const Double_t dEtaMin = -.1;
   const Double_t dEtaMax = .1;
   const Int_t nDEtaBin = (Int_t)ceil((dEtaMax-dEtaMin)/.001);

  //   const Double_t dPtMin = -6;
  //  const Double_t dPtMax = 6;
  //  const Int_t nDPtBin = (Int_t)ceil((dPtMax-dPtMin)/.2);

   const Double_t phiMin = -3.14159;
   const Double_t phiMax = 3.14159;
   const Double_t phiDegMin = -165;
   const Double_t phiDegMax = 195;
   const Int_t nPhiBin = 12;
   const Int_t nPhiSmallBin = 24;
   const Int_t nPhiThinBin = 24*5;

   const Double_t dipMin = -.5;
   const Double_t dipMax = .5;
   const Int_t nDipBin = (Int_t)ceil((dipMax-dipMin)/.02);

   const Double_t dipWideMin = -.5;
   const Double_t dipWideMax = .5;
   const Int_t nDipWideBin = (Int_t)ceil((dipMax-dipMin)/.1);

   const Double_t pzMin = 5;
   const Double_t pzMax = -5;
   const Int_t nPzBin = (Int_t)ceil((pzMax-pzMin)/1);

   const Double_t dcaZGlMin = -.5;
   const Double_t dcaZGlMax = .5;
   const Int_t nDcaZGlBin = (Int_t)ceil((dcaZGlMax-dcaZGlMin)/.02);

   const Double_t dcaZGlWideMin = -.5;
   const Double_t dcaZGlWideMax = .5;
   const Int_t nDcaZGlWideBin = (Int_t)ceil((dcaZGlWideMax-dcaZGlWideMin)/.1);

   const Double_t dDipMin = -.003;
   const Double_t dDipMax = .003;
   const Int_t nDDipBin = (Int_t)ceil((dDipMax-dDipMin)/.0001);

   const Double_t dcaXYPrMin = -.2;
   const Double_t dcaXYPrMax = .2;
   const Int_t    nDcaXYPrBin = (Int_t)ceil((dcaXYPrMax-dcaXYPrMin)/.01);

   const Double_t dedxMin = 0;
   const Double_t dedxMax = 1e-5;
   const Int_t nDedxBin = (Int_t)ceil((dedxMax-dedxMin)/.2e-6);

}






#endif

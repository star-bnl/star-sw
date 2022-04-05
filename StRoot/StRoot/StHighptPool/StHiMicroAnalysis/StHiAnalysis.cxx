/***************************************************************************
 *
 * $Id: StHiAnalysis.cxx,v 1.7 2002/06/13 01:14:25 jklay Exp $                                    
 *
 * Author: Bum Choi, UT Austin, Apr 2002
 *
 ***************************************************************************
 *
 * Description:  Class to perform highpt Analysis on highpt uDSTs
 *
 *
 ***************************************************************************
 *
 * $Log: StHiAnalysis.cxx,v $
 * Revision 1.7  2002/06/13 01:14:25  jklay
 * Combined Spectra histos into analysis
 *
 * Revision 1.2  2002/04/03 00:23:27  jklay
 * Fixed private member access bugs in analysis code
 *
 * Revision 1.1  2002/04/02 20:05:18  jklay
 * Bums analysis tools for highpt uDSTs
 *
 *
 * NOTE: Sector is now computed from Phi
 * 
 **************************************************************************/
#include "StHiAnalysis.h"


//__________________

StHiAnalysis::StHiAnalysis(const char* inputDir,
			   const char* outRootName)
  :  StHiBaseAnalysis(inputDir,outRootName)
     
{


}

//__________________

StHiAnalysis::~StHiAnalysis()
{
}

//_____________________________

void
StHiAnalysis::initHistograms()
{
  cout << "StHiAnalysis::initHistograms()" << endl;
  
  //***********************

  using namespace Bin;
                             
  //***********************
  
  //  gStyle->SetPalette(1,0);
  char title[500],name[500];

  //********************************************************
  // number of events used to scale the spectra
      
  h1NEvent = new TH1D("h1NEvent","h1NEvent",1,1,2);
    
  // eta cut
  
  h1EtaCut = new TH1D("h1EtaCut","h1EtaCut",2,0,2);

  //**********Event Histograms
  h3VertexXYZ = new TH3D("h3VertexXYZ","h3VertexXYZ",nVertexXBin,vertexXMin,vertexXMax,
					nVertexXBin,vertexXMin,vertexXMax,
					nVertexZEvtThinBin,vertexZEvtMin,vertexZEvtMax);
  h3VertexXYZ->SetXTitle("Xvtx (cm)");  h3VertexXYZ->SetYTitle("Yvtx (cm)");  
  h3VertexXYZ->SetZTitle("Zvtx (cm)");  

  h2ZDCSumVsCTB = new TH2D("h2ZDCSumVsCTB","h2ZDCSumVsCTB",nCtbBin,ctbMin,ctbMax,nZdcBin,zdcMin,zdcMax);  
  h2ZDCSumVsCTB->SetYTitle("ZDCSum"); h2ZDCSumVsCTB->SetXTitle("CTB");

  //Event histos after event cuts...
  h2NGoodGlobalsVsNch = new TH2D("h2NGoodGlobalsVsNch","h2NGoodGlobalsVsNch",nNchBin,nChMin,nChMax,nNGGBin,nGGMin,nGGMax);
  h2NGoodGlobalsVsNch->SetXTitle("N_{ch}"); h2NGoodGlobalsVsNch->SetYTitle("N_{GoodGlobals}");

  h1FlowCent = new TH1D("h1FlowCent","h1FlowCent",nFlowCentBin,flowCentMin,flowCentMax);
  h1FlowCent->SetXTitle("Flow Centrality");

  //--------------------------------------------------
  //Track histograms
  //--------------------------------------------------

  //********************************************************
  // both charges

  strcpy(name,"h3ResPtPrGlPtPrDcaXYGl");
  h3ResPtPrGlPtPrDcaXYGl =
    new TH3D(name,name,
             nResPtBin,resPtMin,resPtMax,
             nPtBin,ptMin,ptMax,
             nDcaXYGlWideBin,dcaXYGlWideMin,dcaXYGlWideMax);
  h3ResPtPrGlPtPrDcaXYGl->SetXTitle("(Pr p_{T} - Gl p_{T})/Pr p_{T}");
  h3ResPtPrGlPtPrDcaXYGl->SetYTitle("Primary p_{T} (GeV/c)");
  h3ResPtPrGlPtPrDcaXYGl->SetZTitle("sDca_{XY}");
           
  strcpy(name,"h3ResPtPrGlPtGlDcaXYGl");
  h3ResPtPrGlPtGlDcaXYGl =
    new TH3D(name,name,
             nResPtBin,resPtMin,resPtMax,
             nPtBin,ptMin,ptMax,
             nDcaXYGlWideBin,dcaXYGlWideMin,dcaXYGlWideMax);
  h3ResPtPrGlPtGlDcaXYGl->SetXTitle("(Pr p_{T} - Gl p_{T})/Gl p_{T}");
  h3ResPtPrGlPtGlDcaXYGl->SetYTitle("Global p_{T} (GeV/c)");
  h3ResPtPrGlPtGlDcaXYGl->SetZTitle("sDca_{XY}");

  h1FitPts = new TH1D("h1FitPts","Fit Points",51,-0.5,50.5);
  h1FitPts->SetXTitle("Fit Points");  

  h2DcaGlVsSector = new TH2D("h2DcaGlVsSector","h2DcaGlVsSector",24,0.5,24.5,60,0,3);
  h2DcaGlVsSector->SetXTitle("Sector");  h2DcaGlVsSector->SetYTitle("Dca_{3d}");

  h2DcaXYGlVsSector = new TH2D("h2DcaXYGlVsSector","h2DcaXYGlVsSector",24,0.5,24.5,60,-3,3);
  h2DcaXYGlVsSector->SetXTitle("Sector");  h2DcaXYGlVsSector->SetYTitle("sDca_{XY}");

  h2FitPtsVsSector = new TH2D("h2FitPtsVsSector","h2FitPtsVsSector",24,0.5,24.5,51,-0.5,50.5);
  h2FitPtsVsSector->SetXTitle("Sector");  h2FitPtsVsSector->SetYTitle("Fit Points");

  h2MaxPtsVsSector = new TH2D("h2MaxPtsVsSector","h2MaxPtsVsSector",24,0.5,24.5,51,-0.5,50.5);
  h2MaxPtsVsSector->SetXTitle("Sector");  h2MaxPtsVsSector->SetYTitle("Max Points");

  h2AllPtsVsSector = new TH2D("h2AllPtsVsSector","h2AllPtsVsSector",24,0.5,24.5,51,-0.5,50.5);
  h2AllPtsVsSector->SetXTitle("Sector");  h2AllPtsVsSector->SetYTitle("All Points");

  h1YieldVsSector = new TH1D("h1YieldVsSector","h1YieldVsSector",24,0.5,24.5);
  h1YieldVsSector->SetXTitle("Sector"); h1YieldVsSector->SetYTitle("Raw Yield (h^{+}+h^{-})");

  h2PrPtVsSector = new TH2D("h2PrPtVsSector","h2PrPtVsSector",24,0.5,24.5,20,1.5,21.5);
  h2PrPtVsSector->SetXTitle("Sector");  h2PrPtVsSector->SetYTitle("Primary p_{T} (GeV/c)");

  h2GlPtVsSector = new TH2D("h2GlPtVsSector","h2GlPtVsSector",24,0.5,24.5,20,1.5,21.5);
  h2GlPtVsSector->SetXTitle("Sector");  h2GlPtVsSector->SetYTitle("Global p_{T} (GeV/c)");

  h2ResPrPtVsSector = new TH2D("h2ResPrPtVsSector","h2ResPrPtVsSector",24,0.5,24.5,60,-3,3);
  h2ResPrPtVsSector->SetXTitle("Sector");  h2ResPrPtVsSector->SetYTitle("(Pr p_{T} - Gl p_{T})/Pr p_{T}");

  h2ResGlPtVsSector = new TH2D("h2ResGlPtVsSector","h2ResGlPtVsSector",24,0.5,24.5,60,-3,3);
  h2ResGlPtVsSector->SetXTitle("Sector");  h2ResGlPtVsSector->SetYTitle("(Gl p_{T} - Pr p_{T})/Gl p_{T}");

  TString sPM[3] = { "Plus","Minus","Charged"};
  TString sEW[3] = { "East","West","FullTPC"};

  //*** init var bin 0
  TArrayD* bins0 = new TArrayD;
  initPtAry(bins0,0);
  //*** init var bin 0
  TArrayD* bins1 = new TArrayD;
  initPtAry(bins1,1);

  // east/west
  // plus/minus

  for (Int_t i=0; i < 3; i++) {
    for(Int_t j=0; j<3; j++){

    //******** centrality dependence of pt yields
//    setName(title,"h2ZDCCentralityPtPr",sEW[i].Data(),sPM[j].Data());
//    ew[i].pm[j].h2ZDCCentralityPtPr 
//      = new TH2D(title,title,
//                 nZdcCentBin,zdcCentMin,zdcCentMax,
//                 nPtBin,ptMin,ptMax);
//    ew[i].pm[j].h2ZDCCentralityPtPr->SetXTitle("ZDC Centrality");
//    ew[i].pm[j].h2ZDCCentralityPtPr->SetXTitle("Primary p_{T} (GeV/c)");
//    ew[i].pm[j].h2ZDCCentralityPtPr->Sumw2();

    setName(title,"h2CentralityPtPr",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h2CentralityPtPr
      = new TH2D(title,title,
                 nFlowCentBin,flowCentMin,flowCentMax,
                 nPtBin,ptMin,ptMax);
    ew[i].pm[j].h2CentralityPtPr->SetXTitle("Flow Centrality");
    ew[i].pm[j].h2CentralityPtPr->SetXTitle("Primary p_{T} (GeV/c)");
//    ew[i].pm[j].h2CentralityPtPr->Sumw2();

    //******** sector and global dca xy
    setName(name,"h3PhiPrDcaXYGlPtPr",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h3PhiPrDcaXYGlPtPr
      = new TH3D(name,name,
                 nPhiBin,phiMin,phiMax,
                 nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax,
                 nPtBin,ptMin,ptMax);
    ew[i].pm[j].h3PhiPrDcaXYGlPtPr->SetXTitle("#phi_{primary}");
    ew[i].pm[j].h3PhiPrDcaXYGlPtPr->SetYTitle("sDca_{XY}"); 
    ew[i].pm[j].h3PhiPrDcaXYGlPtPr->SetZTitle("Primary p_{T} (GeV/c)");
//    ew[i].pm[j].h3PhiPrDcaXYGlPtPr->Sumw2();

    setName(name,"h3PhiGlDcaXYGlPtGl",sEW[i],sPM[j].Data());
    ew[i].pm[j].h3PhiGlDcaXYGlPtGl
      = new TH3D(name,name,
                 nPhiBin,phiMin,phiMax,
                 nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax,
                 nPtBin,ptMin,ptMax);
    ew[i].pm[j].h3PhiGlDcaXYGlPtGl->SetXTitle("#phi_{Global}");
    ew[i].pm[j].h3PhiGlDcaXYGlPtGl->SetYTitle("sDca_{XY}"); 
    ew[i].pm[j].h3PhiGlDcaXYGlPtGl->SetZTitle("Global p_{T} (GeV/c)");
//    ew[i].pm[j].h3PhiGlDcaXYGlPtGl->Sumw2();

    //********** 3d dca, dca xy ,pt
    setName(name,"h3DcaGlDcaXYGlPtPr",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h3DcaGlDcaXYGlPtPr =
      new TH3D(name,name,
               nDcaBin,dcaMin,dcaMax,
               nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax,
               nPtBin,ptMin,ptMax);
    ew[i].pm[j].h3DcaGlDcaXYGlPtPr->SetXTitle("dca_{3d}");  
    ew[i].pm[j].h3DcaGlDcaXYGlPtPr->SetYTitle("sDca_{XY}");
    ew[i].pm[j].h3DcaGlDcaXYGlPtPr->SetZTitle("Primary p_{T} (GeV/c)");
//    ew[i].pm[j].h3DcaGlDcaXYGlPtPr->Sumw2();

    setName(name,"h3DcaGlDcaXYGlPtGl",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h3DcaGlDcaXYGlPtGl =
      new TH3D(name,name,
               nDcaBin,dcaMin,dcaMax,
               nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax,
               nPtBin,ptMin,ptMax);
    ew[i].pm[j].h3DcaGlDcaXYGlPtGl->SetXTitle("sDca_{3d}");  
    ew[i].pm[j].h3DcaGlDcaXYGlPtGl->SetYTitle("Dca_{XY}");
    ew[i].pm[j].h3DcaGlDcaXYGlPtGl->SetZTitle("Global p_{T} (GeV/c)");
//    ew[i].pm[j].h3DcaGlDcaXYGlPtGl->Sumw2();

    // for backgrounds
    setName(name,"h2SDcaGlPtPrRebin",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h2SDcaGlPtPrRebin =
      new TH2D(name,name,
               nSDcaBin,sDcaMin,sDcaMax,
               nPtRebinBin,ptRebinMin,ptRebinMax);
    ew[i].pm[j].h2SDcaGlPtPrRebin->SetXTitle("Dca_{3d}");   
    ew[i].pm[j].h2SDcaGlPtPrRebin->SetYTitle("Primary p_{T} (GeV/c)");   
//    ew[i].pm[j].h2SDcaGlPtPrRebin->Sumw2();

    setName(name,"h2DcaXYGlPtPrRebin",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h2DcaXYGlPtPrRebin =
      new TH2D(name,name,
               nDcaXYGlBin,dcaXYGlMin,dcaXYGlMax,
               nPtRebinBin,ptRebinMin,ptRebinMax);
    ew[i].pm[j].h2DcaXYGlPtPrRebin->SetXTitle("Dca_{3d}");
    ew[i].pm[j].h2DcaXYGlPtPrRebin->SetYTitle("Primary p_{T} (GeV/c)");  
//    ew[i].pm[j].h2DcaXYGlPtPrRebin->Sumw2();

    setName(name,"h2DcaGlPtPrRebin",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h2DcaGlPtPrRebin
      = new TH2D(name,name,
                 nDcaBin,dcaMin,dcaMax,
                 nPtRebinBin,ptRebinMin,ptRebinMax);
    ew[i].pm[j].h2DcaGlPtPrRebin->SetXTitle("Dca_{3d}");
    ew[i].pm[j].h2DcaGlPtPrRebin->SetYTitle("Primary p_{T} (GeV/c)");
//    ew[i].pm[j].h2DcaGlPtPrRebin->Sumw2();    

    //********** PhiPr , fit pts, pt
    setName(name,"h3PhiPrFitPtsPtPr",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h3PhiPrFitPtsPtPr =
      new TH3D(name,name,  
               nPhiBin,phiMin,phiMax,
               nFitPtsBin,fitPtsMin,fitPtsMax,
               nPtBin,ptMin,ptMax);
    ew[i].pm[j].h3PhiPrFitPtsPtPr->SetXTitle("#phi_{Primary}");
    ew[i].pm[j].h3PhiPrFitPtsPtPr->SetYTitle("Fit Points");
    ew[i].pm[j].h3PhiPrFitPtsPtPr->SetZTitle("Primary p_{T} (GeV/c)");
//    ew[i].pm[j].h3PhiPrFitPtsPtPr->Sumw2();

    //********** sector , all pts, pt
    setName(name,"h3PhiPrAllPtsPtPr",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h3PhiPrAllPtsPtPr =
      new TH3D(name,name,
               nPhiBin,phiMin,phiMax,
               nFitPtsBin,fitPtsMin,fitPtsMax,
               nPtBin,ptMin,ptMax);
    ew[i].pm[j].h3PhiPrAllPtsPtPr->SetXTitle("#phi_{Primary}"); 
    ew[i].pm[j].h3PhiPrAllPtsPtPr->SetYTitle("All Points");
    ew[i].pm[j].h3PhiPrAllPtsPtPr->SetZTitle("Primary p_{T} (GeV/c)");
//    ew[i].pm[j].h3PhiPrAllPtsPtPr->Sumw2();

    //********** sector , max pts, pt
    setName(name,"h3PhiPrMaxPtsPtPr",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h3PhiPrMaxPtsPtPr =
      new TH3D(name,name,
               nPhiBin,phiMin,phiMax,
               nFitPtsBin,fitPtsMin,fitPtsMax,
               nPtBin,ptMin,ptMax);
    ew[i].pm[j].h3PhiPrMaxPtsPtPr->SetXTitle("#phi_{Primary}"); 
    ew[i].pm[j].h3PhiPrMaxPtsPtPr->SetYTitle("Max Points");
    ew[i].pm[j].h3PhiPrMaxPtsPtPr->SetZTitle("Primary p_{T} (GeV/c)");
//    ew[i].pm[j].h3PhiPrMaxPtsPtPr->Sumw2();

    //********** vtx z, fit pts, pt
    setName(name,"h3VtxZFitPtsPtPr",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h3VtxZFitPtsPtPr =
      new TH3D(name,name,
               nVertexZWideBin,vertexZWideMin,vertexZWideMax,
               nFitPtsBin,fitPtsMin,fitPtsMax,
               nPtBin,ptMin,ptMax);
    ew[i].pm[j].h3VtxZFitPtsPtPr->SetXTitle("Z_{vtx} (cm)");
    ew[i].pm[j].h3VtxZFitPtsPtPr->SetYTitle("Fit Points");
    ew[i].pm[j].h3VtxZFitPtsPtPr->SetZTitle("Primary p_{T} (GeV/c)");
//    ew[i].pm[j].h3VtxZFitPtsPtPr->Sumw2();

    //********* vtx z, fit pts, eta
    setName(name,"h3VtxZFitPtsEtaPr",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h3VtxZFitPtsEtaPr =
      new TH3D(name,name,
               nVertexZWideBin,vertexZWideMin,vertexZWideMax,
               nFitPtsBin,fitPtsMin,fitPtsMax,
               nEtaBin,etaMin,etaMax);
    ew[i].pm[j].h3VtxZFitPtsEtaPr->SetXTitle("Zvtx (cm)"); 
    ew[i].pm[j].h3VtxZFitPtsEtaPr->SetYTitle("Fit Points");
    ew[i].pm[j].h3VtxZFitPtsEtaPr->SetZTitle("#eta_{Primary}");
//    ew[i].pm[j].h3VtxZFitPtsEtaPr->Sumw2();
    
    //********** flow cent, fit pts, pt pr
    setName(name,"h3FlowCentFitPtsPtPr",sEW[i].Data(),sPM[j].Data());
     ew[i].pm[j].h3FlowCentFitPtsPtPr =
      new TH3D(name,name,
               nFlowCentBin,flowCentMin,flowCentMax,
               nFitPtsCentBin,fitPtsCentMin,fitPtsCentMax,
               nPtBin,ptMin,ptMax);
    ew[i].pm[j].h3FlowCentFitPtsPtPr->SetXTitle("Flow Centrality");
    ew[i].pm[j].h3FlowCentFitPtsPtPr->SetYTitle("Fit Points");
    ew[i].pm[j].h3FlowCentFitPtsPtPr->SetZTitle("Primary p_{T} (GeV/c)");
//    ew[i].pm[j].h3FlowCentFitPtsPtPr->Sumw2();    
    
    //********** vtx, eta, pt
    setName(name,"h3VtxZEtaPrPtPr",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h3VtxZEtaPrPtPr
      = new TH3D(name,name,
                 nVertexZBin,vertexZMin,vertexZMax,
                 nEtaSmallBin,etaSmallMin,etaSmallMax,
                 nPtBin,ptMin,ptMax);
    ew[i].pm[j].h3VtxZEtaPrPtPr->SetXTitle("Zvtx (cm)");  
    ew[i].pm[j].h3VtxZEtaPrPtPr->SetYTitle("#eta_{Primary}");
    ew[i].pm[j].h3VtxZEtaPrPtPr->SetZTitle("Primary p_{T} (GeV/c)");
//    ew[i].pm[j].h3VtxZEtaPrPtPr->Sumw2();

    setName(name,"h3VtxZEtaGlPtGl",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h3VtxZEtaGlPtGl
      = new TH3D(name,name,
                 nVertexZBin,vertexZMin,vertexZMax,
                 nEtaSmallBin,etaSmallMin,etaSmallMax,
                 nPtBin,ptMin,ptMax);
    ew[i].pm[j].h3VtxZEtaGlPtGl->SetXTitle("Zvtx (cm)");  
    ew[i].pm[j].h3VtxZEtaGlPtGl->SetYTitle("#eta_{Global}");
    ew[i].pm[j].h3VtxZEtaGlPtGl->SetZTitle("Global p_{T} (GeV/c)");
//    ew[i].pm[j].h3VtxZEtaGlPtGl->Sumw2();

    // varying bins RAW spectra     
    setName(name,"RawPtGlVarBin0",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h1RawPtGlVarBin0
      = new TH1D(name,name,bins0->GetSize()-1,bins0->GetArray());
    ew[i].pm[j].h1RawPtGlVarBin0->SetXTitle("Global p_{T} (GeV/c)");
    ew[i].pm[j].h1RawPtGlVarBin0->Sumw2();

    setName(name,"RawPtGlVarBin1",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h1RawPtGlVarBin1
      = new TH1D(name,name,bins1->GetSize()-1,bins1->GetArray());
    ew[i].pm[j].h1RawPtGlVarBin1->SetXTitle("Global p_{T} (GeV/c)");
    ew[i].pm[j].h1RawPtGlVarBin1->Sumw2();

    setName(name,"RawPtPrVarBin0",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h1RawPtPrVarBin0
      = new TH1D(name,name,bins0->GetSize()-1,bins0->GetArray());
    ew[i].pm[j].h1RawPtPrVarBin0->SetXTitle("Primary p_{T} (GeV/c)");
    ew[i].pm[j].h1RawPtPrVarBin0->Sumw2();

    setName(name,"RawPtPrVarBin1",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h1RawPtPrVarBin1
      = new TH1D(name,name,bins1->GetSize()-1,bins1->GetArray());
    ew[i].pm[j].h1RawPtPrVarBin1->SetXTitle("Primary p_{T} (GeV/c)");
    ew[i].pm[j].h1RawPtPrVarBin1->Sumw2();

    // varying bins ONEOVERPT spectra     
    setName(name,"OneOverPtGlVarBin0",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h1OneOverPtGlVarBin0
      = new TH1D(name,name,bins0->GetSize()-1,bins0->GetArray());
    ew[i].pm[j].h1OneOverPtGlVarBin0->SetXTitle("Global p_{T} (GeV/c)");
    ew[i].pm[j].h1OneOverPtGlVarBin0->Sumw2();

    setName(name,"OneOverPtGlVarBin1",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h1OneOverPtGlVarBin1
      = new TH1D(name,name,bins1->GetSize()-1,bins1->GetArray());
    ew[i].pm[j].h1OneOverPtGlVarBin1->SetXTitle("Global p_{T} (GeV/c)");
    ew[i].pm[j].h1OneOverPtGlVarBin1->Sumw2();

    setName(name,"OneOverPtPrVarBin0",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h1OneOverPtPrVarBin0
      = new TH1D(name,name,bins0->GetSize()-1,bins0->GetArray());
    ew[i].pm[j].h1OneOverPtPrVarBin0->SetXTitle("Primary p_{T} (GeV/c)");
    ew[i].pm[j].h1OneOverPtPrVarBin0->Sumw2();

    setName(name,"OneOverPtPrVarBin1",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h1OneOverPtPrVarBin1
      = new TH1D(name,name,bins1->GetSize()-1,bins1->GetArray());
    ew[i].pm[j].h1OneOverPtPrVarBin1->SetXTitle("Primary p_{T} (GeV/c)");
    ew[i].pm[j].h1OneOverPtPrVarBin1->Sumw2();

    // varying bins WEIGHTEDMEAN spectra     
    setName(name,"WeightedMeanPtGlVarBin0",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h1WeightedMeanPtGlVarBin0
      = new TH1D(name,name,bins0->GetSize()-1,bins0->GetArray());
    ew[i].pm[j].h1WeightedMeanPtGlVarBin0->SetXTitle("Global p_{T} (GeV/c)");
    ew[i].pm[j].h1WeightedMeanPtGlVarBin0->Sumw2();

    setName(name,"WeightedMeanPtGlVarBin1",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h1WeightedMeanPtGlVarBin1
      = new TH1D(name,name,bins1->GetSize()-1,bins1->GetArray());
    ew[i].pm[j].h1WeightedMeanPtGlVarBin1->SetXTitle("Global p_{T} (GeV/c)");
    ew[i].pm[j].h1WeightedMeanPtGlVarBin1->Sumw2();

    setName(name,"WeightedMeanPtPrVarBin0",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h1WeightedMeanPtPrVarBin0
      = new TH1D(name,name,bins0->GetSize()-1,bins0->GetArray());
    ew[i].pm[j].h1WeightedMeanPtPrVarBin0->SetXTitle("Primary p_{T} (GeV/c)");
    ew[i].pm[j].h1WeightedMeanPtPrVarBin0->Sumw2();

    setName(name,"WeightedMeanPtPrVarBin1",sEW[i].Data(),sPM[j].Data());
    ew[i].pm[j].h1WeightedMeanPtPrVarBin1
      = new TH1D(name,name,bins1->GetSize()-1,bins1->GetArray());
    ew[i].pm[j].h1WeightedMeanPtPrVarBin1->SetXTitle("Primary p_{T} (GeV/c)");
    ew[i].pm[j].h1WeightedMeanPtPrVarBin1->Sumw2();
                 
    } //PM    
  } //EW



}
//______________________


void 
StHiAnalysis::trackLoop()
{
  //Standard event cut on vertexZ, centrality and triggerword are already done

  if(mDebug)
    cout << "StHiAnalysis::trackLoop()" << endl;

  //Need this for EAST/WEST analysis
  Float_t vertexZ = mHiMicroEvent->VertexZ();

  Float_t flowCent   = flowCentrality(mHiMicroEvent->NUncorrectedPrimaries());

  Int_t nTrack = mHiMicroEvent->NTrack();
  StHiMicroTrack* track;

  for(Int_t i=0; i<nTrack; i++){
    track =(StHiMicroTrack*) mHiMicroEvent->tracks()->At(i);

    Int_t iCharge = (track->Charge()>0) ? 0 : 1; //plus is 0

    Float_t ptPr  = track->PtPr();
    Float_t ptGl  = track->PtGl();
    Float_t resPtPrGlPr = (ptPr-ptGl)/ptPr;
    Float_t resPtPrGlGl = (ptPr-ptGl)/ptGl;

    Float_t etaGl = track->EtaGl();
    Float_t etaPr  = track->EtaPr();

    Float_t phiPr = track->PhiPr();
    Float_t phiGl = track->PhiGl();
 //   Float_t phiGlDeg = phiGl*180./TMath::Pi();
 //   Float_t phiPrDeg = phiPr*180./TMath::Pi();

    Float_t sector = track->FirstSector();    

    Float_t sDcaGl = (track->DcaXYGl()>0) ? track->DcaGl() : -track->DcaGl();
    Float_t dcaXYGl = track->DcaXYGl();
    Float_t dcaGl = track->DcaGl();

    Float_t fitPts = track->FitPts();
    Float_t allPts = track->AllPts();
    Float_t maxPts = track->MaxPossPts();

    h1FitPts->Fill(fitPts);

    if(CutRc::AcceptEta(track) && CutRc::AcceptFirstPadrow(track) && CutRc::AcceptSameSector(track)) {
      
      if(CutRc::AcceptSDcaGl(track)) {
        h2FitPtsVsSector->Fill(sector,fitPts);
        h2MaxPtsVsSector->Fill(sector,maxPts);
        h2AllPtsVsSector->Fill(sector,allPts);
      }
      if(CutRc::AcceptFitPts(track)) {
        h2DcaGlVsSector->Fill(sector,dcaGl);
        h2DcaXYGlVsSector->Fill(sector,dcaXYGl);
      }
    }

    //Standard track cuts are |eta|<0.75,fitpts>20,SDca<1cm
    if(CutRc::Accept(track)) {
      h1YieldVsSector->Fill(sector);  
      h2PrPtVsSector->Fill(sector,ptPr);
      h2GlPtVsSector->Fill(sector,ptGl);
      h2ResPrPtVsSector->Fill(sector,resPtPrGlPr);
      h2ResGlPtVsSector->Fill(sector,resPtPrGlGl);
    }

//Bum's stuff

    //*********** AcceptNoeta = dca, fitpts and samesector
    
    if(CutRc::AcceptNoEta(track)){
      if(CutRc::AcceptEastSideTrack(track)) {
	ew[0].pm[iCharge].h3VtxZEtaPrPtPr->Fill(vertexZ,etaPr,ptPr);
        ew[0].pm[iCharge].h3VtxZEtaGlPtGl->Fill(vertexZ,etaGl,ptGl);
	ew[0].pm[2].h3VtxZEtaPrPtPr->Fill(vertexZ,etaPr,ptPr);
        ew[0].pm[2].h3VtxZEtaGlPtGl->Fill(vertexZ,etaGl,ptGl);
      }
      if(CutRc::AcceptWestSideTrack(track)) {
	ew[1].pm[iCharge].h3VtxZEtaPrPtPr->Fill(vertexZ,etaPr,ptPr);
        ew[1].pm[iCharge].h3VtxZEtaGlPtGl->Fill(vertexZ,etaGl,ptGl);
	ew[1].pm[2].h3VtxZEtaPrPtPr->Fill(vertexZ,etaPr,ptPr);
        ew[1].pm[2].h3VtxZEtaGlPtGl->Fill(vertexZ,etaGl,ptGl);
      }
      ew[2].pm[2].h3VtxZEtaPrPtPr->Fill(vertexZ,etaPr,ptPr);
      ew[2].pm[2].h3VtxZEtaGlPtGl->Fill(vertexZ,etaGl,ptGl);
      ew[2].pm[iCharge].h3VtxZEtaPrPtPr->Fill(vertexZ,etaPr,ptPr);
      ew[2].pm[iCharge].h3VtxZEtaGlPtGl->Fill(vertexZ,etaGl,ptGl);
    }

    //****** Eta cut

    if(CutRc::AcceptEta(track)){
      if(CutRc::AcceptFitPts(track)){

	if(CutRc::AcceptEastSideTrack(track)) {
         // sector,dcaxy,pt
         ew[0].pm[iCharge].h3PhiPrDcaXYGlPtPr->Fill(phiPr,dcaXYGl,ptPr);
         ew[0].pm[iCharge].h3PhiGlDcaXYGlPtGl->Fill(phiGl,dcaXYGl,ptGl);
         ew[0].pm[2].h3PhiPrDcaXYGlPtPr->Fill(phiPr,dcaXYGl,ptPr);
         ew[0].pm[2].h3PhiGlDcaXYGlPtGl->Fill(phiGl,dcaXYGl,ptGl);
         // dca3d, dcaxy,pt
         ew[0].pm[iCharge].h3DcaGlDcaXYGlPtPr->Fill(dcaGl,dcaXYGl,ptPr);
         ew[0].pm[iCharge].h3DcaGlDcaXYGlPtGl->Fill(dcaGl,dcaXYGl,ptGl);
         ew[0].pm[2].h3DcaGlDcaXYGlPtPr->Fill(dcaGl,dcaXYGl,ptPr);
         ew[0].pm[2].h3DcaGlDcaXYGlPtGl->Fill(dcaGl,dcaXYGl,ptGl);
         // rebin?
         ew[0].pm[iCharge].h2SDcaGlPtPrRebin->Fill(sDcaGl,ptPr);
         ew[0].pm[2].h2SDcaGlPtPrRebin->Fill(sDcaGl,ptPr);
         // dcaxy,pt
         ew[0].pm[iCharge].h2DcaXYGlPtPrRebin->Fill(dcaXYGl,ptPr);
         ew[0].pm[2].h2DcaXYGlPtPrRebin->Fill(dcaXYGl,ptPr);
	}
	if(CutRc::AcceptWestSideTrack(track)) {
         // sector,dcaxy,pt
         ew[1].pm[iCharge].h3PhiPrDcaXYGlPtPr->Fill(phiPr,dcaXYGl,ptPr);
         ew[1].pm[iCharge].h3PhiGlDcaXYGlPtGl->Fill(phiGl,dcaXYGl,ptGl);
         ew[1].pm[2].h3PhiPrDcaXYGlPtPr->Fill(phiPr,dcaXYGl,ptPr);
         ew[1].pm[2].h3PhiGlDcaXYGlPtGl->Fill(phiGl,dcaXYGl,ptGl);
         // dca3d, dcaxy,pt
         ew[1].pm[iCharge].h3DcaGlDcaXYGlPtPr->Fill(dcaGl,dcaXYGl,ptPr);
         ew[1].pm[iCharge].h3DcaGlDcaXYGlPtGl->Fill(dcaGl,dcaXYGl,ptGl);
         ew[1].pm[2].h3DcaGlDcaXYGlPtPr->Fill(dcaGl,dcaXYGl,ptPr);
         ew[1].pm[2].h3DcaGlDcaXYGlPtGl->Fill(dcaGl,dcaXYGl,ptGl);
         // rebin?
         ew[1].pm[iCharge].h2SDcaGlPtPrRebin->Fill(sDcaGl,ptPr);
         ew[1].pm[2].h2SDcaGlPtPrRebin->Fill(sDcaGl,ptPr);
         // dcaxy,pt
         ew[1].pm[iCharge].h2DcaXYGlPtPrRebin->Fill(dcaXYGl,ptPr);
         ew[1].pm[2].h2DcaXYGlPtPrRebin->Fill(dcaXYGl,ptPr);
	}
        // sector,dcaxy,pt
        ew[2].pm[2].h3PhiPrDcaXYGlPtPr->Fill(phiPr,dcaXYGl,ptPr);
        ew[2].pm[2].h3PhiGlDcaXYGlPtGl->Fill(phiGl,dcaXYGl,ptGl);
        ew[2].pm[iCharge].h3PhiPrDcaXYGlPtPr->Fill(phiPr,dcaXYGl,ptPr);
        ew[2].pm[iCharge].h3PhiGlDcaXYGlPtGl->Fill(phiGl,dcaXYGl,ptGl);
        // dca3d, dcaxy,pt
        ew[2].pm[2].h3DcaGlDcaXYGlPtPr->Fill(dcaGl,dcaXYGl,ptPr);
        ew[2].pm[2].h3DcaGlDcaXYGlPtGl->Fill(dcaGl,dcaXYGl,ptGl);
        ew[2].pm[iCharge].h3DcaGlDcaXYGlPtPr->Fill(dcaGl,dcaXYGl,ptPr);
        ew[2].pm[iCharge].h3DcaGlDcaXYGlPtGl->Fill(dcaGl,dcaXYGl,ptGl);
        // rebin?
        ew[2].pm[2].h2SDcaGlPtPrRebin->Fill(sDcaGl,ptPr);
        ew[2].pm[iCharge].h2SDcaGlPtPrRebin->Fill(sDcaGl,ptPr);
        // dcaxy,pt
        ew[2].pm[2].h2DcaXYGlPtPrRebin->Fill(dcaXYGl,ptPr);
        ew[2].pm[iCharge].h2DcaXYGlPtPrRebin->Fill(dcaXYGl,ptPr);

        // respt, pt,dcaxy
        // no charge difference
        h3ResPtPrGlPtPrDcaXYGl->Fill(resPtPrGlPr,ptPr,dcaXYGl);
        h3ResPtPrGlPtGlDcaXYGl->Fill(resPtPrGlGl,ptGl,dcaXYGl);
      
      }//AcceptFitPts(track)

      //*********eta and dca cut
      if(CutRc::AcceptSDcaGl(track)){
	if(CutRc::AcceptEastSideTrack(track)) {
          // sector,fit pts, pt
          ew[0].pm[iCharge].h3PhiPrFitPtsPtPr->Fill(phiPr,fitPts,ptPr);
          ew[0].pm[2].h3PhiPrFitPtsPtPr->Fill(phiPr,fitPts,ptPr);
	  // sector, all pts, pt
          ew[0].pm[iCharge].h3PhiPrAllPtsPtPr->Fill(phiPr,allPts,ptPr);
          ew[0].pm[2].h3PhiPrAllPtsPtPr->Fill(phiPr,allPts,ptPr);
          // sector, max pts, pt
          ew[0].pm[iCharge].h3PhiPrMaxPtsPtPr->Fill(phiPr,maxPts,ptPr);
          ew[0].pm[2].h3PhiPrMaxPtsPtPr->Fill(phiPr,maxPts,ptPr);
          // flow, fit pts, pt
          ew[0].pm[iCharge].h3FlowCentFitPtsPtPr->Fill(flowCent,fitPts,ptPr);
          ew[0].pm[2].h3FlowCentFitPtsPtPr->Fill(flowCent,fitPts,ptPr);
	}
	if(CutRc::AcceptWestSideTrack(track)) {
          // sector,fit pts, pt
          ew[1].pm[iCharge].h3PhiPrFitPtsPtPr->Fill(phiPr,fitPts,ptPr);
          ew[1].pm[2].h3PhiPrFitPtsPtPr->Fill(phiPr,fitPts,ptPr);
	  // sector, all pts, pt
          ew[1].pm[iCharge].h3PhiPrAllPtsPtPr->Fill(phiPr,allPts,ptPr);
          ew[1].pm[2].h3PhiPrAllPtsPtPr->Fill(phiPr,allPts,ptPr);
          // sector, max pts, pt
          ew[1].pm[iCharge].h3PhiPrMaxPtsPtPr->Fill(phiPr,maxPts,ptPr);
          ew[1].pm[2].h3PhiPrMaxPtsPtPr->Fill(phiPr,maxPts,ptPr);
          // flow, fit pts, pt
          ew[1].pm[iCharge].h3FlowCentFitPtsPtPr->Fill(flowCent,fitPts,ptPr);
          ew[1].pm[2].h3FlowCentFitPtsPtPr->Fill(flowCent,fitPts,ptPr);
	}
        // sector,fit pts, pt
        ew[2].pm[2].h3PhiPrFitPtsPtPr->Fill(phiPr,fitPts,ptPr);
        ew[2].pm[iCharge].h3PhiPrFitPtsPtPr->Fill(phiPr,fitPts,ptPr);
	// sector, all pts, pt
        ew[2].pm[2].h3PhiPrAllPtsPtPr->Fill(phiPr,allPts,ptPr);
        ew[2].pm[iCharge].h3PhiPrAllPtsPtPr->Fill(phiPr,allPts,ptPr);
        // sector, max pts, pt
        ew[2].pm[2].h3PhiPrMaxPtsPtPr->Fill(phiPr,maxPts,ptPr);
        ew[2].pm[iCharge].h3PhiPrMaxPtsPtPr->Fill(phiPr,maxPts,ptPr);
        // flow, fit pts, pt
        ew[2].pm[2].h3FlowCentFitPtsPtPr->Fill(flowCent,fitPts,ptPr);
        ew[2].pm[iCharge].h3FlowCentFitPtsPtPr->Fill(flowCent,fitPts,ptPr);

      } //dca
    }//eta

    //******** dca and tight eta cut
    if(CutRc::AcceptSDcaGl(track) && fabs(etaPr)<.1){
      // vtx z, fit pts, pt
      if(CutRc::AcceptEastSideTrack(track)) {
        ew[0].pm[iCharge].h3VtxZFitPtsPtPr->Fill(vertexZ,fitPts,ptPr);
        ew[0].pm[2].h3VtxZFitPtsPtPr->Fill(vertexZ,fitPts,ptPr);
      }
      if(CutRc::AcceptWestSideTrack(track)) {
        ew[1].pm[iCharge].h3VtxZFitPtsPtPr->Fill(vertexZ,fitPts,ptPr);
        ew[1].pm[2].h3VtxZFitPtsPtPr->Fill(vertexZ,fitPts,ptPr);
      }
      ew[2].pm[2].h3VtxZFitPtsPtPr->Fill(vertexZ,fitPts,ptPr);
      ew[2].pm[iCharge].h3VtxZFitPtsPtPr->Fill(vertexZ,fitPts,ptPr);
    } 

    // dca and pt
    if(CutRc::AcceptSDcaGl(track) && ptPr>4 && ptPr<6){
      // vtx z, fit pts, eta
      if(CutRc::AcceptEastSideTrack(track)) {
        ew[0].pm[iCharge].h3VtxZFitPtsEtaPr->Fill(vertexZ,fitPts,etaPr);
        ew[0].pm[2].h3VtxZFitPtsEtaPr->Fill(vertexZ,fitPts,etaPr);
      }
      if(CutRc::AcceptWestSideTrack(track)) {
        ew[1].pm[iCharge].h3VtxZFitPtsEtaPr->Fill(vertexZ,fitPts,etaPr);
        ew[1].pm[2].h3VtxZFitPtsEtaPr->Fill(vertexZ,fitPts,etaPr);
      }
      ew[2].pm[2].h3VtxZFitPtsEtaPr->Fill(vertexZ,fitPts,etaPr);
      ew[2].pm[iCharge].h3VtxZFitPtsEtaPr->Fill(vertexZ,fitPts,etaPr);
    }

    // all cuts
    if(CutRc::Accept(track)){
      // count and centrality and spectra
      if(CutRc::AcceptEastSideTrack(track)) {
       ew[0].pm[iCharge].h2CentralityPtPr->Fill(flowCent,ptPr);
       ew[0].pm[2].h2CentralityPtPr->Fill(flowCent,ptPr);

	//Global RAW
        ew[0].pm[iCharge].h1RawPtGlVarBin0->Fill(ptGl);
        ew[0].pm[iCharge].h1RawPtGlVarBin1->Fill(ptGl);
        ew[0].pm[2].h1RawPtGlVarBin0->Fill(ptGl);
        ew[0].pm[2].h1RawPtGlVarBin1->Fill(ptGl);
	//Global ONEOVERPT
        ew[0].pm[iCharge].h1OneOverPtGlVarBin0->Fill(ptGl,1./ptGl);
        ew[0].pm[iCharge].h1OneOverPtGlVarBin1->Fill(ptGl,1./ptGl);
        ew[0].pm[2].h1OneOverPtGlVarBin0->Fill(ptGl,1./ptGl);
        ew[0].pm[2].h1OneOverPtGlVarBin1->Fill(ptGl,1./ptGl);
	//Primary RAW 
        ew[0].pm[iCharge].h1RawPtPrVarBin0->Fill(ptPr);
        ew[0].pm[iCharge].h1RawPtPrVarBin1->Fill(ptPr);
        ew[0].pm[2].h1RawPtPrVarBin0->Fill(ptPr);
        ew[0].pm[2].h1RawPtPrVarBin1->Fill(ptPr);
	//Primary ONEOVERPT
        ew[0].pm[iCharge].h1OneOverPtPrVarBin0->Fill(ptPr,1./ptPr);
        ew[0].pm[iCharge].h1OneOverPtPrVarBin1->Fill(ptPr,1./ptPr);
        ew[0].pm[2].h1OneOverPtPrVarBin0->Fill(ptPr,1./ptPr);
        ew[0].pm[2].h1OneOverPtPrVarBin1->Fill(ptPr,1./ptPr);
      }
      if(CutRc::AcceptWestSideTrack(track)) {
       ew[1].pm[iCharge].h2CentralityPtPr->Fill(flowCent,ptPr);
       ew[1].pm[2].h2CentralityPtPr->Fill(flowCent,ptPr);

	//Global RAW
        ew[1].pm[iCharge].h1RawPtGlVarBin0->Fill(ptGl);
        ew[1].pm[iCharge].h1RawPtGlVarBin1->Fill(ptGl);
        ew[1].pm[2].h1RawPtGlVarBin0->Fill(ptGl);
        ew[1].pm[2].h1RawPtGlVarBin1->Fill(ptGl);
	//Global ONEOVERPT
        ew[1].pm[iCharge].h1OneOverPtGlVarBin0->Fill(ptGl,1./ptGl);
        ew[1].pm[iCharge].h1OneOverPtGlVarBin1->Fill(ptGl,1./ptGl);
        ew[1].pm[2].h1OneOverPtGlVarBin0->Fill(ptGl,1./ptGl);
        ew[1].pm[2].h1OneOverPtGlVarBin1->Fill(ptGl,1./ptGl);
	//Primary RAW
        ew[1].pm[iCharge].h1RawPtPrVarBin0->Fill(ptPr);
        ew[1].pm[iCharge].h1RawPtPrVarBin1->Fill(ptPr);
        ew[1].pm[2].h1RawPtPrVarBin0->Fill(ptPr);
        ew[1].pm[2].h1RawPtPrVarBin1->Fill(ptPr);
	//Primary ONEOVERPT
        ew[1].pm[iCharge].h1OneOverPtPrVarBin0->Fill(ptPr,1./ptPr);
        ew[1].pm[iCharge].h1OneOverPtPrVarBin1->Fill(ptPr,1./ptPr);
        ew[1].pm[2].h1OneOverPtPrVarBin0->Fill(ptPr,1./ptPr);
        ew[1].pm[2].h1OneOverPtPrVarBin1->Fill(ptPr,1./ptPr);
      }
      ew[2].pm[2].h2CentralityPtPr->Fill(flowCent,ptPr);
      ew[2].pm[iCharge].h2CentralityPtPr->Fill(flowCent,ptPr);

      //Global RAW
      ew[2].pm[2].h1RawPtGlVarBin0->Fill(ptGl);
      ew[2].pm[2].h1RawPtGlVarBin1->Fill(ptGl);
      ew[2].pm[iCharge].h1RawPtGlVarBin0->Fill(ptGl);
      ew[2].pm[iCharge].h1RawPtGlVarBin1->Fill(ptGl);
      //Global ONEOVERPT
      ew[2].pm[2].h1OneOverPtGlVarBin0->Fill(ptGl,1./ptGl);
      ew[2].pm[2].h1OneOverPtGlVarBin1->Fill(ptGl,1./ptGl);
      ew[2].pm[iCharge].h1OneOverPtGlVarBin0->Fill(ptGl,1./ptGl);
      ew[2].pm[iCharge].h1OneOverPtGlVarBin1->Fill(ptGl,1./ptGl);
      //Primary RAW
      ew[2].pm[2].h1RawPtPrVarBin0->Fill(ptPr);
      ew[2].pm[2].h1RawPtPrVarBin1->Fill(ptPr);
      ew[2].pm[iCharge].h1RawPtPrVarBin0->Fill(ptPr);
      ew[2].pm[iCharge].h1RawPtPrVarBin1->Fill(ptPr);
      //Primary ONEOVERPT
      ew[2].pm[2].h1OneOverPtPrVarBin0->Fill(ptPr,1./ptPr);
      ew[2].pm[2].h1OneOverPtPrVarBin1->Fill(ptPr,1./ptPr);
      ew[2].pm[iCharge].h1OneOverPtPrVarBin0->Fill(ptPr,1./ptPr);
      ew[2].pm[iCharge].h1OneOverPtPrVarBin1->Fill(ptPr,1./ptPr);
    }

  } // tracks
         
  if(mDebug)
    cout << "\ttracks : " << nTrack << endl;

}
//_____________________

void
StHiAnalysis::fillEventHistograms()
{
     Float_t flowCent   = flowCentrality(mHiMicroEvent->NUncorrectedPrimaries());
     Float_t Nch = mHiMicroEvent->NUncorrectedPrimaries();
     Float_t NGoodGlobals = mHiMicroEvent->NGoodGlobals();
     Float_t ZDCSum = mHiMicroEvent->ZDCe() + mHiMicroEvent->ZDCw();
     Float_t CTB = mHiMicroEvent->CTB();
     Float_t zvtx = mHiMicroEvent->VertexZ(); 
     Float_t yvtx = mHiMicroEvent->VertexY(); 
     Float_t xvtx = mHiMicroEvent->VertexX(); 

   //This comes before we make event cuts
   //Except we need to make sure and do trigger word cut!!!!
   if (CutRc::AcceptTrgWord(mHiMicroEvent)) {
     if(CutRc::AcceptCent(mHiMicroEvent)) { //Want uncut vertex distribution for selected trgword and centrality
       h3VertexXYZ->Fill(xvtx,yvtx,zvtx);
     }
     if(CutRc::AcceptVertexZ(mHiMicroEvent)) { //Want uncut centrality distributions for selected trgword and vertexZ
      h1FlowCent->Fill(flowCent);
      h2ZDCSumVsCTB->Fill(CTB,ZDCSum);
     }

     if (acceptEvent(mHiMicroEvent)) {
       h2NGoodGlobalsVsNch->Fill(Nch,NGoodGlobals);
     }
   } //Check Trigger Word
}

//______________________

void
StHiAnalysis::finishHistograms()
{
  h1NEvent->SetBinContent(1,mNEventAccepted);
  h1EtaCut->SetBinContent(1,CutRc::mEta[0]);
  h1EtaCut->SetBinContent(2,CutRc::mEta[1]);

  //Make weighted mean distributions...
  for (int iEW = 0; iEW < 3; iEW++) {
    for (int iPM = 0; iPM < 3; iPM++) {
      ew[iEW].pm[iPM].h1WeightedMeanPtGlVarBin0->Divide(ew[iEW].pm[iPM].h1RawPtGlVarBin0,ew[iEW].pm[iPM].h1OneOverPtGlVarBin0);
      ew[iEW].pm[iPM].h1WeightedMeanPtGlVarBin1->Divide(ew[iEW].pm[iPM].h1RawPtGlVarBin1,ew[iEW].pm[iPM].h1OneOverPtGlVarBin1);
      ew[iEW].pm[iPM].h1WeightedMeanPtPrVarBin0->Divide(ew[iEW].pm[iPM].h1RawPtPrVarBin0,ew[iEW].pm[iPM].h1OneOverPtPrVarBin0);
      ew[iEW].pm[iPM].h1WeightedMeanPtPrVarBin1->Divide(ew[iEW].pm[iPM].h1RawPtPrVarBin1,ew[iEW].pm[iPM].h1OneOverPtPrVarBin1);
    }
  }

}
//______________


ClassImp(StHiAnalysis)


#include "StVecBosSpinMaker.h"

#include "StVecBosMaker.h"


ClassImp(StVecBosSpinMaker)


StVecBosSpinMaker::StVecBosSpinMaker(const char *name, const char *etaName): StMaker(name)
{
   wMK            = 0;
   HList          = 0;
   core           = name;
   coreTitle      = etaName;
   par_QPTlow     = 0.010;

   par_QPThighET0 = 25;
   par_QPThighET1 = 50;
   par_QPThighA   = 0.08;
   par_QPThighB   = 0.0013;
   par_leptonEta1 = -1.;
   par_leptonEta2 = 1.;
   par_useNoEEMC  = 0;
}


Int_t StVecBosSpinMaker::Init()
{
   assert(wMK);
   assert(HList);
   InitHistos();
   return StMaker::Init();
}


void StVecBosSpinMaker::InitHistos()
{
  //...... data histograms
  memset(hA,0,sizeof(hA));
  TList *Lx;  TLine *ln;
  TH1 *h;
  char txt[1000], txt0[100];
  int nCase=12;

  hA[0]=h=new TH1F(core+"StatEve",core+" event count",nCase,0,nCase);
  h->GetXaxis()->SetTitleOffset(0.4);  h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);
  h->SetLineColor(kBlue);h->SetLineWidth(2);
  h->SetMarkerSize(2);//<-- large text

  const char *key[]={"inp","badBx48","noZ","BG1","BG2","Wcut","eta","W25","Qlow","Qhigh","Q +","Q -"};
  for(int i=0;i<12;i++) h->Fill(key[i],0.); // preset the order of keys
 

  hA[1]=new TH1F(core+"bX48","Rate vs. raw bx48; bXing= raw bx48",128,-0.5,127.5);
  hA[2]=new TH1F(core+"bX7","Rate vs. raw bx7; bXing= raw bx7",128,-0.5,127.5);

  hA[3]=new TH1F(core+"bX48c","Rate vs. STAR IP bXing(bx48); bXing= bx48+offset",128,-0.5,127.5);
  hA[4]=new TH1F(core+"bX7c","Rate vs. STAR IP bXing(bx7); bXing= bx7+offset",128,-0.5,127.5);

  hA[5]=new TH2F(core+"s4mon","all L2W & vertex ; bXing at STAR (7bit); spin4 from DB",128,-0.5,127.5,32,-0.5,15.5);


  hA[6]=new TH1F(core+"Y0","BG1: L2W-BHT3-rnd & vertex OK & low ET; spin4 ",16,-0.5,15.5);
  hA[7]=new TH1F(core+"Y1",Form("BG2: vertex & ET<20 &  ET 2x2 << 4x4 : %s; spin4 ",coreTitle.Data()),16,-0.5,15.5);

  hA[8]=h=new TH1F(core+"QpT","reco Q/PT,W ET>25 GeV; reco Q/PT  (1/GeV)",100,-0.099,0.099);
  float highCut=par_QPThighA - (par_QPThighET1-par_QPThighET0)*par_QPThighB; 
    
  if(par_QPTlow>0) { // abaility to skip all Q/PT cuts
    Lx=h->GetListOfFunctions();
    ln=new TLine(par_QPTlow,0,par_QPTlow,1e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
    ln=new TLine(-par_QPTlow,0,-par_QPTlow,1e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
    float avrC=(par_QPThighA+highCut)/2.; 
    ln=new TLine(-avrC,0,-avrC,1e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
    ln=new TLine(avrC,0,avrC,1e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
  }

  hA[9]=h=new TH2F(core+"QpT2","TPC PRIM  Q/PT ; 2x2 cluster ET (GeV); Q/PT  (1/GeV)",100,0.,100.,100,-0.1,0.1);
    Lx=h->GetListOfFunctions();
    ln=new TLine(0,0,100,0);  ln->SetLineColor(kBlue);  Lx->Add(ln);

    if(par_QPTlow>0) { // abaility to skip all Q/PT cuts
    ln=new TLine(0,par_QPTlow,100,par_QPTlow);  ln->SetLineColor(kRed);  Lx->Add(ln);  
    ln=new TLine(0,-par_QPTlow,100,-par_QPTlow);  ln->SetLineColor(kRed);  Lx->Add(ln);  
    ln=new TLine(25,-0.1, 25,0.1);  ln->SetLineColor(kRed);  Lx->Add(ln);
    ln=new TLine(par_QPThighET0,par_QPThighA,par_QPThighET1,highCut);  ln->SetLineColor(kRed);  Lx->Add(ln);
    ln=new TLine(par_QPThighET0,-par_QPThighA,par_QPThighET1,-highCut);  ln->SetLineColor(kRed);  Lx->Add(ln);
  }

  //use 10-19
  char cPM[2]={'P','N'}; // Positive, Negative
  char iCol[2]={46,9};
  for(int ipn=0;ipn<2;ipn++){ 
    TH1 *h;
    
    //.... J-peak 
    sprintf(txt0,"ET_%c",cPM[ipn]);
    sprintf(txt,"Final W, charge=%c : %s; 2x2 ET (GeV) ",cPM[ipn],coreTitle.Data());
    hA[10+ipn]=h=new TH1F(core+txt0,txt, 100,1,101); // shifted by 1 for nicer Rebin
    h->SetFillColor(iCol[ipn]);

    //.... 1D spin sorting         
    sprintf(txt0,"Y2_%c",cPM[ipn]);
    sprintf(txt,"Final W Q=%c, 2x2 ET=[25,50]GeV : %s; spin4  ",cPM[ipn],coreTitle.Data());
    hA[12+ipn]=h=new TH1F(core+txt0,txt,16,-0.5,15.5);
    h->SetFillColor(iCol[ipn]);

    sprintf(txt0,"Y3_%c",cPM[ipn]);
    sprintf(txt,"Final W Q=%c, 2x2 ET=[32,44]GeV : %s; spin4  ",cPM[ipn],coreTitle.Data());
    hA[14+ipn]=h=new TH1F(core+txt0,txt,16,-0.5,15.5);
    h->SetFillColor(iCol[ipn]);

    sprintf(txt0,"Y4_%c",cPM[ipn]);
    sprintf(txt,"Final QCD Q=%c, 2x2 ET=[15,20]GeV : %s; spin4  ",cPM[ipn],coreTitle.Data());
    hA[16+ipn]=h=new TH1F(core+txt0,txt,16,-0.5,15.5);
    h->SetFillColor(iCol[ipn]);

    sprintf(txt0,"Y5_%c",cPM[ipn]);
    sprintf(txt,"Final 2x2 ET  Q=%c; spin4 : %s; 2x2 cluster ET (GeV) ",cPM[ipn],coreTitle.Data());
    hA[18+ipn]=h=new TH2F(core+txt0,txt,16,-0.5,15.5,10,0,100);
    h->SetFillColor(iCol[ipn]);
  }
  // free 20-29

  hA[30]=h=new TH1F(core+"LepEta",Form("selecting Ws : %s ; lepton LAB eta",coreTitle.Data()),100, -1.5,1.5);
  Lx=h->GetListOfFunctions();
  ln=new TLine(par_leptonEta1,0,par_leptonEta1,1e6);  ln->SetLineColor(kRed);  Lx->Add(ln);
  ln=new TLine(par_leptonEta2,0,par_leptonEta2,1e6);  ln->SetLineColor(kRed);  Lx->Add(ln);

  hA[31]=h=new TH1F(core+"LumET","Lumi monitor; 2x2 ET (GeV)",100,0.,100.);


  // add histos to the list (if provided)
  for(int i=0;i<mxHA;i++) {
    if(  hA[i]==0) continue;
    HList->Add( hA[i]);
  }
  //  HList->ls();
  LOG_INFO<<Form("%s::InitHistos done1",GetName())<<endm;

}


Int_t StVecBosSpinMaker::FinishRun(int runNo)
{
   return kStOK;
}


Int_t StVecBosSpinMaker::InitRun(int runNo)
{
   //j1  sprintf(txt,"bXing= bx48+off=%d",spinDb->BX48offset());
   //j1 hA[3]->GetXaxis()->SetTitle(txt);

   //j1 sprintf(txt,"bXing= bx7+off=%d",spinDb->BX7offset());
   //j1 hA[4]->GetXaxis()->SetTitle(txt);

   LOG_INFO << Form("::InitRun(%d) done, W-spin sorting  params: exclude |Q/PT| < %.2f OR |Q/PT| above line %.3f*(ET-%.1f)-%6e if ET<%.1f, for AL use leptonEta in[%.1f,%.1f] useNoEEMC=%d", runNo,
                    par_QPTlow, par_QPThighET0, par_QPThighA , par_QPThighB, par_QPThighET1, par_leptonEta1, par_leptonEta2, par_useNoEEMC
                   ) << endm;
   return kStOK;
}


Int_t StVecBosSpinMaker::Make()
{
   //bXingSort();
   return kStOK;
}


void StVecBosSpinMaker::bXingSort()
{
   //has access to whole W-algo-maker data via pointer 'wMK'

   hA[0]->Fill("inp", 1.);

   if (wMK->mVecBosEvent->mVertices.size() <= 0) return;
   // require: L2W-trig (ET or rnd) & vertex is reasonable

   int bx48     = wMK->mVecBosEvent->bx48;
   int bx7      = wMK->mVecBosEvent->bx7;
   int bxStar48 = wMK->mVecBosEvent->bxStar48;
   int bxStar7  = wMK->mVecBosEvent->bxStar7;

   if (bxStar48 != bxStar7) {
      printf("BAD bx7=%d bx48=%d del=%d\n", bx7, bx48, bxStar48 - bxStar7);
      hA[0]->Fill("badBx48", 1.);
      return; // both counters must be in sync
   }

   //remove events tagged as Zs
   if (wMK->mVecBosEvent->zTag) return;
   hA[0]->Fill("noZ", 1.);

   hA[1]->Fill(bx48);
   hA[2]->Fill(bx7);

   hA[3]->Fill(bxStar48);
   hA[4]->Fill(bxStar7);

   int spin4 = wMK->mVecBosEvent->mSpinPattern4Bits;
   hA[5]->Fill(bxStar7, spin4);

   float par_maxDsmThr = 58;
   float par_myET = 25; // monitoring cut

   if ( wMK->mVecBosEvent->l2bitRnd) { // lumi monitor BHT3-random
      // avoid too much energy - can be W-events (1/milion :)
      if (wMK-> mVecBosEvent->bemc.maxHtDsm < par_maxDsmThr)  {
         hA[6]->Fill(spin4);  hA[0]->Fill("BG1", 1.);
      }
      return; // LOGICAL ERROR - FIX IT LATER
   }

   if ( wMK->mVecBosEvent->l2bitET == 0) return;
   //..... it is guaranteed ..... L2W-ET>13 did fired  ......

   // search for Ws
   VecBosVertexPtrSetIter iVertex = wMK->GetVecBosEvent()->mVertices.begin();

   for (short iv=0 ; iVertex != wMK->GetVecBosEvent()->mVertices.end(); ++iVertex, iv++)
   {
      VecBosVertex &V = **iVertex;

      for (uint it = 0; it < V.eleTrack.size(); it++) {
         VecBosTrack &T = V.eleTrack[it];
         if (T.mMatchedTower.id == 0) continue;

         // Collect QCD background for lumi monitors
         float frac24 = T.mCluster2x2.ET / (T.mCluster4x4.ET);

         if (iv == 0 && it == 0 && frac24 < wMK->mMinBClusterEnergyIsoRatio) {
            hA[31]->Fill(T.mCluster2x2.ET);
            if ( T.mCluster2x2.ET < 20. ) { hA[7]->Fill(spin4);  hA[0]->Fill("BG2", 1.);}
         }

         if (T.HasCluster() == false) continue;
         assert(T.mCluster2x2.nTower > 0); // internal logical error
         assert(T.mP3InNearCone.Pt() > 0); // internal logical error

         int iQ = 0; // plus
         float p_Q = T.mStMuTrack->charge();
         if ( p_Q < 0 ) iQ = 1; // minus
         float ET = T.mCluster2x2.ET;

         //put final W cut here
         bool isW = T.mCluster2x2.ET / T.mP3InNearCone.Pt() > wMK->par_nearTotEtFrac; // near cone

         if (par_useNoEEMC)
            isW = isW && T.sPtBalance_noEEMC > wMK->par_ptBalance; // awayET
         else
            isW = isW && T.sPtBalance > wMK->par_ptBalance; // awayET

         if (!isW) { // AL(QCD)
            if (ET > 15 && ET < 20 ) hA[16 + iQ]->Fill(spin4);
            continue;
         }

         hA[0]->Fill("Wcut", 1.);

         hA[30]->Fill(T.mStMuTrack->eta());
         // allows spin specific cuts on eta
         if (T.mStMuTrack->eta() < par_leptonEta1) continue;
         if (T.mStMuTrack->eta() > par_leptonEta2) continue;
         hA[0]->Fill("eta", 1.);

         //::::::::::::::::::::::::::::::::::::::::::::::::
         //:::::accepted W events for x-section :::::::::::
         //::::::::::::::::::::::::::::::::::::::::::::::::

         if (ET > par_myET) hA[0]->Fill("W25", 1.);
         float q2pt = T.mStMuTrack->charge() / T.mStMuTrack->pt();
         if (ET > par_myET) hA[8]->Fill(q2pt);
         hA[9]->Fill(ET, q2pt);

         // apply cut on reco charge
         if ( fabs(q2pt) < par_QPTlow) continue;
         if (ET > par_myET) hA[0]->Fill("Qlow", 1.);

         if (par_QPTlow > 0) { // abaility to skip all Q/PT cuts
            if ( fabs(q2pt) < par_QPTlow) continue;
            float highCut = par_QPThighA - (ET - par_QPThighET0) * par_QPThighB;
            // printf("fff ET=%f q2pr=%f highCut=%f passed=%d\n",ET, q2pt,highCut,fabs(q2pt)<highCut);
            if ( ET > par_myET && ET < par_QPThighET1 && fabs(q2pt) > highCut) continue;
         }

         if (ET > par_myET) {
            hA[0]->Fill("Qhigh", 1.);
            if (p_Q > 0) hA[0]->Fill("Q +", 1.);
            else  hA[0]->Fill("Q -", 1.);
         }

         hA[10 + iQ]->Fill(ET);
         if (ET > 25 && ET < 50 ) hA[12 + iQ]->Fill(spin4);
         if (ET > 32 && ET < 44 ) hA[14 + iQ]->Fill(spin4);

         hA[18 + iQ]->Fill(spin4, ET);
      } // end of loop over tracks
   } // end of loop ove vertices
}

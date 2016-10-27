#include "StVecBosMaker.h"
#include "VecBosMcMaker.h"
#include "StEmcUtil/geometry/StEmcGeom.h"

//need these to get MC record
#include "tables/St_g2t_tpc_hit_Table.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcVertex.hh"
#include "StMcEvent/StMcTrack.hh"

#include "Globals.h"


ClassImp(VecBosMcMaker)


VecBosMcMaker::VecBosMcMaker(const char *name): StMaker(name)
{
   wMK = 0; HList = 0;
}


VecBosMcMaker::~VecBosMcMaker()
{
}


Int_t VecBosMcMaker::Init()
{
   assert(wMK);
   assert(HList);
   initHistos();
   return StMaker::Init();
}


Int_t VecBosMcMaker::Make()
{
   //printf("-----------in %s\n", GetName());
   //only get geant particle info for W MC
   //if (wMK->isMC == 8 || wMK->isMC == 30 || wMK->isMC == 20) { //S.F. oct 12, 2012 
   if (doMCanalysis()) {
      doWanalysis();
      doWefficiency();
   }
   //}
   return kStOK;
}


void VecBosMcMaker::doWanalysis()
{
   //has access to whole W-algo-maker data via pointer 'wMK'

   // run through W cuts to fill other histos............
   VecBosVertexPtrSetIter iVertex = wMK->GetVecBosEvent()->mVertices.begin();

   for ( ; iVertex != wMK->GetVecBosEvent()->mVertices.end(); ++iVertex)
   {
      VecBosVertex &V = **iVertex;

      for (uint it = 0; it < V.eleTrack.size(); it++) {
         VecBosTrack &T = V.eleTrack[it];
         if (!T.HasCluster()) continue;
         assert(T.mCluster2x2.nTower > 0); // internal logical error
         assert(T.mP3InNearCone.Pt() > 0); // internal logical error

         if (T.mCluster2x2.ET / T.mP3InNearCone.Pt() < wMK->GetNearTotEtFrac()) continue; // too large nearET
         if (T.awayTotET > 30.) continue; // too large awayET , Jan
         //Full W cuts applied at this point

         //W info from pythia record
         hA[1]->Fill(mWP.Perp());
         hA[2]->Fill(mWP.z());

         hA[24]->Fill(mWP.Eta());
         hA[25]->Fill(T.hadronicRecoil.Eta());
         hA[26]->Fill(mWP.Perp(), T.hadronicRecoil.Eta());
         if (mWP.Eta() > 0) {
            hA[27]->Fill(T.hadronicRecoil.Eta());
         }
         else {
            hA[28]->Fill(T.hadronicRecoil.Eta());
         }

         //hadronic recoil and correlations with W from pythia
         TVector3 hadronicPt(T.hadronicRecoil.X(), T.hadronicRecoil.Y(), 0); //transverse momentum vector
         hA[3]->Fill(mWP.Perp() - hadronicPt.Perp());
         hA[4]->Fill(mWP.Perp(), hadronicPt.Perp());
         hA[5]->Fill(hadronicPt.Perp());

         float delPhi = mWP.DeltaPhi(-hadronicPt);
         hA[6]->Fill(mWP.Perp(), delPhi);
         hA[7]->Fill(mWP.Perp(), mWP.Perp() - hadronicPt.Perp());
         hA[8]->Fill(T.hadronicRecoil.Perp(), delPhi);

         //electron reco and from geant record
         hA[9]->Fill(mElectronP.Perp());
         hA[10]->Fill(T.mCluster2x2.ET);
         hA[11]->Fill(mElectronP.Perp(), T.mCluster2x2.ET);
         hA[12]->Fill(mElectronP.Perp(), mElectronP.Perp() - T.mCluster2x2.ET);

         TVector3 electronPt(T.mCluster2x2.position.X(), T.mCluster2x2.position.Y(), 0); //transvers momentum vector
         electronPt.SetMag(T.mCluster2x2.ET);

         //neutrino reco and from geant record
         TVector3 neutrinoPt = -1 * (hadronicPt + electronPt);
         hA[13]->Fill(neutrinoPt.Perp());
         hA[14]->Fill(mNeutrinoP.Perp());
         hA[15]->Fill(mNeutrinoP.Perp(), neutrinoPt.Perp());
         hA[16]->Fill(mNeutrinoP.Perp() - neutrinoPt.Perp());

         hA[17]->Fill(mNeutrinoP.Perp(), mElectronP.Perp());

         float recoDeltaPhi = neutrinoPt.DeltaPhi(electronPt);
         float geantDeltaPhi = mNeutrinoP.DeltaPhi(mElectronP);

         hA[18]->Fill(geantDeltaPhi, recoDeltaPhi);
         hA[19]->Fill(cos(geantDeltaPhi) - cos(recoDeltaPhi));

         float Mt = sqrt(2 * T.mCluster2x2.ET * neutrinoPt.Perp() * (1 - cos(recoDeltaPhi))); //real data
         float gMt = sqrt(2 * mElectronP.Perp() * mNeutrinoP.Perp() * (1 - cos(geantDeltaPhi)));

         hA[20]->Fill(Mt);
         hA[21]->Fill(gMt);

         hA[22]->Fill(Mt, T.mCluster2x2.ET);
         hA[23]->Fill(gMt - Mt);

         //Kinematics
         //reconstruct W pL from reconstructed quantities
         float trueWpL = mWP.z();
         float eleTheta = T.mP3AtDca.Theta();
         float ratioE = T.mCluster2x2.mEnergy / 40.0;
         float pLRecoPlus = 80.0 * (ratioE) * ((cos(eleTheta)) + sqrt(cos(eleTheta) * cos(eleTheta) + sin(eleTheta) * sin(eleTheta) * (1 - ratioE * ratioE))) / (ratioE * ratioE * sin(eleTheta) * sin(eleTheta)); //+ sqrt solution
         float pLRecoMinus = 80.0 * (ratioE) * ((cos(eleTheta)) - sqrt(cos(eleTheta) * cos(eleTheta) + sin(eleTheta) * sin(eleTheta) * (1 - ratioE * ratioE))) / (ratioE * ratioE * sin(eleTheta) * sin(eleTheta)); //- sqrt solution
         hA[29]->Fill(pLRecoPlus);
         hA[30]->Fill(trueWpL - pLRecoPlus);
         hA[31]->Fill(trueWpL, pLRecoPlus);

         hA[32]->Fill(pLRecoMinus);
         hA[33]->Fill(trueWpL - pLRecoMinus);
         hA[34]->Fill(trueWpL, pLRecoMinus);

         const StMuTrack *prTr = T.mStMuTrack; assert(prTr);
         float p_chrg = prTr->charge();
         if (p_chrg > 0) continue;

         float eleEta = T.mP3AtDca.Eta();
         //sort 2 solutions by electron eta
         if (eleEta < -0.8) {
            hA[35]->Fill(trueWpL, pLRecoMinus);
            hA[37]->Fill(trueWpL, pLRecoPlus);
         }
         else if (eleEta > 0.8) {
            hA[36]->Fill(trueWpL, pLRecoMinus);
            hA[38]->Fill(trueWpL, pLRecoPlus);
         }

         if (T.mCluster2x2.ET < 30) continue; //only W's we find in data
         //Correlate W pL with electron E in 3 electron eta ranges
         if (eleEta < -0.8) {
            hA[39]->Fill(mWP.z(), T.mCluster2x2.mEnergy);
            hA[42]->Fill(T.mCluster2x2.mEnergy);
         }
         if (eleEta > 0.8) {
            hA[40]->Fill(mWP.z(), T.mCluster2x2.mEnergy);
            hA[43]->Fill(T.mCluster2x2.mEnergy);
         }
         if (eleEta > -0.1 && eleEta < 0.1) {
            hA[41]->Fill(mWP.z(), T.mCluster2x2.mEnergy);
            hA[44]->Fill(T.mCluster2x2.mEnergy);
         }

      }
   }
}


void VecBosMcMaker::doWefficiency()
{
   //only count leptons in our eta range
   if (fabs(mElectronP.Eta()) > 1.) return;
   //ele has |eta| < 1
   hA[50]->Fill(mElectronP.Perp());
   hA[54]->Fill(mElectronP.Eta());
   hA[58]->Fill(mVertex.Z());
   hA[62]->Fill(mElectronP.Phi());

   hA[68]->Fill(mElectronP.Perp()); //forJoe

   //plot for Scott
   TVector3 detEle; //where lepton would hit BEMC
   float Rcylinder = gBTowGeom->Radius();
   detEle.SetPtEtaPhi(Rcylinder, mElectronP.Eta(), mElectronP.Phi());
   detEle.SetZ(detEle.Z() + mVertex.Z());
   hA[66]->Fill(detEle.Eta(), mElectronP.Perp());

   //trigger efficiency
   if (!wMK->GetVecBosEvent()->l2bitET) return;
   //good trig
   hA[51]->Fill(mElectronP.Perp());
   hA[55]->Fill(mElectronP.Eta());
   hA[59]->Fill(mVertex.Z());
   hA[63]->Fill(mElectronP.Phi());
   hA[69]->Fill(mElectronP.Perp());//forJoe

   hA[67]->Fill(detEle.Eta(), mElectronP.Perp());

   //vertex efficiency
   if (wMK->GetVecBosEvent()->mVertices.size() <= 0) return;
   //vertex rank>0 and |z|<100
   hA[52]->Fill(mElectronP.Perp());
   hA[56]->Fill(mElectronP.Eta());
   hA[60]->Fill(mVertex.Z());
   hA[64]->Fill(mElectronP.Phi());

   hA[70]->Fill(mElectronP.Perp());//forJoe

   //float diff=wMK->GetVecBosEvent()->mVertices[0].z - mVertex.Z();
   //cout<<"diff="<<diff<<endl;

   //reco efficiency
   VecBosVertexPtrSetIter iVertex = wMK->GetVecBosEvent()->mVertices.begin();

   for ( ; iVertex != wMK->GetVecBosEvent()->mVertices.end(); ++iVertex)
   {
      VecBosVertex &V = **iVertex;

      for (uint it = 0; it < V.eleTrack.size(); it++) {
         VecBosTrack &T = V.eleTrack[it];
         if (!T.HasCluster()) continue;
         assert(T.mCluster2x2.nTower > 0); // internal logical error
         assert(T.mP3InNearCone.Pt() > 0); // internal logical error

         if (T.mCluster2x2.ET / T.mP3InNearCone.Pt() < wMK->GetNearTotEtFrac())
            continue; // too large nearET
         if (T.ptBalance.Perp() < wMK->GetPtBalance() || T.awayTotET > 30.) //Jan
            continue;

         //pass all W cuts
         hA[53]->Fill(mElectronP.Perp());
         hA[57]->Fill(mElectronP.Eta());
         hA[61]->Fill(mVertex.Z());
         hA[65]->Fill(mElectronP.Phi());

         hA[71]->Fill(mElectronP.Perp());//forJoe
      }
   }
}


bool VecBosMcMaker::doMCanalysis()
{
   StMcEvent *mMcEvent = 0;
   mMcEvent = (StMcEvent *) StMaker::GetChain()->GetDataSet("StMcEvent");
   assert(mMcEvent);

   //initialize momentum vectors
   float eW;
   StThreeVectorF pW;
   StThreeVectorF pNeutrino;
   StThreeVectorF pElectron;

   StMcVertex *V = mMcEvent->primaryVertex();
   mVertex = TVector3(V->position().x(), V->position().y(), V->position().z());

   uint i = 1;
   int found = 0;
   while (found < 2 && i < mMcEvent->tracks().size()) { //loop tracks
      StMcTrack *mcTrack = mMcEvent->tracks()[i];
      int pdgId = mcTrack->pdgId();
      //float pt=mcTrack->pt();
      //LOG_INFO<<"pdgId "<<pdgId<<" pt "<<pt<<" pz "<<mcTrack->momentum().z()<<endm;

      if (pdgId == 11 || pdgId == -11) { //select e+ and e-
         if (abs(mcTrack->parent()->pdgId()) == 24 ) {
            pElectron = mcTrack->momentum();
            //LOG_INFO<<"pdgId "<<pdgId<<" pt "<<pt<<" pz "<<mcTrack->momentum().z()<<endm;
            pW = mcTrack->parent()->momentum();
            eW = mcTrack->parent()->energy();
            //LOG_INFO<<"pdgId "<<mcTrack->parent()->pdgId()<<" pt "<<mcTrack->parent()->pt()<<" pz "<<mcTrack->parent()->momentum().z()<<endm;
            found++;
         }
      }

      if (pdgId == 12 || pdgId == -12) { //select neutrino
         if (abs(mcTrack->parent()->pdgId()) == 24 ) {
            pNeutrino = mcTrack->momentum();
            //LOG_INFO<<"pdgId "<<pdgId<<" pt "<<pt<<" pz "<<mcTrack->momentum().z()<<endm;
            pW = mcTrack->parent()->momentum();
            eW = mcTrack->parent()->energy();
            //LOG_INFO<<"pdgId "<<mcTrack->parent()->pdgId()<<" pt "<<mcTrack->parent()->pt()<<" pz "<<mcTrack->parent()->momentum().z()<<endm;
            found++;
         }
      }
      i++;
   }

   if (found != 2) return false;

   mWP = TVector3(pW.x(), pW.y(), pW.z());
   mNeutrinoP = TVector3(pNeutrino.x(), pNeutrino.y(), pNeutrino.z());
   mElectronP = TVector3(pElectron.x(), pElectron.y(), pElectron.z());
   TVector3 diff = mWP - mNeutrinoP - mElectronP;

   if (diff.Mag() > 0.0001) //should get exactly right
      LOG_INFO << "\n \n W+e+nu vector sum =" << diff.Mag() << endm;

   if (mElectronP.Mag() < 0.0001)
      LOG_INFO << "\n \n no lepton track =" << endm;

   //calculate x1 and x2 from W rapidity
   float rapW = 0.5 * log((eW + mWP.Z()) / (eW - mWP.Z()));
   float mw2sqs = 80.4 / 500.;
   float x1 = mw2sqs * exp(rapW);
   float x2 = mw2sqs * exp(-rapW);

   hA[72]->Fill(rapW);
   if (fabs(mElectronP.Eta()) < 1) { //require midrapidity e
      hA[73]->Fill(x1);
      hA[74]->Fill(x2);
      hA[75]->Fill(x1, x2);
      hA[76]->Fill(x1 - x2);
   }

   return true;
}


void VecBosMcMaker::initHistos()
{
  //const float PI=TMath::Pi();
  TString core="MC"; // prefix added to every histo name, to allow for multipl maker saving histos in the same root file

  //...... data histograms
  memset(hA,0,sizeof(hA));
  TH1 *h; //TH2 *h2;
  //TList *Lx;  TLine *ln; 
  const float PI=TMath::Pi();
    
  //Quantities from geant and correlations w/ reconstructed values 
  hA[1]=h=new TH1F(core+"Wpt","W pt from Geant; W pt",50,0,50);
  hA[2]=h=new TH1F(core+"WpL","W pL from Geant; W pL",100,-100,100);
  hA[3]=h=new TH1F(core+"WminusHadRecoilpt","W pt (from Geant) - Hadronic Recoil pt ; W - hadronic recoil pt",100,-20,20);
  hA[4]=h=new TH2F(core+"hadRec_Wpt","Hadronic Recoil pt vs W pt from Geant; W pT; hadronic recoil pt",100,0,20,100,0,20); 
  hA[5]=h=new TH1F(core+"hadRecoilPt","Hadronic recoil pt; Hadronic recoil pt",50,0,50);
  hA[6]=h=new TH2F(core+"delPhi_Wpt","#Delta #phi (W + hadronic recoil) vs W pT from Geant; W pT; #Delta #phi",50,0,50,100,-4,4);
  hA[7]=h=new TH2F(core+"WptminusHad_Wpt","W pt (from Geant) - Hadronic Recoil pt vs W pt; W pt; W - hadronic recoil pt",50,0,50,100,-20,20);
  hA[8]=h=new TH2F(core+"delPhi_Recoilpt","#Delta #phi (W + hadronic recoil) vs Hadronic Recoil pt; Hadronic Recoil pT; #Delta #phi",50,0,50,100,-4,4);
  
  //electron plots
  hA[9]=h=new TH1F(core+"electronGeantPt","Electron pt from Geant; Geant electron pt",60,0,60);
  hA[10]=h=new TH1F(core+"electronRecoPt","Electron Reco pt ; 2x2 cluster ET",60,0,60);
  hA[11]=h=new TH2F(core+"electronRecovsGeant","Electron Reco pt vs Geant pt; Geant electron pt; 2x2 cluster Et",60,0,60,60,0,60);
  hA[12]=h=new TH2F(core+"diffElectronPtvsGeantpt","Electron pt (Geant - Reco) vs Geant; Geant Electron pT; Geant - Reco Electron pT",60,0,60,100,-20,20); 
  
  //neutrino plots
  hA[13]=h=new TH1F(core+"neutrinoRecoPt","Neutrino pt (ie -(had. recoil - ele) pT); Reco Neutrino pT",60,0,60);
  hA[14]=h=new TH1F(core+"neutrinoGeantPt","Neutrino pt from Geant; Geant neutrino pT",60,0,60);
  hA[15]=h=new TH2F(core+"neutrinoRecovsGeant","Neutrino Reco pt vs Geant pt; Geant neutrino pt; Reco neutrino Et",60,0,60,60,0,60);
  hA[16]=h=new TH1F(core+"diffNeutrinoPt","Neutrino pt (Geant - Reco); Geant - Reco Neutrino pT",100,-20,20);
  hA[17]=h=new TH2F(core+"eleG_neutrinoG","Electron Geant pt vs Neutrino Geant pt; Geant Neutrino pt; Geant Electron pt",60,0,60,60,0,60);
  
  //Transvers Mass Reco
  hA[18]=h=new TH2F(core+"delPhiGeant_Reco","Reco #phi_{e,#nu} vs Geant #phi_{e,#nu}; #phi_{e,#nu} Reco ; #phi_{e,#nu} Geant",50,-6,6,50,-6,6);  
  hA[19]=h=new TH1F(core+"delPhiGeantminusReco","Reco cos(#phi_{e,#nu}) minus Geant cos(#phi_{e,#nu}); Geant-Reco ",50,-.2,.2);
  hA[20]=h=new TH1F(core+"mT","Reco Transverse Mass; Reco m_{T}",100,0,100);
  hA[21]=h=new TH1F(core+"gMT","Geant Transverse Mass; Geant m_{T}",100,0,100);
  hA[22]=h=new TH2F(core+"mTvsEleEt","Reco Electron ET vs Reco Transverse Mass; m_{T} GeV;2x2 Cluster ET GeV",100,0,100,60,0,60);
  hA[23]=h=new TH1F(core+"GmTminusmT","Geant - Reco Transverse Mass; Geant - Reco mT (GeV)",100,-20,20);

  //Hadronic Recoil Eta
  hA[24]=h=new TH1F(core+"Weta","W #eta from Geant; W #eta from Geant",100,-4,4);
  hA[25]=h=new TH1F(core+"RecoilEta_all","Hadronic Recoil #eta (for all W #eta); Hadronic Recoil #eta",50,-4,4);
  hA[26]=h=new TH2F(core+"RecoilEtaAll_Wpt","Hadronic Recoil #eta (for all W #eta) vs W pt (from Geant); W pt; Hadronic Recoil #eta",50,0,50,50,-4,4);
  hA[27]=h=new TH1F(core+"RecoilEta_WetaPos","Hadronic Recoil #eta (for W #eta > 0); Hadronic Recoil #eta",50,-4,4);
  hA[28]=h=new TH1F(core+"RecoilEta_WetaNeg","Hadronic Recoil #eta (for W #eta < 0); Hadronic Recoil #eta",50,-4,4);
    

  //Reconstructing W pL
  hA[29]=h=new TH1F(core+"RecoPlusWpL","Reco W pL; RecoPlus W pL",100,-200,200);
  hA[30]=h=new TH1F(core+"GeantMinusRecoPlusWpL","Geant W pL - Reco W pL; Geant - RecoPlus W pL",100,-200,200);
  hA[31]=h=new TH2F(core+"RecoPlusvsGeantWpL","RecoPlus W pL vs Geant W pL ; Geant W pL;Reco W pL",100,-200,200,200,-200,200);
  hA[32]=h=new TH1F(core+"RecoMinusWpL","RecoMinus W pL; RecoMinus W pL",100,-200,200);
  hA[33]=h=new TH1F(core+"GeantMinusRecoMinusWpL","Geant W pL - RecoMinus W pL; Geant - RecoMinus W pL",100,-200,200);
  hA[34]=h=new TH2F(core+"RecoMinusvsGeantWpL","RecoMinus W pL vs Geant W pL ; Geant W pL;RecoMinus W pL",100,-200,200,100,-200,200);

  hA[35]=h=new TH2F(core+"RecoMinusvsGeantWpL_neg","RecoMinus W pL vs Geant W pL (for ele #eta < -0.8); Geant W pL;RecoMinus W pL",100,-200,200,100,-200,200);
  hA[36]=h=new TH2F(core+"RecoMinusvsGeantWpL_pos","RecoMinus W pL vs Geant W pL (for ele #eta > 0.8); Geant W pL;RecoMinus W pL",100,-200,200,100,-200,200);
  hA[37]=h=new TH2F(core+"RecoPlusvsGeantWpL_neg","RecoPlus W pL vs Geant W pL (for ele #eta < -0.8); Geant W pL;RecoPlus W pL",100,-200,200,100,-200,200);
  hA[38]=h=new TH2F(core+"RecoPlusvsGeantWpL_pos","RecoPlus W pL vs Geant W pL (for ele #eta > 0.8); Geant W pL;RecoPlus W pL",100,-200,200,100,-200,200);
  
  //ele E vs W pL for different thetas
  hA[39]=h=new TH2F(core+"ElectronEvsWpL_neg","Electron E vs W pL (from Geant) for #eta < -0.8; W pL from Geant; Reco Electron E",100,-100,100,100,0,100);
  hA[40]=h=new TH2F(core+"ElectronEvsWpL_pos","Electron E vs W pL (from Geant) for #eta > 0.8; W pL from Geant; Reco Electron E",100,-100,100,100,0,100);
  hA[41]=h=new TH2F(core+"ElectronEvsWpL_zero","Electron E vs W pL (from Geant) for -0.1 < #eta < 0.1; W pL from Geant; Reco Electron E",100,-100,100,100,0,100);
  hA[42]=h=new TH1F(core+"ElectronE_neg","Electron E for #eta < -0.8;  Reco Electron E",100,0,100);
  hA[43]=h=new TH1F(core+"ElectronE_pos","Electron E for #eta > 0.8;  Reco Electron E",100,0,100);
  hA[44]=h=new TH1F(core+"ElectronE_zero","Electron E for -0.1 < #eta < 0.1; Reco Electron E",100,0,100);

  //free 45-49

  //efficiency histos
  hA[50]=h=new TH1F(core+"eleETall","pt of all leptons ; lepton pt (from Geant)",100,0,100);
  hA[51]=h=new TH1F(core+"eleETtrig","pt of leptons that satisfy trigger ; lepton pt (from Geant)",100,0,100);
  hA[52]=h=new TH1F(core+"eleETvert","pt of leptons that w/ good vertex ; lepton pt (from Geant)",100,0,100);
  hA[53]=h=new TH1F(core+"eleETreco","pt of leptons that pass W cuts ; lepton pt (from Geant)",100,0,100);
  hA[54]=h=new TH1F(core+"eleEtaAll","#eta of all leptons ; lepton #eta (from Geant)",100,-1.1,1.1);
  hA[55]=h=new TH1F(core+"eleEtaTrig","#eta of leptons that satisfy trigger ; lepton #eta (from Geant)",100,-1.1,1.1);
  hA[56]=h=new TH1F(core+"eleEtaVert","#eta of leptons that w/ good vertex ; lepton #eta (from Geant)",100,-1.1,1.1);
  hA[57]=h=new TH1F(core+"eleEtaReco","#eta of leptons that pass W cuts ; lepton #eta (from Geant)",100,-1.1,1.1);
  hA[58]=h=new TH1F(core+"eleZvertAll","zVertex of all events ; zVertex (from Geant)",100,-100,100);
  hA[59]=h=new TH1F(core+"eleZvertTrig","zVertex of events that satisfy trigger ; zVertex (from Geant)",100,-100,100);
  hA[60]=h=new TH1F(core+"eleZvertVert","zVertex of events that w/ good vertex ; zVertex (from Geant)",100,-100,100);
  hA[61]=h=new TH1F(core+"eleZvertReco","zVertex of events that pass W cuts ; zVertex (from Geant)",100,-100,100);
  hA[62]=h=new TH1F(core+"elePhiAll","#phi of all leptons; lepton #phi (from Geant)",64,-PI,PI);
  hA[63]=h=new TH1F(core+"elePhiTrig","#phi of leptons that satisfy trigger ; lepton #phi (from Geant)",64,-PI,PI);
  hA[64]=h=new TH1F(core+"elePhiVert","#phi of leptons that w/ good vertex ; lepton #phi (from Geant)",64,-PI,PI);
  hA[65]=h=new TH1F(core+"elePhiReco","#phi of leptons that pass W cuts ; lepton #phi (from Geant)",64,-PI,PI);
  
  //different binning for ET histograms
  hA[68]=h=new TH1F(core+"eleETallJoe","pt of all leptons ; lepton pt (from Geant)",100,1,101);
  hA[69]=h=new TH1F(core+"eleETtrigJoe","pt of leptons with good trigger; lepton pt (from Geant)",100,1,101);
  hA[70]=h=new TH1F(core+"eleETvertJoe","pt of leptons with good vertex; lepton pt (from Geant)",100,1,101);
  hA[71]=h=new TH1F(core+"eleETrecoJoe","pt of reconstructed leptons; lepton pt (from Geant)",100,1,101);

  //plot for Scott to look at trigger effic W+ vs W-
  hA[66]=h=new TH2F(core+"eleEta_ptPreTrig","Reconstructed lepton pt vs lepton detector #eta from Geant (before trig); lepton detector #eta; lepton pt (from Geant)",100,-1.1,1.1,100,0,100);
  hA[67]=h=new TH2F(core+"eleEta_ptPostTrig","Reconstructed lepton pt vs lepton detector #eta from Geant (after trig); lepton detector #eta; lepton pt (from Geant)",100,-1.1,1.1,100,0,100);
  
  //x1 and x2 distributions
  hA[72]=h=new TH1F(core+"wRapid","Rapidity of W; W rapidity",100,-2,2);
  hA[73]=h=new TH1F(core+"x1","x1 distribution; x1",100,0,1);
  hA[74]=h=new TH1F(core+"x2","x2 distribution; x2",100,0,1);
  hA[75]=h=new TH2F(core+"x2_x1","x2 vs x1 ; x1; x2",100,0,1,100,0,1);
  hA[76]=h=new TH1F(core+"x1minusx2","x1 - x2; x1-x2",100,-1,1);
  // add histos to the list (if provided)
  for(int i=0;i<mxHA;i++) {
    if(  hA[i]==0) continue;
    HList->Add( hA[i]);
  }

  // HList->ls();
  LOG_INFO<<Form("%s::initHistos done",GetName())<<endm;
}

/**********************************************************************
 *
 * $Id: StEStructHAdd.cxx,v 1.11 2008/05/01 23:46:39 prindle Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description: Simple helper class for calculating
 *              delta-rho, delta-rho/rho, and delta-rho/sqrt(rho)
 *              plus some other goodies
 *
 ***********************************************************************/
#include "StEStructHAdd.h"

#include "StEStructPool/Correlations/StEStructBinning.h"  

#include "Stiostream.h"
#include "TH1.h"
#include "TH2.h"
#include "TH2D.h"
#include "TFile.h"

ClassImp(StEStructHAdd)

  //------------------------------------------------------------------------
void StEStructHAdd::addCuts(const char* outfile, TFile* inFile,
                            int* nlist, int ntot, int parentDist[][2], int nParentDist, int symmXX) {

    const char* base[]={"Sib","Mix"};
    const char* tpe[]={"pp","pm","mp","mm"};
    const char* knd[]={"YtYt",         "NYtYt",        "PtPt",
                       "PhiPhi",       "NPhiPhi",      "PrPhiPhi",      "PaPhiPhi",      "PbPhiPhi",
                       "EtaEta",                       "PrEtaEta",      "PaEtaEta",      "PbEtaEta",
                       "DEtaDPhiArr",  "NDEtaDPhiArr", "PrDEtaDPhiArr", "PaDEtaDPhiArr", "PbDEtaDPhiArr",
                       "SYtDYt",       "NSYtDYt",
                       "SEtaDPhiArr",  "NSEtaDPhiArr", "PrSEtaDPhiArr", "PaSEtaDPhiArr", "PbSEtaDPhiArr",
                       "Qinv",         "NQinv"};
    int isXXHist[]={1,   1,   1,
                    1,   1,   1,   1,   1,
                    1,   1,   1,   1,
                    0,   0,   0,   0,   0,
                    0,   0,
                    0,   0,   0,   0,   0,
                    0,   0,
                    0,   0};
    int unSymmed[7][4] = {{0, 1, 1, 0},   {1, 1, 1, 1},   {1, 1, 1, 1},
                          {0, 1, 1, 0},   {1, 1, 1, 1},   {0, 1, 1, 0},   {0, 1, 1, 0}};
    const char* symEtaPhi[]={"DEtaDPhi", "NDEtaDPhi", "PrDEtaDPhi", "PaDEtaDPhi", "PbDEtaDPhi"};
    const char* symPhi[]={"SEtaDPhi", "NSEtaDPhi", "PrSEtaDPhi", "PaSEtaDPhi", "PbSEtaDPhi"};
    char* Title[]={"Sibling","Mixed"};
    char* Species[]={"A","B"};
    char* Type[]={" : +.+"," : +.-"," : -.+"," : -.-"};
    StEStructBinning* b=StEStructBinning::Instance();

    TFile* outFile = new TFile(outfile,"RECREATE");

    for (int i=0;i<2;i++) {
        for (int j=0;j<4;j++) {
            for (int k=0;k<26;k++) {

                TH2D *outhist=0;
                TH2D *tmp=0, *cpy=0;
                inFile->cd();
                int zBin = 0;
                while (zBin<99) {
                    TString htype;
                    // Changed histogram naming when I added the z-buffer binning to
                    // the 2pt correlation class. New name is of form
                    //    SibppDEtaDPhi_cutBin_n_zBuf_m
                    // where n is the usual cut bin number and m is the zbuffer index.
                    // Try to be backward compatible with old name of format
                    //    SibppDEtaDPhin
                    htype+=base[i]; htype+=tpe[j]; htype+=knd[k];
                    for (int n=0;n<ntot;n++) {
                        TString fullName(htype.Data()); fullName+=nlist[n];
                        inFile->GetObject(fullName.Data(),tmp);
                        if(tmp) {
                            // Histogram with older naming style was found.
                            // No z-binning here.
                            if (0==n) {
                                outhist=(TH2D *)tmp->Clone();
                                // This part is only intended to be used with mode 5 (pid)
                                if (symmXX && isXXHist[k]) {
                                    if (unSymmed[nlist[n]][j]) {
                                        symmetrizeXX(outhist);
                                    }
                                }
                                outhist->SetName(htype.Data());
                                outhist->SetTitle(tmp->GetTitle());
                            } else {
                                cpy = (TH2D *)tmp->Clone();
                                // This part is only intended to be used with mode 5 (pid)
                                if (symmXX && isXXHist[k]) {
                                    if (unSymmed[nlist[n]][j]) {
                                        symmetrizeXX(cpy);
                                    }
                                }
                                outhist->Add(cpy);
                                delete cpy;
                            }
                            zBin = 100;
                        } else {
                            TString fullName(htype.Data()); fullName+="_cutBin_"; fullName+=nlist[n]; fullName+="_zBuf_"; fullName+=zBin;
                            TString hName(htype.Data()); hName+="_zBuf_"; hName+=zBin;
                            inFile->GetObject(fullName.Data(),tmp);
                            if (!tmp) {
                                // Presumably have processed all zbins by now.
                                goto lastZ;
                            }
                            if (0==n) {
                                outhist=(TH2D *)tmp->Clone();
                                // This part is only intended to be used with mode 5 (pid)
                                if (symmXX && isXXHist[k]) {
                                    if (unSymmed[nlist[n]][j]) {
                                        symmetrizeXX(outhist);
                                    }
                                }
                                outhist->SetName(hName.Data());
                                outhist->SetTitle(tmp->GetTitle());
                            } else {
                                cpy = (TH2D *)tmp->Clone();
                                // This part is only intended to be used with mode 5 (pid)
                                if (symmXX && isXXHist[k]) {
                                    if (unSymmed[nlist[n]][j]) {
                                        symmetrizeXX(cpy);
                                    }
                                }
                                outhist->Add(cpy);
                                delete cpy;
                            }
                        }
                    }
                    for (int isym=0;isym<5;isym++) {
                        if (!strncmp(symEtaPhi[isym],knd[k],8)) {
                            b->setNDEtaBins(outhist->GetNbinsX());
                            b->setNDPhiBins(outhist->GetNbinsY());
                            TString hname(base[i]);
                            hname+=tpe[j];
                            hname+=symEtaPhi[isym];
                            TString zName(hname);
                            zName+="_zBuf_";
                            zName+=zBin;
                            TString htitle(Title[i]);
                            htitle+=Type[j];
                            htitle+=symEtaPhi[isym];
                            TString zTitle(htitle);
                            zTitle+="_zBuf_";
                            zTitle+=zBin;
                            tmp = new TH2D(zName.Data(),zTitle.Data(),b->hdetaBins(),b->detaMin(),b->detaMax(),b->hdphiBins(),b->dphiMin(),b->dphiMax());
                            for(int ieta=0;ieta<b->detaBins();ieta++){
                                for(int iphi=0;iphi<b->dphiBins();iphi++){
                                    float eta1 = b->detaVal(ieta);
                                    float eta2 = -eta1;
                                    float phi1 = b->dphiVal(iphi,1);
                                    float phi2 = b->dphiVal(iphi,2);
                                    double val = outhist->GetBinContent(ieta+1,iphi+1);
                                    tmp->Fill(eta1,phi1,val);
                                    tmp->Fill(eta2,phi1,val);
                                    tmp->Fill(eta1,phi2,val);
                                    tmp->Fill(eta2,phi2,val);
                                }
                            }
                            if(tmp->GetBinContent(1,1)==0) {  // fill the repeated bin
                                for(int ieta=0;ieta<tmp->GetNbinsX();ieta++) tmp->SetBinContent(ieta+1,1, tmp->GetBinContent(ieta+1,tmp->GetNbinsY()));
                            }
                            // Create rotated view from -pi to pi for CD 
                            hname+="Rot";  hname +="_zBuf_", hname+=zBin;
                            htitle+="Rot"; htitle+="_zBuf_", htitle+=zBin;
                            TH2D* rot = new TH2D(hname.Data(),htitle.Data(),b->hdetaBins(),b->detaMin(),b->detaMax(),b->hdphiBins(),-M_PI-M_PI/24,M_PI+M_PI/24);
                            for(int ieta=0;ieta<b->detaBins();ieta++){
                                for(int iphi=0;iphi<b->dphiBins();iphi++) {
                                    float eta1 = b->detaVal(ieta);
                                    float eta2 = -eta1;
                                    float phi1 = b->dphiVal(iphi,1);
                                    if (phi1>M_PI) phi1=2*M_PI - phi1;  // get phi1 to be [0..pi]
                                    float phi2 = -phi1;                 // phi2 [-pi..0]
                                    double val = outhist->GetBinContent(ieta+1,iphi+1);
                                    rot->Fill(eta1,phi1,val);
                                    rot->Fill(eta2,phi1,val);
                                    rot->Fill(eta1,phi2,val);
                                    rot->Fill(eta2,phi2,val);
                                }
                            }
                            int bin1 = rot->GetYaxis()->FindBin(M_PI); // need to double bin values at pi and -pi 
                            int bin2 = rot->GetYaxis()->FindBin(-M_PI);
                            for(int ieta=0;ieta<rot->GetNbinsX();ieta++) { 
                                rot->SetBinContent(ieta+1,bin1, 2*rot->GetBinContent(ieta+1,bin1));
                                rot->SetBinContent(ieta+1,bin2, 2*rot->GetBinContent(ieta+1,bin2));
                            }
                            outFile->cd();
                            rot->Write();
                            delete rot;
                            // end of rotated view
                            delete outhist;
                            outhist = tmp;
                        }
                    }
                    for (int isym=0;isym<5;isym++) {
                        if (!strncmp(symPhi[isym],knd[k],8)) {
                            b->setNSEtaBins(outhist->GetNbinsX());
                            b->setNDPhiBins(outhist->GetNbinsY());
                            TString hname(base[i]);
                            hname+=tpe[j];
                            hname+=symPhi[isym];
                            TString zName(hname);
                            zName+="_zBuf_";
                            zName+=zBin;
                            TString htitle(Title[i]);
                            htitle+=Type[j];
                            htitle+=symPhi[isym];
                            TString zTitle(htitle);
                            zTitle+="_zBuf_";
                            zTitle+=zBin;
                            tmp = new TH2D(zName.Data(),zTitle.Data(),b->hdetaBins(),b->detaMin(),b->detaMax(),b->hdphiBins(),b->dphiMin(),b->dphiMax());
                            for(int ieta=0;ieta<b->setaBins();ieta++){
                                for(int iphi=0;iphi<b->dphiBins();iphi++){
                                    float eta1 = b->setaVal(ieta);
                                    float phi1 = b->dphiVal(iphi,1);
                                    float phi2 = b->dphiVal(iphi,2);
                                    double val = outhist->GetBinContent(ieta+1,iphi+1);
                                    tmp->Fill(eta1,phi1,val);
                                    tmp->Fill(eta1,phi2,val);
                                }
                            }
                            if(tmp->GetBinContent(1,1)==0) {  // fill the repeated bin
                                for(int ieta=0;ieta<tmp->GetNbinsX();ieta++) tmp->SetBinContent(ieta+1,1, tmp->GetBinContent(ieta+1,tmp->GetNbinsY()));
                            }
                            delete outhist;
                            outhist = tmp;
                        }
                    }
                    outFile->cd();
                    if (outhist) {
                        outhist->Write();
                        delete outhist;
                    }
                    zBin++;
                }
                lastZ:
                continue;
            }
        }
    }


    inFile->cd();
    TH1D* sib; inFile->GetObject("NEventsSame",sib);
    TH1D* mix; inFile->GetObject("NEventsMixed",mix);
    if(sib && mix) {
        // Histogram with older naming style was found.
        // No z-binning here.
        outFile->cd();
        sib->Write();
        mix->Write();
    } else {
        int zBin = 0;
        TString hSibName("NEventsSib_zBuf_");
        TString hMixName("NEventsMix_zBuf_");
        TString hSibPosName("NEventsPosSib_zBuf_");
        TString hMixPosName("NEventsPosMix_zBuf_");
        TString hSibNegName("NEventsNegSib_zBuf_");
        TString hMixNegName("NEventsNegMix_zBuf_");
        while (zBin<99) {
            TString fullSibName(hSibName.Data()); fullSibName+=zBin;
            TString fullMixName(hMixName.Data()); fullMixName+=zBin;
            TString fullSibPosName(hSibPosName.Data()); fullSibPosName+=zBin;
            TString fullMixPosName(hMixPosName.Data()); fullMixPosName+=zBin;
            TString fullSibNegName(hSibNegName.Data()); fullSibNegName+=zBin;
            TString fullMixNegName(hMixNegName.Data()); fullMixNegName+=zBin;
            inFile->cd();
            inFile->GetObject(fullSibName.Data(),sib);
            inFile->GetObject(fullMixName.Data(),mix);
            TH1D *sibPos; inFile->GetObject(fullSibPosName.Data(),sibPos);
            TH1D *mixPos; inFile->GetObject(fullMixPosName.Data(),mixPos);
            TH1D *sibNeg; inFile->GetObject(fullSibNegName.Data(),sibNeg);
            TH1D *mixNeg; inFile->GetObject(fullMixNegName.Data(),mixNeg);
            if (sib && mix) {
                outFile->cd();
                sib->Write();
                mix->Write();
                sibPos->Write();
                mixPos->Write();
                sibNeg->Write();
                mixNeg->Write();
            } else {
                break;
            }
            TString ptPName;
            TString ptMName;
            TString etaPName;
            TString etaMName;
            TH1 *tmp, *ptP, *ptM, *etaP, *etaM;
            for (int iSpecies=0;iSpecies<2;iSpecies++) {
                inFile->cd();
                for (int iPar=0;iPar<nParentDist;iPar++) {
                    int jPar = parentDist[iPar][iSpecies];
                    ptPName  = "meanPtP_parentBin"; ptPName  += jPar; ptPName  += "_zBuf_"; ptPName  += zBin;
                    ptMName  = "meanPtM_parentBin"; ptMName  += jPar; ptMName  += "_zBuf_"; ptMName  += zBin;
                    etaPName = "etaP_parentBin";    etaPName += jPar; etaPName += "_zBuf_"; etaPName += zBin;
                    etaMName = "etaM_parentBin";    etaMName += jPar; etaMName += "_zBuf_"; etaMName += zBin;
                    if (0 == iPar) {
                        inFile->GetObject(ptPName.Data(),ptP);
                        inFile->GetObject(ptMName.Data(),ptM);
                        inFile->GetObject(etaPName.Data(),etaP);
                        inFile->GetObject(etaMName.Data(),etaM);
                    } else {
                        inFile->GetObject(ptPName.Data(),tmp);  ptP->Add(tmp);
                        inFile->GetObject(ptMName.Data(),tmp);  ptM->Add(tmp);
                        inFile->GetObject(etaPName.Data(),tmp); etaP->Add(tmp);
                        inFile->GetObject(etaMName.Data(),tmp); etaM->Add(tmp);
                    }
                }
                outFile->cd();
                if (ptP) {
                    ptPName = "meanPtP";  ptPName += Species[iSpecies]; ptPName += "_zBuf_"; ptPName += zBin;
                    ptP->SetName(ptPName.Data());
                    ptP->Write();
                }
                if (ptM) {
                    ptMName = "meanPtM"; ptMName += Species[iSpecies]; ptMName += "_zBuf_"; ptMName += zBin;
                    ptM->SetName(ptMName.Data());
                    ptM->Write();
                }
                if (etaP) {
                    etaPName = "etaP"; etaPName += Species[iSpecies]; etaPName += "_zBuf_"; etaPName += zBin;
                    etaP->SetName(etaPName.Data());
                    etaP->Write();
                }
                if (etaM) {
                    etaMName = "etaM"; etaMName += Species[iSpecies]; etaMName += "_zBuf_"; etaMName += zBin;
                    etaM->SetName(etaMName.Data());
                    etaM->Write();
                }
            }
            inFile->cd();
            zBin++;
        }
    }
    outFile->Close();
};
//--------------------------------------------------------------------------
void StEStructHAdd::addCuts(const char* outfile, const char* infile,
                            int* nlist, int ntot, int parentDist[][2], int nParentDist, int symmXX) {

    TFile* tf=new TFile(infile);
    if (tf) {
        addCuts(outfile,tf,nlist,ntot,parentDist,nParentDist,symmXX);
    }
    return;
};
//------------------------------------------------------------------------
void StEStructHAdd::symmetrizeXX(TH2 *hist) {
    for (int ix=1;ix<=hist->GetNbinsX();ix++) {
        for (int iy=ix;iy<=hist->GetNbinsY();iy++) {
            double sum = hist->GetBinContent(ix,iy) + hist->GetBinContent(iy,ix);
            hist->SetBinContent(ix,iy,sum);
            hist->SetBinContent(iy,ix,sum);
        }
    }

};
//------------------------------------------------------------------------
void StEStructHAdd::addDensities(const char* outfile, TFile* inFile) {

    TFile* outFile = new TFile(outfile,"RECREATE");

    // If pair density histograms are in input file add z and cut bins,
    // form LS and US, take ratio of sibling to mixed and write to output file.
    inFile->cd();
    TH2 *sibDens; inFile->GetObject("SibppTPCMidTZ_cutBin_0_zBuf_0",sibDens);
    TH2 *mixDens; inFile->GetObject("MixppTPCMidTZ_cutBin_0_zBuf_0",mixDens);
    if(sibDens && mixDens) {
        int nCut = 0;
        int nZBuf = 0;
        TString hSibName("SibppTPCMidTZ_cutBin_");
        while (nCut<99) {
            TString full(hSibName.Data()); full += nCut; full += "_zBuf_0";
            inFile->GetObject(full.Data(),sibDens);
            if (sibDens) {
                nCut++;
            } else {
                break;
            }
        }
        while (nZBuf<99) {
            TString full(hSibName.Data()); full += "0_zBuf_"; full += nZBuf;
            inFile->GetObject(full.Data(),sibDens);
            if (sibDens) {
                nZBuf++;
            } else {
                break;
            }
        }

        TH2D *ls[6], *us[6], *tmp;
        inFile->GetObject("SibppTPCMidTZ_cutBin_0_zBuf_0",tmp);
        for (int i=0;i<6;i++) {
            ls[i] = (TH2D *) tmp->Clone();  ls[i]->Clear();
            us[i] = (TH2D *) tmp->Clone();  us[i]->Clear();
        }
        TString hDensName;
        char *typePP[] = {"SibppTPCMidTZ_cutBin_", "SibppTPCMidTZC_cutBin_", "SibppTPCMidTZNC_cutBin_", "MixppTPCMidTZ_cutBin_", "MixppTPCMidTZC_cutBin_", "MixppTPCMidTZNC_cutBin_"};
        char *typeMM[] = {"SibmmTPCMidTZ_cutBin_", "SibmmTPCMidTZC_cutBin_", "SibmmTPCMidTZNC_cutBin_", "MixmmTPCMidTZ_cutBin_", "MixmmTPCMidTZC_cutBin_", "MixmmTPCMidTZNC_cutBin_"};
        char *typePM[] = {"SibpmTPCMidTZ_cutBin_", "SibpmTPCMidTZC_cutBin_", "SibpmTPCMidTZNC_cutBin_", "MixpmTPCMidTZ_cutBin_", "MixpmTPCMidTZC_cutBin_", "MixpmTPCMidTZNC_cutBin_"};
        char *typeMP[] = {"SibmpTPCMidTZ_cutBin_", "SibmpTPCMidTZC_cutBin_", "SibmpTPCMidTZNC_cutBin_", "MixmpTPCMidTZ_cutBin_", "MixmpTPCMidTZC_cutBin_", "MixmpTPCMidTZNC_cutBin_"};
        for (int iCut=0;iCut<nCut;iCut++) {
            for (int zBuf=0;zBuf<nZBuf;zBuf++) {
                for (int it=0;it<6;it++) {
                    hDensName = typePP[it]; hDensName += iCut; hDensName += "_zBuf_"; hDensName += zBuf;
                    inFile->GetObject(hDensName.Data(),tmp);
                    ls[it]->Add(tmp);
                    hDensName = typeMM[it]; hDensName += iCut; hDensName += "_zBuf_"; hDensName += zBuf;
                    inFile->GetObject(hDensName.Data(),tmp);
                    ls[it]->Add(tmp);

                    hDensName = typePM[it]; hDensName += iCut; hDensName += "_zBuf_"; hDensName += zBuf;
                    inFile->GetObject(hDensName.Data(),tmp);
                    us[it]->Add(tmp);
                    hDensName = typeMP[it]; hDensName += iCut; hDensName += "_zBuf_"; hDensName += zBuf;
                    inFile->GetObject(hDensName.Data(),tmp);
                    us[it]->Add(tmp);
                }
            }
        }
        ls[0]->Divide(ls[3]);  ls[0]->SetName("TPCMidTZ_LS");
        ls[1]->Divide(ls[4]);  ls[1]->SetName("TPCMidTZC_LS");
        ls[2]->Divide(ls[5]);  ls[2]->SetName("TPCMidTZNC_LS");
        us[0]->Divide(us[3]);  us[0]->SetName("TPCMidTZ_US");
        us[1]->Divide(us[4]);  us[1]->SetName("TPCMidTZC_US");
        us[2]->Divide(us[5]);  us[2]->SetName("TPCMidTZNC_US");
        outFile->cd();
        ls[0]->Write();
        ls[1]->Write();
        ls[2]->Write();
        us[0]->Write();
        us[1]->Write();
        us[2]->Write();
    }
    outFile->Close();
}

//------------------------------------------------------------------------
void StEStructHAdd::combineUS(TFile * modFile) {

    // Add the -+ to the +- and clear the -+.
    // This is for pid case when we are creating all. For non-identical particles we
    // distinguish between +- and -+ but for identical particles we do not. In the
    // case of 'all', which we make from summing all other cases, we don't
    // want to distinguish +- and -+.

    const char* base[]={"Sib","Mix"};
    const char* tpe[]={"pp","pm","mp","mm"};
    const char* knd[]={"YtYt",     "NYtYt",     "PtPt",
                       "PhiPhi",   "NPhiPhi",   "PrPhiPhi",   "PaPhiPhi",   "PbPhiPhi",
                       "EtaEta",                "PrEtaEta",   "PaEtaEta",   "PbEtaEta",
                       "DEtaDPhi", "NDEtaDPhi", "PrDEtaDPhi", "PaDEtaDPhi", "PbDEtaDPhi",
                       "SYtDYt",   "NSYtDYt",
                       "SEtaDPhi", "NSEtaDPhi", "PrSEtaDPhi", "PaSEtaDPhi", "PbSEtaDPhi",
                       "Qinv",     "NQinv"
                       "DEtaDPhiRot", "NDEtaDPhiRot", "PrDEtaDPhiRot", "PaDEtaDPhiRot", "PbDEtaDPhiRot"};

//    TFile* modFile = new TFile(fileName,"UPDATE");

    for (int i=0;i<2;i++) {
        for (int k=0;k<28;k++) {

            TH1 *hpm=0, *hmp=0;
            int zBin = 0;
            while (zBin<99) {
                if (k<31) {
                    // Changed histogram naming when I added the z-buffer binning to
                    // the 2pt correlation class. New name is of form
                    //    SibppDEtaDPhi_cutBin_n_zBuf_m
                    // where n is the usual cut bin number and m is the zbuffer index.
                    // Try to be backward compatible with old name of format
                    //    SibppDEtaDPhin
                    TString pmHist; pmHist+=base[i]; pmHist+=tpe[1]; pmHist+=knd[k];

                    hpm=(TH1*)modFile->Get(pmHist.Data());
                    if(hpm) {
                        // Histogram with older naming style was found.
                        // No z-binning here. Get -+ and add to +-.
                        TString mpHist; mpHist+=base[i]; mpHist+=tpe[2]; mpHist+=knd[k];
                        hmp=(TH1*)modFile->Get(mpHist.Data());
                        zBin = 100;
                    } else {
                        TString pmHist; pmHist+=base[i]; pmHist+=tpe[1]; pmHist+=knd[k];
                        pmHist+="_zBuf_"; pmHist+=zBin;
                        hpm=(TH1*)modFile->Get(pmHist.Data());
                        if (!hpm) {
                            // Presumably have processed all zbins by now.
                            goto lastZ;
                        }
                        TString mpHist; mpHist+=base[i]; mpHist+=tpe[2]; mpHist+=knd[k];
                        mpHist+="_zBuf_"; mpHist+=zBin;
                        hmp=(TH1*)modFile->Get(mpHist.Data());
                    }
                    hpm->Add(hmp);
                    hmp->Scale(0);
                    modFile->cd();
                    hpm->Write();
                    hmp->Write();
                }
                zBin++;
            }
            lastZ:
            continue;
        }
    }
//    modFile->Close();
};


/*
 * Want to symmetrize DEtaDPhi and SEtaDPhi here.
    createHist2D(mHJtDEtaDPhi,"DEtaDPhi",i,y,ncb,b->hdetaBins(),b->detaMin(),b->detaMax(),b->hdphiBins(),b->dphiMin(),b->dphiMax());
    createHist2D(mHJtNDEtaDPhi,"NDEtaDPhi",i,y,ncb,b->hdetaBins(),b->detaMin(),b->detaMax(),b->hdphiBins(),b->dphiMin(),b->dphiMax());
    createHist2D(mHPrJtDEtaDPhi,"PrDEtaDPhi",i,y,ncb,b->hdetaBins(),b->detaMin(),b->detaMax(),b->hdphiBins(),b->dphiMin(),b->dphiMax());
    createHist2D(mHPaJtDEtaDPhi,"PaDEtaDPhi",i,y,ncb,b->hdetaBins(),b->detaMin(),b->detaMax(),b->hdphiBins(),b->dphiMin(),b->dphiMax());
    createHist2D(mHPbJtDEtaDPhi,"PbDEtaDPhi",i,y,ncb,b->hdetaBins(),b->detaMin(),b->detaMax(),b->hdphiBins(),b->dphiMin(),b->dphiMax());
    for(int k=0;k<b->detaBins();k++){
      for(int j=0;j<b->dphiBins();j++){
        // Symmetrize dEta,dPhi here for now. Want to move this to selectAll
        // macro to make intermediate root files smaller and speed up hadd.
        float eta1 = b->detaVal(k);
        float eta2 = -eta1;
        float phi1 = b->dphiVal(j,1);
        float phi2 = b->dphiVal(j,2);
        mHJtDEtaDPhi[i][y]->Fill(eta1,phi1,jtdetadphi[y][k].dphi[j]);
        mHJtNDEtaDPhi[i][y]->Fill(eta1,phi1,jtndetadphi[y][k].dphi[j]);
        mHPrJtDEtaDPhi[i][y]->Fill(eta1,phi1,prjtdetadphi[y][k].dphi[j]);
        mHPaJtDEtaDPhi[i][y]->Fill(eta1,phi1,pajtdetadphi[y][k].dphi[j]);
        mHPbJtDEtaDPhi[i][y]->Fill(eta1,phi1,pbjtdetadphi[y][k].dphi[j]);

        mHJtDEtaDPhi[i][y]->Fill(eta2,phi1,jtdetadphi[y][k].dphi[j]);
        mHJtNDEtaDPhi[i][y]->Fill(eta2,phi1,jtndetadphi[y][k].dphi[j]);
        mHPrJtDEtaDPhi[i][y]->Fill(eta2,phi1,prjtdetadphi[y][k].dphi[j]);
        mHPaJtDEtaDPhi[i][y]->Fill(eta2,phi1,pajtdetadphi[y][k].dphi[j]);
        mHPbJtDEtaDPhi[i][y]->Fill(eta2,phi1,pbjtdetadphi[y][k].dphi[j]);

        mHJtDEtaDPhi[i][y]->Fill(eta1,phi2,jtdetadphi[y][k].dphi[j]);
        mHJtNDEtaDPhi[i][y]->Fill(eta1,phi2,jtndetadphi[y][k].dphi[j]);
        mHPrJtDEtaDPhi[i][y]->Fill(eta1,phi2,prjtdetadphi[y][k].dphi[j]);
        mHPaJtDEtaDPhi[i][y]->Fill(eta1,phi2,pajtdetadphi[y][k].dphi[j]);
        mHPbJtDEtaDPhi[i][y]->Fill(eta1,phi2,pbjtdetadphi[y][k].dphi[j]);

        mHJtDEtaDPhi[i][y]->Fill(eta2,phi2,jtdetadphi[y][k].dphi[j]);
        mHJtNDEtaDPhi[i][y]->Fill(eta2,phi2,jtndetadphi[y][k].dphi[j]);
        mHPrJtDEtaDPhi[i][y]->Fill(eta2,phi2,prjtdetadphi[y][k].dphi[j]);
        mHPaJtDEtaDPhi[i][y]->Fill(eta2,phi2,pajtdetadphi[y][k].dphi[j]);
        mHPbJtDEtaDPhi[i][y]->Fill(eta2,phi2,pbjtdetadphi[y][k].dphi[j]);
      }
    }
    createHist2D(mHJtSEtaDPhi,"SEtaDPhi",i,y,ncb,b->setaBins(),b->setaMin(),b->setaMax(),b->hdphiBins(),b->dphiMin(),b->dphiMax());
    createHist2D(mHJtNSEtaDPhi,"NSEtaDPhi",i,y,ncb,b->setaBins(),b->setaMin(),b->setaMax(),b->hdphiBins(),b->dphiMin(),b->dphiMax());
    createHist2D(mHPrJtSEtaDPhi,"PrSEtaDPhi",i,y,ncb,b->setaBins(),b->setaMin(),b->setaMax(),b->hdphiBins(),b->dphiMin(),b->dphiMax());
    createHist2D(mHPaJtSEtaDPhi,"PaSEtaDPhi",i,y,ncb,b->setaBins(),b->setaMin(),b->setaMax(),b->hdphiBins(),b->dphiMin(),b->dphiMax());
    createHist2D(mHPbJtSEtaDPhi,"PbSEtaDPhi",i,y,ncb,b->setaBins(),b->setaMin(),b->setaMax(),b->hdphiBins(),b->dphiMin(),b->dphiMax());
    for(int k=0;k<b->setaBins();k++) {
      for(int j=0;j<b->dphiBins();j++) {
        // Symmetrize dEta,dPhi here for now. Want to move this to selectAll
        // macro to make intermediate root files smaller and speed up hadd.
        float phi1 = b->dphiVal(j,1);
        mHJtSEtaDPhi[i][y]->Fill(xv=b->setaVal(k),phi1,jtsetadphi[y][k].dphi[j]);
        mHJtNSEtaDPhi[i][y]->Fill(xv,phi1,jtnsetadphi[y][k].dphi[j]);
        mHPrJtSEtaDPhi[i][y]->Fill(xv,phi1,prjtsetadphi[y][k].dphi[j]);
        mHPaJtSEtaDPhi[i][y]->Fill(xv,phi1,pajtsetadphi[y][k].dphi[j]);
        mHPbJtSEtaDPhi[i][y]->Fill(xv,phi1,pbjtsetadphi[y][k].dphi[j]);

        float phi2 = b->dphiVal(j,2);
        mHJtSEtaDPhi[i][y]->Fill(xv,phi2,jtsetadphi[y][k].dphi[j]);
        mHJtNSEtaDPhi[i][y]->Fill(xv,phi2,jtnsetadphi[y][k].dphi[j]);
        mHPrJtSEtaDPhi[i][y]->Fill(xv,phi2,prjtsetadphi[y][k].dphi[j]);
        mHPaJtSEtaDPhi[i][y]->Fill(xv,phi2,pajtsetadphi[y][k].dphi[j]);
        mHPbJtSEtaDPhi[i][y]->Fill(xv,phi2,pbjtsetadphi[y][k].dphi[j]);
      }
    }
 */

/***********************************************************************
 *
 * $Log: StEStructHAdd.cxx,v $
 * Revision 1.11  2008/05/01 23:46:39  prindle
 * Changed to use TH1D and TH2D (instead of TH1 and TH2) in some places so
 * we can use GetObject method to enforce type checking. Found I had missed
 * duplicating a \phi_\Delta row in one case. Also added a method to include
 * sum of pairdensity histograms in output file.
 *
 * Revision 1.10  2007/11/26 20:23:20  prindle
 * (Hadd to trick cvs to allow me to commit this modified file?)
 * Modifications to support saving of individual z-bins and the concept
 * of parent distributions important for pid analysis.
 *
 * Revision 1.9  2007/06/26 20:22:17  msd
 * Very minor bug fix
 *
 * Revision 1.8  2007/05/27 22:46:00  msd
 * Added buildChargeTypes mode 3 which takes rho_ref from track counts.
 * Added buildChargeTypeSumOfRatios.
 *
 * Revision 1.7  2007/01/26 17:20:57  msd
 * Updated HAdd for new binning scheme.
 * Improved Support::buildChargeTypes.
 *
 * Revision 1.6  2006/10/02 22:26:48  prindle
 * Hadd now symmetrizes histograms while adding them, so output is usable
 * in Support as before. Need to load library for Correlation so we know
 * how many bins there are.
 * Added  alternative versions of methods to calculate Delta\sigma^2.
 * Important for pt correlations where we need proper normalization before
 * subtracting mixed reference.
 *
 * Revision 1.5  2006/04/06 01:09:46  prindle
 *   Calculating pt for each cut bin caused changes in HAdd.
 * The splitting of +- into +- and -+ caused changes in Support.
 *
 * Revision 1.4  2005/09/29 17:42:14  msd
 * Added Xt to hadd
 *
 * Revision 1.3  2005/03/08 21:56:42  porter
 * fixed bug in StEStructHAdd.cxx and added diagnostic option in ptcorrelations to
 * view individual terms separately
 *
 * Revision 1.2  2005/03/03 01:33:04  porter
 * Added pt-correlations method to support and included
 * these histograms to the HAdd routine
 *
 * Revision 1.1  2004/07/01 00:37:13  porter
 * new code previously my StEStructHelper. Takes hists from correltation
 * pass and builds final ressults.  Also the StEStructHAdd.h is a simple
 * replacemnt for my sumyt.C macro which could be expanded later as needed.
 *
 *
 *
 *********************************************************************/




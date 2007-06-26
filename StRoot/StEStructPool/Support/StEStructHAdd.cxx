/**********************************************************************
 *
 * $Id: StEStructHAdd.cxx,v 1.9 2007/06/26 20:22:17 msd Exp $
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
#include "TH2D.h"
#include "TFile.h"

ClassImp(StEStructHAdd)

  //------------------------------------------------------------------------
void StEStructHAdd::addCuts(const char* outfile, TFile* inFile,
                            int* nlist, int ntot, int all ) {

    const char* base[]={"Sib","Mix"};
    const char* tpe[]={"pp","pm","mp","mm"};
    const char* knd[]={"YtYt", "NYtYt", "PtPt",
               "PhiPhi",   "NPhiPhi",   "PrPhiPhi",   "PaPhiPhi",   "PbPhiPhi",
               "EtaEta",                "PrEtaEta",   "PaEtaEta",   "PbEtaEta",
               "DEtaDPhiArr", "NDEtaDPhiArr", "PrDEtaDPhiArr", "PaDEtaDPhiArr", "PbDEtaDPhiArr",
               "SYtDYt",   "NSYtDYt",
               "SEtaDPhiArr", "NSEtaDPhiArr", "PrSEtaDPhiArr", "PaSEtaDPhiArr", "PbSEtaDPhiArr",
               "Qinv", "NQinv", "pta","ptb"};
    const char* symEtaPhi[]={"DEtaDPhi", "NDEtaDPhi", "PrDEtaDPhi", "PaDEtaDPhi", "PbDEtaDPhi"};
    const char* symPhi[]={"SEtaDPhi", "NSEtaDPhi", "PrSEtaDPhi", "PaSEtaDPhi", "PbSEtaDPhi"};
    char* Title[]={"Sibling","Mixed"};
    char* Type[]={" : +.+"," : +.-"," : -.+"," : -.-"};
    StEStructBinning* b=StEStructBinning::Instance();

    TFile* outFile = new TFile(outfile,"RECREATE");

    for (int i=0;i<2;i++) {
        for (int j=0;j<4;j++) {
            for (int k=0;k<28;k++) {
                TString htype;
                if (k<26) {
                    htype+=base[i];htype+=tpe[j];
                } else if(i>0 || j>0) {
                    continue;
	            }
                htype+=knd[k];

                TH1* outhist=0;
                TH2D *tmp=0;
                for (int n=0;n<ntot;n++) {
                    TString fullName(htype.Data()); 
		    if(nlist[n]>=0) fullName+=nlist[n];  // allows cb mode 0 to be symmetrized
                    inFile->cd();
                    TH1* tmp=(TH1*)inFile->Get(fullName.Data());
                    if(!tmp){
                        cout<<"Cannot find "<<fullName.Data()<<endl;
                        break;
                    }
                    if (n==0) {
                        outhist=(TH1*)tmp->Clone();
                        outhist->SetName(htype.Data());
                        outhist->SetTitle(tmp->GetTitle());
                    } else {
                        outhist->Add(tmp);
                    }
                }
                if ((k>=24)  && all) {
                    TH1* tmp=(TH1*)inFile->Get("ptAll");
                    if (tmp) {
                        outhist = (TH1*)tmp->Clone();
                    }
                }
                for (int isym=0;isym<5;isym++) {
                    if (!strncmp(symEtaPhi[isym],knd[k],8)) {
                        TString hname(base[i]);
                        hname+=tpe[j];
                        hname+=symEtaPhi[isym];
                        TString htitle(Title[i]);
                        htitle+=Type[j];
                        htitle+=symEtaPhi[isym];
                        tmp = new TH2D(hname.Data(),htitle.Data(),b->hdetaBins(),b->detaMin(),b->detaMax(),b->hdphiBins(),b->dphiMin(),b->dphiMax());
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
			hname+="Rot"; htitle+="Rot";
			TH2D* rot = new TH2D(hname.Data(),htitle.Data(),b->hdetaBins(),b->detaMin(),b->detaMax(),b->hdphiBins(),-M_PI-M_PI/24,M_PI+M_PI/24);
                        for(int ieta=0;ieta<b->detaBins();ieta++){
			  for(int iphi=0;iphi<b->dphiBins();iphi++){
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
                        TString hname(base[i]);
                        hname+=tpe[j];
                        hname+=symPhi[isym];
                        TString htitle(Title[i]);
                        htitle+=Type[j];
                        htitle+=symPhi[isym];
                        tmp = new TH2D(hname.Data(),htitle.Data(),b->hdetaBins(),b->detaMin(),b->detaMax(),b->hdphiBins(),b->dphiMin(),b->dphiMax());
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
                        delete outhist;
                        outhist = tmp;
                    }
                }
                outFile->cd();
                if (outhist) {
                    outhist->Write();
                    delete outhist;
	            }
            }
        }
    }

    inFile->cd();
    TH1* tmp=(TH1*)inFile->Get("NEventsSame");
    outFile->cd();
    tmp->Write();

    inFile->cd();
    tmp=(TH1*)inFile->Get("NEventsMixed");
    outFile->cd();
    tmp->Write();

    inFile->cd();
    tmp=(TH1*)inFile->Get("NPosSame");
    if(tmp) {
      outFile->cd();
      tmp->Write();
    }
    
    inFile->cd();
    tmp=(TH1*)inFile->Get("NPosMixed");
    if(tmp) {
      outFile->cd();
      tmp->Write();
    }

    inFile->cd();
    tmp=(TH1*)inFile->Get("NNegSame");
    if(tmp) {
      outFile->cd();
      tmp->Write();
    }

    inFile->cd();
    tmp=(TH1*)inFile->Get("NNegMixed");
    if(tmp) {
      outFile->cd();
      tmp->Write();
    }

    outFile->Close();
};

//--------------------------------------------------------------------------
void StEStructHAdd::addCuts(const char* outfile, const char* infile,
                            int* nlist, int ntot, int all) {

    TFile* tf=new TFile(infile);
    if (tf) {
        addCuts(outfile,tf,nlist,ntot,all);
    }
    return;
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




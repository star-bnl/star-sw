TH1D *tmp;
TFile f, *fNew;

char buffer[1024];
char nEvName[1024];
char nHistName[1024];
double nEvents[100], nTmp;
double n[100][4][2], pHat[100][4][2];
char *types[] = {"all", "soft", "neck", "hard"};
int nc = 0;
{
    for (int ic=0;ic<100;ic++) {
        nEvents[ic] = 0;
        for (int it=0;it<4;it++) {
            n[ic][it][0] = 0;
            n[ic][it][1] = 0;
            pHat[ic][it][0] = 0;
            pHat[ic][it][1] = 0;
        }
    }
    for (int ic=0;ic<100;ic++) {
        for (int it=0;it<4;it++) {
            sprintf(buffer,"Data%i%s.root",ic,types[it]);
            fNew = f->Open(buffer);
            if (!fNew) {
                break;
            }
            nc++;
            int iz=0;
            sprintf(nEvName,"NEventsSib_zBuf_%i",iz);
            while (tmp = (TH1D *) gDirectory->Get(nEvName)) {
                if (0 == it) {
                    nEvents[ic] += tmp->Integral();
                }
                sprintf(nHistName,"meanPtPA_zBuf_%i",iz);
                tmp = (TH1D *) gDirectory->Get(nHistName);
                nTmp  = 0.5*tmp->Integral();
                pHat[ic][it][0] += nTmp*tmp->GetMean();
                n[ic][it][0] += nTmp;
                sprintf(nHistName,"meanPtMA_zBuf_%i",iz);
                tmp = (TH1D *) gDirectory->Get(nHistName);
                nTmp  = 0.5*tmp->Integral();
                pHat[ic][it][1] += nTmp*tmp->GetMean();
                n[ic][it][1] += nTmp;
                iz++;
                sprintf(nEvName,"NEventsSib_zBuf_%i",iz);
            }
        }
        if (!fNew) {
            break;
        }
    }
    nc /= 4;
}

{
    printf("  Following information is for each centrality bin \n\n");
    printf("Centrality :");
    for (int ic=0;ic<nc;ic++) {
        printf("%11i",ic);
    }
    printf("\n");
    printf(" numEvents :");
    for (int ic=0;ic<nc;ic++) {
        printf("%11.0f",nEvents[ic]);
    }
    printf("\n");
    printf("\n");

    printf("            ");
    for (int it=0;it<4;it++) {
        printf("%10s+%10s-",types[it],types[it]);
    }
    printf("\n");
    for (int ic=0;ic<nc;ic++) {
        if (0 == ic) {
            printf(" NTracks %i :",ic);
        } else {
            printf("         %i :",ic);
        }
        for (int it=0;it<4;it++) {
            printf("%11i%11i",n[ic][it][0],n[ic][it][1]);
        }
        printf("\n");
    }
    printf("\n");
    for (int ic=0;ic<nc;ic++) {
        if (0 == ic) {
            printf(" dN/dEta %i :",ic);
        } else {
            printf("         %i :",ic);
        }
        for (int it=0;it<4;it++) {
            printf("%11.2f%11.2f",n[ic][it][0]/nEvents[ic],n[ic][it][1]/nEvents[ic]);
        }
        printf("\n");
    }
    printf("\n");
    for (int ic=0;ic<nc;ic++) {
        if (0 == ic) {
            printf("    pHat %i :",ic);
        } else {
            printf("         %i :",ic);
        }
        double pHatp, pHatm;
        for (int it=0;it<4;it++) {
            if (n[ic][it][0] > 0) {
                pHatp = pHat[ic][it][0] / n[ic][it][0];
            } else {
                pHatp = -1;
            }
            if (n[ic][it][1] > 0) {
                pHatm = pHat[ic][it][1] / n[ic][it][1];
            } else {
                pHatp = -1;
            }
            printf("%11.3f%11.3f",pHatp,pHatm);
        }
        printf("\n");
    }
    printf("\n");
}

// Try extracting information from Sum files.
char *sums[] = {"Sum0_4"};
int nSums = 1;
{
    for (int ic=0;ic<nSums;ic++) {
        nEvents[ic] = 0;
        for (int it=0;it<4;it++) {
            n[ic][it][0] = 0;
            n[ic][it][1] = 0;
            pHat[ic][it][0] = 0;
            pHat[ic][it][1] = 0;
        }
    }
    for (int ic=0;ic<nSums;ic++) {
        for (int it=0;it<4;it++) {
            sprintf(buffer,"%s%s.root",sums[ic],types[it]);
            fNew = f->Open(buffer);
            if (!fNew) {
                break;
            }
            int iz=0;
            sprintf(nEvName,"NEventsSib_zBuf_%i",iz);
            while (tmp = (TH1D *) gDirectory->Get(nEvName)) {
                if (0 == it) {
                    nEvents[ic] += tmp->Integral();
                }
                sprintf(nHistName,"meanPtPA_zBuf_%i",iz);
                tmp = (TH1D *) gDirectory->Get(nHistName);
                nTmp  = 0.5*tmp->Integral();
                pHat[ic][it][0] += nTmp*tmp->GetMean();
                n[ic][it][0] += nTmp;
                sprintf(nHistName,"meanPtMA_zBuf_%i",iz);
                tmp = (TH1D *) gDirectory->Get(nHistName);
                nTmp  = 0.5*tmp->Integral();
                pHat[ic][it][1] += nTmp*tmp->GetMean();
                n[ic][it][1] += nTmp;
                iz++;
                sprintf(nEvName,"NEventsSib_zBuf_%i",iz);
            }
        }
        if (!fNew) {
            break;
        }
    }
}
{
    printf("  Following information is for combined centrality bins \n\n");
    printf("Summed bin :");
    for (int ic=0;ic<nSums;ic++) {
        printf("%11i",ic);
    }
    printf("\n");
    printf(" numEvents :");
    for (int ic=0;ic<nSums;ic++) {
        printf("%11.0f",nEvents[ic]);
    }
    printf("\n");
    printf("\n");

    printf("            ");
    for (int it=0;it<4;it++) {
        printf("%10s+%10s-",types[it],types[it]);
    }
    printf("\n");
    for (int ic=0;ic<nSums;ic++) {
        if (0 == ic) {
            printf(" NTracks %i :",ic);
        } else {
            printf("         %i :",ic);
        }
        for (int it=0;it<4;it++) {
            printf("%11i%11i",n[ic][it][0],n[ic][it][1]);
        }
        printf("\n");
    }
    printf("\n");
    for (int ic=0;ic<nSums;ic++) {
        if (0 == ic) {
            printf(" dN/dEta %i :",ic);
        } else {
            printf("         %i :",ic);
        }
        for (int it=0;it<4;it++) {
            printf("%11.2f%11.2f",n[ic][it][0]/nEvents[ic],n[ic][it][1]/nEvents[ic]);
        }
        printf("\n");
    }
    printf("\n");
    for (int ic=0;ic<nSums;ic++) {
        if (0 == ic) {
            printf("    pHat %i :",ic);
        } else {
            printf("         %i :",ic);
        }
        double pHatp, pHatm;
        for (int it=0;it<4;it++) {
            if (n[ic][it][0] > 0) {
                pHatp = pHat[ic][it][0] / n[ic][it][0];
            } else {
                pHatp = -1;
            }
            if (n[ic][it][1] > 0) {
                pHatm = pHat[ic][it][1] / n[ic][it][1];
            } else {
                pHatp = -1;
            }
            printf("%11.3f%11.3f",pHatp,pHatm);
        }
        printf("\n");
    }
    printf("\n");
}




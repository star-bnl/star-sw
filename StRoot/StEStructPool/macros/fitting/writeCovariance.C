#include <stdio.h>

void writeCovariance(FILE *fOut, int ibin, int icharge, char **paramNames, double *errors, double *covar, int nCent, int nParams) {
    const char* binName[]={"all","soft","neck","hard","softHard"};
    const char* chargeName[] = {"LS", "US", "CD", "CI"};
    if (fOut) {
        fprintf(fOut," covariance matrices for %s, %s\n",binName[ibin],chargeName[icharge]);\
    } else {
        printf(" covariance matrices for %s, %s\n",binName[ibin],chargeName[icharge]);\
    }

    for (int ic=0;ic<nCent;ic++) {
        if (fOut) {
            fprintf(fOut," Normalized covariance for centrality %i\n",ic);
            fprintf(fOut,"          ");
            for (int ip=0;ip<nParams;ip++) {
                fprintf(fOut,"  %s",paramNames[ip]);
            }
            fprintf(fOut,"\n");
        } else {
            printf(" Normalized covariance for centrality %i\n",ic);
            printf("          ");
            for (int ip=0;ip<nParams;ip++) {
                printf("  %s",paramNames[ip]);
            }
            printf("\n");
        }
        int kp = 0;
        for (int ip=0;ip<nParams;ip++) {
            if (fOut) {
                fprintf(fOut,"%s  ",paramNames[ip]);
            } else {
                printf("%s  ",paramNames[ip]);
            }
            if (0 < errors[ic*nParams+ip]) {
                int lp = 0;
                for (int jp=0;jp<=ip;jp++) {
                    if (0 < errors[ic*nParams+jp]) {
                        double den = sqrt(covar[(ic*nParams+kp)*nParams+kp]*covar[(ic*nParams+lp)*nParams+lp]);
                        if (fOut) {
                            fprintf(fOut,"  %8.3f",covar[(ic*nParams+kp)*nParams+lp]/den);
                        } else {
                            printf("  %8.3f",covar[(ic*nParams+kp)*nParams+lp]/den);
                        }
                        lp++;
                    } else {
                        if (fOut) {
                            fprintf(fOut,"          ");
                        } else {
                            printf("          ");
                        }
                    }
                }
                kp++;
            }
            if (fOut) {
                fprintf(fOut,"\n");
            } else {
                printf("\n");
            }
        }
    }
}

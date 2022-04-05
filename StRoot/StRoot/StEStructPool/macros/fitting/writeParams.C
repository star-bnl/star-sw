#include <stdio.h>

void writeParams(FILE *fOut, int ibin, int icharge, char **paramNames, double *chisq, double *params, double *errors, int nCent, int nParams ) {
    const char* binName[]={"all","soft","neck","hard","softHard"};
    const char* chargeName[] = {"LS", "US", "CD", "CI"};
    if (fOut) {
        fprintf(fOut," Fits for %s, %s\n",binName[ibin],chargeName[icharge]);
        for (int ip=0;ip<=nParams;ip++) {
            fprintf(fOut,"%s",paramNames[ip]);
        }
        fprintf(fOut,"\n");
    } else {
        printf(" Fits for %s, %s\n",binName[ibin],chargeName[icharge]);
        for (int ip=0;ip<=nParams;ip++) {
            printf("%s",paramNames[ip]);
        }
        printf("\n");
    }

    for (int ic=0;ic<nCent;ic++) {
        if (fOut) {
            fprintf(fOut,"%2i%5.0f   ",ic,chisq[ic]);
            for (int ip=0;ip<nParams;ip++) {
                fprintf(fOut,"%7.3f(%3.0f)",params[ic*nParams+ip],1000*errors[ic*nParams+ip]);
            }
            fprintf(fOut,"\n");
        } else {
            printf("%2i%5.0f   ",ic,chisq[ic]);
            for (int ip=0;ip<nParams;ip++) {
                printf("%7.3f(%3.0f)",params[ic*nParams+ip],1000*errors[ic*nParams+ip]);
            }
            printf("\n");
        }
    }
}

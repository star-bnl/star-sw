#include <stdio.h>

void readParams(char *fileName, float *params, float *errors, int nCent, int nParams ) {
    FILE *fIn = fopen(fileName,"r");
    if (!fIn) {
        printf("Could not open file %s\n",fileName);
        return;
    }
    char line[1024];
    fgets(line,1024,fIn);
    fgets(line,1024,fIn);
    for (int ic=0;ic<nCent;ic++) {
        float chisq, p, pe;
        int icent;
        fscanf(fIn,"%i",&icent);
        fscanf(fIn,"%f",&chisq);
        for (int ip=0;ip<nParams;ip++) {
            fscanf(fIn,"%f(%f)",&p,&pe);
            params[ic*nParams+ip] = p;
            errors[ic*nParams+ip] = pe/1000;
        }
    }
    fclose(fIn);
}

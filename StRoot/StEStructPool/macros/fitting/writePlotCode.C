#include <stdio.h>
#include <string.h>

void writePlotCode(char *fileName, char *tag, char **pNames, double *chisq, double *params, double *errors, int nCent, int nParams ) {
    FILE *fOut = fopen(fileName,"w");

    fprintf(fOut,"{\n");
    // plot for chi^2.
    fprintf(fOut,"    if (ChiSquares_position > 0) {\n");
    fprintf(fOut,"        double %sChiSquareX[] = {",tag);
    for (int ic=0;ic<nCent;ic++) {
        fprintf(fOut,"%i",ic);
        if (ic < nCent-1) {
            fprintf(fOut,",");
        } else {
            fprintf(fOut,"};\n");
        }
    }
    fprintf(fOut,"        double %sChiSquares[] = {",tag);
    for (int ic=0;ic<nCent;ic++) {
        fprintf(fOut,"%7.2f",chisq[ic]);
        if (ic < nCent-1) {
            fprintf(fOut,",");
        } else {
            fprintf(fOut,"};\n");
        }
    }
    fprintf(fOut,"        TGraph *g%sChiSquares = new TGraph(%i,%sChiSquareX,%sChiSquares);\n\n",tag,nCent,tag,tag);
    fprintf(fOut,"        TAxis *x = g%sChiSquares->GetXaxis();\n",tag);
    fprintf(fOut,"        TAxis *y = g%sChiSquares->GetYaxis();\n",tag);
    fprintf(fOut,"        x->SetLimits(0,%i);\n",nCent-1);
    fprintf(fOut,"        x->SetTitle(\"centrality bin\");\n");
    fprintf(fOut,"        x->SetTitleSize(0.05);\n");
    fprintf(fOut,"        x->SetTitleOffset(0.6);\n");
    fprintf(fOut,"        y->SetTitle(\"ChiSquares\");\n");
    fprintf(fOut,"        y->SetTitleSize(0.05);\n");
    fprintf(fOut,"        y->SetTitleOffset(0.6);\n");
    fprintf(fOut,"        y->SetNdivisions(505);\n");
    fprintf(fOut,"        g%sChiSquares->SetLineColor(%s_color);\n",tag,tag);
    fprintf(fOut,"        g%sChiSquares->SetMarkerStyle(%s_style);\n",tag,tag);
    fprintf(fOut,"        g%sChiSquares->SetMarkerColor(%s_color);\n",tag,tag);
    fprintf(fOut,"        g%sChiSquares->SetMarkerSize(0.75);\n",tag);
    fprintf(fOut,"        c1->cd(ChiSquares_position);\n");
    fprintf(fOut,"        gPad->SetFillColor(0);\n");
    fprintf(fOut,"        gPad->SetLeftMargin(ChiSquares_margin);\n");
    fprintf(fOut,"        g%sChiSquares->SetMinimum(ChiSquares_min);\n",tag);
    fprintf(fOut,"        g%sChiSquares->SetMaximum(ChiSquares_max);\n",tag);
    fprintf(fOut,"        g%sChiSquares->Draw(%s_mode);\n",tag,tag);
    fprintf(fOut,"    }\n");

    // One plot for each parameter.
    for (int ip=0;ip<nParams;ip++) {
        fprintf(fOut,"    if (%s_position > 0) {\n",pNames[ip]);
        fprintf(fOut,"        double %s%s[] = {",tag,pNames[ip]);
        int isign = 1;
        if (1 == ip) {
            isign = -1;
        }
        for (int ic=0;ic<nCent;ic++) {
            fprintf(fOut,"%7.4f",isign*params[ic*nParams+ip]);
            if (ic < nCent-1) {
                fprintf(fOut,",");
            } else {
                fprintf(fOut,"};\n");
            }
        }
        fprintf(fOut,"        double e%s%s[] = {",tag,pNames[ip]);
        for (int ic=0;ic<nCent;ic++) {
            fprintf(fOut,"%7.4f",errors[ic*nParams+ip]);
            if (ic < nCent-1) {
                fprintf(fOut," ,");
            } else {
                fprintf(fOut,"};\n");
            }
        }
        if (!strcmp(pNames[ip],"cos2Phi")) {
            fprintf(fOut,"        TGraph *g%s%s = new TGraphErrors(%i,%sBbyB0,%s%s,e%sxpos,e%s%s);\n",tag,pNames[ip],nCent,tag,tag,pNames[ip],tag,tag,pNames[ip]);
            fprintf(fOut,"        TAxis *x = g%s%s->GetXaxis();\n",tag,pNames[ip]);
            fprintf(fOut,"        TAxis *y = g%s%s->GetYaxis();\n",tag,pNames[ip]);
            fprintf(fOut,"        x->SetLimits(0.0,1.0);\n");
            fprintf(fOut,"        x->SetTitle(\"1-b/b_0\");\n");
        } else {
            fprintf(fOut,"        TGraph *g%s%s = new TGraphErrors(%i,%sxpos,%s%s,e%sxpos,e%s%s);\n",tag,pNames[ip],nCent,tag,tag,pNames[ip],tag,tag,pNames[ip]);
            fprintf(fOut,"        TAxis *x = g%s%s->GetXaxis();\n",tag,pNames[ip]);
            fprintf(fOut,"        TAxis *y = g%s%s->GetYaxis();\n",tag,pNames[ip]);
            fprintf(fOut,"        x->SetLimits(1,6.5);\n");
            fprintf(fOut,"        x->SetTitle(\"#nu\");\n");
        }
        fprintf(fOut,"        x->SetTitleSize(0.05);\n");
        fprintf(fOut,"        x->SetTitleOffset(0.6);\n");
        fprintf(fOut,"        y->SetTitle(\"%s\");\n",pNames[ip]);
        fprintf(fOut,"        y->SetTitleSize(0.05);\n");
        fprintf(fOut,"        y->SetTitleOffset(0.6);\n");
        fprintf(fOut,"        y->SetNdivisions(505);\n");
        fprintf(fOut,"        g%s%s->SetLineColor(%s_color);\n",tag,pNames[ip],tag);
        fprintf(fOut,"        g%s%s->SetMarkerStyle(%s_style);\n",tag,pNames[ip],tag);
        fprintf(fOut,"        g%s%s->SetMarkerColor(%s_color);\n",tag,pNames[ip],tag);
        fprintf(fOut,"        g%s%s->SetMarkerSize(0.75);\n",tag,pNames[ip]);
        fprintf(fOut,"        c1->cd(%s_position);\n",pNames[ip]);
        fprintf(fOut,"        gPad->SetFillColor(0);\n");
        fprintf(fOut,"        gPad->SetLeftMargin(%s_margin);\n",pNames[ip]);
        fprintf(fOut,"        g%s%s->SetMinimum(%s_min);\n",tag,pNames[ip],pNames[ip]);
        fprintf(fOut,"        g%s%s->SetMaximum(%s_max);\n",tag,pNames[ip],pNames[ip]);
        fprintf(fOut,"        g%s%s->Draw(%s_mode);\n",tag,pNames[ip],tag);
        fprintf(fOut,"    }\n\n\n");
    }


    // plot ratio of minijet amplitude to cos(phi) amplitude.
    fprintf(fOut,"    if (jetOvercosphi_position > 0) {\n");
    fprintf(fOut,"        double %sjetOvercosphi[] = {",tag);
    for (int ic=0;ic<nCent;ic++) {
        double rat  = -params[ic*nParams+3]/params[ic*nParams+1];
        fprintf(fOut,"%7.4f",rat);
        if (ic < nCent-1) {
            fprintf(fOut,",");
        } else {
            fprintf(fOut,"};\n");
        }
    }
    fprintf(fOut,"        double e%sjetOvercosphi[] = {",tag);
    for (int ic=0;ic<nCent;ic++) {
        double rat  = -params[ic*nParams+3]/params[ic*nParams+1];
        double erat = rat * sqrt( pow(errors[ic*nParams+1]/params[ic*nParams+1],2) + pow(errors[ic*nParams+3]/params[ic*nParams+3],2));
        fprintf(fOut,"%7.4f",erat);
        if (ic < nCent-1) {
            fprintf(fOut," ,");
        } else {
            fprintf(fOut,"};\n");
        }
    }
    fprintf(fOut,"        TGraph *g%sjetOvercosphi = new TGraphErrors(%i,%sxpos,%sjetOvercosphi,e%sxpos,e%sjetOvercosphi);\n\n",tag,nCent,tag,tag,tag,tag);
    fprintf(fOut,"        TAxis *x = g%sjetOvercosphi->GetXaxis();\n",tag);
    fprintf(fOut,"        TAxis *y = g%sjetOvercosphi->GetYaxis();\n",tag);
    fprintf(fOut,"        x->SetLimits(1,6.5);\n");
    fprintf(fOut,"        x->SetTitle(\"#nu\");\n");
    fprintf(fOut,"        x->SetTitleSize(0.05);\n");
    fprintf(fOut,"        x->SetTitleOffset(0.6);\n");
    fprintf(fOut,"        y->SetTitle(\"jetOvercosphi\");\n");
    fprintf(fOut,"        y->SetTitleSize(0.05);\n");
    fprintf(fOut,"        y->SetTitleOffset(0.6);\n");
    fprintf(fOut,"        y->SetNdivisions(505);\n");
    fprintf(fOut,"        g%sjetOvercosphi->SetLineColor(%s_color);\n",tag,tag);
    fprintf(fOut,"        g%sjetOvercosphi->SetMarkerStyle(%s_style);\n",tag,tag);
    fprintf(fOut,"        g%sjetOvercosphi->SetMarkerColor(%s_color);\n",tag,tag);
    fprintf(fOut,"        g%sjetOvercosphi->SetMarkerSize(0.75);\n",tag);
    fprintf(fOut,"        c1->cd(jetOvercosphi_position);\n");
    fprintf(fOut,"        gPad->SetFillColor(0);\n");
    fprintf(fOut,"        gPad->SetLeftMargin(jetOvercosphi_margin);\n");
    fprintf(fOut,"        g%sjetOvercosphi->SetMinimum(jetOvercosphi_min);\n",tag);
    fprintf(fOut,"        g%sjetOvercosphi->SetMaximum(jetOvercosphi_max);\n",tag);
    fprintf(fOut,"        g%sjetOvercosphi->Draw(%s_mode);\n",tag,tag);
    fprintf(fOut,"    }\n");
    fprintf(fOut,"}\n");

    fclose(fOut);
}

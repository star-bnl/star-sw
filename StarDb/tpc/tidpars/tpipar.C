St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/tidpars/tpipar Allocated rows: 1  Used rows: 1  Row size: 132 bytes
//  Table: tpipar_st[0]--> tpipar_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpipar")) return 0;
tpipar_st row;
St_tpipar *tableSet = new St_tpipar("tpipar",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.method[0]	 =          1; // control used to change method of calc. ;
    row.method[1]	 =          2;
    row.nrec_cut	 =          4; // cut on nrec ;
    row.chisq1_cut	 = 5.04467e-44; // cut on chisq1/nrec ;
    row.chisq2_cut	 = 5.04467e-44; // cut on chisq2/nrec ;
    row.mean1[0]	 =  6.967e-08; // dedx vs p mean values 1st param,e,pi,k,p ;
    row.mean1[1]	 =   1.83e-07;
    row.mean1[2]	 =  1.606e-07;
    row.mean1[3]	 =  1.342e-07;
    row.mean2[0]	 =      18.12; // dedx vs p mean 2nd param, e, pi,k,p ;
    row.mean2[1]	 =     1.2909;
    row.mean2[2]	 =     2.0803;
    row.mean2[3]	 =     3.3306;
    row.mean3[0]	 =       6000; // dedx vs p mean 3rd param, e, pi,k,p ;
    row.mean3[1]	 =       6000;
    row.mean3[2]	 =       6000;
    row.mean3[3]	 =       6000;
    row.sigma1[0]	 =        0.1; // dedx sigma fun of p 1st param, e, pi, K, ;
    row.sigma1[1]	 =        0.1;
    row.sigma1[2]	 =        0.1;
    row.sigma1[3]	 =        0.1;
    row.sigma2[0]	 =          0; // sigma dedx fn of p,2nd param e, pi, K, p ;
    row.sigma2[1]	 =          0;
    row.sigma2[2]	 =          0;
    row.sigma2[3]	 =          0;
    row.skew[0]	 = 1.84922e+31; // skew of dedx, e, pi, k, p ;
    row.skew[1]	 = 2.04067e+20;
    row.skew[2]	 =          0;
    row.skew[3]	 =          0;
    row.skew[4]	 =          0;
    row.skew[5]	 =          0;
    row.skew[6]	 =          0;
    row.skew[7]	 =          0;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}

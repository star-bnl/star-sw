TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.data/StarDb/Calibrations/tracker/.tpcOuterHitError/tpcOuterHitError Allocated rows: 1  Used rows: 1  Row size: 48 bytes
//  Table: HitError_st[0]--> HitError_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_HitError")) return 0;
HitError_st row;
St_HitError *tableSet = new St_HitError("tpcOuterHitError",1);
//
memset(&row,0,tableSet->GetRowSize());
//                                        hit errors are squared !
    row.coeff[0]	 = 0.000421985;    // u intrinsic
    row.coeff[1]	 = 0.00124327 ;    // u cross
    row.coeff[2]	 = 0.0257111  ;    // u drift
    row.coeff[3]	 = 0.000402954;    // z intrinsic
    row.coeff[4]	 = 0.00346896 ;    // z dip
    row.coeff[5]	 = 0.0377259  ;    // z drift
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
#if 0
3 events

0.000421985
0.00124327
0.0257111
0.000402954
0.00346896
0.0377259
0.0
0.000936669
0.000496741
0.00018648
0.00507244
0.002654















0.00048634
0.00145771
0.0277514
0.000675568
0.00419374
0.0385963


Fit(2): Iter=87 Sum=6.0836 Der=7.45871e-08 Fail=0
0.0
0.00105437
0.000719948
0.00122828
0.00560929
0.00247209


// u intrinsic
// u cross
// u drift
// z intrinsic
// z dip
// z drift



+0.000575834
+0.00140004
+0.0282569
+0.000946761
+0.00418744
+0.0382733

		
+0.0
+0.00112419
+0.0012487
+0.00161102
+0.00576447
+0.00259441
#endif//0

TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Calibrations/rich/.spaceChargeCorR2/spaceChargeCorR2 Allocated rows: 8  Used rows: 8  Row size: 64 bytes
//  Table: spaceChargeCor_st[0]--> spaceChargeCor_st[7]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_spaceChargeCor")) return 0;
spaceChargeCor_st row;
St_spaceChargeCor *tableSet = new St_spaceChargeCor("spaceChargeCorR2",8);
//
memset(&row,0,tableSet->GetRowSize());
    row.fullFieldB	 =  2.672e-07; // Negative Full Field Correction  ;
    row.halfFieldB	 =          0; // Negative Half Field Correction  ;
    row.zeroField	 =          0; // Zero Field " "  ;
    row.halfFieldA	 =          0; // Postive Half " " ;
    row.fullFieldA	 =          0; // Postive Full " " ;
    row.satRate	 =      1e+10; // Saturation Rate Hz  ;
    row.factor	 =          1; // Multiplicative Factor ;
    row.detector	 =         15; // 0=VPDx, 1=BBCx, 2=ZDCx, 3=ZDCe+w, 4=BBCe+w, ... ;
    row.offset	 =       2020; // Offset at zero luminosity ;
    row.ewratio	 =          1; // Ratio of charge east/west ;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.fullFieldB	 =          0; // Negative Full Field Correction  ;
    row.halfFieldB	 =          0; // Negative Half Field Correction  ;
    row.zeroField	 =          0; // Zero Field " "  ;
    row.halfFieldA	 =          0; // Postive Half " " ;
    row.fullFieldA	 =          0; // Postive Full " " ;
    row.satRate	 =      1e+10; // Saturation Rate Hz  ;
    row.factor	 =          1; // Multiplicative Factor ;
    row.detector	 =         -1; // 0=VPDx, 1=BBCx, 2=ZDCx, 3=ZDCe+w, 4=BBCe+w, ... ;
    row.offset	 =          0; // Offset at zero luminosity ;
    row.ewratio	 =          1; // Ratio of charge east/west ;
    for (Int_t i = 1; i < 8; i++) {
      tableSet->AddAt(&row);
    }
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}

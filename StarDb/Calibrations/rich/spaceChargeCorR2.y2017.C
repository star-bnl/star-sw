TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/Calibrations/rich/.spaceChargeCorR2/spaceChargeCorR2 Allocated rows: 1  Used rows: 1  Row size: 60 bytes
//  Table: spaceChargeCor_st[0]--> spaceChargeCor_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_spaceChargeCor")) return 0;
spaceChargeCor_st row;
St_spaceChargeCor *tableSet = new St_spaceChargeCor("spaceChargeCorR2",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.fullFieldB	 =          0; // Negative Full Field Correction  ;
    row.halfFieldB	 =          0; // Negative Half Field Correction  ;
    row.zeroField	 =          0; // Zero Field " "  ;
    row.halfFieldA	 =          0; // Postive Half " " ;
    row.fullFieldA	 =          0; // Postive Full " " ;
    row.satRate	 =    1000000; // Saturation Rate Hz  ;
    row.factor	 =          1; // Multiplicative Factor ;
    row.detector	 =          2; // 0=RICH, 1=BBCx, 2=ZDCx e+w, 4=BBCe+w ;
    row.offset	 =          0; // Offset at zero luminosity ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}

TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/RunLog/onl/.beamInfo/beamInfo Allocated rows: 1  Used rows: 1  Row size: 120 bytes
//  Table: beamInfo_st[0]--> beamInfo_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_beamInfo")) return 0;
beamInfo_st row;
St_beamInfo *tableSet = new St_beamInfo("beamInfo",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =    8178072; // ;
    row.entryTag	 =          0; // 0=startrun, 1=endrun, 2=runave, 3=std  ;
 memcpy(&row.blueSpecies,"Au",2);// species  
    row.blueMassNumber	 =        197; // ;
    row.blueEnergy	 =      0.931; // energy  ;
    row.blueIntensity	 =     -0.003; // Ions  ;
    row.blueLifeTime	 =          0; // Ions per minute  ;
    row.blueBunchIntensity	 =          0; // bunch intensity  ;
 memcpy(&row.yellowSpecies,"Au",2);// species  
    row.yellowMassNumber	 =        197; // ;
    row.yellowEnergy	 =      0.931; // energy  ;
    row.yellowIntensity	 =     -0.002; // Ions  ;
    row.yellowLifeTime	 =          0; // Ions per minute  ;
    row.yellowBunchIntensity	 =          0; // bunch intensity  ;
    row.blueFillNumber	 =       9057; // ;
    row.yellowFillNumber	 =       9057; // ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}

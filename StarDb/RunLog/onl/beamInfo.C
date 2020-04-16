TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/RunLog/onl/.beamInfo/beamInfo Allocated rows: 1  Used rows: 1  Row size: 120 bytes
//  Table: beamInfo_st[0]--> beamInfo_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_beamInfo")) return 0;
beamInfo_st row;
St_beamInfo *tableSet = new St_beamInfo("beamInfo",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.runNumber	 =   17101001; // ;
    row.entryTag	 =          0; // 0=startrun, 1=endrun, 2=runave, 3=std  ;
 memcpy(&row.blueSpecies,"Au",2);// species  
    row.blueMassNumber	 =        197; // ;
    row.blueEnergy	 =     99.908; // energy  ;
    row.blueIntensity	 =    219.761; // Ions  ;
    row.blueLifeTime	 =          0; // Ions per minute  ;
    row.blueBunchIntensity	 =          0; // bunch intensity  ;
 memcpy(&row.yellowSpecies,"Au",2);// species  
    row.yellowMassNumber	 =        197; // ;
    row.yellowEnergy	 =     99.908; // energy  ;
    row.yellowIntensity	 =    227.272; // Ions  ;
    row.yellowLifeTime	 =          0; // Ions per minute  ;
    row.yellowBunchIntensity	 =          0; // bunch intensity  ;
    row.blueFillNumber	 =      19744; // ;
    row.yellowFillNumber	 =      19744; // ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}

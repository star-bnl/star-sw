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
    row.runNumber	 =   18100004; // ;
    row.entryTag	 =          0; // 0=startrun, 1=endrun, 2=runave, 3=std  ;
 memcpy(&row.blueSpecies,"PP",2);// species  
    row.blueMassNumber	 =          0; // ;
    row.blueEnergy	 =    254.867; // energy  ;
    row.blueIntensity	 =   20580.55; // Ions  ;
    row.blueLifeTime	 =          0; // Ions per minute  ;
    row.blueBunchIntensity	 =          0; // bunch intensity  ;
 memcpy(&row.yellowSpecies,"PP",2);// species  
    row.yellowMassNumber	 =          0; // ;
    row.yellowEnergy	 =    254.867; // energy  ;
    row.yellowIntensity	 =   21687.02; // Ions  ;
    row.yellowLifeTime	 =          0; // Ions per minute  ;
    row.yellowBunchIntensity	 =          0; // bunch intensity  ;
    row.blueFillNumber	 =      20740; // ;
    row.yellowFillNumber	 =      20740; // ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}

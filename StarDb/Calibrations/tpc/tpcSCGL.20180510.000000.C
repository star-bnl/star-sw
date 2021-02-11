TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Calibrations/tpc/.tpcSCGL/tpcSCGL Allocated rows: 1  Used rows: 1  Row size: 588 bytes
//  Table: tpcSCGL_st[0]--> tpcSCGL_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcSCGL")) return 0;
tpcSCGL_st row;
St_tpcSCGL *tableSet = new St_tpcSCGL("tpcSCGL",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.SC[0]	 =   4.92e-05; // Scale factor relating luminosity scaler to SpaceCharge ;
    row.SC[1]	 =     181.67;
    row.SC[2]	 =          0;
    row.SC[3]	 =          0;
    row.SC[4]	 =          0;
    row.SC[5]	 =          0;
    row.SC[6]	 =          0;
    row.SC[7]	 =          0;
    row.SCoffset[0]	 =          0; // Offset to define luminosity for SpaceCharge ;
    row.SCoffset[1]	 =          0;
    row.SCoffset[2]	 =          0;
    row.SCoffset[3]	 =          0;
    row.SCoffset[4]	 =          0;
    row.SCoffset[5]	 =          0;
    row.SCoffset[6]	 =          0;
    row.SCoffset[7]	 =          0;
    row.SCexponent[0]	 =          0; // Luminosity exponential factor for SpaceCharge ;
    row.SCexponent[1]	 =          0;
    row.SCexponent[2]	 =          0;
    row.SCexponent[3]	 =          0;
    row.SCexponent[4]	 =          0;
    row.SCexponent[5]	 =          0;
    row.SCexponent[6]	 =          0;
    row.SCexponent[7]	 =          0;
    row.SCscaler[0]	 =          0; // Luminosity detector scaler ;
    row.SCscaler[1]	 =          0;
    row.SCscaler[2]	 =          0;
    row.SCscaler[3]	 =          0;
    row.SCscaler[4]	 =          0;
    row.SCscaler[5]	 =          0;
    row.SCscaler[6]	 =          0;
    row.SCscaler[7]	 =          0;
    row.GL[0]	 =          0; // Scale factor relating SpaceCharge to GridLeak ;
    row.GL[1]	 =          0;
    row.GL[2]	 =          0;
    row.GL[3]	 =          0;
    row.GL[4]	 =          0;
    row.GL[5]	 =          0;
    row.GL[6]	 =          0;
    row.GL[7]	 =          0;
    row.GL[8]	 =          0;
    row.GL[9]	 =          0;
    row.GL[10]	 =          0;
    row.GL[11]	 =          0;
    row.GL[12]	 =          0;
    row.GL[13]	 =          0;
    row.GL[14]	 =          0;
    row.GL[15]	 =          0;
    row.GL[16]	 =          0;
    row.GL[17]	 =          0;
    row.GL[18]	 =          0;
    row.GL[19]	 =          0;
    row.GL[20]	 =          0;
    row.GL[21]	 =          0;
    row.GL[22]	 =          0;
    row.GL[23]	 =          0;
    row.GLoffset[0]	 =          0; // Offset to define luminosity for GridLeak ;
    row.GLoffset[1]	 =          0;
    row.GLoffset[2]	 =          0;
    row.GLoffset[3]	 =          0;
    row.GLoffset[4]	 =          0;
    row.GLoffset[5]	 =          0;
    row.GLoffset[6]	 =          0;
    row.GLoffset[7]	 =          0;
    row.GLoffset[8]	 =          0;
    row.GLoffset[9]	 =          0;
    row.GLoffset[10]	 =          0;
    row.GLoffset[11]	 =          0;
    row.GLoffset[12]	 =          0;
    row.GLoffset[13]	 =          0;
    row.GLoffset[14]	 =          0;
    row.GLoffset[15]	 =          0;
    row.GLoffset[16]	 =          0;
    row.GLoffset[17]	 =          0;
    row.GLoffset[18]	 =          0;
    row.GLoffset[19]	 =          0;
    row.GLoffset[20]	 =          0;
    row.GLoffset[21]	 =          0;
    row.GLoffset[22]	 =          0;
    row.GLoffset[23]	 =          0;
    row.GLradius	 =      121.8; // Radius of GridLeak between inner/outer sectors ;
    row.GLwidth	 =          3; // Width of GridLeak between inner/outer sectors ;
    row.mode	 =          0; // Modes to simplify parameter controls ;
 memcpy(&row.comment,"default",7);// 
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}

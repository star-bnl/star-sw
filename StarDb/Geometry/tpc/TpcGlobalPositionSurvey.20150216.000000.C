TDataSet *CreateTable() { 
  if (!TClass::GetClass("St_Survey")) return 0;

  Survey_st row;
  St_Survey *tableSet = new St_Survey("TpcGlobalPositionSurvey",1);

  memset(&row,0,tableSet->GetRowSize());

  row.Id = 1;
  row.r00 = 1.0;
  row.r01 = 0.0;
  row.r02 = 0.0000000;
  row.r10 = 0.0;
  row.r11 = 1.0;
  row.r12 =-0.000000;
  row.r20 =-0.0000000;
  row.r21 = 0.0000000;
  row.r22 = 1.0;

/* zero out translation ... 
  row.t0 = -0.178;
  row.t1 = -0.675200;
  row.t2 = -0.080860;
*/

  memcpy(&row.comment,"\x00",1);// 
  tableSet->AddAt(&row);

  return (TDataSet *)tableSet;

};



#if 0
TDataSet *CreateTable() { 
  if (!TClass::GetClass("St_tpcGlobalPosition")) return 0;
  tpcGlobalPosition_st row;
  St_tpcGlobalPosition *tableSet = new St_tpcGlobalPosition("tpcGlobalPosition",1);
  memset(&row,0,tableSet->GetRowSize());
  row.XX	 =          1; // XX element of rotation matrix  ;
  row.YY	 =          1; // YY element of rotation matrix  ;
  row.ZZ	 =          1; // ZZ element of rotation matrix  ;
  row.XX_geom	 =          1; // XX element of geometrical rotation matrix  ;
  row.YY_geom	 =          1; // YY element of geometrical rotation matrix  ;
  row.ZZ_geom	 =          1; // ZZ element of geometrical rotation matrix  ;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
#endif

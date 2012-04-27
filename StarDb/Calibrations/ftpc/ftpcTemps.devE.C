TDataSet *CreateTable() { 
  if (!TClass::GetClass("St_ftpcTemps")) return 0;
  ftpcTemps_st row;
  St_ftpcTemps *tableSet = new St_ftpcTemps("ftpcTemps",1);
  memset(&row,0,tableSet->GetRowSize());
  row.extra1East	 =      21.56; // ftpc_t_ex_2_1.VAL;
  row.extra2East	 =    21.1225; // ftpc_t_ex_2_2.VAL;
  row.extra3East	 =    23.1575; // ftpc_t_ex_2_3.VAL;
  row.extra4East	 =      21.01; // ftpc_t_ex_2_4.VAL;
  row.extra5East	 =    20.7825; // ftpc_t_ex_2_5.VAL;
  row.extra6East	 =    20.8875; // ftpc_t_ex_2_6.VAL;
  row.extra7East	 =     20.665; // ftpc_t_ex_2_7.VAL;
  row.extra1West	 =     21.715; // ftpc_t_ex_1_1.VAL;
  row.extra2West	 =    23.5375; // ftpc_t_ex_1_2.VAL;
  row.extra3West	 =    19.8575; // ftpc_t_ex_1_3.VAL;
  row.extra4West	 =    21.2125; // ftpc_t_ex_1_4.VAL;
  row.extra5West	 =    21.2875; // ftpc_t_ex_1_5.VAL;
  row.extra6West	 =         22; // ftpc_t_ex_1_6.VAL;
  row.extra7West	 =          0; // ftpc_t_ex_1_7.VAL=0.0(not active;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}

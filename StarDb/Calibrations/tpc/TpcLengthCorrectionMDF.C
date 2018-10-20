TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_MDFCorrection")) return 0;
  Int_t nrows = 6;
  MDFCorrection_st row;
  St_MDFCorrection *tableSet = new St_MDFCorrection("TpcLengthCorrectionMDF",nrows);
  memset(&row,0,tableSet->GetRowSize()); //  TPoints70UGPRunXVII14; h2mdf("mu",5,1,20);        
  row.nrows      = nrows;
  row.idx        =     1;// 
  tableSet->AddAt(&row);// 0 -> I70                                                            
  memset(&row,0,tableSet->GetRowSize()); //  TPoints70UGPRunXVII14; h2mdf("sigma",5,1,20)     
  row.nrows      = nrows;
  row.idx        =     2;// 
  tableSet->AddAt(&row);// 1 -> sigma.I70   						       
  memset(&row,0,tableSet->GetRowSize()); //  TPointsNUGPRunXVII14; h2mdf("mu",5,1,20);         
  row.nrows      = nrows;
  row.idx        =     3;// 
  tableSet->AddAt(&row);// 2 -> dN/dx         						       
  memset(&row,0,tableSet->GetRowSize()); //  TPointsNUGPRunXVII14; h2mdf("sigma",5,1,20);      
  row.nrows      = nrows;
  row.idx        =     4;// 
  tableSet->AddAt(&row);// 3 -> sigma.dN/dx   						       
  memset(&row,0,tableSet->GetRowSize()); //  TPointsFUGPRunXVII14; h2mdf("mu",5,1,20); 	       
  row.nrows      = nrows;
  row.idx        =     5;// 
  tableSet->AddAt(&row);// 4 -> Ifit 	     
  memset(&row,0,tableSet->GetRowSize()); //  TPointsFUGPRunXVII14; h2mdf("sigma",5,1,20);      
  row.nrows      = nrows;
  row.idx        =     6;// 
  tableSet->AddAt(&row);// 5 -> sigma.Ifit	     
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_MDFCorrection")) return 0;
  Int_t nrows = 6;
  MDFCorrection_st row;
  St_MDFCorrection *tableSet = new St_MDFCorrection("TpcLengthCorrectionMDF",nrows);
  //  row.nrows      = nrows;
  //  row.idx        =     1;// 
  memset(&row,0,tableSet->GetRowSize()); //  TPoints70BUGPRunXVII14; h2mdf("mu",5,1,20);        
  tableSet->AddAt(&row);// 0 -> I70                                                            
  memset(&row,0,tableSet->GetRowSize()); //  TPoints70BUGPRunXVII14; h2mdf("sigma",5,1,20)     
  tableSet->AddAt(&row);// 1 -> sigma.I70   						       
  memset(&row,0,tableSet->GetRowSize()); //  TPointsNUGPRunXVII14; h2mdf("mu",5,1,20);         
  tableSet->AddAt(&row);// 2 -> dN/dx         						       
  memset(&row,0,tableSet->GetRowSize()); //  TPointsNUGPRunXVII14; h2mdf("sigma",5,1,20);      
  tableSet->AddAt(&row);// 3 -> sigma.dN/dx   						       
  memset(&row,0,tableSet->GetRowSize()); //  TPointsBUGPRunXVII14; h2mdf("mu",5,1,20); 	       
  tableSet->AddAt(&row);// 4 -> Ifit 	     
  memset(&row,0,tableSet->GetRowSize()); //  TPointsBUGPRunXVII14; h2mdf("sigma",5,1,20);      
  tableSet->AddAt(&row);// 5 -> sigma.Ifit	     
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

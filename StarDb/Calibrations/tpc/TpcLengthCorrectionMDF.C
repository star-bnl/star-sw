TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_MDFCorrection")) return 0;
  Int_t nrows = 6;
  MDFCorrection_st row;
  St_MDFCorrection *tableSet = new St_MDFCorrection("TpcLengthCorrectionMDF",nrows);
  //  row.nrows      = nrows;
  //  row.idx        =     1;// 
  memset(&row,0,tableSet->GetRowSize()); //  TPoints70BUGPRunXVII14; h2mdf("mu",5,1,20);        
  tableSet->AddAt(&row);// 0 -> I70                                                            
  memset(&row,0,tableSet->GetRowSize());     TPoints70BUGPRunXVII14; h2mdf("sigma",5,1,20)     
  tableSet->AddAt(&row);// 1 -> sigma.I70   						       
  memset(&row,0,tableSet->GetRowSize()); //  TPointsNUGPRunXVII14; h2mdf("mu",5,1,20);         
  tableSet->AddAt(&row);// 2 -> I60         						       
  memset(&row,0,tableSet->GetRowSize()); //  TPointsNUGPRunXVII14; h2mdf("sigma",5,1,20);      
  tableSet->AddAt(&row);// 3 -> sigma.I60   						       
  memset(&row,0,tableSet->GetRowSize()); //                                                    
  tableSet->AddAt(&row);// 4 -> I 	     TPointsBUGPRunXVII14; h2mdf("mu",5,1,20); 	       
  memset(&row,0,tableSet->GetRowSize()); //                                                    
  tableSet->AddAt(&row);// 5 -> sigma.I	     TPointsBUGPRunXVII14; h2mdf("sigma",5,1,20);      
  memset(&row,0,tableSet->GetRowSize()); // 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// tofr5INLtable Allocated rows: 40
//  Table: tofr5INLtable_st[0]-->tofr5INLtable_st[39]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tofr5INLtable")) return 0;
  tofr5INLtable_st row;
  St_tofr5INLtable *tableSet = new St_tofr5INLtable("tofr5INLtable",40);

  memset(&row,0,tableSet->GetRowSize());

  // read in data and prepare to put in db.
  // read in inltable file by file.
  string filetdcid[4]={"1","2","3","4"};string filehitid[4]={"1","10","22","1"};
  string boardid[10]={"mx38162","mx36804","mx36803","mx36805","mx36802",
                            "mx38163","mx36799","mx36801","mx38164","mx38161"};
  char tmpchr[200];
  int index=0;
  for(int iboard=0;iboard<10;iboard++){
    for(int itdc=0;itdc<4;itdc++){
      for(int i=0;i<1024;i++){row.INLcorrection[i]=0.;}      
      cout<<"Process board "<<boardid[iboard]<<endl;
      // make file name-----------
      if(boardid[iboard]=="mx38163") boardid[iboard]="mx38162";
      sprintf(tmpchr,"inltabledat/%s-tdc%s-h%s.inl-1024",boardid[iboard].c_str(),
              filetdcid[itdc].c_str(),filehitid[itdc].c_str());
      if(itdc==3)sprintf(tmpchr,"inltabledat/%s-tdc%s-h%s.inl-256",boardid[iboard].c_str(),
              filetdcid[itdc].c_str(),filehitid[itdc].c_str());
      //--------------------------
      ifstream infile(tmpchr);
      if(!infile) {cout<<"Can not open "<<tmpchr<<" Please check filename!!!"<<endl;return -1;}

      cout<<"open file:"<<tmpchr<<endl;
      int tdcindex=iboard*4+itdc;
      sprintf(row.boardID,"%s",boardid[iboard].c_str());
      row.boardNumber= iboard;

      row.TDCID=itdc;
      float bin;float data;
      int totbin=0;
      while(infile>>bin>>data){
        int ibin=int(bin-0.5);
        int datax1000=int(data*1000);
        float rdata=datax1000/1000.;
        row.INLcorrection[ibin]=rdata;
        //cout<<"readin bin="<<bin<<" ibin="<<ibin<<" data="<<data<<endl;
        totbin++;
      }
      cout<<"totbin="<<totbin<<endl;
      infile.close();
      // upload data to DB here.
      cout<<"board id ="<<row.boardID<<" board number= "<<row.boardNumber<<" TDCID="<<row.TDCID<<endl;
      for(int i=0;i<10;i++){cout<<i<<" "<<row.INLcorrection[i]<<endl;}
      for(int i=1014;i<1024;i++){cout<<i<<" "<<row.INLcorrection[i]<<endl;}
      cout<<"index="<<index<<endl;
      index++;
      tableSet->AddAt(&row);
    }  // end of loop TDC
      cout<<"================================================="<<endl;
  } // end of loop board


// ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

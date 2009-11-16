TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// 
// Positioning of the SSD wafers in the SSD ladder coordinate systems
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row = {0, 1,0,0, 0,1,0, 0,0,1,  0,0,0, .1,.1,.1,.002,.1,.002,"Ideal"};
  Int_t NW = 16;
  Int_t NL = 20;
  St_Survey *tableSet = new St_Survey("SsdWafersOnLadders",NW*NL);
  for (Int_t wafer = 1; wafer <= NW; wafer++) {
    for (Int_t ladder = 1; ladder <= NL; ladder++) {
      row.Id = 7000 + 100*wafer + ladder;
      row.t2 = -32.625 + 4.35*(wafer-1);
      tableSet->AddAt(&row.Id);
    }
  }
  return (TDataSet *)tableSet;
}

TDataSet *CreateTable() { 
  if (!TClass::GetClass("St_TMVArank")) return 0;
  St_TMVArank *tableSet = new St_TMVArank("TMVArank4KFV",1);
  TMVArank_st row = {"BDT","postx:prompt:cross:tof:notof:BEMC:noBEMC:nWE:chi2",""};
  TString weightfile( __FILE__ );// cout << "weightfile\t" << weightfile.Data() << endl;
  weightfile.ReplaceAll(".C",".weights.xml");// cout << "weightfile\t" << weightfile.Data() << endl;
  gSystem->FindFile("",weightfile);
  if (weightfile != "") {
    strncpy(row.XmlFile, weightfile.Data(), 256);
    //    cout << "row.XmlFile\t" << row.XmlFile << endl;
  }
  tableSet->AddAt(&row);
 return (TDataSet *)tableSet;
}

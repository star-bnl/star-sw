
void histCopy(const char *nameIn, const char *nameOut) 
{
// Input file
      TFile* fileIn = new TFile(nameIn, "READ");
      if (!fileIn->IsOpen()) {
        cout << "### Can't find file" << nameIn << endl;
        return;
      }
      TFile* fileOut = new TFile(nameOut, "RECREATE");
      if (!fileOut->IsOpen()){
        cout << "### Can't open file" << nameOut << endl;
        return;
      }
 
      TKey*    key;
      TObject* obj;
      TIter nextkey(fileIn->GetListOfKeys());
      int num=0;
      while (key = (TKey*)nextkey()) {
        fileIn->cd();
        char* objName = key->GetName();
        char* clsName = key->GetClassName();
        char* ignore ="";
        if (strcmp("TNtuple",clsName)==0) ignore ="  IGNORED";
        if (strcmp("TTree"  ,clsName)==0) ignore ="  IGNORED";
        obj=0;
        if (*ignore==0) obj = key->ReadObj();
        num++;
 
        cout << num << "  obj name= " << clsName << "::" << objName << ignore << endl;
        if (obj) {
	  fileOut->cd();
          obj->Write(objName);
          delete obj;
        }
      }
      fileIn->Close();
      fileOut->Close();
}

#include "HistManager.h"

ClassImp(HistManager)

HistManager::HistManager():TObjArray()
{
}

HistManager::~HistManager()
{
  delete mOutputFile;
}

TFile* HistManager::InitFile(const char* fname, Option_t* option, const char* ftitle, Int_t compress )
{
  TString name(fname);
  if( name.Length()!=0 && mOutputFile==0 ){ /*std::cout << "NEWFILE" << std::endl;*/ mOutputFile = new TFile(fname,option,ftitle,compress); }
  return mOutputFile;
}

TH1* HistManager::Find(const char* histname) const
{
  auto exists = mHistNames.find(histname);
  if( exists==mHistNames.cend() ){ return 0; }
  else{ return exists->second; }
}

bool HistManager::InitialCheck(TH1*& hist, std::string histname)
{
  auto exists = mHistNames.find(histname);
  if( exists!=mHistNames.cend() ){
    if( hist==0 ){      //Only set null pointers to avoid dangling pointers
      hist = exists->second;
      return true;  //Already added so don't add
    }
  }
  //std::cout << "HistManager::AddH1F|h1:"<< h1 << std::endl;
  if( hist!=0 ){
    //std::cout << " + |HistManager::AddH1F|"<<h1->GetName()<<"|kND:"<<h1->TestBit(kNotDeleted) <<"|22:"<< h1->TestBit(22) << std::endl;
    //Here I am using bit 22 since that is unused by TH1
    if( hist->TestBit(22) ){ delete hist; hist=0; } //It is true that it was loaded from the file so safe to delete pointer and reset as long as TH1::AddDirectory(kFALSE) is called since deleting/closing a #TFile deletes the histograms even if this object is set as the owner, which causes undefined behavior for the status bits which this class needs
    else{ Add(hist,histname.c_str()); return true; } //Object was new so stop and return 0 and re-add to array
    //h1 = 0; //@[Dec, 16, 2024] > Hack to make loading files work
  }
  return false; //Failed the initial check so a new object needs to be created or loaded from the file, pointer should have been set to zero if the histogram already existed
}

void HistManager::Add(TH1* h, const char* name)
{
  TObjArray::Add(h);
  auto added = mHistNames.emplace(name,h);
  if( ! (added.second) ){ std::cout << "For some reason histogram \"" << name << "\" already exists and was not added|existing:"<<added.first->second << "|h:"<<h << std::endl;
  }
}

UInt_t HistManager::AddH1F(TFile* file, TH1*& h1, const char* name, const char* title, Int_t nbins, Double_t xlow, Double_t xhigh)
{
  UInt_t status = 0;
  if( InitialCheck(h1,name) ){ return status; }
  //std::cout << "B|File:"<<file << "|h1:"<<h1 << std::endl;
  if( file!=0 ){ h1 = (TH1*)file->Get(name); }
  if( h1==0 ){
    //h1 = (TH1*)FindObject(name);
    //if( h1==0 ){
      h1 = new TH1F(name,title,nbins,xlow,xhigh);
      h1->Sumw2();
      //}
      //else{ return 1; }
  }
  else{
    h1->SetBit(22);
    ++status;
  }
  //std::cout << "A|File:"<<file << "|h1:"<<h1 << std::endl;
  h1->SetTitle(title);
  Add(h1,name);
  return status;//1 if histogram loaded or exists, 0 otherwise
}

UInt_t HistManager::AddH1D(TFile* file, TH1*& h1, const char* name, const char* title, Int_t nbins, Double_t xlow, Double_t xhigh)
{
  UInt_t status = 0;
  if( InitialCheck(h1,name) ){ return status; }
  
  if( file!=0 ){ h1 = (TH1*)file->Get(name); }
  if( h1==0 ){
    //h1 = (TH1*)FindObject(name);
    //if( h1==0 ){
      h1 = new TH1D(name,title,nbins,xlow,xhigh);
      h1->Sumw2();
      //}
      //else{ return 1; }
  }
  else{
    h1->SetBit(22);
    ++status;
  }
  h1->SetTitle(title);
  Add(h1,name);
  return status;//1 if histogram loaded or exists, 0 otherwise
}

UInt_t HistManager::AddH1D(TFile* file, TH1*& h1, const char* name, const char* title, Int_t nbins, const Double_t* xbins)
{
  UInt_t status = 0;
  if( InitialCheck(h1,name) ){ return status; }
  
  if( file!=0 ){ h1 = (TH1*)file->Get(name); }
  if( h1==0 ){
    //h1 = (TH1*)FindObject(name);
    //if( h1==0 ){
    h1 = new TH1D(name,title,nbins,xbins);
    h1->Sumw2();
    //}
    //else{ return 1; }
  }
  else{
    h1->SetBit(22);
    ++status;
  }
  h1->SetTitle(title);
  Add(h1,name);
  return status;//1 if histogram loaded or exists, 0 otherwise
}

UInt_t HistManager::AddH1FArr(TFile* file, TObjArray*& arr, UInt_t nobjs, const char* name, const char* title, Int_t nbins, Double_t xlow, Double_t xhigh)
{
  if( arr==0 ){ return 0; }
  UInt_t status = 0;
  for( UInt_t iobj = 0; iobj<nobjs; ++iobj ){
    TH1* h1 = 0;
    bool inarray = false;
    if( ((int)iobj)<arr->GetEntriesFast() && arr->GetEntriesFast()>0 ){ //This means the array already has some object at iobj
      h1 = (TH1*)arr->UncheckedAt(iobj);
      if( h1!=0 ){
	inarray = true;
	//Here I am using bit 22 since that is unused by TH1
	//std::cout << " + |HistManager::AddH2FArr|"<<h2->GetName()<<"|kND:"<<h2->TestBit(kNotDeleted) <<"|22:"<< h2->TestBit(22) << std::endl;
	if( h1->TestBit(22) ){ arr->RemoveAt(iobj); delete h1; h1=0; } //It is true that the file was loaded so safe to change pointer without delete
	else{ ++status; continue; } //Object was added to array as new so just keep going
      }
    }
    std::stringstream ss_name;
    ss_name << name << "_" << iobj;
    if( InitialCheck(h1,ss_name.str()) ){ return status; }
    if( file!=0 ){ h1 = (TH1F*)file->Get(ss_name.str().c_str()); }
    if( h1==0 ){
      h1 = new TH1F(ss_name.str().c_str(),title,nbins,xlow,xhigh);
      h1->Sumw2();
    }
    else{
      h1->SetBit(22);
      ++status;
    }
    h1->SetTitle(title);
    if( inarray ){ arr->AddAt(h1,iobj); }
    else{ arr->Add(h1); }
    Add(h1,ss_name.str().c_str());
  }
  return status;
}

UInt_t HistManager::AddH2F(TFile* file, TH1*& h2, const char* name, const char* title, Int_t nbinsx, Double_t xlow, Double_t xhigh, Int_t nbinsy, Double_t ylow, Double_t yhigh)
{
  UInt_t status = 0;
  if( InitialCheck(h2,name) ){ return status; }

  if( file!=0 ){ h2 = (TH1*)file->Get(name); }
  if( h2==0 ){
    //h2 = (TH2*)FindObject(name);
    //if( h2==0 ){
    h2 = new TH2F(name,title, nbinsx,xlow,xhigh, nbinsy,ylow,yhigh);
    h2->Sumw2();
    //}
    //else{ return 1; }
  }
  else{
    h2->SetBit(22);
    ++status;
  }
  h2->SetTitle(title);
  Add(h2,name);
  return status;//1 if histogram loaded, 0 if new
}

UInt_t HistManager::AddH2F(TFile* file, TH1*& h2, const char* name, const char* title, Int_t nbinsx, Double_t xlow, Double_t xhigh, Int_t nbinsy, Double_t* ybins)
{
  UInt_t status = 0;
  if( InitialCheck(h2,name) ){ return status; }

  if( file!=0 ){ h2 = (TH1*)file->Get(name); }
  if( h2==0 ){
    //h2 = (TH2*)FindObject(name);
    //if( h2==0 ){
    h2 = new TH2F(name,title, nbinsx,xlow,xhigh, nbinsy,ybins);
    h2->Sumw2();
    //}
    //else{ return 1; }
  }
  else{
    h2->SetBit(22);
    ++status;
  }
  h2->SetTitle(title);
  Add(h2,name);
  return status;//1 if histogram loaded, 0 if new
}

UInt_t HistManager::AddH2F(TFile* file, TH1*& h, const char* name, const char* title, Int_t nbinsx, Double_t xlow, Double_t xhigh, const Int_t nbinsy, const Double_t* ybins)
{
  Double_t nonconst_ybins[nbinsy+1];
  for( int i=0; i<nbinsy+1; ++i ){
    nonconst_ybins[i] = ybins[i];
  }
  return AddH2F(file,h,name,title,nbinsx,xlow,xhigh,nbinsy,nonconst_ybins);
}

UInt_t HistManager::AddH2FArr(TFile* file, TObjArray*& arr, UInt_t nobjs, const char* name, const char* title, Int_t nbinsx, Double_t xlow, Double_t xhigh, Int_t nbinsy, Double_t ylow, Double_t yhigh)
{
  if( arr==0 ){ return 0; }
  UInt_t status = 0;
  for( UInt_t iobj = 0; iobj<nobjs; ++iobj ){
    TH1* h2 = 0;
    bool inarray = false;
    if( ((int)iobj)<arr->GetEntriesFast() && arr->GetEntriesFast()>0 ){ //This means the array already has some object at iobj
      h2 = (TH1*)arr->UncheckedAt(iobj);
      if( h2!=0 ){
	inarray = true;
	//Here I am using bit 22 since that is unused by TH1
	//std::cout << " + |HistManager::AddH2FArr|"<<h2->GetName()<<"|kND:"<<h2->TestBit(kNotDeleted) <<"|22:"<< h2->TestBit(22) << std::endl;
	if( h2->TestBit(22) ){ arr->RemoveAt(iobj); delete h2; h2=0; } //It is true that the file was loaded so safe to change pointer without delete
	else{ ++status; continue; } //Object was added to array as new so just keep going
      }
    }
    std::stringstream ss_name;
    ss_name << name << "_" << iobj;
    if( InitialCheck(h2,ss_name.str()) ){ return status; }
    if( file!=0 ){ h2 = (TH2F*)file->Get(ss_name.str().c_str()); }
    if( h2==0 ){
      h2 = new TH2F(ss_name.str().c_str(),title,nbinsx,xlow,xhigh, nbinsy,ylow,yhigh);
	h2->Sumw2();
    }
    else{
      h2->SetBit(22);
      ++status;
    }
    h2->SetTitle(title);
    if( inarray ){ arr->AddAt(h2,iobj); }
    else{ arr->Add(h2); }
    Add(h2,ss_name.str().c_str());
  }
  return status; 
}

UInt_t HistManager::AddH3F(TFile* file,  TH1*& h3, const char* name, const char* title,  Int_t nbinsx, Double_t* xbins, Int_t nbinsy, Double_t* ybins, Int_t nbinsz, Double_t *zbins)
{
  UInt_t status = 0;
  if( InitialCheck(h3,name) ){ return status; }

  if( file!=0 ){ h3 = (TH1*)file->Get(name); }
  if( h3==0 ){
    //h3 = (TH2*)FindObject(name);
    //if( h2==0 ){

    h3 = new TH3F(name,title, nbinsx,xbins, nbinsy,ybins, nbinsz, zbins);
    h3->Sumw2();
    //}
    //else{ return 1; }
  }
  else{
    h3->SetBit(22);
    ++status;
  }
  h3->SetTitle(title);
  Add(h3,name);
  return status;//1 if histogram loaded, 0 if new
}

UInt_t HistManager::AddH3F(TFile* file,  TH1*& h, const char* name, const char* title,  Int_t nbinsx, Double_t* xbins, const Int_t nbinsy, const Double_t* ybins, Int_t nbinsz, Double_t *zbins)
{
  Double_t nonconst_ybins[nbinsy+1];
  for( int i=0; i<nbinsy+1; ++i ){
    nonconst_ybins[i] = ybins[i];
  }
  return AddH3F(file,h,name,title,nbinsx,xbins,nbinsy,nonconst_ybins,nbinsz,zbins);
}

void HistManager::Draw(Option_t* option)
{
  std::string opt(option);
  //if( opt.Length()==0 ){ TCollection::Draw(); return; } Maybe not a good idea since TCollection should draw them all with some option

  TString histname("");
  TString histopt("");
  std::size_t firstcolon = opt.find(";");
  if( firstcolon!=std::string::npos ){
    histname = opt.substr(0,firstcolon);
    histopt = opt.substr(firstcolon+1);
  }
  else{
    histname = option;
  }
  TObject* h = FindObject(histname.Data());
  std::cout << "          +|histname:"<<histname << "|histopt:"<<histopt << std::endl;
  if( h==0 ){ std::cout << "HistManager::Draw - Warning:Could not find histogram '"<<histname<< "'" << std::endl; }
  else{ h->Draw(histopt.Data()); }

}

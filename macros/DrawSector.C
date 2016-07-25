struct Geom_t {
  Int_t Barrel;
  Int_t Layer;
  Int_t NoLadders;
  Int_t NoWafers;
};
const Int_t NoLayers = 7;
// Barrel, Layer  ladder wafer
static const Geom_t SvtSsdConfig[NoLayers] = 
{    {1,     1,      8,   4}, // even
     {1,     2,      8,   4}, // odd
     {2,     3,     12,   6}, // event
     {2,     4,     12,   6}, // odd
     {3,     5,     16,   7}, // even
     {3,     6,     16,   7}, // odd
     {4,     7,     20,  16}  // Ssd
};
static const Int_t ssdSector[20] = {// 100*sector + ladder
  101, 102,
  203, 204, 205, 206, 207, 208, 209,
  310, 311, 312, 
  413, 414, 415, 416, 417, 418, 419,
  120
};
//________________________________________________________________________________
TH2F* DrawSector(const Char_t *name="duvP", Int_t sector = 0, Int_t W = 0) {
  TH2F *hist = 0;
  TIter keys(gDirectory->GetListOfKeys());
  TKey *key = 0;
  while ((key = (TKey*) keys())) {
    TString Name(key->GetName());
    if (! Name.BeginsWith(name)) continue;
    Name.ReplaceAll(name,"");
    Int_t Id = Name.Atoi(); //  ladder + 100*(wafer + 10*layer)
    if (Id <= 1000 || Id > 9000) continue;
    Int_t ladder = Id%100;
    Int_t layer  = Id/1000;
    if (layer > 7) layer = 7;
    Int_t wafer = Id/100 - 10*layer;
    if (wafer != 0) continue;
    Int_t barrel = 0;
    Int_t sector = -1;
    if (layer < 7) {
      sector = 0;
      barrel = (layer+1)/2;
      if (ladder > SvtSsdConfig[layer-1].NoLadders/2) sector = 1; 
    } else         {sector = ssdSector[ladder-1]/100 + 1; barrel = 4;}
    if (sector < 0) continue;
    TH2F *h = (TH2F *) gDirectory->Get(key->GetName());
    if (! h) continue;
    if (! hist) {
      hist = (TH2F *) h->Clone(Form("%s_S%i",name,sector));
      TString Title(h->GetTitle());
      Title += Form(" sum for Sector %i",sector);
      hist->SetTitle(Title);
      cout << "Create " << hist->GetName() << "/" << hist->GetTitle() << " with nX/nY " << hist->GetNbinsX() << "/" << hist->GetNbinsY() << endl;
    } else {
      hist->Add(h);
      cout << "Add to " << hist->GetName() << "\t" << h->GetName() << " with nX/nY " << hist->GetNbinsX() << "/" << hist->GetNbinsY() << endl;
    }
  }
  return hist;
}

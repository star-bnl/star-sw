class St_ValiSet : public TDataSet{
public:
   TDatime fTimeMin;
   TDatime fTimeMax;
   TDataSet *fDat;
   TString fFla;
   Int_t  fMod;
   St_ValiSet(const char *name,TDataSet *parent);
   virtual ~St_ValiSet(){};
   virtual void ls(Int_t lev=1) const;
   virtual void ls(const Option_t *opt) const {TDataSet::ls(opt);}
           void Modified(int m=1){fMod=m;}
          Int_t IsModified(){return fMod;}
};

//_____________________________________________________________________________
St_ValiSet::St_ValiSet(const char *name,TDataSet *parent): TDataSet(name,parent)
{
  SetTitle(".Val");
  fFla = "ofl";
  fTimeMin.Set(kMaxTime,0);
  fTimeMax.Set(kMinTime,0);
  fDat =0;
  Modified(0);
}

//_____________________________________________________________________________
void St_ValiSet::ls(Int_t lev) const
{
  printf("  %s.Validity = %s ",GetName(),fTimeMin.AsString());
  printf(" <-> %s\n",     fTimeMax.AsString());
  if (fDat) printf("  Contains DataSet %s\n",fDat->GetName());
  TDataSet::ls(lev);
}


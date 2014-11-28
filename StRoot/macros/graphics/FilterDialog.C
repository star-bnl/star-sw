

class TGCompositeFrame;
class TGFrame;

TGFrame *GetFrame(const TGCompositeFrame *frame,Int_t id);
class FilterDialog {

private:

char **fNamVal;
float *fDefs;
float *fVals;
int    fNVals;
int    *fFlagg;
TList *fGarb;
TGMainFrame *fFrame;

public:
FilterDialog(const char *wName=0,const char **NamVal=0,const float *defs=0, float *vals=0,int *flagg=0);
~FilterDialog();

void     Update();
void     Show  ();
void     Reset ();	//Reset defaults
TGFrame *GetFrame(UInt_t id);
};
//_______________________________________________________________________________________

const char*  NamValQQ[]={
   "  RandomSelect = ",
   "  RxyMin       = ",
   "  RxyMax       = ",
   "  ZMin         = ",
   "  ZMax         = ",
   "  PhiMin       = ",
   "  PhiMax       = ",
   "  PtMin        = ",
   "  PtMax        = ",
   "  QMin         = ",
   "  QMax         = ", 
   0};

const float  DefsQQ[]={
   /*  RandomSelect=*/    1.00,
   /*  RxyMin      =*/    0.00,
   /*  RxyMax      =*/  200.00,
   /*  ZMin        =*/ -200.00,
   /*  ZMax        =*/ +200.00,
   /*  PhiMin      =*/ -180.00,
   /*  PhiMax      =*/ +180.00,
   /*  PtMin       =*/    0.00,
   /*  PtMax       =*/  999.00,
   /*  QMin        =*/   -1   ,
   /*  QMax        =*/   +1   ,
   0};

//_______________________________________________________________________________________
FilterDialog::FilterDialog(const char *wName,const char **NamVal,const float *defs, float *vals,int *flagg)
{
  printf("*** FilterDialog::FilterDialog STARTED ***\n");

  char cbuf[200];
  if (!wName) wName = "DefaultFilterDialog";
  fNamVal = NamVal;
  if(!fNamVal) fNamVal = NamValQQ;
  fDefs = defs;
  if (!fDefs) fDefs = DefsQQ;
  fVals = vals;
  if (!fVals) fVals = DefsQQ;
  fGarb = new TList;
  
  int nCol=2,nRow;
  for (nRow=0;fNamVal[nRow];nRow++){};
  fNVals = nRow;
  fFlagg = flagg;
  
    
  int PixInLetX=15;
  int PixInLetY=30;
  int LetInNam =12;
  int LetInVal =10;
  int nGridX   = 2;
  int nGridY   = 2;
  UInt_t uHintsNam = kLHintsCenterY | kLHintsExpandX;
  UInt_t uHintsVal = kLHintsCenterY | kLHintsExpandX;
  
//  TGTransientFrame *fFrame = new TGTransientFrame(gClient->GetRoot(),0
  fFrame = new TGMainFrame(gClient->GetRoot()
                          ,PixInLetX*(LetInNam+LetInVal)+10,PixInLetY*(nRow+2));

  fFrame->SetWindowName(wName);

  TGTableLayout  *lay = new TGTableLayout(fFrame,(nRow+2)*nGridY,(LetInNam+1+LetInVal+1)*nGridX);
  fFrame->SetLayoutManager(lay);

  TGTextView  *tw;
  TGTextEntry *te;
  TGTableLayoutHints *hi = 0;
  int jRowTop,jRowBot,jColLeft,jColRite;
  for (int iRow=0;iRow<nRow;iRow++) {

    tw = new TGTextView(fFrame,PixInLetX*LetInNam,PixInLetY,fNamVal[iRow]);
    tw->SetUniqueID(1000*(iRow+1)+1);
    jRowTop = iRow*nGridY; jRowBot = (iRow+1)*nGridY-1;
    jColLeft=0; jColRite = (LetInNam+1)*nGridX-1;
    hi = new TGTableLayoutHints(jColLeft,jColRite,jRowTop,jRowBot,uHintsNam);
    fFrame->AddFrame(tw,hi);

    sprintf(cbuf,"%+10g",fVals[iRow]);
    te = new TGTextEntry(fFrame,cbuf);
    te->SetUniqueID(1000*(iRow+1)+2);
    te->SetWidth( PixInLetX*LetInVal);
    te->SetHeight(PixInLetY);
    jColLeft =jColRite +1;jColRite = jColLeft+(LetInVal+1)*nGridX-1;
    hi = new TGTableLayoutHints(jColLeft,jColRite,jRowTop,jRowBot,uHintsVal);
    fFrame->AddFrame(te,hi);
    fGarb->Add(hi);
  }

  TGTextButton *bat =0;
  sprintf(cbuf,"((FilterDialog*)%p)->Update();",this);
  bat = new TGTextButton(fFrame," OK ",cbuf);
  jRowTop = nRow*nGridY; jRowBot = (nRow+1)*nGridY-1;
  jColLeft=0; jColRite = (LetInNam+1)*nGridX-1;
  hi = new TGTableLayoutHints(jColLeft,jColRite,jRowTop,jRowBot,uHintsNam);
  fFrame->AddFrame(bat,hi);
  fGarb->Add(hi);

 
  sprintf(cbuf,"((FilterDialog*)%p)->Reset();",this);
  bat = new TGTextButton(fFrame,"Default Values",cbuf);
  jColLeft =jColRite +1;jColRite = jColLeft+(LetInVal+1)*nGridX-1;
  hi = new TGTableLayoutHints(jColLeft,jColRite,jRowTop,jRowBot,uHintsVal);
  fFrame->AddFrame(bat,hi);
  fGarb->Add(hi);
  Show();
  printf("*** FilterDialog::FilterDialog FINISHED ***\n");
}  
//_______________________________________________________________________________________
FilterDialog::~FilterDialog()   
{
  printf("*** FilterDialog::~FilterDialog STARTED ***\n");
  TList *list =  fFrame->GetList();
  TListIter nextFrame(list);
  TGFrameElement *fe=0;
  while ((fe=(TGFrameElement*)nextFrame())) {delete fe->fFrame;}
  list->Delete();



  printf("*** After Destroy ***\n");
  delete fFrame; 
  fGarb->Delete(); 
  delete fGarb;
  printf("*** FilterDialog::~FilterDialog FINISHED ***\n");
}

//_______________________________________________________________________________________
void FilterDialog::Reset()   
{
  char cbuf[100];
  for (int irow=0; irow<fNVals; irow++) {
    int id = (irow+1)*1000+2;
    TGTextEntry *te = (TGTextEntry*)GetFrame(id);
    assert(te);
    sprintf(cbuf,"%+10g",fDefs[irow]);
    te->SetText(cbuf);
  }
  Show();
}
//_______________________________________________________________________________________
void FilterDialog::Update()   
{
  printf("*** FilterDialog::Update STARTED ***\n");
  const char *txt=0;
  for (int irow=0; irow<fNVals; irow++) {
    int id = (irow+1)*1000+2;
    TGTextEntry *te = (TGTextEntry*)GetFrame(id);
    assert(te);
    txt = te->GetText();
    float f = strtod(txt,0);
//    printf("f = %f\n",f);
    if (fVals) fVals[irow]=f;
  }
  Show();
  assert(fFlagg);
  fFlagg[0]=0;
  printf("*** FilterDialog::Update FINISHED ***\n");
}
//_______________________________________________________________________________________
void  FilterDialog::Show()
{ 
  fFrame->Layout();
  fFrame->MapSubwindows();
  fFrame->MapWindow();
}
//_______________________________________________________________________________________
TGFrame *FilterDialog::GetFrame(UInt_t id)
{
  return ::GetFrame(fFrame,id);
}
//_______________________________________________________________________________________
TGFrame *GetFrame(const TGCompositeFrame *frame,UInt_t id)
{
  TList *tl = frame->GetList();
  TListIter next(tl);
  TGFrameElement *el;
  while ((el = (TGFrameElement *)next())) {
    TGFrame *fr = el->fFrame;
    if (!fr) continue;
//    printf("%d -- %d\n",id,fr->GetUniqueID());
    if (fr->GetUniqueID()==id) break;
  }
  if (!el) return 0;
  return el->fFrame;
}



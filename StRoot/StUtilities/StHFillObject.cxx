// $Id: StHFillObject.cxx,v 1.3 1999/08/10 21:29:20 genevb Exp $
// $Log: StHFillObject.cxx,v $
// Revision 1.3  1999/08/10 21:29:20  genevb
// Use formulas, separate headers, use StMessMgr, spaces no longer separate
//
// Revision 1.2  1999/08/03 02:29:35  genevb
// Re-implemented using TMethodCall's
//
// Revision 1.1  1999/07/29 23:27:34  genevb
// Introduction of new class
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StHFillObject allows member functions to be histogrammed             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TROOT.h"
#include "StHFillObject.h"
#include "StHFillVars.h"
#include "TH1.h"
#include "TMethodCall.h"
#include "TDataMember.h"
#include "TList.h"
#include "TObjArray.h"
#include "TArrayI.h"
#include "TArrayD.h"
#include "TClass.h"
#include <string.h>
#include <iostream.h>
#include <stdlib.h>
#include "StMessMgr.h"
// Need for StHFillFormula::Eval
#include "TMath.h"
#include "TRandom.h"


static Char_t space = ' ';
static Char_t semic[] = ";";
static Char_t endol[] = "\n";
static Char_t nostr[] = "";

//*****************************************************************************
//
// StHFillVars
//
//_____________________________________________________________________________
StHFillVars::StHFillVars():
thisClass(0),
oldClass(0),
pubMembers(0),
pubMethods(0),
access(0),
calcs(0),
valPtrs(0),
obj(0) {
  memset(buffer,0,bufSize);
  memset(buffer1,0,bufSize);
  memset(formula,0,(nsMax*dimMax*sizeof(StHFillFormula*)));
  memset(histo,0,(nsMax*sizeof(TH1*)));
  memset(used,0,(nsMax*dimMax*sizeof(TObjArray*)));
  for (size_t j=0; j<nsMax; j++) {
    for (size_t i=0; i<dimMax; i++) {
      nused[j][i] = 0;
    }
  }
  used[0][0] = new TArrayI(0);
}
//_____________________________________________________________________________
StHFillVars::~StHFillVars() {
  size_t i,j;
  for (j=0; j<nsMax; j++) {
    for (i=0; i<dimMax; i++) {
      if (formula[j][i]) delete formula[j][i];
      if (used[j][i]) delete used[j][i];
    }
    if (access) {delete access; delete calcs; delete valPtrs;}
  }
}
//_____________________________________________________________________________
void StHFillVars::ClearBuffer() {
  memset(buffer,0,bufSize);
}
//_____________________________________________________________________________
void StHFillVars::ClearSets() {
  if (pubMethods) delete (pubMethods);
  pubMethods = thisClass->GetListOfAllPublicMethods();
  if (pubMembers) delete (pubMembers);
  pubMembers = thisClass->GetListOfAllPublicDataMembers();
  methMem = pubMethods->LastIndex();
  oldClass = thisClass;
  if (access) {delete access; delete calcs; delete valPtrs;}
  accSize = methMem + pubMembers->LastIndex();
  access = new TObjArray(accSize);
  calcs = new TArrayD(accSize);
  valPtrs = new TArrayI(accSize);
  for (size_t j=0; j<nsets; j++) {
     if (!(used[j][0])) {
      used[j][0] = new TArrayI(accSize);
    } else {
      used[j][0]->Set(accSize);
    }
  }
}
//_____________________________________________________________________________
void StHFillVars::ClearUsed() {
  for (size_t j=0; j<nsets; j++) {
    for (size_t i=0; i<dimMax; i++) {
      if (used[j][i]) used[j][i]->Reset();
      nused[j][i] = 0;
    }
    if (formula[j][dimMax1]) {
      delete formula[j][dimMax1];
      formula[j][dimMax1]=0;
    }
  }
}
//_____________________________________________________________________________
void StHFillVars::Setup(Option_t* option, Int_t hists) {
  ClearBuffer();
  strcpy(buffer,option);
  strcpy(buffer1,option);

  // Parse sets
  Char_t* tokenPtr = buffer1;
  Char_t* token = 0;
  if (strstr(tokenPtr,semic))
    token = semic;
  else if (strstr(tokenPtr,endol))
    token = endol;
  size_t j=1;
  opt[0] = tokenPtr;
  if (token) {
    tokenPtr = strtok(tokenPtr,token);
    tokenPtr = strtok(NULL,token);
    while (tokenPtr != NULL) {
      memset((--tokenPtr),0,1);
      opt[j++] = (++tokenPtr);
      tokenPtr = strtok(NULL,token);
    }
  }
  nsets = j;

  // Parse dimensions
  size_t i;
  ClearUsed();
  for (j=0; j<nsets; j++) {
    set = j;
    if (hists) {                        // First variable is histogram name
      Char_t* histName = strtok(opt[j],":");
      if (histName == NULL)
        histo[j] = 0;
      else
        while ((*histName)==space) histName++;
        Char_t* trailSpace=0;
        if ((trailSpace = strchr(histName,space)))
          memset(trailSpace,0,1);
        histo[j] = (TH1*) gROOT->FindObject(histName);
      tokenPtr = strtok(NULL,":");
    } else {
      tokenPtr = strtok(opt[j],":");
    }
    i=0;
    while (tokenPtr != NULL) {          // Loop over variables
      Char_t* cut = strchr(tokenPtr,'{');
      if (cut==tokenPtr) {                      // Cut selection
        cut = strrchr(++tokenPtr,'}');
        if (cut) {
          memset(cut,0,1);
          dim = dimMax1;
          if (formula[j][dimMax1]) delete formula[j][dimMax1];
          formula[j][dimMax1] = new StHFillFormula(tokenPtr);
        } else {
          gMessMgr->Error() << "StHFillObject: syntax error in cut selection, "
            << "no right bracket" << endm;
          if (formula[j][dimMax1]) {
            delete formula[j][dimMax1];
            formula[j][dimMax1]=0;
          }
        }
      } else {                                  // Formula
        dim = i;
        if (formula[j][i]) delete formula[j][i];
        formula[j][i] = new StHFillFormula(tokenPtr);
        i++;
      }
      if (i==dimMax1) break;
      tokenPtr = strtok(NULL,":");
    }
    dims[j] = i;
  }
}  
//_____________________________________________________________________________
void StHFillVars::GetValues(Int_t printIt) {
  // Get all the calculated variables needed
  for (Int_t k=0; k<accSize; k++) {
    Double_t temp_double=0.0;
    TMethodCall* meth = (TMethodCall*) access->At(k);
    if (meth) {
      if (valPtrs->At(k)) {
        Long_t temp_long;
        meth->Execute(obj,temp_long);
        temp_double = (Double_t) temp_long;
      } else {
        meth->Execute(obj,temp_double);
      }
    }
    calcs->AddAt(temp_double,k);
  }

  // Evaluate the formulas for each set and dimension
  for (size_t j=0; j<nsets; j++) {
    set = j;
    if (formula[j][dimMax1]) {
      dim = dimMax1;
      weight[j] = (Stat_t) formula[j][dimMax1]->Eval();
    } else {
      weight[j] = 1.0;
    }
    for (size_t i=0; i<dims[j]; i++) {
      dim = i;
      values[j][i] = (Float_t) formula[j][i]->Eval();
      if (printIt && weight[j]) {
        if (i)
          cout << " : ";
        else if (nsets>1)
          cout << (j+1) << ": ";
        cout.width(10);
        cout << values[j][i];
      }
    }
    if (printIt && weight[j]) cout << endl;
  }
}
//_____________________________________________________________________________
static StHFillVars& vars = * (new StHFillVars());



//*****************************************************************************
//
// StHFillFormula
//
//_____________________________________________________________________________
StHFillFormula::StHFillFormula(const char* expression) : TFormula() {
  Compile(expression);
}
//_____________________________________________________________________________
StHFillFormula::~StHFillFormula() {}
//_____________________________________________________________________________
Int_t StHFillFormula::DefinedVariable(TString& variable) {
  if (vars.thisClass != vars.oldClass)
    vars.ClearSets();
  if (!(vars.used[vars.set][vars.dim]))
    vars.used[vars.set][vars.dim] = new TArrayI(vars.accSize);

  Char_t* vary = new Char_t[variable.Length()];
  strcpy(vary,variable.Data());
  Int_t isPtr=0;

  Char_t* args = strchr(vary,'[');          // Check for an array
  if (args) {
    isPtr = 1;
    memset(args,0,1);
  }

  args = strchr(vary,'(');                  // Check for function arguments
  Char_t* endargs = strrchr(vary,')');
  if (args && endargs) {
    memset(args++,0,1);
    memset(endargs,0,1);
  } else {
    args = nostr;
  }

  TObject* varObj(0);                       // Find the member
  TObject* meObj(0);
  Int_t acc_code = -1;
  if ((varObj = vars.pubMethods->FindObject(vary))) {
    acc_code = vars.pubMethods->IndexOf(varObj);
    if (!(vars.access->At(acc_code))) {
      meObj = (TObject*) new TMethodCall(vars.thisClass,vary,args);
      vars.access->AddAt(meObj,acc_code);
    }
  } else if ((varObj = vars.pubMembers->FindObject(vary))) {
    acc_code = vars.methMem + vars.pubMembers->IndexOf(varObj);
    if (!(vars.access->At(acc_code))) {
      meObj = (TObject*) vars.thisClass->GetDataMember(vary)->GetterMethod();
      vars.access->AddAt(meObj,acc_code);
    }
  }
  delete [] vary;
  if (acc_code < 0) return -1;
  for (size_t k=0; k<vars.nused[vars.set][vars.dim]; k++) {
    if (acc_code == vars.used[vars.set][vars.dim]->At(k)) {
      fNval--;
      return k;
    }
  }
  vars.used[vars.set][vars.dim]->AddAt(acc_code,vars.nused[vars.set][vars.dim]);
  if ((isPtr) && !(vars.valPtrs->At(acc_code)))
    vars.valPtrs->AddAt(isPtr,acc_code);
  return vars.nused[vars.set][vars.dim]++;
}
//_____________________________________________________________________________
Double_t StHFillFormula::DefinedValue(Int_t code) {
  if (code < 0) return 0.0;
  return vars.calcs->At(vars.used[vars.set][vars.dim]->At(code));
}
//_____________________________________________________________________________
Double_t StHFillFormula::Eval()
{
//
// Redefine TFormula::Eval() (EvalPar()) correctly
//
  static const Int_t kMAXFOUND = 50;
  static const Int_t kMAXSTRINGFOUND = 10;
  
  Int_t i,j,pos,pos2,inter,inter2,int1,int2;
  Float_t aresult;
  Double_t tab[kMAXFOUND];
  char *tab2[kMAXSTRINGFOUND];
  Double_t param_calc[kMAXFOUND];
  Double_t dexp,intermede,intermede1,intermede2;
  char *string_calc[kMAXSTRINGFOUND];
  Int_t precalculated = 0;
  Int_t precalculated_str = 0;
  Double_t x[1];

  x[0] = 1.0;
  pos  = 0;
  pos2 = 0;
  for (i=0; i<fNoper; i++) {
    Int_t action = fOper[i];
//*-*- a variable
    if (action >= 110000) {
       pos++; tab[pos-1] = x[action-110000];
       continue;
    }
//*-*- a tree string
    if (action >= 105000) {
       if (!precalculated_str) {
          precalculated_str=1;
          for (j=0;j<fNstring;j++) string_calc[j]=DefinedString(j);
       }
       pos2++; tab2[pos2-1] = string_calc[action-105000];
       continue;
    }
//*-*- a tree variable
    if (action >= 100000) {
       if (!precalculated) {
          precalculated = 1;
          for(j=0;j<fNval;j++) param_calc[j]=DefinedValue(j);
       }
// ***************************************************************
// This is custom StHFillFormula code to deal with brackets after
// names of defined variables.
// ***************************************************************
// Original line:
//     pos++; tab[pos-1] = param_calc[action-100000];
// New code
       pos++;
       Char_t* bracket = strchr(fExpr[i],'[');
       if (bracket) {
          int arrayOffset = atoi(++bracket);
          Long_t paramPtrL = (Long_t) param_calc[action-100000];
          float* paramPtr = (float*) paramPtrL;
          tab[pos-1] = (Double_t) paramPtr[arrayOffset];
       } else {
          tab[pos-1] = param_calc[action-100000];
       }
// End new code
       continue;
    }
//*-*- String
    if (action == 80000) {
       pos2++; tab2[pos2-1] = (char*)fExpr[i].Data();
       continue;
    }
//*-*- numerical value
    if (action >= 50000) {
       pos++; tab[pos-1] = fConst[action-50000];
       continue;
    }
    if (action == 0) {
      pos++;
      sscanf((const char*)fExpr[i],"%g",&aresult);
      tab[pos-1] = aresult;
//*-*- basic operators and mathematical library
    } else if (action < 100) {
        switch(action) {
          case   1 : pos--; tab[pos-1] += tab[pos]; break;
          case   2 : pos--; tab[pos-1] -= tab[pos]; break;
          case   3 : pos--; tab[pos-1] *= tab[pos]; break;
          case   4 : if (tab[pos-1] == 0) {tab[pos-1] = 0;} //  division by 0
                     else { pos--; tab[pos-1] /= tab[pos]; }
                     break;
          case   5 : {pos--; int1=Int_t(tab[pos-1]); int2=Int_t(tab[pos]); tab[pos-1] = Double_t(int1%int2); break;}
          case  10 : tab[pos-1] = TMath::Cos(tab[pos-1]); break;
          case  11 : tab[pos-1] = TMath::Sin(tab[pos-1]); break;
          case  12 : if (TMath::Cos(tab[pos-1]) == 0) {tab[pos-1] = 0;} // { tangente indeterminee }
                     else tab[pos-1] = TMath::Tan(tab[pos-1]);
                     break;
          case  13 : if (TMath::Abs(tab[pos-1]) > 1) {tab[pos-1] = 0;} //  indetermination
                     else tab[pos-1] = TMath::ACos(tab[pos-1]);
                     break;
          case  14 : if (TMath::Abs(tab[pos-1]) > 1) {tab[pos-1] = 0;} //  indetermination
                     else tab[pos-1] = TMath::ASin(tab[pos-1]);
                     break;
          case  15 : tab[pos-1] = TMath::ATan(tab[pos-1]); break;
          case  70 : tab[pos-1] = TMath::CosH(tab[pos-1]); break;
          case  71 : tab[pos-1] = TMath::SinH(tab[pos-1]); break;
          case  72 : if (TMath::CosH(tab[pos-1]) == 0) {tab[pos-1] = 0;} // { tangente indeterminee }
                     else tab[pos-1] = TMath::TanH(tab[pos-1]);
                     break;
          case  73 : if (tab[pos-1] < 1) {tab[pos-1] = 0;} //  indetermination
                     else tab[pos-1] = TMath::ACosH(tab[pos-1]);
                     break;
          case  74 : tab[pos-1] = TMath::ASinH(tab[pos-1]); break;
          case  75 : if (TMath::Abs(tab[pos-1]) > 1) {tab[pos-1] = 0;} // indetermination
                     else tab[pos-1] = TMath::ATanH(tab[pos-1]); break;
          case  16 : pos--; tab[pos-1] = TMath::ATan2(tab[pos-1],tab[pos]); break;
          case  20 : pos--; tab[pos-1] = TMath::Power(tab[pos-1],tab[pos]); break;
          case  21 : tab[pos-1] = tab[pos-1]*tab[pos-1]; break;
          case  22 : tab[pos-1] = TMath::Sqrt(TMath::Abs(tab[pos-1])); break;
          case  23 : pos2 -= 2; pos++;if (strstr(tab2[pos2],tab2[pos2+1])) tab[pos-1]=1;
                            else tab[pos-1]=0; break;
          case  30 : if (tab[pos-1] > 0) tab[pos-1] = TMath::Log(tab[pos-1]);
                     else {tab[pos-1] = 0;} //{indetermination }
                     break;
          case  31 : dexp = tab[pos-1];
                     if (dexp < -70) {tab[pos-1] = 0; break;}
                     if (dexp >  70) {tab[pos-1] = TMath::Exp(70); break;}
                     tab[pos-1] = TMath::Exp(dexp); break;
          case  32 : if (tab[pos-1] > 0) tab[pos-1] = TMath::Log10(tab[pos-1]);
                     else {tab[pos-1] = 0;} //{indetermination }
                     break;
          case  40 : pos++; tab[pos-1] = TMath::ACos(-1); break;
          case  41 : tab[pos-1] = TMath::Abs(tab[pos-1]); break;
          case  42 : if (tab[pos-1] < 0) tab[pos-1] = -1; else tab[pos-1] = 1; break;
          case  50 : tab[pos-1] = gRandom->Rndm(1); break;
          case  60 : pos--; if (tab[pos-1]!=0 && tab[pos]!=0) tab[pos-1]=1;
                            else tab[pos-1]=0; break;
          case  61 : pos--; if (tab[pos-1]!=0 || tab[pos]!=0) tab[pos-1]=1;
                            else tab[pos-1]=0; break;
          case  62 : pos--; if (tab[pos-1] == tab[pos]) tab[pos-1]=1;
                            else tab[pos-1]=0; break;
          case  63 : pos--; if (tab[pos-1] != tab[pos]) tab[pos-1]=1;
                            else tab[pos-1]=0; break;
          case  64 : pos--; if (tab[pos-1] < tab[pos]) tab[pos-1]=1;
                            else tab[pos-1]=0; break;
          case  65 : pos--; if (tab[pos-1] > tab[pos]) tab[pos-1]=1;
                            else tab[pos-1]=0; break;
          case  66 : pos--; if (tab[pos-1]<=tab[pos]) tab[pos-1]=1;
                            else tab[pos-1]=0; break;
          case  67 : pos--; if (tab[pos-1]>=tab[pos]) tab[pos-1]=1;
                            else tab[pos-1]=0; break;
          case  68 : if (tab[pos-1]!=0) tab[pos-1] = 0; else tab[pos-1] = 1; break;
          case  76 : pos2 -= 2; pos++; if (!strcmp(tab2[pos2+1],tab2[pos2])) tab[pos-1]=1;
                            else tab[pos-1]=0; break;
          case  77 : pos2 -= 2; pos++;if (strcmp(tab2[pos2+1],tab2[pos2])) tab[pos-1]=1;
                            else tab[pos-1]=0; break;
          case  78 : pos--; tab[pos-1]= ((Int_t) tab[pos-1]) & ((Int_t) tab[pos]); break;
          case  79 : pos--; tab[pos-1]= ((Int_t) tab[pos-1]) | ((Int_t) tab[pos]); break;
       }
//*-*- Parameter substitution
    } else if (action > 100 && action < 200) {
          pos++;
          tab[pos-1] = fParams[action - 101];
//*-*- Polynomial
    } else if (action > 10000 && action < 50000) {
          pos++;
          tab[pos-1] = 0;intermede = 1;
          inter2= action/10000; //arrondit
          inter = action/100-100*inter2; //arrondit
          int1=action-inter2*10000-inter*100-1; // aucune simplification ! (sic)
          int2=inter2-1;
          for (j=0 ;j<inter+1;j++) {
              tab[pos-1] += intermede*fParams[j+int1];
              intermede *= x[int2];
          }
//*-*- expo or xexpo or yexpo or zexpo
    } else if (action > 1000 && action < 1500) {
          pos++;
          inter=action/100-10;
          int1=action-inter*100-1000;
          tab[pos-1] = TMath::Exp(fParams[int1-1]+fParams[int1]*x[inter]);
//*-*- xyexpo
    } else if (action > 1500 && action < 1600) {
          pos++;
          int1=action-1499;
          tab[pos-1] = TMath::Exp(fParams[int1-2]+fParams[int1-1]*x[0]+fParams[int1]*x[1]);
//*-*- gaus, xgaus, ygaus or zgaus
    } else if (action > 2000 && action < 2500) {
          pos++;
          inter=action/100-20;
          int1=action-inter*100;
          intermede2=Float_t((x[inter]-fParams[int1-2000])/fParams[int1-1999]);
          tab[pos-1] = fParams[int1-2001]*TMath::Exp(-0.5*intermede2*intermede2);
//*-*- xygaus
    } else if (action > 2500 && action < 2600) {
          pos++;
          intermede1=Float_t((x[0]-fParams[action-2500])/fParams[action-2499]);
          intermede2=Float_t((x[1]-fParams[action-2498])/fParams[action-2497]);
          tab[pos-1] = fParams[action-2501]*TMath::Exp(-0.5*(intermede1*intermede1+intermede2*intermede2));
//*-*- landau, xlandau, ylandau or zlandau
    } else if (action > 4000 && action < 4500) {
          pos++;
          inter=action/100-40;
          int1=action-inter*100;
          tab[pos-1] = fParams[int1-4001]*TMath::Landau(x[inter],fParams[int1-4000],fParams[int1-3999]);
//*-*- xylandau
    } else if (action > 4500 && action < 4600) {
          pos++;
          intermede1=TMath::Landau(x[0], fParams[action-4500], fParams[action-4499]);
          intermede2=TMath::Landau(x[1], fParams[action-4498], fParams[action-4497]);
          tab[pos-1] = fParams[action-4501]*intermede1*intermede2;
    }
  }
  Double_t result = tab[0];
  return result;
}




//*****************************************************************************
//
// StHFillObject
//
//_____________________________________________________________________________
StHFillObject::StHFillObject() : TObject() {
}
//_____________________________________________________________________________
StHFillObject::~StHFillObject() {
}
//_____________________________________________________________________________
void StHFillObject::Reset() {
  vars.ClearBuffer();
}
//_____________________________________________________________________________
void StHFillObject::Draw(Option_t* option) {
  vars.thisClass = this->IsA();
  if (strcmp(vars.buffer,option)) vars.Setup(option,1);
  vars.obj = (TObject*) this;
  vars.GetValues(0);
  
  // Fill histograms
  for (size_t j=0; j<vars.nsets; j++) {
    if (!vars.histo[j]) return;
    switch (vars.dims[j]) {
      case 1:
        vars.histo[j]->Fill((Axis_t) vars.values[j][0],
                            vars.weight[j]);
        break;
      case 2:
        vars.histo[j]->Fill((Axis_t) vars.values[j][0],
                            (Axis_t) vars.values[j][1],
                            vars.weight[j]);
        break;
      case 3:
        vars.histo[j]->Fill((Axis_t) vars.values[j][0],
                            (Axis_t) vars.values[j][1],
                            (Axis_t) vars.values[j][2],
                            vars.weight[j]);
        break;
      default:
        gMessMgr->Error() << "StHFillObject: Dimensions not understood" << endm;
    }
  }
  return;
}
//_____________________________________________________________________________
void StHFillObject::Print(Option_t* option) {
  vars.thisClass = this->IsA();
  if (strcmp(vars.buffer,option)) vars.Setup(option,0);
  vars.obj = (TObject*) this;
  vars.GetValues(1);
  return;
}
//_____________________________________________________________________________
void StHFillObject::ls(Option_t* option) {
  if ((option) && !(strcmp(option,"Update")))
    Update();
  else
    TObject::ls(option);
}
//_____________________________________________________________________________
void StHFillObject::Update() {
  gMessMgr->Warning() << "Update() has not been implemented " <<
    "for the derived class!" << endm;
}
ClassImp(StHFillObject)

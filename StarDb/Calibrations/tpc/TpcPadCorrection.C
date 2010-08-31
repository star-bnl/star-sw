// $Id: TpcPadCorrection.C,v 1.1 2010/08/31 21:56:32 fisyak Exp $
TDataSet *CreateTable() { 
  if (!TClass::GetClass("St_TpcPadCorrection")) return 0;
  Int_t n = 1;
  St_TpcPadCorrection *tableSet = new St_TpcPadCorrection("TpcPadCorrection",n);
  return (TDataSet *)tableSet;
}
// $Log: TpcPadCorrection.C,v $
// Revision 1.1  2010/08/31 21:56:32  fisyak
// Default pad correction table
//

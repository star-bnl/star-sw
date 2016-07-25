#include "fee_map.h"
#ifndef __CINT__
#include "tables/St_pad_vs_fee_Table.h"
#include "tables/St_rdo_vs_fee_Table.h"
#include "tables/St_mezz_vs_fee_Table.h"
#include "tables/St_offset_vs_fee_Table.h"
#include "tables/St_row_vs_fee_Table.h"
#include "tables/St_fee_vs_pad_row_Table.h"
#include "TFile.h"
#else
class St_fee_vs_pad_row;
class St_mezz_vs_fee;
class St_offset_vs_fee;
class St_pad_vs_fee;
class St_rdo_vs_fee;
class St_row_vs_fee;
#endif
St_fee_vs_pad_row *Tfee_vs_pad_row = 0;
St_mezz_vs_fee *Tmezz_vs_fee = 0;
St_offset_vs_fee *Toffset_vs_fee = 0;
St_pad_vs_fee *Tpad_vs_fee = 0;
St_rdo_vs_fee *Trdo_vs_fee = 0;
St_row_vs_fee *Trow_vs_fee = 0;
const Int_t NRows =  45;
const Int_t NPads = 144;
const Int_t noFee = 182;
const Int_t noPin =  32;
unsigned short fee_vs_pad_row[NRows][NPads];

void MakeDaqTables() {
#ifdef __CINT__
  gSystem->Load("libStDb_Tables");
#endif
  TFile *f = 0;
  Trow_vs_fee = new St_row_vs_fee("row_vs_fee",noFee);
  Trow_vs_fee->Adopt(noFee,&row_vs_fee);
  Trow_vs_fee->Print(0,5);
  f = new TFile("row_vs_fee.root","recreate");
  Trow_vs_fee->Write();
  delete f;
  Tpad_vs_fee = new St_pad_vs_fee("pad_vs_fee",noFee);
  Tpad_vs_fee->Adopt(noFee,&pad_vs_fee);
  Tpad_vs_fee->Print(0,5);
  f = new TFile("pad_vs_fee.root","recreate");
  Tpad_vs_fee->Write();
  delete f;
  Trdo_vs_fee = new St_rdo_vs_fee("rdo_vs_fee",noFee);
  Trdo_vs_fee->Adopt(noFee,&rdo_vs_fee);
  Trdo_vs_fee->Print(0,5);
  f = new TFile("rdo_vs_fee.root","recreate");
  Trdo_vs_fee->Write();
  delete f;
  Tmezz_vs_fee = new St_mezz_vs_fee("mezz_vs_fee",noFee);
  Tmezz_vs_fee->Adopt(noFee,&mezz_vs_fee);
  Tmezz_vs_fee->Print(0,5);
  f = new TFile("mezz_vs_fee.root","recreate");
  Tmezz_vs_fee->Write();
  delete f;
  Toffset_vs_fee = new St_offset_vs_fee("offset_vs_fee",noFee);
  Toffset_vs_fee->Adopt(noFee,&offset_vs_fee);
  Toffset_vs_fee->Print(0,5);
  f = new TFile("offset_vs_fee.root","recreate");
  Toffset_vs_fee->Write();
  delete f;
#if 0
  for (int row = 1; row <= NRows; row++) {
    for (int pad = 1; pad <= NPads; pad++) {
      int fee = -1;
      for (int iFee=0;iFee<noFee;iFee++) {
	for (int pin=0;pin<noPin;pin++) {
	  if (row_vs_fee[iFee][pin] == row &&
	      pad_vs_fee[iFee][pin] == pad) {
	    fee = iFee;
	    fee_vs_pad_row[row-1][pad-1] = fee;
	    //	    pin = pin;
	    //	rdo = rdo_vs_fee[iFee][pin];
	    goto BREAK;
	  }
	}
      }
    BREAK:
      printf("row %i pad %i => fee %i\n", row, pad, fee); 
    }
  }
  Tfee_vs_pad_row = new St_fee_vs_pad_row("fee_vs_pad_row",45);
  Tfee_vs_pad_row->Adopt(45,fee_vs_pad_row);
  Tfee_vs_pad_row->Print(0,5);
  f = new TFile("fee_vs_pad_row.root","recreate");
  Tfee_vs_pad_row->Write();
  delete f;
#endif
}

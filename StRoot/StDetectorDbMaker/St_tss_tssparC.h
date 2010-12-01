#ifndef St_tss_tssparC_h
#define St_tss_tssparC_h

#include "TChair.h"
#include "tables/St_tss_tsspar_Table.h"
class St_tss_tssparC : public TChair {
 public:
  static St_tss_tssparC* 	instance();
  tss_tsspar_st 	*Struct(Int_t i = 0) 	{return ((St_tss_tsspar*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	{return GetNRows();}
  Char_t* 	fileout(Int_t i = 0) 	{return Struct(i)->fileout;}
  Int_t 	dynam(Int_t i = 0) 	{return Struct(i)->dynam;}
  Int_t 	format(Int_t i = 0) 	{return Struct(i)->format;}
  Int_t 	max_itime(Int_t i = 0) 	{return Struct(i)->max_itime;}
  Int_t 	max_pads(Int_t i = 0) 	{return Struct(i)->max_pads;}
  Int_t 	max_row(Int_t i = 0) 	{return Struct(i)->max_row;}
  Int_t 	max_sect(Int_t i = 0) 	{return Struct(i)->max_sect;}
  Int_t 	min_itime(Int_t i = 0) 	{return Struct(i)->min_itime;}
  Int_t 	min_pads(Int_t i = 0) 	{return Struct(i)->min_pads;}
  Int_t 	min_row(Int_t i = 0) 	{return Struct(i)->min_row;}
  Int_t 	min_sect(Int_t i = 0) 	{return Struct(i)->min_sect;}
  Int_t 	mode(Int_t i = 0) 	{return Struct(i)->mode;}
  Int_t 	nele_laser(Int_t i = 0) {return Struct(i)->nele_laser;}
  Int_t 	ngain(Int_t i = 0) 	{return Struct(i)->ngain;}
  Int_t 	nseg(Int_t i = 0) 	{return Struct(i)->nseg;}
  Int_t 	ntime(Int_t i = 0) 	{return Struct(i)->ntime;}
  Int_t 	printout(Int_t i = 0) 	{return Struct(i)->printout;}
  Int_t 	tpc_half(Int_t i = 0) 	{return Struct(i)->tpc_half;}
  Int_t 	reset(Int_t i = 0) 	{return Struct(i)->reset;}
  Float_t 	ave_ion_pot(Int_t i = 0){return Struct(i)->ave_ion_pot;}
  Float_t 	bfield(Int_t i = 0) 	{return Struct(i)->bfield;}
  Float_t 	c_test(Int_t i = 0) 	{return Struct(i)->c_test;}
  Float_t 	diff_long(Int_t i = 0) 	{return Struct(i)->diff_long;}
  Float_t 	diff_trans(Int_t i = 0) {return Struct(i)->diff_trans;}
  Float_t 	gain_in(Int_t i = 0)    {return Struct(i)->gain_in;}
  Float_t 	gain_in(Int_t sec, Int_t row) {return gain(sec,row);}
  Float_t 	gain_out(Int_t i = 0)   {return Struct(i)->gain_out;}
  Float_t 	gain_out(Int_t sec, Int_t row)  {return gain(sec,row);}
  Float_t 	gain(Int_t sec, Int_t row);
  Float_t 	prf_in(Int_t i = 0) 	{return Struct(i)->prf_in;}
  Float_t 	prf_out(Int_t i = 0) 	{return Struct(i)->prf_out;}
  Float_t 	sca_rms(Int_t i = 0) 	{return Struct(i)->sca_rms;}
  Float_t 	scale(Int_t i = 0) 	{return Struct(i)->scale;}
  Float_t 	step_size(Int_t i = 0) 	{return Struct(i)->step_size;}
  Float_t 	tau(Int_t i = 0) 	{return Struct(i)->tau;}
  Float_t 	threshold(Int_t i = 0) 	{return Struct(i)->threshold;}
  Float_t 	time_offset(Int_t i = 0){return Struct(i)->time_offset;}
  Float_t 	v_test(Int_t i = 0) 	{return Struct(i)->v_test;}
  Float_t 	white_rms(Int_t i = 0) 	{return Struct(i)->white_rms;}
  Float_t 	wire_coupling_in(Int_t i = 0) 	{return Struct(i)->wire_coupling_in;}
  Float_t 	wire_coupling_out(Int_t i = 0) 	{return Struct(i)->wire_coupling_out;}
  Float_t 	x_laser(Int_t i = 0) 	{return Struct(i)->x_laser;}
  Float_t 	y_laser(Int_t i = 0) 	{return Struct(i)->y_laser;}
  Float_t 	z_laser(Int_t i = 0) 	{return Struct(i)->z_laser;}
 protected:
  St_tss_tssparC(St_tss_tsspar *table=0) : TChair(table) {}
  virtual ~St_tss_tssparC() {fgInstance = 0;}
 private:
  static St_tss_tssparC* fgInstance;
  ClassDefChair(St_tss_tsspar, tss_tsspar_st )
  ClassDef(St_tss_tssparC,1) //C++ TChair for tss_tsspar table class
};
#endif

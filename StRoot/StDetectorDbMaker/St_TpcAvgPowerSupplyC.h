#ifndef St_TpcAvgPowerSupplyC_h
#define St_TpcAvgPowerSupplyC_h

#include "TChair.h"
#include "tables/St_TpcAvgPowerSupply_Table.h"
#include "St_TpcAvgCurrentC.h"
class St_TpcAvgPowerSupplyC : public TChair {
 public:
  static St_TpcAvgPowerSupplyC* 	instance();
  TpcAvgPowerSupply_st 	*Struct(Int_t i = 0) 	const {return ((St_TpcAvgPowerSupply*) Table())->GetTable()+i;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  Int_t 	run(Int_t i = 0) 	const {return Struct(i)->run;}
  Int_t 	start_time(Int_t i = 0) 	const {return Struct(i)->start_time;}
  Int_t 	stop_time(Int_t i = 0) 	const {return Struct(i)->stop_time;}
  Float_t* 	Current(Int_t i = 0) 	const {return Struct(i)->Current;}
  Float_t* 	Charge(Int_t i = 0) 	const {return Struct(i)->Charge;}
  Float_t* 	Voltage(Int_t i = 0) 	const {return Struct(i)->Voltage;}
  Float_t	voltagePadrow(Int_t sec = 1, Int_t padrow = 1) const; // sector=1..24 , padrow=1..100
  Bool_t        tripped(Int_t sec = 1, Int_t row = 1) const {return voltagePadrow(sec,row) < -100;}
  static Int_t  ChannelFromRow(Int_t sector, Int_t row) {return St_TpcAvgCurrentC::ChannelFromRow(sector,row);}
  static Int_t  ChannelFromSocket(Int_t socket) {return St_TpcAvgCurrentC::ChannelFromSocket(socket);}
  Float_t       AvCurrent(Int_t sector = 1, Int_t channel = 1) {
    return (sector > 0 && sector <= 24 && channel > 0 && channel <= 8) ? 
      Struct()->Current[8*(sector-1)+channel-1] : 
      0;}
  Float_t       AvCurrSocket(Int_t sector = 1, Int_t socket = 1) {return AvCurrent(sector,ChannelFromSocket(socket));}
  Float_t       AvCurrRow(Int_t sector = 1, Int_t row = 1) {return AvCurrent(sector,ChannelFromRow(sector,row));}
  Float_t       AcCharge(Int_t sector = 1, Int_t channel = 1) {
    return (sector > 0 && sector <= 24 && channel > 0 && channel <= 8) ? 
    Struct()->Charge[8*(sector-1)+channel-1] : 
    0;
  }
  Float_t       AcChargeSocket(Int_t sector = 1, Int_t socket = 1) {return AcCharge(sector,ChannelFromSocket(socket));}
  Float_t       AcChargeRow(Int_t sector = 1, Int_t row = 1) {return AcCharge(sector,ChannelFromRow(sector,row));}
  Float_t       AcChargeL(Int_t sector = 1, Int_t channel = 1); // C/cm
  Float_t       AcChargeRowL(Int_t sector = 1, Int_t row = 1) {return AcChargeL(sector,ChannelFromRow(sector,row));}
  Bool_t        livePadrow(Int_t sec = 1, Int_t padrow = 1) const { return voltagePadrow(sec,padrow) >  500;}
 protected:
  St_TpcAvgPowerSupplyC(St_TpcAvgPowerSupply *table=0) : TChair(table) {}
  virtual ~St_TpcAvgPowerSupplyC() {fgInstance = 0;}
 private:
  static St_TpcAvgPowerSupplyC* fgInstance;
  ClassDefChair(St_TpcAvgPowerSupply, TpcAvgPowerSupply_st )
  ClassDef(St_TpcAvgPowerSupplyC,1) //C++ TChair for TpcAvgPowerSupply table class
};
#endif

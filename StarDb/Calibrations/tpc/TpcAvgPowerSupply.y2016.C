TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_TpcAvgPowerSupply")) return 0;
  TpcAvgPowerSupply_st row;
  memset(row, 0, sizeof(TpcAvgPowerSupply_st));
  St_TpcAvgPowerSupply *tableSet = new St_TpcAvgPowerSupply("TpcAvgPowerSupply",1);
  Int_t NUM_CARDS      = 12;
  Int_t NUM_CHANNELS   =  8;
  // float tpcVoltages[2][NUM_CARDS][NUM_CHANNELS];
  for (Int_t io = 0; io < 2; io++) {
    for (Int_t module = 0; module < NUM_CARDS; module++) {
      for (Int_t channel = 0; channel < NUM_CHANNELS; channel++) {
	Int_t sec  = 1 + 2*module + channel/4;
	Int_t socket  = channel%4 + 4*io + 1;
	Int_t index = 8*(sec-1)+socket-1;
	//	cout << "sec " << sec << " socket " << socket << " index " << index << endl;
	if (! io) row.Voltage[index] = 1100;
	else      row.Voltage[index] = 1390;
      }
    }
  }
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}

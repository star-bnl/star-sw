TDataSet *CreateTable() { 
	// ------  Test whether this table share library was loaded ------
	if (!gROOT->GetClass("St_tpcCorrection")) return 0;
	Double_t rowsGain[45] = {
		0.06316,   0.06766,   0.06447,   0.06979,   0.06898,
		0.08678,   0.07354,   0.06084,   0.06127,   0.06316,
		0.06263,   0.05677,   0.05280,  -0.05655,  -0.05075,
		-0.05313,  -0.05299,  -0.04989,  -0.04747,  -0.05261,
		-0.05583,  -0.04799,  -0.04884,  -0.05278,  -0.05447,
		-0.05305,  -0.05113,  -0.05679,  -0.05601,  -0.05459,
		-0.05393,  -0.06555,   0.00000,  -0.06053,  -0.05936,
		-0.06534,  -0.07037,  -0.06146,  -0.05734,  -0.06542,
		-0.07352,  -0.07567,  -0.07654,  -0.08406,  -0.09589,
	};

#if 0
	Double_t parI[2] = { 3.85206e-03 - 1.65932e-03, -1.59469e-05};
	Double_t parO[2] = { 1.42389e-03 + 3.58961e-05, -6.87313e-05};
#else
	Double_t parI[2] = {0,0};
	Double_t parO[2] = {0,0};
#endif
	St_tpcCorrection *tableSet = new St_tpcCorrection("TpcRowQ",45);
	tpcCorrection_st row;
	memset(&row,0,tableSet->GetRowSize()); 
	row.nrows = 45;
	row.npar  =  2;
	for (Int_t i = 0; i < 45; i++) {
		row.idx  = i + 1;
           	row.a[0] = rowsGain[i] ;
		if (i < 13) {
			row.a[0] += parI[0];
			row.a[1]  = parI[1];
		} else {
			row.a[0] += parO[0];
			row.a[1]  = parO[1];
		}
		tableSet->AddAt(&row);
	}
	// ----------------- end of code ---------------
	return (TDataSet *)tableSet;
}

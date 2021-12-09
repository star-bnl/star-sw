void StvELossTrak::Test()
{

//Stores the following standard material constants in the data structure [] JMATE.
//Material	No.	A	Z	Density	Radiat L
class MyMat_t {public: const char* Material; int No; double A,Z,Density,RadLen;}; 
static MyMat_t myMat[] = {
{"Hydrogen",	1,	1.010,		 1.000,	0.071,		865.000},
{"Deuterium",	2,	2.010,		 1.000,	0.162,		757.000},
{"Helium",	3,	4.000,		 2.000,	0.125,		755.000},
{"Lithium",	4,	6.940,		 3.000,	0.534,		155.000},
{"Beryllium",	5,	9.010,		 4.000,	1.848,		35.300 },
{"Carbon",	6,	12.010,		 6.000,	2.265,		18.8   },
{"Nitrogen",	7,	14.010,		 7.000,	0.808,		44.500 },
{"Neon",	8,	20.180,		10.000,	1.207,		24.000 },
{"Aluminium",	9,	26.980,		13.000,	2.700,		8.900  },
{"Iron",	10,	55.850,		26.000,	7.870,		1.760  },
{"Copper",	11,	63.540,		29.000,	8.960,		1.430  },
{"Tungsten",	12,	183.850,	74.000,	19.300,		0.350  },
{"Lead",	13,	207.190,	82.000,	11.350,		0.560  },
{"Uranium",	14,	238.030,	92.000,	18.950,		0.320  },
{"Air",		15,	14.610,		 7.300,	1.205e-3,	30423  },
{"Vacuum",	16,	1e-16,		 1e-16,	1e-16,		1e16   }};

  for (int imat=0;imat<15;imat++) {
    TGeoMaterial *mat = new TGeoMaterial (myMat[imat].Material
                                         ,myMat[imat].A
		  		         ,myMat[imat].Z
				         ,myMat[imat].Density
				         ,myMat[imat].RadLen);
				       
    delete mat;
  }
}				       

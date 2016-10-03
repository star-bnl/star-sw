{
//look at tau decays
Setup::decay_particle=23;

Setup::debug_mode=false;

Setup::mass_power=2;
Setup::mass_scale_on=true;

// Setup histograms
int n_bins=60;
double default_min_bin=0.0;
double default_max_bin=10000.0; //Max in GeV
 if (Setup::mass_scale_on) default_max_bin=1.1;

Setup::SetHistogramDefaults(n_bins,default_min_bin,default_max_bin);


Setup::gen1_desc_1="TEST";
Setup::gen1_desc_2="Pythia + Tauola interface (1.212)";
Setup::gen1_desc_3="TEST";

//Setup::gen2_desc_1="TEST"
//Setup::gen2_desc_2="Pythia + Tauola C++ interface";
//Setup::gen2_desc_3="TEST"

if (Setup::stage==0)
    printf("Setup loaded from SETUP.C, ANALYSIS stage.\n");
else 
    printf("Setup loaded from SETUP.C, GENERATION stage %i.\n",Setup::stage);

 Setup::SuppressDecay(111); // suppress pi0 decays

};


#ifndef PHOTONANALYSISSETTINGS
#define PHOTONANALYSISSETTINGS

#include <TString.h>

class PhotonAnalysisSettings {
public:
    TString name;

    TString input_datapoints_dir;
    TString input_decaybackground_file;
    TString input_binwidth_file;
    TString input_pion_file;
    TString input_pioneff_file;
    TString input_gammaeff_file;
    TString input_nbareff_file;
    TString input_systematics_file;

    TString output_invmassplots_file;
    TString output_invmassplotseta_file;
    TString output_nbarcontam_file;
    TString output_pionhistograms_file;
    TString output_pioncanvases_file;
    TString output_pionxsec_file;
    TString output_pionxsecoverfit_file;
    TString output_pionxsecratio_file;
    TString output_inclphotonyield_file;
    TString output_gammaoverpion_file;
    TString output_inclphotonyieldcorr_file;
    TString output_nbarcont_file;
    TString output_gammadoubleratio_file;
    TString output_gammadirphoton_file;
};

#endif

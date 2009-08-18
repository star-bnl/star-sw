#if !defined(__CINT__) || defined(__MAKECINT__)

#include <TString.h>

#include <StEmcPool/StPhotonCommon/PhotonAnalysisSettings.h>

#include <StEmcPool/StPhotonAnalysis/PhotonAnalysisUtil.h>

#endif

void getSettings(PhotonAnalysisSettings &settings, const Char_t *name = "") {

    TString inputDir = "./data_photon/";
    TString outputDir = "./output_photon/";

    settings.name = name;

    inputDir += settings.name + "/";
    outputDir += settings.name + "/";

    settings.input_datapoints_dir         = "./data_photon/dataPoints/";

    settings.input_decaybackground_file   = inputDir + "gammaDecaySum.root";
    settings.input_binwidth_file          = inputDir + "binCorrections.root";
    settings.input_pion_file              = inputDir + "pi0.root";
    settings.input_pioneff_file           = inputDir + "pion_eff.root";
    settings.input_gammaeff_file          = inputDir + "gamma_eff.root";
    settings.input_nbareff_file           = inputDir + "antineutron_eff.root";
    settings.input_systematics_file       = inputDir + "systematics.root";

    settings.output_invmassplots_file     = outputDir + "invmassplots.ps";
    settings.output_invmassplotseta_file  = outputDir + "invmassplots2.ps";
    settings.output_nbarcontam_file       = outputDir + "nbar_contam.eps";
    settings.output_pionhistograms_file   = outputDir + "pion_histograms.root";
    settings.output_pioncanvases_file     = outputDir + "pion_canvases.eps";
    settings.output_pionxsec_file         = outputDir + "pion_xsec.eps";
    settings.output_pionxsecoverfit_file  = outputDir + "pion_xsecoverfit.eps";
    settings.output_pionxsecratio_file    = outputDir + "pion_xsecratio.eps";
    settings.output_inclphotonyield_file  = outputDir + "inclphotonyield.eps";
    settings.output_gammaoverpion_file    = outputDir + "gammaoverpion.eps";
    settings.output_inclphotonyieldcorr_file = outputDir + "inclphotonyieldcorr.eps";
    settings.output_nbarcont_file         = outputDir + "nbarcont.eps";
    settings.output_gammadoubleratio_file = outputDir + "gammadoubleratio.eps";
    settings.output_gammadirphoton_file   = outputDir + "gammadirphoton.eps";
}

void show_photon() {

    Bool_t show_pp2005 = false;
    Bool_t show_dAu2003 = true;

    if (show_pp2005) {
	PhotonAnalysisSettings s;
	getSettings(s, "pp05");
	getPhotonSpectrum(s);
    }
    if (show_dAu2003) {
	PhotonAnalysisSettings s;
	getSettings(s, "dAu");
	getPhotonSpectrum(s);
    }
}

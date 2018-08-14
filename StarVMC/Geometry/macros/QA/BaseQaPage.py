#
# BaseQA.py generates a web page displaying the results of the baseqa track evaluation
# code.  It creates a web page in multi-column format, with each column displaying the
# results of a seperate run of the package.
#
# You should first generate your QA plots, and store them in your web directory.  Your
# web directory is located here --
#
# /afs/rhic.bnl.gov/star/users/_USER_NAME_/WWW
#
# You'll want to setup a subdirectory for your study...
#
# mkdir /afs/rhic.bnl.gov/star/users/_USER_NAME_/WWW/NewGeometryStudy
#
# and subdirectories for each set of QA plots you want to display
#
# cd /afs/rhic.bnl.gov/star/users/_USER_NAME_/WWW/NewGeometryStudy
# mkdir QA1 QA2 QA3
# cd -
#
# After you run the baseqa codes, you'll want to setup the __main__ below to point to the
# QA directories, using relative paths.
# 


import datetime

_default_subsections = [ '---' ]
_figures = 0


 

class Contents:
    def __init__(self,name,title,comments=None):
        self.name     = name
        self.title    = title
        self.comments = comments
        self.abstract = None
        self.summary  = None
        self.sections = []
        self.datasets = []

        
        section  = self.addSection( "avgChi2nfp", "Average chi^2 vs no. fit points" )
        plot     = section.addPlot( "chi2_global_xy_nfp",  "ChiSqXYGl_zx_pfx.png", "Global track chi2 vs N fit" )
        plot     = section.addPlot( "chi2_global_xy_nbp",  "ChiSqXYGl_zy_pfx.png", "Global track chi2 vs N bad" )
        plot     = section.addPlot( "chi2_primary_xy_nfp", "ChiSqXYPr_zx_pfx.png", "Primary track chi2 vs N fit" )
        plot     = section.addPlot( "chi2_primary_xy_nbp", "ChiSqXYPr_zy_pfx.png", "Primary track chi2 vs N bad" )
        plot     = section.addPlot( "chi2_primvtx_nfp",    "ChiSqZPr_zx_pfx.png",  "chi2 to primary vertex vs N fit")
        plot     = section.addPlot( "chi2_primvtx_nbp",    "ChiSqZPr_zy_pfx.png",  "chi2 to primary vertex vs N bad")

        section  = self.addSection( "avgChi2all", "Average chi^2 for all" )
        plot     = section.addPlot( "chi2_global_zx_eta",  "AllChiSqXYGl_zx_pfx.png", "Global track chi2 vs &eta;" )
        plot     = section.addPlot( "chi2_global_zy_pt" ,  "AllChiSqXYGl_zy_pfx.png", "Global track chi2 vs pT" )
        plot     = section.addPlot( "chi2_global_zx_phi",  "AllChiSqXYGl_phi_zx_pfx.png", "Global track chi2 vs &phi;" )
        plot     = section.addPlot( "chi2_primary_zx_eta", "AllChiSqXYPr_zx_pfx.png", "Primary track chi2 vs &eta;" )
        plot     = section.addPlot( "chi2_primary_zy_pt" , "AllChiSqXYPr_zy_pfx.png", "Primary track chi2 vs pT" )
        plot     = section.addPlot( "chi2_primary_zx_phi", "AllChiSqXYPr_phi_zx_pfx.png", "Primary track chi2 vs &eta;" )
        plot     = section.addPlot( "chi2_primvtx_zx_eta", "AllChiSqZPr_zx_pfx.png",  "chi2 to primary vertex vs &phi;" )
        plot     = section.addPlot( "chi2_primvtx_zy_pt",  "AllChiSqZPr_zy_pfx.png",  "chi2 to primary vertex vs pT" )
        plot     = section.addPlot( "chi2_primvtx_zx_phi", "AllChiSqZPr_phi_zx_pfx.png",  "chi2 to primary vertex vs &phi;" )

        section  = self.addSection( "piChi2all", "Average chi^2 for pion" )
        plot     = section.addPlot( "pichi2_global_zx_eta",  "piChiSqXYGl_zx_pfx.png", "Global track chi2 vs &eta;" )
        plot     = section.addPlot( "pichi2_global_zy_pt" ,  "piChiSqXYGl_zy_pfx.png", "Global track chi2 vs pT" )
        plot     = section.addPlot( "pichi2_global_zx_phi",  "piChiSqXYGl_phi_zx_pfx.png", "Global track chi2 vs &phi;" )
        plot     = section.addPlot( "pichi2_primary_zx_eta", "piChiSqXYPr_zx_pfx.png", "Primary track chi2 vs &eta;" )
        plot     = section.addPlot( "pichi2_primary_zy_pt" , "piChiSqXYPr_zy_pfx.png", "Primary track chi2 vs pT" )
        plot     = section.addPlot( "pichi2_primary_zx_phi", "piChiSqXYPr_phi_zx_pfx.png", "Primary track chi2 vs &phi;" )
        plot     = section.addPlot( "pichi2_primvtx_zx_eta", "piChiSqZPr_zx_pfx.png",  "chi2 to primary vertex vs &phi;" )
        plot     = section.addPlot( "pichi2_primvtx_zy_pt",  "piChiSqZPr_zy_pfx.png",  "chi2 to primary vertex vs pT" )
        plot     = section.addPlot( "pichi2_primvtx_zx_phi", "piChiSqZPr_phi_zx_pfx.png",  "chi2 to primary vertex vs &phi;" )

        section  = self.addSection("glDevNfp","Global Deviation vs no. fit points (all or bad)")
        plot     = section.addPlot("diff_dcaxy_global_nfit", "dDcaXYGl_zx_1.png", "Global tracks fit difference in DCAxy vs N fit" )
        plot     = section.addPlot("diff_dcaxy_global_nbad", "dDcaXYGl_zy_1.png", "Global tracks fit difference in DCAxy vs N bad" )        
        plot     = section.addPlot("diff_dcaz_global_nfit", "dDcaZGl_zx_1.png", "Global tracks fit difference in DCAz vs N fit" )
        plot     = section.addPlot("diff_dcaz_global_nbad", "dDcaZGl_zy_1.png", "Global tracks fit difference in DCAz vs N bad" )
        plot     = section.addPlot("diff_psi_global_nfit", "dPsiGl_zx_1.png", "Global tracks fit difference in &Psi; vs N fit" )
        plot     = section.addPlot("diff_psi_global_nbad", "dPsiGl_zy_1.png", "Global tracks fit difference in &Psi; vs N bad" )        
        plot     = section.addPlot("diff_pti_global_nfit", "dPtiGl_zx_1.png", "Global tracks fit difference in q/pT; vs N fit" )
        plot     = section.addPlot("diff_pti_global_nbad", "dPtiGl_zy_1.png", "Global tracks fit difference in q/pT; vs N bad" )        
        plot     = section.addPlot("diff_ptr_global_nfit", "dPtiRGl_zx_1.png", "Global tracks fit difference in relative q/pT; vs N fit" )
        plot     = section.addPlot("diff_ptr_global_nbad", "dPtiRGl_zy_1.png", "Global tracks fit difference in relative q/pT; vs N bad" )        
        plot     = section.addPlot("diff_tanl_global_nfit", "dTanLGl_zx_1.png", "Global tracks fit difference in TAN(&lambda;) vs N fit" )
        plot     = section.addPlot("diff_tanl_global_nbad", "dTanLGl_zy_1.png", "Global tracks fit difference in TAN(&lambda;) vs N bad" )

        section  = self.addSection("glDevAll","Global Deviation for All")
        plot     = section.addPlot("diff_dcaxy_global_eta", "AlldDcaXYGl_zx_1.png", "Global tracks fit difference in DCAxy vs &eta;");
        plot     = section.addPlot("diff_dcaxy_global_pt",  "AlldDcaXYGl_zy_1.png", "Global tracks fit difference in DCAxy vs pT");
        plot     = section.addPlot("diff_dcaxy_global_phi", "AlldDcaXYGl_phi_zx_1.png", "Global tracks fit difference in DCAxy vs &phi;");
        plot     = section.addPlot("diff_dcaz_global_eta", "AlldDcaZGl_zx_1.png", "Global tracks fit difference in DCAz vs &eta;");
        plot     = section.addPlot("diff_dcaz_global_pt",  "AlldDcaZGl_zy_1.png", "Global tracks fit difference in DCAz vs pT");
        plot     = section.addPlot("diff_dcaz_global_phi", "AlldDcaZGl_phi_zx_1.png", "Global tracks fit difference in DCAz vs &phi;");
        plot     = section.addPlot("diff_dpsi_global_eta", "AlldPsiGl_zx_1.png", "Global tracks fit difference in &Psi; vs &eta;");
        plot     = section.addPlot("diff_dpsi_global_pt",  "AlldPsiGl_zy_1.png", "Global tracks fit difference in &Psi; vs pT");
        plot     = section.addPlot("diff_dpsi_global_phi", "AlldPsiGl_phi_zx_1.png", "Global tracks fit difference in &Psi; vs &phi;");
        plot     = section.addPlot("diff_pti_global_eta", "AlldPtiGl_zx_1.png", "Global tracks fit difference in q/pT vs &eta;");
        plot     = section.addPlot("diff_pti_global_pt",  "AlldPtiGl_zy_1.png", "Global tracks fit difference in q/pT vs pT");
        plot     = section.addPlot("diff_pti_global_phi", "AlldPtiGl_phi_zx_1.png", "Global tracks fit difference in q/pT vs &phi;");
        plot     = section.addPlot("diff_ptr_global_eta", "AlldPtiRGl_zx_1.png", "Global tracks fit difference in relative q/pT vs &eta;");
        plot     = section.addPlot("diff_ptr_global_pt",  "AlldPtiRGl_zy_1.png", "Global tracks fit difference in relative q/pT vs pT");
        plot     = section.addPlot("diff_ptr_global_phi", "AlldPtiRGl_phi_zx_1.png", "Global tracks fit difference in relative q/pT vs &phi;");
        plot     = section.addPlot("diff_tanl_global_eta", "AlldTanLGl_zx_1.png", "Global tracks fit difference in TAN(&lambda;) vs &eta;");
        plot     = section.addPlot("diff_tanl_global_pt",  "AlldTanLGl_zy_1.png", "Global tracks fit difference in TAN(&lambda;) vs pT");
        plot     = section.addPlot("diff_tanl_global_phi", "AlldTanLGl_phi_zx_1.png", "Global tracks fit difference in TAN(&lambda;) vs &phi;");

        section  = self.addSection("glDevPion","Global Deviation for Pion")
        plot     = section.addPlot("pidiff_dcaxy_global_eta", "pidDcaXYGl_zx_1.png", "Global tracks fit difference in DCAxy vs &eta;");
        plot     = section.addPlot("pidiff_dcaxy_global_pt",  "pidDcaXYGl_zy_1.png", "Global tracks fit difference in DCAxy vs pT");
        plot     = section.addPlot("pidiff_dcaxy_global_phi", "pidDcaXYGl_phi_zx_1.png", "Global tracks fit difference in DCAxy vs &phi;");
        plot     = section.addPlot("pidiff_dcaz_global_eta", "pidDcaZGl_zx_1.png", "Global tracks fit difference in DCAz vs &eta;");
        plot     = section.addPlot("pidiff_dcaz_global_pt",  "pidDcaZGl_zy_1.png", "Global tracks fit difference in DCAz vs pT");
        plot     = section.addPlot("pidiff_dcaz_global_phi", "pidDcaZGl_phi_zx_1.png", "Global tracks fit difference in DCAz vs &phi;");
        plot     = section.addPlot("pidiff_dpsi_global_eta", "pidPsiGl_zx_1.png", "Global tracks fit difference in &Psi; vs &eta;");
        plot     = section.addPlot("pidiff_dpsi_global_pt",  "pidPsiGl_zy_1.png", "Global tracks fit difference in &Psi; vs pT");
        plot     = section.addPlot("pidiff_dpsi_global_phi", "pidPsiGl_phi_zx_1.png", "Global tracks fit difference in &Psi; vs &phi;");
        plot     = section.addPlot("pidiff_pti_global_eta", "pidPtiGl_zx_1.png", "Global tracks fit difference in q/pT vs &eta;");
        plot     = section.addPlot("pidiff_pti_global_pt",  "pidPtiGl_zy_1.png", "Global tracks fit difference in q/pT vs pT");
        plot     = section.addPlot("pidiff_pti_global_phi", "pidPtiGl_phi_zx_1.png", "Global tracks fit difference in q/pT vs &phi;");
        plot     = section.addPlot("pidiff_ptr_global_eta", "pidPtiRGl_zx_1.png", "Global tracks fit difference in relative q/pT vs &eta;");
        plot     = section.addPlot("pidiff_ptr_global_pt",  "pidPtiRGl_zy_1.png", "Global tracks fit difference in relative q/pT vs pT");
        plot     = section.addPlot("pidiff_ptr_global_phi", "pidPtiRGl_phi_zx_1.png", "Global tracks fit difference in relative q/pT vs &phi;");
        plot     = section.addPlot("pidiff_tanl_global_eta", "pidTanLGl_zx_1.png", "Global tracks fit difference in TAN(&lambda;) vs &eta;");
        plot     = section.addPlot("pidiff_tanl_global_pt",  "pidTanLGl_zy_1.png", "Global tracks fit difference in TAN(&lambda;) vs pT");
        plot     = section.addPlot("pidiff_tanl_global_phi", "pidTanLGl_phi_zx_1.png", "Global tracks fit difference in TAN(&lambda;) vs &phi;");

        section  = self.addSection("glPullsNfp","Global Pulls vs. no. fit points (all or bad)")
        plot     = section.addPlot("pull_dcaxy_global_eta", "pDcaXYGl_zx_1.png", "Global tracks pull in DCAxy vs &eta;");
        plot     = section.addPlot("pull_dcaxy_global_pt",  "pDcaXYGl_zy_1.png", "Global tracks pull in DCAxy vs pT");
        #plot     = section.addPlot("pull_dcaxy_global_phi", "pDcaXYGl_phi_zx_1.png", "Global tracks pull in DCAxy vs &phi;");
        plot     = section.addPlot("pull_dcaz_global_eta", "pDcaZGl_zx_1.png", "Global tracks pull in DCAz vs &eta;");
        plot     = section.addPlot("pull_dcaz_global_pt",  "pDcaZGl_zy_1.png", "Global tracks pull in DCAz vs pT");
        #plot     = section.addPlot("pull_dcaz_global_phi", "pDcaZGl_phi_zx_1.png", "Global tracks pull in DCAz vs &phi;");
        plot     = section.addPlot("pull_dpsi_global_eta", "pPsiGl_zx_1.png", "Global tracks pull in &Psi; vs &eta;");
        plot     = section.addPlot("pull_dpsi_global_pt",  "pPsiGl_zy_1.png", "Global tracks pull in &Psi; vs pT");
        #plot     = section.addPlot("pull_dpsi_global_phi", "pPsiGl_phi_zx_1.png", "Global tracks pull in &Psi; vs &phi;");
        plot     = section.addPlot("pull_pti_global_eta", "pPtiGl_zx_1.png", "Global tracks pull in q/pT vs &eta;");
        plot     = section.addPlot("pull_pti_global_pt",  "pPtiGl_zy_1.png", "Global tracks pull in q/pT vs pT");
        #plot     = section.addPlot("pull_pti_global_phi", "pPtiGl_phi_zx_1.png", "Global tracks pull in q/pT vs &phi;");
        plot     = section.addPlot("pull_ptr_global_eta", "pPtiRGl_zx_1.png", "Global tracks pull in relative q/pT vs &eta;");
        plot     = section.addPlot("pull_ptr_global_pt",  "pPtiRGl_zy_1.png", "Global tracks pull in relative q/pT vs pT");
        #plot     = section.addPlot("pull_ptr_global_phi", "pPtiRGl_phi_zx_1.png", "Global tracks pull in relative q/pT vs &phi;");
        plot     = section.addPlot("pull_tanl_global_eta", "pTanLGl_zx_1.png", "Global tracks pull in TAN(&lambda;) vs &eta;");
        plot     = section.addPlot("pull_tanl_global_pt",  "pTanLGl_zy_1.png", "Global tracks pull in TAN(&lambda;) vs pT");
        #plot     = section.addPlot("pull_tanl_global_phi", "pTanLGl_phi_zx_1.png", "Global tracks pull in TAN(&lambda;) vs &phi;");

        section  = self.addSection("glPullsAll","Global Pulls for All")
        plot     = section.addPlot("pull_dcaxy_global_eta", "AllpDcaXYGl_zx_1.png", "Global tracks fit pull in DCAxy vs &eta;");
        plot     = section.addPlot("pull_dcaxy_global_pt",  "AllpDcaXYGl_zy_1.png", "Global tracks fit pull in DCAxy vs pT");
        #plot     = section.addPlot("pull_dcaxy_global_phi", "AllpDcaXYGl_phi_zx_1.png", "Global tracks fit pull in DCAxy vs &phi;");
        plot     = section.addPlot("pull_dcaz_global_eta", "AllpDcaZGl_zx_1.png", "Global tracks fit pull in DCAz vs &eta;");
        plot     = section.addPlot("pull_dcaz_global_pt",  "AllpDcaZGl_zy_1.png", "Global tracks fit pull in DCAz vs pT");
        #plot     = section.addPlot("pull_dcaz_global_phi", "AllpDcaZGl_phi_zx_1.png", "Global tracks fit pull in DCAz vs &phi;");
        plot     = section.addPlot("pull_dpsi_global_eta", "AllpPsiGl_zx_1.png", "Global tracks fit pull in &Psi; vs &eta;");
        plot     = section.addPlot("pull_dpsi_global_pt",  "AllpPsiGl_zy_1.png", "Global tracks fit pull in &Psi; vs pT");
        #plot     = section.addPlot("pull_dpsi_global_phi", "AllpPsiGl_phi_zx_1.png", "Global tracks fit pull in &Psi; vs &phi;");
        plot     = section.addPlot("pull_pti_global_eta", "AllpPtiGl_zx_1.png", "Global tracks fit pull in q/pT vs &eta;");
        plot     = section.addPlot("pull_pti_global_pt",  "AllpPtiGl_zy_1.png", "Global tracks fit pull in q/pT vs pT");
        #plot     = section.addPlot("pull_pti_global_phi", "AllpPtiGl_phi_zx_1.png", "Global tracks fit pull in q/pT vs &phi;");
        plot     = section.addPlot("pull_ptr_global_eta", "AllpPtiRGl_zx_1.png", "Global tracks fit pull in relative q/pT vs &eta;");
        plot     = section.addPlot("pull_ptr_global_pt",  "AllpPtiRGl_zy_1.png", "Global tracks fit pull in relative q/pT vs pT");
        #plot     = section.addPlot("pull_ptr_global_phi", "AllpPtiRGl_phi_zx_1.png", "Global tracks fit pull in relative q/pT vs &phi;");
        plot     = section.addPlot("pull_tanl_global_eta", "AllpTanLGl_zx_1.png", "Global tracks fit pull in TAN(&lambda;) vs &eta;");
        plot     = section.addPlot("pull_tanl_global_pt",  "AllpTanLGl_zy_1.png", "Global tracks fit pull in TAN(&lambda;) vs pT");
        #plot     = section.addPlot("pull_tanl_global_phi", "AllpTanLGl_phi_zx_1.png", "Global tracks fit pull in TAN(&lambda;) vs &phi;");

        section  = self.addSection("glPullsPion","Global Pulls for Pion")
        plot     = section.addPlot("pipull_dcaxy_global_eta", "pipDcaXYGl_zx_1.png", "Global tracks fit pull in DCAxy vs &eta;");
        plot     = section.addPlot("pipull_dcaxy_global_pt",  "pipDcaXYGl_zy_1.png", "Global tracks fit pull in DCAxy vs pT");
        #plot     = section.addPlot("pipull_dcaxy_global_phi", "pipDcaXYGl_phi_zx_1.png", "Global tracks fit pull in DCAxy vs &phi;");
        plot     = section.addPlot("pipull_dcaz_global_eta", "pipDcaZGl_zx_1.png", "Global tracks fit pull in DCAz vs &eta;");
        plot     = section.addPlot("pipull_dcaz_global_pt",  "pipDcaZGl_zy_1.png", "Global tracks fit pull in DCAz vs pT");
        #plot     = section.addPlot("pipull_dcaz_global_phi", "pipDcaZGl_phi_zx_1.png", "Global tracks fit pull in DCAz vs &phi;");
        plot     = section.addPlot("pipull_dpsi_global_eta", "pipPsiGl_zx_1.png", "Global tracks fit pull in &Psi; vs &eta;");
        plot     = section.addPlot("pipull_dpsi_global_pt",  "pipPsiGl_zy_1.png", "Global tracks fit pull in &Psi; vs pT");
        #plot     = section.addPlot("pipull_dpsi_global_phi", "pipPsiGl_phi_zx_1.png", "Global tracks fit pull in &Psi; vs &phi;");
        plot     = section.addPlot("pipull_pti_global_eta", "pipPtiGl_zx_1.png", "Global tracks fit pull in q/pT vs &eta;");
        plot     = section.addPlot("pipull_pti_global_pt",  "pipPtiGl_zy_1.png", "Global tracks fit pull in q/pT vs pT");
        #plot     = section.addPlot("pipull_pti_global_phi", "pipPtiGl_phi_zx_1.png", "Global tracks fit pull in q/pT vs &phi;");
        plot     = section.addPlot("pipull_ptr_global_eta", "pipPtiRGl_zx_1.png", "Global tracks fit pull in relative q/pT vs &eta;");
        plot     = section.addPlot("pipull_ptr_global_pt",  "pipPtiRGl_zy_1.png", "Global tracks fit pull in relative q/pT vs pT");
        #plot     = section.addPlot("pipull_ptr_global_phi", "pipPtiRGl_phi_zx_1.png", "Global tracks fit pull in relative q/pT vs &phi;");
        plot     = section.addPlot("pipull_tanl_global_eta", "pipTanLGl_zx_1.png", "Global tracks fit pull in TAN(&lambda;) vs &eta;");
        plot     = section.addPlot("pipull_tanl_global_pt",  "pipTanLGl_zy_1.png", "Global tracks fit pull in TAN(&lambda;) vs pT");
        #plot     = section.addPlot("pipull_tanl_global_phi", "pipTanLGl_phi_zx_1.png", "Global tracks fit pull in TAN(&lambda;) vs &phi;");

        section  = self.addSection("prDevAll","Primary Deviation for All")
        #plot     = section.addPlot("diff_dcaxy_primary_eta", "AlldDcaXYPr_zx_1.png", "Primary tracks fit difference in DCAxy vs &eta;");
        #plot     = section.addPlot("diff_dcaxy_primary_pt",  "AlldDcaXYPr_zy_1.png", "Primary tracks fit difference in DCAxy vs pT");
        #plot     = section.addPlot("diff_dcaxy_primary_phi", "AlldDcaXYPr_phi_zx_1.png", "Primary tracks fit difference in DCAxy vs &phi;");
        #plot     = section.addPlot("diff_dcaz_primary_eta", "AlldDcaZPr_zx_1.png", "Primary tracks fit difference in DCAz vs &eta;");
        #plot     = section.addPlot("diff_dcaz_primary_pt",  "AlldDcaZPr_zy_1.png", "Primary tracks fit difference in DCAz vs pT");
        #plot     = section.addPlot("diff_dcaz_primary_phi", "AlldDcaZPr_phi_zx_1.png", "Primary tracks fit difference in DCAz vs &phi;");
        plot     = section.addPlot("diff_dpsi_primary_eta", "AlldPsiPr_zx_1.png", "Primary tracks fit difference in &Psi; vs &eta;");
        plot     = section.addPlot("diff_dpsi_primary_pt",  "AlldPsiPr_zy_1.png", "Primary tracks fit difference in &Psi; vs pT");
        plot     = section.addPlot("diff_dpsi_primary_phi", "AlldPsiPr_phi_zx_1.png", "Primary tracks fit difference in &Psi; vs &phi;");
        plot     = section.addPlot("diff_pti_primary_eta", "AlldPtiPr_zx_1.png", "Primary tracks fit difference in q/pT vs &eta;");
        plot     = section.addPlot("diff_pti_primary_pt",  "AlldPtiPr_zy_1.png", "Primary tracks fit difference in q/pT vs pT");
        plot     = section.addPlot("diff_pti_primary_phi", "AlldPtiPr_phi_zx_1.png", "Primary tracks fit difference in q/pT vs &phi;");
        plot     = section.addPlot("diff_ptr_primary_eta", "AlldPtiRPr_zx_1.png", "Primary tracks fit difference in relative q/pT vs &eta;");
        plot     = section.addPlot("diff_ptr_primary_pt",  "AlldPtiRPr_zy_1.png", "Primary tracks fit difference in relative q/pT vs pT");
        plot     = section.addPlot("diff_ptr_primary_phi", "AlldPtiRPr_phi_zx_1.png", "Primary tracks fit difference in relative q/pT vs &phi;");
        #plot     = section.addPlot("diff_tanl_primary_eta", "AlldTanLPr_zx_1.png", "Primary tracks fit difference in TAN(&lambda;) vs &eta;");
        #plot     = section.addPlot("diff_tanl_primary_pt",  "AlldTanLPr_zy_1.png", "Primary tracks fit difference in TAN(&lambda;) vs pT");
        #plot     = section.addPlot("diff_tanl_primary_phi", "AlldTanLPr_phi_zx_1.png", "Primary tracks fit difference in TAN(&lambda;) vs &phi;");

        section  = self.addSection("prDevPion","Primary Deviation for Pion")
        #plot     = section.addPlot("pidiff_dcaxy_primary_eta", "pidDcaXYPr_zx_1.png", "Primary tracks fit difference in DCAxy vs &eta;");
        #plot     = section.addPlot("pidiff_dcaxy_primary_pt",  "pidDcaXYPr_zy_1.png", "Primary tracks fit difference in DCAxy vs pT");
        #plot     = section.addPlot("pidiff_dcaxy_primary_phi", "pidDcaXYPr_phi_zx_1.png", "Primary tracks fit difference in DCAxy vs &phi;");
        #plot     = section.addPlot("pidiff_dcaz_primary_eta", "pidDcaZPr_zx_1.png", "Primary tracks fit difference in DCAz vs &eta;");
        #plot     = section.addPlot("pidiff_dcaz_primary_pt",  "pidDcaZPr_zy_1.png", "Primary tracks fit difference in DCAz vs pT");
        #plot     = section.addPlot("pidiff_dcaz_primary_phi", "pidDcaZPr_phi_zx_1.png", "Primary tracks fit difference in DCAz vs &phi;");
        plot     = section.addPlot("pidiff_dpsi_primary_eta", "pidPsiPr_zx_1.png", "Primary tracks fit difference in &Psi; vs &eta;");
        plot     = section.addPlot("pidiff_dpsi_primary_pt",  "pidPsiPr_zy_1.png", "Primary tracks fit difference in &Psi; vs pT");
        plot     = section.addPlot("pidiff_dpsi_primary_phi", "pidPsiPr_phi_zx_1.png", "Primary tracks fit difference in &Psi; vs &phi;");
        plot     = section.addPlot("pidiff_pti_primary_eta", "pidPtiPr_zx_1.png", "Primary tracks fit difference in q/pT vs &eta;");
        plot     = section.addPlot("pidiff_pti_primary_pt",  "pidPtiPr_zy_1.png", "Primary tracks fit difference in q/pT vs pT");
        plot     = section.addPlot("pidiff_pti_primary_phi", "pidPtiPr_phi_zx_1.png", "Primary tracks fit difference in q/pT vs &phi;");
        plot     = section.addPlot("pidiff_ptr_primary_eta", "pidPtiRPr_zx_1.png", "Primary tracks fit difference in relative q/pT vs &eta;");
        plot     = section.addPlot("pidiff_ptr_primary_pt",  "pidPtiRPr_zy_1.png", "Primary tracks fit difference in relative q/pT vs pT");
        plot     = section.addPlot("pidiff_ptr_primary_phi", "pidPtiRPr_phi_zx_1.png", "Primary tracks fit difference in relative q/pT vs &phi;");
        #plot     = section.addPlot("pidiff_tanl_primary_eta", "pidTanLPr_zx_1.png", "Primary tracks fit difference in TAN(&lambda;) vs &eta;");
        #plot     = section.addPlot("pidiff_tanl_primary_pt",  "pidTanLPr_zy_1.png", "Primary tracks fit difference in TAN(&lambda;) vs pT");
        #plot     = section.addPlot("pidiff_tanl_primary_phi", "pidTanLPr_phi_zx_1.png", "Primary tracks fit difference in TAN(&lambda;) vs &phi;");
        
        section  = self.addSection("prPullsNfp","Primary Pulls vs. no. fit points (all or bad)")
        #plot     = section.addPlot("pull_dcaxy_primary_eta", "pDcaXYPr_zx_1.png", "Primary tracks pull in DCAxy vs &eta;");
        #plot     = section.addPlot("pull_dcaxy_primary_pt",  "pDcaXYPr_zy_1.png", "Primary tracks pull in DCAxy vs pT");
        #plot     = section.addPlot("pull_dcaxy_primary_phi", "pDcaXYPr_phi_zx_1.png", "Primary tracks pull in DCAxy vs &phi;");
        #plot     = section.addPlot("pull_dcaz_primary_eta", "pDcaZPr_zx_1.png", "Primary tracks pull in DCAz vs &eta;");
        #plot     = section.addPlot("pull_dcaz_primary_pt",  "pDcaZPr_zy_1.png", "Primary tracks pull in DCAz vs pT");
        #plot     = section.addPlot("pull_dcaz_primary_phi", "pDcaZPr_phi_zx_1.png", "Primary tracks pull in DCAz vs &phi;");
        plot     = section.addPlot("pull_dpsi_primary_eta", "pPsiPr_zx_1.png", "Primary tracks pull in &Psi; vs N fit");
        plot     = section.addPlot("pull_dpsi_primary_pt",  "pPsiPr_zy_1.png", "Primary tracks pull in &Psi; vs N bad");
        #plot     = section.addPlot("pull_dpsi_primary_phi", "pPsiPr_phi_zx_1.png", "Primary tracks pull in &Psi; vs &phi;");
        plot     = section.addPlot("pull_pti_primary_eta", "pPtiPr_zx_1.png", "Primary tracks pull in q/pT vs N fit");
        plot     = section.addPlot("pull_pti_primary_pt",  "pPtiPr_zy_1.png", "Primary tracks pull in q/pT vs N bad");
        #plot     = section.addPlot("pull_pti_primary_phi", "pPtiPr_phi_zx_1.png", "Primary tracks pull in q/pT vs &phi;");
        plot     = section.addPlot("pull_ptr_primary_eta", "pPtiRPr_zx_1.png", "Primary tracks pull in relative q/pT vs N fit");
        plot     = section.addPlot("pull_ptr_primary_pt",  "pPtiRPr_zy_1.png", "Primary tracks pull in relative q/pT vs N bad");
        #plot     = section.addPlot("pull_ptr_primary_phi", "pPtiRPr_phi_zx_1.png", "Primary tracks pull in relative q/pT vs &phi;");
        plot     = section.addPlot("pull_eta_primary_eta", "petaPr_zx_1.png", "Primary tracks pull in &eta; vs N fit");
        plot     = section.addPlot("pull_eta_primary_pt",  "petaPr_zy_1.png", "Primary tracks pull in &eta; vs N bad");
        #plot     = section.addPlot("pull_eta_primary_phi", "petaPr_phi_zx_1.png", "Primary tracks pull in TAN(&lambda;) vs &phi;");

        section  = self.addSection("prPullsAll","Primary Pulls for All")
        #plot     = section.addPlot("pull_dcaxy_primary_eta", "AllpDcaXYPr_zx_1.png", "Primary tracks fit pull in DCAxy vs &eta;");
        #plot     = section.addPlot("pull_dcaxy_primary_pt",  "AllpDcaXYPr_zy_1.png", "Primary tracks fit pull in DCAxy vs pT");
        #plot     = section.addPlot("pull_dcaxy_primary_phi", "AllpDcaXYPr_phi_zx_1.png", "Primary tracks fit pull in DCAxy vs &phi;");
        #plot     = section.addPlot("pull_dcaz_primary_eta", "AllpDcaZPr_zx_1.png", "Primary tracks fit pull in DCAz vs &eta;");
        #plot     = section.addPlot("pull_dcaz_primary_pt",  "AllpDcaZPr_zy_1.png", "Primary tracks fit pull in DCAz vs pT");
        #plot     = section.addPlot("pull_dcaz_primary_phi", "AllpDcaZPr_phi_zx_1.png", "Primary tracks fit pull in DCAz vs &phi;");
        plot     = section.addPlot("pull_dpsi_primary_eta", "AllpPsiPr_zx_1.png", "Primary tracks fit pull in &Psi; vs &eta;");
        plot     = section.addPlot("pull_dpsi_primary_pt",  "AllpPsiPr_zy_1.png", "Primary tracks fit pull in &Psi; vs pT");
        #plot     = section.addPlot("pull_dpsi_primary_phi", "AllpPsiPr_phi_zx_1.png", "Primary tracks fit pull in &Psi; vs &phi;");
        plot     = section.addPlot("pull_pti_primary_eta", "AllpPtiPr_zx_1.png", "Primary tracks fit pull in q/pT vs &eta;");
        plot     = section.addPlot("pull_pti_primary_pt",  "AllpPtiPr_zy_1.png", "Primary tracks fit pull in q/pT vs pT");
        #plot     = section.addPlot("pull_pti_primary_phi", "AllpPtiPr_phi_zx_1.png", "Primary tracks fit pull in q/pT vs &phi;");
        plot     = section.addPlot("pull_ptr_primary_eta", "AllpPtiRPr_zx_1.png", "Primary tracks fit pull in relative q/pT vs &eta;");
        plot     = section.addPlot("pull_ptr_primary_pt",  "AllpPtiRPr_zy_1.png", "Primary tracks fit pull in relative q/pT vs pT");
        #plot     = section.addPlot("pull_ptr_primary_phi", "AllpPtiRPr_phi_zx_1.png", "Primary tracks fit pull in relative q/pT vs &phi;");
        plot     = section.addPlot("pull_eta_primary_eta", "AllpetaPr_zx_1.png", "Primary tracks fit pull in &eta; vs &eta;");
        plot     = section.addPlot("pull_eta_primary_pt",  "AllpetaPr_zy_1.png", "Primary tracks fit pull in &eta; vs pT");
        #plot     = section.addPlot("pull_eta_primary_phi", "AllpetaPr_phi_zx_1.png", "Primary tracks fit pull in TAN(&lambda;) vs &phi;");

        section  = self.addSection("prPullsPion","Primary Pulls for Pion")
        #plot     = section.addPlot("pipull_dcaxy_primary_eta", "pipDcaXYPr_zx_1.png", "Primary tracks fit pull in DCAxy vs &eta;");
        #plot     = section.addPlot("pipull_dcaxy_primary_pt",  "pipDcaXYPr_zy_1.png", "Primary tracks fit pull in DCAxy vs pT");
        #plot     = section.addPlot("pipull_dcaxy_primary_phi", "pipDcaXYPr_phi_zx_1.png", "Primary tracks fit pull in DCAxy vs &phi;");
        #plot     = section.addPlot("pipull_dcaz_primary_eta", "pipDcaZPr_zx_1.png", "Primary tracks fit pull in DCAz vs &eta;");
        #plot     = section.addPlot("pipull_dcaz_primary_pt",  "pipDcaZPr_zy_1.png", "Primary tracks fit pull in DCAz vs pT");
        #plot     = section.addPlot("pipull_dcaz_primary_phi", "pipDcaZPr_phi_zx_1.png", "Primary tracks fit pull in DCAz vs &phi;");
        plot     = section.addPlot("pipull_dpsi_primary_eta", "pipPsiPr_zx_1.png", "Primary tracks fit pull in &Psi; vs &eta;");
        plot     = section.addPlot("pipull_dpsi_primary_pt",  "pipPsiPr_zy_1.png", "Primary tracks fit pull in &Psi; vs pT");
        #plot     = section.addPlot("pipull_dpsi_primary_phi", "pipPsiPr_phi_zx_1.png", "Primary tracks fit pull in &Psi; vs &phi;");
        plot     = section.addPlot("pipull_pti_primary_eta", "pipPtiPr_zx_1.png", "Primary tracks fit pull in q/pT vs &eta;");
        plot     = section.addPlot("pipull_pti_primary_pt",  "pipPtiPr_zy_1.png", "Primary tracks fit pull in q/pT vs pT");
        #plot     = section.addPlot("pipull_pti_primary_phi", "pipPtiPr_phi_zx_1.png", "Primary tracks fit pull in q/pT vs &phi;");
        plot     = section.addPlot("pipull_ptr_primary_eta", "pipPtiRPr_zx_1.png", "Primary tracks fit pull in relative q/pT vs &eta;");
        plot     = section.addPlot("pipull_ptr_primary_pt",  "pipPtiRPr_zy_1.png", "Primary tracks fit pull in relative q/pT vs pT");
        #plot     = section.addPlot("pipull_ptr_primary_phi", "pipPtiRPr_phi_zx_1.png", "Primary tracks fit pull in relative q/pT vs &phi;");
        plot     = section.addPlot("pipull_eta_primary_eta", "pipetaPr_zx_1.png", "Primary tracks fit pull in &eta; vs &eta;");
        plot     = section.addPlot("pipull_eta_primary_pt",  "pipetaPr_zy_1.png", "Primary tracks fit pull in &eta; vs pT");
        #plot     = section.addPlot("pipull_eta_primary_phi", "pipetaPr_phi_zx_1.png", "Primary tracks fit pull in TAN(&lambda;) vs &phi;");

        section  = self.addSection("geomAcceptance","Geometric Acceptance for tracks: N McHit FTS &ge; 7.")
        plot     = section.addPlot("geomAccGlobal_phi",  "GeomAPosMcHitGl_x.png", "Geometric acceptance for global tracks vs &phi; for |&eta;| &lt; 1 and pT &gt; 0.11 GeV" );
        plot     = section.addPlot("geomAccGlobal_eta",  "GeomAPosMcHitGl_y.png", "Geometric acceptance for global tracks vs &eta; for pT &gt; 0.11 GeV" );
        plot     = section.addPlot("geomAccGlobal_z",    "GeomAPosMcHitGl_z.png", "Geometric acceptance for global tracks vs z for |&eta;| &lt; 1 and pT &gt; 0.11 GeV" );
        plot     = section.addPlot("geomAccPrimary_phi", "GeomAPosMcHitPr_x.png", "Geometric acceptance for primary tracks vs &phi; for |&eta;| &lt; 1 and pT &gt; 0.11 GeV" );
        plot     = section.addPlot("geomAccPrimary_eta", "GeomAPosMcHitPr_y.png", "Geometric acceptance for primary tracks vs &eta; for pT &gt; 0.11 GeV" );
        plot     = section.addPlot("geomAccPrimary_z",   "GeomAPosMcHitPr_z.png", "Geometric acceptance for primary tracks vs z for |&eta;| &lt; 1 and pT &gt; 0.11 GeV" );

        section  = self.addSection("EfficiencyAll","Efficiency over all. Only 1 RC track matched to MC track")
        plot     = section.addPlot("effaccGlobal_phi",  "EffAPosMcRecGl_x.png", "Efficiency for global tracks vs &phi; for |&eta;| &lt; 1 and pT &gt; 0.11 GeV" );
        plot     = section.addPlot("effaccGlobal_eta",  "EffAPosMcRecGl_y.png", "Efficiency for global tracks vs &eta; for pT &gt; 0.11 GeV" );
        plot     = section.addPlot("effaccGlobal_z",    "EffAPosMcRecGl_z.png", "Efficiency for global tracks vs z for |&eta;| &lt; 1 and pT &gt; 0.11 GeV" );
        plot     = section.addPlot("effaccPrimary_phi", "EffAPosMcRecPr_x.png", "Efficiency for primary tracks vs &phi; for |&eta;| &lt; 1 and pT &gt; 0.11 GeV" );
        plot     = section.addPlot("effaccPrimary_eta", "EffAPosMcRecPr_y.png", "Efficiency for primary tracks vs &eta; for pT &gt; 0.11 GeV" );
        plot     = section.addPlot("effaccPrimary_z",   "EffAPosMcRecPr_z.png", "Efficiency for primary tracks vs z for |&eta;| &lt; 1 and pT &gt; 0.11 GeV" );

        section  = self.addSection("EfficiencyAllWR","Total efficiency over all (divided by acceptance). Only 1 RC track matched to MC track")
        plot     = section.addPlot("effgaccGlobal_phi",  "EffGPosMcRecGl_x.png", "Efficiency (w/r to geom) for global tracks vs &phi; for |&eta;| &lt; 1 and pT &gt; 0.11 GeV" );
        plot     = section.addPlot("effgaccGlobal_eta",  "EffGPosMcRecGl_y.png", "Efficiency (w/r to geom) for global tracks vs &eta; for pT &gt; 0.11 GeV" );
        plot     = section.addPlot("effgaccGlobal_z",    "EffGPosMcRecGl_z.png", "Efficiency (w/r to geom) for global tracks vs z for |&eta;| &lt; 1 and pT &gt; 0.11 GeV" );
        plot     = section.addPlot("effgaccPrimary_phi", "EffGPosMcRecPr_x.png", "Efficiency (w/r to geom) for primary tracks vs &phi; for |&eta;| &lt; 1 and pT &gt; 0.11 GeV" );
        plot     = section.addPlot("effgaccPrimary_eta", "EffGPosMcRecPr_y.png", "Efficiency (w/r to geom) for primary tracks vs &eta; for pT &gt; 0.11 GeV" );
        plot     = section.addPlot("effgaccPrimary_z",   "EffGPosMcRecPr_z.png", "Efficiency (w/r to geom) for primary tracks vs z for |&eta;| &lt; 1 and pT &gt; 0.11 GeV" );

         
    def addDataset(self,name,title,source):
        ds = Dataset(name,title,source)
        self.datasets.append(ds)

    def addSection(self,name,title):
        section       = Section()
        section.name  = name
        section.title = title
        section.contents = self
        self.sections.append(section)
        return section

    def __str__(self):
        out = '<a name="top" />\n'
        out += '<h1>%s</h1>\n'%self.title
        out += '<i>Webpage generated on %s</i>'%( str(datetime.datetime.now()) )

        if self.abstract:
            out += "<h3>Abstract</h3>\n"
            out += self.abstract

        if self.summary:
            out += "<h3>Summary</h3>\n"
            out += self.summary

        out += """
        <a name="legend" />
        <h3>Legend</h3>
        <p>
        The plots are designed to address the following questions:
        </p>
        <ul>
        <li>What tracks (globals and primaries) can be considered as "good" ones depending on total number of fit points and total number of bad points.
        <li>How do the track parameters and errors depend on the track kinematics, e.g. &phi;, &eta;, 1/pT?
        <li>What are the dependencies of the pulls on the track kinematics?
        <li>Define the geometric acceptance for TPC tracks
        <li>What are the track reconstruction efficiencies for:
        <ul>
        <li>Reconstruction efficiency for tracks which uniquely match one MC (true) track
        <li>Clone rate, i.e. efficiency for tracks which match multiple MC tracks
        <li>Loss rate, i.e. rate of MC tracks with no matched reconstructed track
        <li>Ghost rate, i.e. rate of reconstructed tracks with no matched MC track
        </ul>
        <li>Marker styles: BLACK is positive.  <font color="red">RED is negative.</font>  Circle = mean, Box = sigma.
        </ul>        
        """

        # First, table of contents
        out += '<a name="sections" />'
        out += "<h3>Sections:</h3>\n"
        out += "<ol>\n"
        for section in self.sections:
            out += '<li><a href="#%s" alt="%s">%s</a>\n'%( section.name, section.title, section.title )
        out += "</ol>\n"
                    
        # Each object should contain an anchor to jump back to
        for section in self.sections:
            out += str(section)
        return out

class Dataset:
    def __init__(self,name,title,source):
        self.name = name
        self.title = title
        self.source = source

class Section:
    def __init__(self):
        self.name        = None
        self.title       = None
        self.plots       = [] # List of plots
        self.subsections = _default_subsections # Replicated subsections
        self.contents    = None

    def addPlot(self,name,image,caption=None):
        global _figures
        _figures = _figures + 1
        plot = Plot()
        plot.name = name
        plot.image = image
        plot.caption = caption
        plot.contents = self.contents
        plot.number   = _figures
        plot.section  = self
        self.plots.append(plot)
        return plot

    def addSubsection(self,name):
        self.subsections.append(name)

    def showSubsection(self,sub):
        """
        NOTE:  Not clear what I was thinking about as far as subsections are concerned.
               Different versions of plots probably, but this would be awkward to handle.
        """
        out  = '<hr>\n'
        out += '<a name="%s" />'%(self.name)
        out += '<h2>%s</h2>\n'%(self.title)

        # Show a TOC for the plots
        out += "<ul>\n"
        for plot in self.plots:
            out += '  <li><a href="#%s">'%(plot.name)
            out += "Fig. %i %s</a>\n"%(plot.number,plot.caption)
        out += "</ul>\n"    

        out += '<table width="90%" border="1" cellspacing="2" cellpadding="0">\n'

        # Show the plots
        for plot in self.plots:
            out += "<tr>\n"
            for ds in self.contents.datasets:
                out += '   <td bgcolor="#C0C0C0">%s</td>'%ds.title
            out += "</tr>\n"

            out += str(plot)
        out += '</table>'
        return out

    def __str__(self):
        out = ""
        for sub in self.subsections:
            out += self.showSubsection( sub )
        return out

class Plot:
    def __init(self):
        self.name        = None
        self.caption     = None
        self.image       = None
        self.contents    = None
        self.section     = None
        self.number      = None
    def __str__(self):
        out  = "   <tr>\n"
        for ds in contents.datasets:
            out += '     <td>'
            out += '<a name="%s" />'%(self.name) # anchor for each plot
            out += 'Fig. %i <a href="#%s">&nbsp;[up]&nbsp;</a><a href="#top">&nbsp;[top]&nbsp;</a>&nbsp;<a href="#%s">[link]</a>&nbsp;<br><img src="%s/%s" alt="%s (%s)">     </td>\n'%( self.number, self.section.name, self.name, ds.source, self.image, self.caption, ds.title )
        out += "   </tr>\n"
        out  = out.replace("//","/")
        return out
            

if __name__ == '__main__':    
    #
    # Top level object for webpage creation.  Name and title.  Title will appear on the
    # webpage at the very top.
    #
    contents = Contents("contents","Base QA DEV / Geometry Study ")

    # Add in a short abstract (or not.  If you leave it blank, it won't appear in the page).
    contents.abstract = """
    """

    contents.summary = """    
    <ul>
    <li>Column 1: FTSref6a (beam pipe v2)
    <li>Column 2: SITRver0 (beam pipe v1) | MC points
    <li>Column 3: SITRver0 (beam pipe v1) | Double sided Si w/ 7.5 degree stereo angle
    <li>Column 4: SITRver0 (beam pipe v1) | Double sided Si w/ 80 degree stereo angle
    <li>Column 5: SITRver1 (beam pipe v2) | Double sided Si w/ 7.5 degree stereo angle
    </ul>
    """ 

    #
    # Add in a summary of your observations (or not.  If you leave it blank, ... )
    #
    #contents.summary = None

    # 
    # Add datasets to the webpage.  First argument is a name.  Second a title which will
    # appear as a column heading when the plots are displayed.  Third arguement is the 
    # relative path to the plots.  e.g. one directory down from the location of the
    # html page.
    #

#   dataset = contents.addDataset("FTSref6a", "FTSref6a", "ftsref6a/")
    dataset = contents.addDataset("SITRver0", "SITRver0 MC points", "simpleFast/")
    dataset = contents.addDataset("SITRver0", "SITRver0 Segmented stereo=7.5", "sitrver0/")        
    dataset = contents.addDataset("SITRver0", "SITRver0 Segmented stereo=15", "angle15/")
    dataset = contents.addDataset("SITRver0", "SITRver0 Segmented stereo=30", "angle30/")
    dataset = contents.addDataset("SITRver0", "SITRver0 Segmented stereo=60", "angle60/")    
    dataset = contents.addDataset("SITRver0", "SITRver0 Segmented stereo=80", "angle80/")

#   dataset = contents.addDataset("SITRver1", "SITRver1 Segmented stereo=7.5", "sitrver1/")
#   dataset = contents.addDataset("SITRver1", "SITRver1 Segmented stereo=80", "sitrver1_angle80/")
#   dataset = contents.addDataset("SITRver2", "SITRver2 Segmented stereo=7.5 rotated", "sitrver2/")
#   dataset = contents.addDataset("SITRver2", "SITRver2 Segmented stereo=80  rotated", "sitrver2_angle80/")        

#   dataset = contents.addDataset("SITRver1", "SITRver1 Segmented stereo=7.5 [corrected errs]", "sitrver1_fastsimu2/")
#   dataset = contents.addDataset("SITRver1", "SITRver1 Segmented stereo=80 [corrected errs]", "sitrver1_fastsimu2_angle80/")
#   dataset = contents.addDataset("SITRver2", "SITRver2 Segmented stereo=7.5 rotated [corrected errs] ", "sitrver2_fastsimu2/")
#   dataset = contents.addDataset("SITRver2", "SITRver2 Segmented stereo=80  rotated [corrected errs]", "sitrver2_fastsimu2_angle80/")        

#   dataset = contents.addDataset("SITRver2", "SITRver2 Segmented stereo=7.5 rotated [corrected errs | vtx err] ", "sitrver2_fastsimu2_VtxErr/")
#   dataset = contents.addDataset("SITRver2", "SITRver2 Segmented stereo=80  rotated [corrected errs | vtx err]",  "sitrver2_fastsimu2_VtxErr_angle80/")        






    #
    # Prints contents to standard out (for now... future revisions will be more command line
    # tool like...)
    #
    print contents    






<h1>  TFG (Tracking Focus Group) branch of the offline Software libraries for STAR experiment</h1> 
<p> This repository (TFG)  has been forked from the github "official" STAR software (upsteam git@github.com:star-bnl/star-sw.git)
in order to provide express calibration, express production, and express analysis.
<p>
 TFG software version contains multiple changes with respect to ~official~ one, but most essential modifications are:
<ol>
<li>CA tracker (loopers (2020+)  and Vc 1.4.2),
<li>KFParticle, 
<li>VMC simulation (based on ALICE geant3 version),
<li>ROOT6 (6.27/01) used for TMVA related tasks, and
<li>Support of gcc versions 4.8.5, 6.3.1, 10.3.0, 11.2.0.
<li> ...
</ol>

This repository is located at  git@github.com:fisyak/star-sw.git (as TFG branch).
The list of stable releases can be found TFG_Releases.md


<h2>How to use  TFG release </h2>
<ul>
<li>  <b>starver .DEV2</b>
<li>  <b>source $STAR/setupDEV2.csh</b>
<li>  <b>starver _desired_TFG_version__</b>, from TFG16a  to TFG21n, .DEV2 == TFG is check out HEAD from TFG repository, it could be unstable.
<li>  switch between ROOT5 and ROOT6: 
<ul>
  <li> setup root6
  <li> setup root5
</ul> 
</ul>
<p>
In order to switch back to the "official" STAR software do: 
<ul>
<li><b>source unsetupDEV2.csh</b>
</ul>
<p>
To use macros available in TFG releases we advise you to add in your ~/.rootrc file the next two lines: 

Unix.*.Root.DynamicPath:    :$HOME/macros:$STAR/macros/.$STAR_HOST_SYS:$STAR/macros:$STAR/macros/.$STAR_HOST_SYS:$(LD_LIBRARY_PATH)
Unix.*.Root.MacroPath:      :$HOME/macros:$STAR/macros:$(ROOTSYS)/macros:./StRoot/macros:./StRoot/macros/graphics:./StRoot/macros/analysis:./StRoot/macros/test:./StRoot/macros/examples:./StRoot/macros/html:./StRoot/macros/qa:./StRoot/macros/mudst:$(STAR)/StRoot/macros:$(STAR)/StRoot/macros/graphics:$(STAR)/StRoot/macros/analysis:$(STAR)/StRoot/macros/test:$(STAR)/StRoot/macros/examples:$(STAR)/StRoot
/macros/html:$(STAR)/StRoot/macros/qa:$(STAR)/StRoot/macros/mudst:$(STAR)/StRoot/macros/calib:./StRoot/macros/embedding:$(STAR)/StRoot/macros/embedding:$(ROOTSYS)/macros:$ROOTSYS/tutorials:$(ROOTROOT)/root/tmva/test

or just copy 
<b>cp ~fisyak/.rootrc_noqt ~/.rootrc</b>

<h2>
A tutorial : How to use  KFParticle for analysis can be found at 
https://drupal.star.bnl.gov/STAR/subsys/hlt/kfparticle-tutorial
and Maxsym's presentation 2017 in Wuhan:
https://indico.gsi.de/event/2715/contributions/11364/attachments/8578/10506/KFParticle_Zyzak_TW_13.05.2014.pdf

#! /usr/bin/env perl
use File::Basename;
use Cwd;
#my @particles = qw(pythia);
#my @particles = qw(pionMIP);
#my @particles = qw(electron- muon pion kaon proton+ deuteron triton He3 alpha pionMIP);
#my @particles = qw(pion He3 alpha);
#my @particles = qw(electron- electron+);
#my @particles = qw(muon+ muon- electron- electron+ pion+ pion- kaon+ kaon- proton+ proton- deuteron triton He3 alpha pionMIP);
#my @particles = qw(muon+ muon- electron- electron+ pion+ pion- kaon+ kaon- proton+ proton- deuteron triton He3 alpha);
#my @particles = qw(proton+ pionMIP);
#my @particles = qw(pion+ pion-);
#my @particles = qw(electron-MIP electron+MIP pion+MIP pion-MIP kaon+MIP kaon-MIP proton+MIP proton-MIP deuteronMIP tritonMIP muon+MIP muon-MIP);
#my @particles = qw(tritonMIP muon+MIP muon-MIP);
#my @particles = qw(muon+ muon- electron- electron+ pion+ pion- kaon+ kaon- proton+ proton- deuteron triton He3 alpha); # HE6 Li5 Li6 Li7 Be7 Be9 Be10 B11);
#my @particles = qw(deuteron triton He3 alpha HE6); # Li5 Li6 Li7 Be7 Be9 Be10 B11);
#my @particles = qw(alpha); # Li5 Li6 Li7 Be7 Be9 Be10 B11);
my @particles = qw(muon+ muon- pion+ pion- electron- electron+ kaon+ kaon- proton+ proton- deuteron triton He3 alpha  HE6 Li5 Li6 Li7 Be7 Be9 Be10 B11);
#my @particles = qw(pion+ pion-);
my $i1 =   1;
my $N  =   200; # 400;# 00;
for (my $i = $i1; $i <= $N; $i++) {
  foreach my $part (@particles) {
    #    my $log = $part . "_" . $i . "B.log";
    #    if (! -r $log) {
    my $glob = "./*" . $part . "_" . $i ."*.MuDst.root";
    my @list = glob $glob;# print"$glob => @list\n";
    if ($#list < 0) {
      my $string = "string:" . $part . "_" . $i;
      print "$string\n";
    }
    #    last;
  }
  #  last;
}
__END__
# my @cmd = qw(
# foreach f (muon pion proton+ proton- deuteron triton He3 alpha pionMIP)
# 	    echo ${f}; hadd -T ${f}.root gstar*${f}*0.root
# end
# );
# foreach p (muon+ muon- electron- electron+ pion+ pion- kaon+ kaon- proton+ proton- deuteron triton He3 alpha pionMIP)
#    hadd ${p}.root ../*${p}*.root >& ${p}.log &
# end
#  foreach p (muon electron pion kaon proton deuteron triton He3 alpha )
#    if (! -d ${p})  mkdir ${p}
#    cd ${p}
#    hadd.pl files='../'${p}'*.root'
#    /net/l402/data/fisyak/STAR/packages/.DEV2/scripts/star-submit hadd{p}.xml
#    cd ..
#  end
# cd Fit
#  foreach p (muon electron pion kaon proton deuteron triton He3 alpha )
#     hadd ${p}.root ../${p}/All*.root >& ${p}.log &
#   end
#   foreach p (muon electron pion kaon proton deuteron triton He3 alpha )
#     if (! -d ${p})  mkdir ${p}
#     hadd.pl option=-T files='../'${p}'*0.root'
#     /net/l402/data/fisyak/STAR/packages/.DEV2/scripts/star-submit hadd${p}0.xml
#   end
#    foreach p (muon electron pion kaon proton deuteron triton He3 alpha )
#       hadd ${p}.root All*${p}*.root >& ${p}.log &
#    end
#
#  foreach p (muon electron pion kaon proton deuteron triton He3 alpha HE6 Li5 Li6 Li7 Be7 Be9 Be10 B11)
#    if (! -d ${p})  mkdir ${p}
#    cd ${p}
#    ln -s ../../${p}*.root .
#    hadd.pl option=-T files=${p}'*0.root'
#    /net/l402/data/fisyak/STAR/packages/.DEV2/scripts/star-submit hadd${p}0.xml
#    cd -
#  end
# Link files
 foreach p (muon+ muon- pion+ pion- electron- electron+ kaon+ kaon- proton+ proton- deuteron triton He3 alpha  HE6 Li5 Li6 Li7 Be7 Be9 Be10 B11)
   if (! -d ${p})  mkdir ${p}
   cd ${p}
   ln -s ../../${p}*20.root .
   ln -s ~/macros/.sl* .
   root.exe -q -b 'Chain.C+("*20.root","TpcT")' >& Chain.log &
   cd -
 end
# run AdcTpcT.C  & hadd 
 foreach p (muon+ muon- pion+ pion- electron- electron+ kaon+ kaon- proton+ proton- deuteron triton He3 alpha  HE6 Li5 Li6 Li7 Be7 Be9 Be10 B11)
   ls ${p}/*Cut17.root
   if (! $?) continue
   if (! -d ${p})  mkdir ${p}
   cd ${p}
   ln -s ~/macros/.sl* .
   root.exe -q -b AdcTpcT.C >& AdcTpcT.log &
   hadd.pl option=-T files='*0.root'
   /net/l402/data/fisyak/STAR/packages/.DEV2/scripts/star-submit hadd*.xml
   cd -
 end

# run AdcTpcT.C 
#   ls ${p}/*Cut16.root
#   if (! $?) continue
 foreach p (muon+ muon- pion+ pion- electron- electron+ kaon+ kaon- proton+ proton- deuteron triton He3 alpha  HE6 Li5 Li6 Li7 Be7 Be9 Be10 B11)
   ls ${p}/*Cut16.root
   if (! $?) continue
   if (! -d ${p})  mkdir ${p}
   cd ${p}
   ln -s ~/macros/.sl* .
   root.exe -q -b AdcTpcT.C >& AdcTpcT.log &
   cd - 
 end

# Link files
 foreach p (muon+ muon- pion+ pion- electron- electron+ kaon+ kaon- proton+ proton- deuteron triton He3 alpha  HE6 Li5 Li6 Li7 Be7 Be9 Be10 B11)
   if (! -d ${p})  mkdir ${p}
   cd ${p}
   ln -s ../../${p}*20.root .
   ln -s ~/macros/.sl* .
   root.exe -q -b 'Chain.C+("*20.root","TpcT")' >& Chain.log &
   cd -
 end

# add +/-
foreach p (`ls -1d *- | sed -e 's/-//'`)
     mkdir ${p}
   cd ${p}
   hadd ADCut17.root ../${p}?/ADCut17.root >& ${p}.log &
   cd -
end

# Link files
 foreach p (muon+ muon- pion+ pion- electron- electron+ kaon+ kaon- proton+ proton- deuteron triton He3 alpha  HE6 Li5 Li6 Li7 Be7 Be9 Be10 B11)
   if (! -d ${p})  mkdir ${p}
   cd ${p}
   ln -s ../../${p}*20.root .
   cd -
 end

 foreach p (muon+ muon- pion+ pion- electron- electron+ kaon+ kaon- proton+ proton- deuteron triton He3 alpha  HE6 Li5 Li6 Li7 Be7 Be9 Be10 B11)
   cd ${p}
   ln -s ~/macros/.sl* .
   root.exe -q -b AdcTpcT.C >& AdcTpcT.log &
   cd - 
 end

# sum signs
foreach p (`ls -1d *- | sed -e 's/-//'`)
 mkdir ${p}; 
 cd ${p}
 hadd ADCut23.root ../${p}?/*ADCut23.root
 cd -
 end

# Fit
 foreach p (muon+ muon- muon  pion+ pion- pion electron- electron+ electron  kaon+ kaon- kaon  proton+ proton- proton deuteron triton He3 alpha  HE6 Li5 Li6 Li7 Be7 Be9 Be10 B11)
   ln -s ../TpcRS_2021.COL.100keV/dEdx/${p} ${p}COL   
   ln -s ../TpcRS_2021.FXT.100keV/dEdx/${p} ${p}FXT
 end

fit.pl */ADCut23.root G=

#! /usr/local/bin/tcsh -f
setenv NODEBUG yes
setup 64b
xtitl
foreach f (muon+ muon- electron positron pion+ pion- kaon+ kaon- proton pbar deuteron triton He3 alpha pionMIP)
    hadd ${f}.root gstar*${f}*.root >& ${f}.log &
end
# E O D 

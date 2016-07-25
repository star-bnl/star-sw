#! /usr/local/bin/tcsh -f
#           rcas6011 rcas6012 
#set list = "rcas6013 rcas6014 rcas6015 rcas6016 rcas6017 rcas6018 rcas6019 \
#set list =  "rcas6020 rcas6021 rcas6022 rcas6023 rcas6024 rcas6025 rcas6026 rcas6027"
set list = "rcas6016 rcas6017 rcas6018 rcas6019"
foreach node ($list)
    echo "Doing $node"
    ssh1 -n ${node}.rcf.bnl.gov csh -x ~/bin/roncpy.csh
end
#end of job

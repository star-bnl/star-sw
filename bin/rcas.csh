#! /usr/local/bin/tcsh -f
set list = (#         rcas6001 rcas6002 rcas6003 rcas6004 rcas6005 rcas6006 rcas6007 rcas6008 rcas6009 \
            #rcas6010 rcas6011 rcas6012 rcas6013 rcas6014 rcas6015 rcas6016 rcas6017                   \
                                                                                    rcas6028  rcas6029 \
            rcas6030 rcas6031 rcas6032 rcas6033 rcas6034 rcas6035 rcas6036 rcas6037 rcas6038          \
            rcas6040 rcas6041 rcas6042 rcas6043 rcas6044 rcas6045 rcas6046 rcas6047          rcas6049 \
                                                rcrs6034 rcrs6035 rcrs6036 rcrs6037 rcrs6038 rcrs6039 \
            rcrs6040 rcrs6041 rcrs6042 rcrs6043 rcrs6044 rcrs6045 rcrs6046 rcrs6047 rcrs6048 )
foreach node ( $list ) 
    xt $node
end
# end 

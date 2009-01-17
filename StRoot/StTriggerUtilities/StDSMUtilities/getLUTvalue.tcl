proc getLUTvalue { board patch index } {
    global lutScale lutPed lutSigma lutUseMask lutUsePowerup
    global triggermask1 triggermask2

    set ped [getLUTped $board $patch]
    set nsigma $lutSigma([expr $patch+1],$board)
    set patchLutScale [getLUTscale $board $patch]
    set value [expr ($index - $ped) / $patchLutScale]
    if {$value < 0} then {set value 0}
    if {$value > 62} then {set value 62}
    if {[expr $index - $ped] < $nsigma} then {set value 0}
    return [expr round($value)]
}

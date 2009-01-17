proc getLUTrange { board patch fast } {
    global lutScale lutPed lutSigma lutUseMask lutUsePowerup
    global triggermask1 triggermask2

    set ped [getLUTped $board $patch]
    set patchLutScale [getLUTscale $board $patch]
    set range [expr $patchLutScale * ($ped + 62)]
    if {$range < 78} {set range 78}
    if {$fast == "slow"} {set range 4096}
    return $range
}

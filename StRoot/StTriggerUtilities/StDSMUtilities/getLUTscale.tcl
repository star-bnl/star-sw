proc getLUTscale { board patch } {    global lutScale lutPed lutSigma lutUseMask lutUsePowerup
    global triggermask1 triggermask2

    set patchLutScale $lutScale
    if {$lutUseMask == 1} {
        if {$patch == 0} {set triggerMask $triggermask1($board)} else {set triggerMask $triggermask2($board)}
        set numberOfMaskedChannels 0
        for {set bit 1} {$bit <= 0xffff} {set bit [expr $bit * 2]} {
            if {[expr ($triggerMask & $bit)] == 0} {incr numberOfMaskedChannels}
        }
        if {$numberOfMaskedChannels != 16} {
            set patchLutScale [expr $lutScale * ((16.0 - $numberOfMaskedChannels) / 16.0)]
        } else {
            set patchLutScale 1
        }
    }
    return $patchLutScale
}

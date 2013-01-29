# completion for nodey-tools
function nt_avail {
    reply=(`nodey-tools --quiet list`)
}
compctl -K nt_avail nodey-tools

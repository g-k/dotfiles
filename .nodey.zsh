# completion for nodey-tools
function nt_avail {
    opts=`nodey-tools list --quiet`
    eval "reply=($opts)"
}
compctl -K nt_avail nodey-tools